#
#  ComparingSubsetsRDSbuilder.R - RDS file generator for Comparing Subsets vignette
#

#
#  Load the DataRobot package
#

library(datarobot)

#
#  workerLimit default is 2; increase for faster execution of this script
#

workerLimit <- 8

#
#  Get the data for the first example
#

library(mlbench)
data(PimaIndiansDiabetes)

#
#  Create the modified dataset with insulinMissing response variable
#

insulinMissing <- as.numeric(PimaIndiansDiabetes$insulin == 0)
modifiedPima <- PimaIndiansDiabetes
modifiedPima$insulin <- NULL
modifiedPima$insulinMissing <- insulinMissing

#
#  Set up and run the basic insulinMissing project
#

insulinProject <- SetupProject(dataSource = modifiedPima, projectName = "InsulinProject")
SetTarget(insulinProject, "insulinMissing")
UpdateProject(insulinProject, workerCount = workerLimit)
WaitForAutopilot(insulinProject)
insulinModelList <- GetAllModels(insulinProject, verbosity = 0)

#
#  Save insulinModelList as RDS file
#

saveRDS(insulinModelList, "insulinModelList.rds")

#
#  Set up permutation variable importance results for insulinProject
#

write.csv(modifiedPima, "modifiedPima.csv", row.names = FALSE)

#
#  Create the random permutation results using PermuteColumn
#

PermuteColumn <- function(originalFile, colName, permutedFile, iseed = 317){
  #
  set.seed(iseed)
  #
  dframe <- read.csv(originalFile)
  varNames <- colnames(dframe)
  colIndex <- which(varNames == colName)
  x <- dframe[,colIndex]
  y <- sample(x)
  outFrame <- dframe
  outFrame[,colIndex] <- y
  #
  write.csv(outFrame, permutedFile, row.names=FALSE)
}

#
#  Loop to apply permutations to all covariates
#


modelList <- list(n = 9)
modelList[[1]] <- insulinModelList
allVars <- colnames(modifiedPima)[1:8]
permFile <- tempfile(fileext = "permFile.csv")
for (i in 1:8){
  varName <- allVars[i]
  PermuteColumn("modifiedPima.csv", varName, permFile)
  projName <- paste("PermProject",varName,sep="")
  permProject <- SetupProject(permFile, projectName = projName)
  message(projName, "started: awaiting completion.")
  SetTarget(permProject, target = "insulinMissing")
  UpdateProject(permProject, workerCount = workerLimit)
  WaitForAutopilot(permProject, verbosity = 0)
  modelList[[i+1]] <- GetAllModels(permProject)
}

#
#  Clean up - delete temporary file
#

unlink(permFile)

#
#  Given modelList, merge, compute deltas, and save deltas as rds object
#

#
#  Merge function:
#

PermutationMerge <- function(compositeList, matchPct = NULL, metricNames, matchMetric = NULL){
  #
  df <- as.data.frame(compositeList[[1]], simple = FALSE)
  if (is.null(matchPct)){
    index <- seq(1, nrow(df), 1)
  } else {
    index <- which(round(df$samplePct) == matchPct)
  }
  if (is.null(matchMetric)){
    projectMetric <- compositeList[[1]][[1]]$projectMetric
    matchMetric <- paste(projectMetric, "validation", sep = ".")
  }
  getCols <- c("modelType", "expandedModel", "samplePct", "blueprintId", matchMetric)
  outFrame <- df[index, getCols]
  keepCols <- getCols
  keepCols[5] <- metricNames[1]
  colnames(outFrame) <- keepCols
  n <- length(compositeList)
  for (i in 2:n){
    df <- as.data.frame(compositeList[[i]], simple = FALSE)
    if (is.null(matchPct)){
      index <- seq(1, nrow(df), 1)
    } else {
      index <- which(round(df$samplePct) == matchPct)
    }
    upFrame <- df[index, c("blueprintId", matchMetric)]
    colnames(upFrame) <- c("blueprintId", metricNames[i])
    outFrame <- merge(outFrame, upFrame, by = "blueprintId")
  }
  return(outFrame)
}

#
#  Create permutation merge dataframe
#

LogLossNames <- c("originalLogLoss", paste(colnames(modifiedPima)[1:8], "LogLoss", sep=""))
insulinMergeFrame <- PermutationMerge(modelList, metricNames = LogLossNames)

#
#  Compute deltas and save
#

ComputeDeltas <- function(mergeFrame, refCol, permNames, shiftNames){
  #
  allNames <- colnames(mergeFrame)
  refIndex <- which(allNames == refCol)
  xRef <- mergeFrame[,refIndex]
  permCols <- which(allNames %in% permNames)
  xPerm <- mergeFrame[,permCols]
  deltas <- xPerm - xRef
  colnames(deltas) <- shiftNames
  deltas$New <- xRef
  newIndex <- which(colnames(deltas) == "New")
  colnames(deltas)[newIndex] <- refCol
  deltas$modelType <- mergeFrame$modelType
  deltas$expandedModel <- mergeFrame$expandedModel
  return(deltas)
}

allNames <- colnames(insulinMergeFrame)
refCol <- allNames[5]
permNames <- allNames[6:length(allNames)]
shiftNames <- gsub("LogLoss", "", permNames)
insulinDeltaFrame <- ComputeDeltas(insulinMergeFrame, refCol, permNames, shiftNames)


saveRDS(insulinDeltaFrame, "insulinDeltaFrame.rds")

#
#  Create and save AUC shift dataframe
#

AUCnames <- c("originalAUC", paste(colnames(modifiedPima)[1:8], "AUC", sep=""))
insulinAUCmerge <- PermutationMerge(modelList, metricNames = AUCnames, matchMetric = "AUC.validation")

saveRDS(insulinAUCmerge, "AUCshiftFrame.rds")

#
#  Second example - Australian vehicle insurance data
#

#
#  First, get the original data and set up the classification dataset
#

library(insuranceData)
data(dataCar)

lossIndex <- which(dataCar$claimcst0 > 0)
keepVars <- c("veh_value","exposure","claimcst0","veh_body","veh_age","gender","area","agecat")
lossFrame <- subset(dataCar, claimcst0 > 0, select = keepVars)

anomaly <- as.numeric(lossFrame$claimcst0 == 200)
anomFrame <- lossFrame
anomFrame$claimcst0 <- NULL
anomFrame$anomaly <- anomaly

#
#  Set up the basic DataRobot modeling project
#

anomProject <- SetupProject(dataSource = anomFrame, projectName = "AnomalyProject")
SetTarget(anomProject, target = "anomaly")
UpdateProject(insulinProject, workerCount = workerLimit)
WaitForAutopilot(anomProject, verbosity = 0)
anomalyModelList <- GetAllModels(anomProject)

saveRDS(anomalyModelList, "anomalyModelList.rds")

#
#  Construct the AUC-based variable importance results
#

#
#  First, save anomFrame as anomFrame.csv
#

write.csv(anomFrame, "anomFrame.csv", row.names = FALSE)

#
#  Loop to apply permutations to all covariates
#

AUCmodelList <- list(n = 8)
AUCmodelList[[1]] <- anomalyModelList
allVars <- colnames(anomFrame)[1:7]
permFile <- tempfile(fileext = "permFile.csv")
for (i in 1:7){
  varName <- allVars[i]
  PermuteColumn("anomFrame.csv", varName, permFile)
  projName <- paste("PermProject",varName,sep="")
  permProject <- SetupProject(permFile, projectName = projName)
  message(projName, "started: awaiting completion.")
  SetTarget(permProject, target = "anomaly")
  UpdateProject(permProject, workerCount = workerLimit)
  WaitForAutopilot(permProject, verbosity = 0)
  AUCmodelList[[i+1]] <- GetAllModels(permProject)
}

#
#  Clean up - delete temporary file
#

unlink(permFile)

#
#  Create permutation merge dataframe
#

AUCnames <- c("originalAUC", paste(colnames(anomFrame)[1:7], "AUC", sep=""))
anomalyAUCmerge <- PermutationMerge(AUCmodelList, matchPct = 64, metricNames = AUCnames, matchMetric = "AUC.validation")

#
#  Compute deltas and save
#

allNames <- colnames(anomalyAUCmerge)
refCol <- allNames[5]
permNames <- allNames[6:length(allNames)]
shiftNames <- gsub("AUC", "", permNames)
anomAUCDeltaFrame <- ComputeDeltas(anomalyAUCmerge, refCol, permNames, shiftNames)

saveRDS(anomAUCDeltaFrame, "anomAUCDeltaFrame.rds")
