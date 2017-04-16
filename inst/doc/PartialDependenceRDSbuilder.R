#
#  PartialDependenceRDSbuilder.R - create RDS files for Partial Dependence vignette
#

#
#  workerLimit default is 2; increase for faster execution of this script
#

workerLimit <- 8

#
#  First, read the concrete dataset
#

concreteFrame <- read.csv("concreteData.csv")

#
#  Next, set up & run the concrete modeling project
#

library(datarobot)
myDRproject <- SetupProject(concreteFrame, "ConcreteProject")
SetTarget(myDRproject, target = "strength")
UpdateProject(myDRproject, workerCount = workerLimit)
WaitForAutopilot(myDRproject, verbosity = 0)
concreteModels <- GetAllModels(myDRproject)
saveRDS(concreteModels, "concreteModels.rds")


#
#  All of the partial dependence plots are based on models from this project
#  To generate the results required to construct these plots, a modified
#  dataset is needed; the following function generates this modified dataset:
#

FullAverageDataset <- function(covarFrame, refCovar, numGrid, plotRange = NULL){
  #
  covars <- colnames(covarFrame)
  refIndex <- which(covars == refCovar)
  refVar <- covarFrame[, refIndex]
  if (is.null(plotRange)){
    start <- min(refVar)
    end <- max(refVar)
  } else {
    start <- plotRange[1]
    end <- plotRange[2]
  }
  grid <- seq(start, end, length = numGrid)
  #
  outFrame <- covarFrame
  outFrame[, refIndex] <- grid[1]
  for (i in 2:numGrid){
    upFrame <- covarFrame
    upFrame[, refIndex] <- grid[i]
    outFrame <- rbind.data.frame(outFrame, upFrame)
  }
  return(outFrame)
}

#
#  The following function takes a list of selected models and a specified covariate
#  and generates the PDP functions for all models in the list
#

PDPbuilder <- function(covarFrame, refCovar, listOfModels,
                       numGrid = 100, plotRange = NULL){
  #
  augmentedFrame <- FullAverageDataset(covarFrame, refCovar,
                                       numGrid, plotRange)
  #
  nModels <- length(listOfModels)
  library(doBy)
  model <- listOfModels[[1]]
  predictJobId <- RequestPredictions(model, newdata = augmentedFrame)
  projectId <- model$projectId
  yHat <- GetPredictions(projectId, predictJobId)
  hatFrame <- augmentedFrame
  hatFrame$prediction <- yHat
  hatSum <- summaryBy(list(c("prediction"), c(refCovar)), data = hatFrame, FUN = mean)
  colnames(hatSum)[2] <- model$modelType
  #
  for (i in 2:nModels){
    model <- listOfModels[[i]]
    predictJobId <- RequestPredictions(model, newdata = augmentedFrame)
    projectId <- model$projectId
    yHat <- GetPredictions(projectId, predictJobId)
    hatFrame <- augmentedFrame
    hatFrame$prediction <- yHat
    upSum <- summaryBy(list(c("prediction"), c(refCovar)), data = hatFrame, FUN = mean)
    colnames(upSum)[2] <- model$modelType
    hatSum <- merge(hatSum, upSum)
  }
  return(hatSum)
}

#
#  Define the list of models to be used here:
#

listOfModels <- list(concreteModels[[1]], concreteModels[[5]],
                     concreteModels[[12]], concreteModels[[29]])

#
#  Compute the PDP results for the age covariate and save as RDS file
#

covarFrame <- concreteFrame[, 1:8]
agePDPframe <- PDPbuilder(covarFrame, "age", listOfModels)
saveRDS(agePDPframe, "agePDPframe.rds")

#
#  Compute the PDP results for the cement covariate and save as RDS file
#

cementPDPframe <- PDPbuilder(covarFrame, "cement", listOfModels)
saveRDS(cementPDPframe, "cementPDPframe.rds")

#
#  Compute the PDP results for the water covariate and save as RDS file
#

waterPDPframe <- PDPbuilder(covarFrame, "water", listOfModels)
saveRDS(waterPDPframe, "waterPDPframe.rds")

#
#  Compute the PDP results for the blastFurnaceSlag covariate and save as RDS file
#

blastPDPframe <- PDPbuilder(covarFrame, "blastFurnaceSlag", listOfModels)
saveRDS(blastPDPframe, "blastPDPframe.rds")
