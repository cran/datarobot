#
#  VariableImportanceRDSbuilder.R - RDS file generator for Variable Importance vignette
#

#
#  workerLimit default is 2; increase for faster execution of this script
#

workerLimit <- 8

#
#  First, create the simulation dataset
#

library(mlbench)
set.seed(33)
FriedmanList <- mlbench.friedman1(n = 5000)
df <- data.frame(FriedmanList$x, Y = FriedmanList$y)
write.csv(df, "Friedman1.csv", row.names = FALSE)

#
#  Next, create the original DataRobot project for this example
#

originalProject <- SetupProject("Friedman1.csv", "OriginalProject")
SetTarget(originalProject, target = "Y")
UpdateProject(originalProject, workerCount = workerLimit)
message("Starting original modeling project")

#
#  Retrieve the original models
#

WaitForAutopilot(originalProject, verbosity = 0)
originalModels <- GetAllModels(originalProject)

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
#  Loop to apply permutations to variables X1 through X10
#


modelList <- list(n = 11)
modelList[[1]] <- originalModels
permFile <- tempfile(fileext = "permFile.csv")
for (i in 1:10){
  varName <- paste("X",i,sep="")
  PermuteColumn("Friedman1.csv", varName, permFile)
  projName <- paste("PermProject",varName,sep="")
  permProject <- SetupProject(permFile, projectName = projName)
  message(projName, "started: awaiting completion.")
  SetTarget(permProject, target = "Y")
  UpdateProject(permProject, workerCount = workerLimit)
  WaitForAutopilot(permProject, verbosity = 0)
  modelList[[i+1]] <- GetAllModels(permProject)
}


#
#  Save modelList as rds object - all other code can be run in vignette
#

saveRDS(modelList, "PermutationModelList.rds")

#
#  Clean up - delete temporary file
#

unlink(permFile)

