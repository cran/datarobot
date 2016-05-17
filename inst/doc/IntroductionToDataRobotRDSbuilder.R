#
#  IntroductionToDataRobotRDSbuilder.R - RDS file generator for Introduction to DataRobot vignette
#

#
#  Load the required R packages
#

library(datarobot)
require(MASS)

#
#  Execute SetupProject to create the new DataRobot project and save the results
#

projectObject <- SetupProject(dataSource = Boston, projectName = "BostonVignetteProject")
saveRDS(projectObject, "projectObject.rds")

#
#  Start the autopilot and increase the worker count to 8 to speed execution

SetTarget(project = projectObject, target = "medv")

#
#  Wait until Autopilot is finished before requesting models
#

WaitForAutopilot(projectObject, verbosity = 0)

#
#  Retrieve and save the models
#

listOfBostonModels <- GetAllModels(projectObject)
saveRDS(listOfBostonModels, "listOfBostonModels.rds")

#
#  Generate and save the predictions from the best model
#

modelFrame <- as.data.frame(listOfBostonModels)
metric <- modelFrame$validationMetric
bestIndex <- which.min(metric)
bestModel <- listOfBostonModels[[bestIndex]]
bestPredictJobId <- RequestPredictions(bestModel, newdata = Boston)
bestPredictions <- GetPredictions(projectObject, bestPredictJobId)
saveRDS(bestPredictions, "bestPredictions.rds")
