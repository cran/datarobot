## ----results = 'asis', message=F, warning=F------------------------------
library(datarobot)
library(httr)
library(knitr)
library(data.table)

## ----results = 'asis', message = FALSE, warning = FALSE, eval = FALSE----
#  endpoint <- "https://<YOUR ENDPOINT HERE>/api/v2"
#  apiToken <- "<YOUR API TOKEN HERE>"
#  ConnectToDataRobot(endpoint = endpoint, token = apiToken)

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE----
Lending <- fread("lendingClub.csv")
EDA <- t(summary(Lending))
kable(EDA, longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE----
#  target <- "is_bad"
#  projectName <- "Credit Scoring"
#  numWorkers <- 10
#  
#  set.seed(1111)
#  split <- sample(nrow(Lending), round(0.9 * nrow(Lending)), replace = FALSE)
#  train <- Lending[split,]
#  test <- Lending[-split,]
#  
#  project <- SetupProject(dataSource = train,
#                         projectName = projectName)
#  SetTarget(project = project,
#            target = target)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE----
#  # increase the number of workers used by this project
#  UpdateProject(project = project$projectId,
#                workerCount = numWorkers)
#  WaitForAutopilot(project, verbosity = 1, timeout = 999999)
#  
#  results <- as.data.frame(ListModels(project))
#  kable(head(results), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE----
results <- readRDS("results.rds")
kable(head(results), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE----
#  allModels <- ListModels(project)
#  modelFrame <- as.data.frame(allModels)
#  metric <- modelFrame$validationMetric
#  bestIndex <- which.min(metric)
#  bestModel <- allModels[[bestIndex]]
#  dataset <- UploadPredictionDataset(project, test, maxWait = 1200)
#  bestPredictJobId <- RequestPredictionsForDataset(project, bestModel$modelId, dataset$id)
#  bestPredictions <- GetPredictions(project, bestPredictJobId, type="probability")
#  testPredictions <- data.frame(original = test$is_bad, prediction = bestPredictions)
#  kable(head(testPredictions), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE----
testPredictions <- readRDS("testPredictions.rds")
kable(head(testPredictions), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE----
#  featureImpactJobId <- RequestFeatureImpact(bestModel)
#  featureImpact <- GetFeatureImpactForJobId(project, featureImpactJobId, maxWait = 1200)
#  #Print top 10 features
#  kable(featureImpact[1:10,], longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----echo = FALSE, results = 'asis',message=F, warning=F-----------------
featureImpact <- readRDS('featureImpact.rds')
kable(featureImpact[1:10,], longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE----
#  # Calculate reason codes
#  reasonCodeJobID <- RequestReasonCodesInitialization(bestModel)
#  reasonCodeJobIDInitialization <- GetReasonCodesInitializationFromJobId(project,reasonCodeJobID)
#  # Calculate reason codes for our dataset
#  reasonCodeRequest <- RequestReasonCodes(bestModel, dataset$id, maxCodes = 3, thresholdLow = 0.25, thresholdHigh = 0.75)
#  # Get the reason codes we calculated
#  reasonCodeRequestMetaData <- GetReasonCodesMetadataFromJobId(project, reasonCodeRequest, maxWait = 1800)
#  reasonCodeMetadata <- GetReasonCodesMetadata(project, reasonCodeRequestMetaData$id)
#  reasonCodeAsDataFrame <- GetAllReasonCodesRowsAsDataFrame(project, reasonCodeRequestMetaData$id)
#  reasonCodeAsDataFrame$rowId <- NULL
#  #subset top 3 and bottom 3 predictions
#  reasonCodeAsDataFrameTopBottom <- rbind(reasonCodeAsDataFrame[order(reasonCodeAsDataFrame$class1Probability),][1:3,],
#                                          reasonCodeAsDataFrame[order(reasonCodeAsDataFrame$class2Probability),][1:3,])
#  kable(head(reasonCodeAsDataFrameTopBottom), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE----
reasonCodeAsDataFrameTopBottom <- readRDS("reasonCodeAsDataFrameTopBottom.rds")
kable(head(reasonCodeAsDataFrameTopBottom), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE----
#  reasonCodeAsDataFrameWithExposure <- GetAllReasonCodesRowsAsDataFrame(project, reasonCodeRequestMetaData$id, excludeAdjustedPredictions = FALSE)
#  kable(head(reasonCodeAsDataFrameWithExposure), longtable = TRUE, booktabs = TRUE, row.names = TRUE)
#  

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE----
reasonCodeAsDataFrameWithExposure <- readRDS("reasonCodeAsDataFrameWithExposure.rds")
kable(head(reasonCodeAsDataFrameWithExposure), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

