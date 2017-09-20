## ----results = 'asis', message=F, warning=F------------------------------
library(datarobot)
library(httr)
library(knitr)
library(data.table)

## ----results = 'asis',message=F, warning=F, eval = FALSE-----------------
#  endpoint = 'https://app.datarobot.com/api/v2'
#  apiToken = 'dqmtAG9B7pB7wIuxtmQ81s4BF0mWxZOi'
#  ConnectToDataRobot(endpoint = endpoint, token = apiToken)

## ----echo = FALSE, results = 'asis',message=F, warning=F-----------------
Lending <- fread('lendingClub.csv')
EDA <- t(summary(Lending))
kable(EDA,longtable=TRUE, booktabs=TRUE,row.names = TRUE)

## ----results = 'asis',message=F, warning=F, eval = FALSE-----------------
#  target = "is_bad"
#  projectName = "Credit Scoring"
#  numWorkers = 20
#  
#  set.seed(1111)
#  split <- sample(nrow(Lending), round(0.9*nrow(Lending)), replace = FALSE)
#  train <- Lending[split,]
#  test <- Lending[-split,]
#  
#  project <- SetupProject(dataSource = train,
#                         projectName = projectName)
#  SetTarget(project = project,
#            target = target)

## ----results = 'asis',message=F, warning=F, eval = FALSE-----------------
#  # increase the number of workers used by this project
#  UpdateProject(project = project$projectId,
#                workerCount = numWorkers)
#  WaitForAutopilot(project, verbosity = 1, timeout = 999999)
#  
#  results <- as.data.frame(GetAllModels(project))
#  kable(head(results),longtable=TRUE, booktabs=TRUE,row.names = TRUE)

## ----echo = FALSE, results = 'asis',message=F, warning=F-----------------
results <- readRDS('results.rds')
kable(head(results),longtable=TRUE, booktabs=TRUE,row.names = TRUE)

## ----results = 'asis',message=F, warning=F, eval = FALSE-----------------
#  allModels <- GetAllModels(project)
#  modelFrame <- as.data.frame(allModels)
#  metric <- modelFrame$validationMetric
#  bestIndex <- which.min(metric)
#  bestModel <- allModels[[bestIndex]]
#  dataset <- UploadPredictionDataset(project,test,maxWait=1200)
#  bestPredictJobId <- RequestPredictionsForDataset(project, bestModel$modelId, dataset$id)
#  bestPredictions <- GetPredictions(project, bestPredictJobId,type='probability')
#  testPredictions <- data.frame(original=test$is_bad,prediction=bestPredictions)
#  write.csv(testPredictions,file="testPredictions.csv",row.names=FALSE)
#  kable(head(testPredictions),longtable=TRUE, booktabs=TRUE,row.names = TRUE)

## ----echo = FALSE, results = 'asis',message=F, warning=F-----------------
testPredictions <- readRDS('testPredictions.rds')
kable(head(testPredictions),longtable=TRUE, booktabs=TRUE,row.names = TRUE)

## ----results = 'asis',message=F, warning=F, eval = FALSE-----------------
#  featureImpactJobId <- RequestFeatureImpact(bestModel)
#  featureImpact <- GetFeatureImpactForJobId(project, featureImpactJobId, maxWait=1200)
#  #Print top 10 features
#  kable(featureImpact[1:10,],longtable=TRUE, booktabs=TRUE,row.names = TRUE)

## ----echo = FALSE, results = 'asis',message=F, warning=F-----------------
featureImpact <- readRDS('featureImpact.rds')
kable(featureImpact[1:10,],longtable=TRUE, booktabs=TRUE,row.names = TRUE)

## ----results = 'asis',message=F, warning=F, eval = FALSE-----------------
#  reasonCodeJobID <- RequestReasonCodesInitialization(bestModel)
#  reasonCodeJobIDInitialization <- GetReasonCodesInitializationFromJobId(project,reasonCodeJobID)
#  reasonCodeRequest <- RequestReasonCodes(bestModel, dataset$id, maxCodes = 3, thresholdLow = 0.25, thresholdHigh = 0.75)
#  reasonCodeRequestMetaData <- GetReasonCodesMetadataFromJobId(project, reasonCodeRequest, maxWait = 1800)
#  reasonCodeMetadata <- GetReasonCodesMetadata(project, reasonCodeRequestMetaData$id)
#  reasonCodeAsDataFrame <- GetAllReasonCodesRowsAsDataFrame(project,reasonCodeRequestMetaData$id)
#  reasonCodeAsDataFrame$rowId <- NULL
#  write.csv(reasonCodeAsDataFrame,'reasonCodeAsDataFrame.csv',row.names = FALSE)
#  #subset top 3 and bottom 3 predictions
#  reasonCodeAsDataFrameTopBottom <- rbind(reasonCodeAsDataFrame[order(reasonCodeAsDataFrame$class1Probability),][1:3,],
#                                          reasonCodeAsDataFrame[order(reasonCodeAsDataFrame$class2Probability),][1:3,])
#  kable(head(reasonCodeAsDataFrameTopBottom),longtable=TRUE, booktabs=TRUE,row.names = TRUE)

## ----echo = FALSE, results = 'asis',message=F, warning=F-----------------
reasonCodeAsDataFrameTopBottom <- readRDS('reasonCodeAsDataFrameTopBottom.rds')
kable(head(reasonCodeAsDataFrameTopBottom),longtable=TRUE, booktabs=TRUE,row.names = TRUE)

