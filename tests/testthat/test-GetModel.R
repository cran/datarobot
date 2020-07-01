context("Test GetModel")
library(stubthat)
library(testthat)

test_that("Required parameters are present", {
  expect_error(GetModel())
  expect_error(GetModel(fakeProject))
  expect_error(GetModel(modelId = modelId))
})

test_that("Message if modelId is blank", {
  expect_error(GetModel(fakeProject, ""))
})


modelJson <- fileToChar("responses/getModel.json")
completedModelResponse <- httr:::response(url = modelUrl,
                                          status_code = 200L,
                                          content = charToRaw(modelJson))
mockedModel <- datarobot:::as.dataRobotModel(fromJSON(modelJson))

test_that("GetModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedModelResponse)
  model <- with_mock("httr::GET" = getStub$f,
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken,
                     "datarobot::GetProject" = function(project) {
                        p <- fromJSON(fileToChar("responses/GetProject.json"))
                        datarobot:::as.dataRobotProject(p)
                      }, GetModel(fakeProject, fakeModelId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(model, "dataRobotModel")
  ExpectHasKeys(model, c("projectId", "projectName", "modelId", "blueprintId",
                         "featurelistId", "isStarred", "predictionThreshold",
                         "predictionThresholdReadOnly"), allowAdditional = TRUE)
  expect_is(model$modelId, "character")
  expect_is(model$projectId, "character")
  expect_is(model$samplePct, "numeric")
  expect_is(model$trainingRowCount, "integer")
  expect_is(model$isStarred, "logical")
  expect_is(model$predictionThreshold, "numeric")
  expect_is(model$predictionThresholdReadOnly, "logical")
  expect_true("LogLoss" %in% names(model$metrics))
  expect_is(model$metrics, "list")
  expect_is(model$metrics$LogLoss, "data.frame")
  expect_true("validation" %in% names(model$metrics$LogLoss))
})


test_that("Zero-length processes element works", {
  modifiedModel <- mockedModel
  modifiedModel$processes <- character(0)
  class(modifiedModel) <- "list"  # Functionally equivalent and necessary for toJSON to work
  modifiedModel <- toJSON(modifiedModel)
  modifiedModel <- gsub("{}", "null", modifiedModel, fixed = TRUE) # Correct JSON coercion
  getStub <- stub(httr::GET)
  completedModelResponse <- httr:::response(url = modelUrl,
                                            status_code = 200L,
                                            content = charToRaw(modifiedModel))
  getStub$onCall(1)$returns(completedModelResponse)
  model <- with_mock("httr::GET" = getStub$f,
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken,
                     "datarobot::GetProject" = function(project) {
                        p <- fromJSON(fileToChar("responses/GetProject.json"))
                        datarobot:::as.dataRobotProject(p)
                      }, GetModel(fakeProject, fakeModelId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(model, "dataRobotModel")
})
