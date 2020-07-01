context("Test GetBlenderModel")
library(stubthat)
library(testthat)

blenderModelUrl <- UrlJoin(projectUrl, "blenderModels", fakeModelId)
blenderModelJson <- fileToChar("responses/blendModel.json")
completedBlenderModelResponse <- httr:::response(url = blenderModelUrl,
                                                 status_code = 200L,
                                                 content = charToRaw(blenderModelJson))

test_that("Required parameters are present", {
  expect_error(GetBlenderModel())
  expect_error(GetBlenderModel(fakeProject))
  expect_error(GetBlenderModel(fakeProjectId))
  expect_error(GetBlenderModel(modelId = fakeModelId))
})

test_that("GetBlendModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedBlenderModelResponse)
  blendModel <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          "datarobot::GetProject" = function(project) {
                            p <- fromJSON(fileToChar("responses/GetProject.json"))
                            datarobot:::as.dataRobotProject(p)
                          }, GetBlenderModel(fakeProject, fakeModelId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(blendModel, "dataRobotBlenderModel")
  expect_is(blendModel$modelId, "character")
  expect_is(blendModel$samplePct, "numeric")
  expect_is(blendModel$trainingRowCount, "integer")
  expect_is(blendModel$modelIds, "character")
  expect_is(blendModel$projectId, "character")
})
