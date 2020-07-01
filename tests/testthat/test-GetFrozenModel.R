context("Test GetFrozenModel")
library(stubthat)
library(testthat)

frozenModeltUrl <- UrlJoin(projectUrl, "frozenModels", fakeModelId)

test_that("GetFrozenModel succeeds", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  frozenModelJson <- fileToChar("responses/frozenModel.json")
  completedFrozenModelResponse <- httr:::response(url = frozenModeltUrl,
                                                  status_code = 200L,
                                                  content = charToRaw(frozenModelJson))
  getStub$onCall(2)$returns(completedFrozenModelResponse)
  frozenModel <- with_mock("httr::GET" = getStub$f,
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken,
                           GetFrozenModel(fakeProject, fakeModelId))
  expect_equal(getStub$calledTimes(), 2)
  expect_is(frozenModel, "dataRobotFrozenModel")
  expect_is(frozenModel$modelId, "character")
  expect_equal(frozenModel$isFrozen, TRUE)
  expect_is(frozenModel$parentModelId, "character")
  expect_is(frozenModel$projectId, "character")
})
