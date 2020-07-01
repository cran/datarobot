context("Test GetPrimeModel")
library(stubthat)
library(testthat)

primeModeltUrl <- UrlJoin(projectUrl, "primeModels", fakeModelId)
primeModelJson <- fileToChar("responses/primeModel.json")
completedPrimeModelResponse <- httr:::response(url = primeModeltUrl,
                                               status_code = 200L,
                                               content = charToRaw(primeModelJson))

test_that("GetPrimeModel succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeModelResponse)
  primeModel <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetPrimeModel(fakeProjectId, fakeModelId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(primeModel, "dataRobotPrimeModel")
  expect_is(primeModel$id, "character")
  expect_is(primeModel$ruleCount, "integer")
  expect_is(primeModel$parentModelId, "character")
  expect_is(primeModel$projectId, "character")
})
