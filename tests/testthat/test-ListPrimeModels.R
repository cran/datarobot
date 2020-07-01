context("Test ListPrimeModels")
library(stubthat)
library(testthat)

primeModelstUrl <- UrlJoin(projectUrl, "primeModels")
primeModelsJson <- fileToChar("responses/primeModels.json")
completedPrimeModelsResponse <- httr:::response(url = primeModelstUrl,
                                                status_code = 200L,
                                                content = charToRaw(primeModelsJson))

test_that("ListPrimeModels succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeModelsResponse)
  primeModels <- with_mock("httr::GET" = getStub$f,
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken,
                           ListPrimeModels(fakeProject))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(primeModels, "data.frame")
  expect_is(primeModels$id, "character")
  expect_is(primeModels$ruleCount, "integer")
  expect_is(primeModels$parentModelId, "character")
  expect_is(primeModels$projectId, "character")
})
