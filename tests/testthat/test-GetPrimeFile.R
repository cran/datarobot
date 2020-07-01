context("Test GetPrimeFile")
library(stubthat)
library(testthat)

fakePrimeFileId <- "primefile-id"
primeFiletUrl <- UrlJoin(projectUrl, "primeFile", fakePrimeFileId)
primeFileJson <- fileToChar("responses/primeFile.json")
completedPrimeFileResponse <- httr:::response(url = primeFiletUrl,
                                              status_code = 200L,
                                              content = charToRaw(primeFileJson))

test_that("GetPrimeFile succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedPrimeFileResponse)
  primeFile <- with_mock("httr::GET" = getStub$f,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         GetPrimeFile(fakeProject, fakePrimeFileId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(primeFile, "list")
  expect_is(primeFile$language, "character")
  expect_is(primeFile$isValid, "logical")
  expect_is(primeFile$rulesetId, "integer")
  expect_is(primeFile$parentModelId, "character")
  expect_is(primeFile$projectId, "character")
  expect_is(primeFile$id, "character")
  expect_is(primeFile$modelId, "character")
})
