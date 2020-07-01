context("Test RequestNewModel")
library(stubthat)
library(testthat)

requestResponse <- httr:::response(url = modelUrl,
                                   status_code = 202L,
                                   headers = list(location = modelUrl),
                                   content = raw(0))

test_that("CrossValidateModel succeeds", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock("httr::POST" = postStub$f,
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken, {
                        expect_message({ jobId <- CrossValidateModel(fakeModel) },
                                       "Cross validation")
                        jobId
                      })
  expect_equal(postStub$calledTimes(), 1)
  expect_is(jobId, "character")
})

test_that("CrossValidateModel is not implemented for Prime models", {
  expect_error(CrossValidateModel(fakePrimeModel), "not implemented")
})

test_that("CrossValidateModel is not implemented for Datetime models", {
  expect_error(CrossValidateModel(fakeDatetimeModel), "not implemented")
})


describe("GetCrossValidationScores", {
    getCVUrl <- UrlJoin(projectUrl, "models", fakeModelId, "crossValidationScores")
    getCVJson <- fileToChar("responses/GetCrossValidationScores.json")
    CVResponse <- httr:::response(url = getCVUrl,
                                  status_code = 200L,
                                  content = charToRaw(getCVJson))

  test_that("it works", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(CVResponse)
    cvScores <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetCrossValidationScores(fakeModel))
    expect_equal(getStub$calledTimes(), 1)
    expect_is(cvScores, "list")
    ExpectHasKeys(cvScores, "RMSE", allowAdditional = TRUE)
    expect_is(cvScores$RMSE, "list")
    ExpectHasKeys(cvScores$RMSE, c("0.0", "1.0", "2.0", "3.0", "4.0"))
    expect_is(cvScores$RMSE[["4.0"]], "numeric")
  })

  test_that("GetCrossValidationScores filter by metric", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(CVResponse)
    cvScores <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetCrossValidationScores(fakeModel))
    expect_equal(getStub$calledTimes(), 1)
    expect_is(cvScores, "list")
    expect_is(cvScores$RMSE, "list")
    ExpectHasKeys(cvScores$RMSE, c("0.0", "1.0", "2.0", "3.0", "4.0"))
  })

  test_that("GetCrossValidationScores filter by fold", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(CVResponse)
    cvScores <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetCrossValidationScores(fakeModel))
    expect_equal(getStub$calledTimes(), 1)
    expect_is(cvScores, "list")
    expect_is(cvScores$RMSE, "list")
  })

  test_that("GetCrossValidationScores filter by fold and metric", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(CVResponse)
    cvScores <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetCrossValidationScores(fakeModel))
    expect_equal(getStub$calledTimes(), 1)
    expect_is(cvScores, "list")
    expect_is(cvScores$RMSE, "list")
  })
})
