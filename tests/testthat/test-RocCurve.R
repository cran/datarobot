context("RocCurve")
library(stubthat)
library(testthat)

fakeSource <- "validation"
rocCurveUrl <- UrlJoin(projectUrl, "models", fakeModelId, "rocCurve", fakeSource)
rocCurveAllUrl <- UrlJoin(projectUrl, "models", fakeModelId, "rocCurve")

rocCurveJson <- fileToChar("responses/rocCurve.json")
rocCurveResponse <- httr:::response(url = rocCurveUrl,
                                    status_code = 200L,
                                    content = charToRaw(rocCurveJson))
errorMsg <- jsonlite::toJSON(list(message = list("404 whatever")))
noCurveResponse <- httr:::response(url = rocCurveUrl,
                                   status_code = 404L,
                                   content = charToRaw(errorMsg))
rocCurveAllJson <- fileToChar("responses/rocCurveAll.json")
rocCurveAllResponse <- httr:::response(url = rocCurveAllUrl,
                                       status_code = 200L,
                                       content = charToRaw(rocCurveAllJson))

rocElements <- c("source",
                 "negativeClassPredictions",
                 "rocPoints",
                 "positiveClassPredictions")
rocPointsElements <- c("accuracy", "f1Score", "falseNegativeScore",
                       "trueNegativeScore", "truePositiveScore",
                       "falsePositiveScore", "trueNegativeRate",
                       "falsePositiveRate", "truePositiveRate",
                       "matthewsCorrelationCoefficient", "positivePredictiveValue",
                       "negativePredictiveValue", "threshold",
                       "fractionPredictedAsPositive", "fractionPredictedAsNegative",
                       "liftPositive", "liftNegative")


test_that("GetRocCurve succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(rocCurveResponse)
  rocCurve <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetRocCurve(fakeModel))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(rocCurve, "list")
  ExpectHasKeys(rocCurve, rocElements)
  expect_is(rocCurve$source, "character")
  expect_is(rocCurve$negativeClassPredictions, "numeric")
  expect_is(rocCurve$rocPoints, "data.frame")
  ExpectHasKeys(rocCurve$rocPoints, rocPointsElements)
  expect_is(rocCurve$positiveClassPredictions, "numeric")
})

test_that("GetRocCurve succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noCurveResponse)
  getStub$onCall(2)$returns(rocCurveResponse)
  rocCurve <- with_mock("httr::GET" = getStub$f,
                        "datarobot::GetFrozenModel" = function(...) fakeModel,
                        "datarobot::GetModel" = function(...) fakeModel,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetRocCurve(fakeModel, fallbackToParentInsights = TRUE))
  expect_equal(getStub$calledTimes(), 2)
  expect_is(rocCurve, "list")
  expect_is(rocCurve$source, "character")
  expect_is(rocCurve$negativeClassPredictions, "numeric")
  expect_is(rocCurve$rocPoints, "data.frame")
  expect_is(rocCurve$positiveClassPredictions, "numeric")
})

test_that("GetRocCurve fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noCurveResponse)
  expect_error(with_mock("httr::GET" = getStub$f,
                         "datarobot::GetFrozenModel" = function(...) fakeModel,
                         "datarobot::GetModel" = function(...) fakeModel,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         GetRocCurve(fakeModel, fallbackToParentInsights = FALSE)), "404")
})


test_that("ListRocCurves succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(rocCurveAllResponse)
  rocCurveAll <- with_mock("httr::GET" = getStub$f,
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken,
                           ListRocCurves(fakeModel))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(rocCurveAll, "list")
  expect_is(rocCurveAll$validation, "list")
  expect_is(rocCurveAll$validation$source, "character")
  expect_is(rocCurveAll$validation$negativeClassPredictions, "numeric")
  expect_is(rocCurveAll$validation$rocPoints, "data.frame")
  expect_is(rocCurveAll$validation$positiveClassPredictions, "numeric")
  expect_is(rocCurveAll$crossValidation, "list")
  expect_is(rocCurveAll$crossValidation$source, "character")
  expect_is(rocCurveAll$crossValidation$negativeClassPredictions, "numeric")
  expect_is(rocCurveAll$crossValidation$rocPoints, "data.frame")
  expect_is(rocCurveAll$crossValidation$positiveClassPredictions, "numeric")
  expect_is(rocCurveAll$holdout, "list")
  expect_is(rocCurveAll$holdout$source, "character")
  expect_is(rocCurveAll$holdout$negativeClassPredictions, "numeric")
  expect_is(rocCurveAll$holdout$rocPoints, "data.frame")
  expect_is(rocCurveAll$holdout$positiveClassPredictions, "numeric")
})

test_that("ListLiftCurves succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noCurveResponse)
  getStub$onCall(2)$returns(rocCurveAllResponse)
  rocCurveAll <- with_mock("httr::GET" = getStub$f,
                           "datarobot::GetFrozenModel" = function(...) fakeModel,
                           "datarobot::GetModel" = function(...) fakeModel,
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken,
                           ListRocCurves(fakeModel, fallbackToParentInsights = TRUE))
  expect_equal(getStub$calledTimes(), 2)
  expect_is(rocCurveAll, "list")
})

test_that("ListRocCurves fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noCurveResponse)
  getStub$onCall(2)$returns(rocCurveAllResponse)
  expect_error(with_mock("httr::GET" = getStub$f,
                         "datarobot::GetFrozenModel" = function(...) fakeModel,
                         "datarobot::GetModel" = function(...) fakeModel,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         ListRocCurves(fakeModel, fallbackToParentInsights = FALSE)), "404")
})
