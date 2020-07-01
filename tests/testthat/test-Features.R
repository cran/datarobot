context("Test Features")
library(testthat)
library(stubthat)

fakeDateFeature <- "fake feature date"
fakeFeatureInt <- 1

test_that("Required parameters are present", {
  expect_error(ListModelFeatures())
})

test_that("With modelId only", {
  expect_error(ListModelFeatures(fakeModel$modelId))
})

test_that("With projectId only", {
  expect_error(ListModelFeatures(fakeModel$projectId))
})


test_that("With non-list Model", {
  expect_error(ListModelFeatures("Not a model"))
})

response <- list(featureNames = c("age", "black", "chas", "crim", "dis",
                                  "indus", "lstat", "medv", "nox", "ptratio",
                                  "rad", "rm", "tax", "zn"))
test_that("Use fakeModel", {
  testReturn <- with_mock(
    "datarobot::DataRobotGET" = function(RouteString, AddURL, ...) response,
    ListModelFeatures(fakeModel)
 )
  expect_equivalent(testReturn, response$featureNames)
})

featuresInfoJson <- '[{"featureType": "Numeric", "lowInformation": false, "name": "feature",
                        "uniqueCount": 200, "importance": 1, "id": 34, "naCount": 0,
                        "mean": 1, "median": 1, "stdDev": 1, "min": 1, "max": 10,
                        "timeSeriesEligible": false,
                        "timeSeriesEligibilityReason": "notADate",
                        "timeStep": null, "timeUnit": null, "targetLeakage": "SKIPPED_DETECTION"}]'
featureInfoJson <- '{"featureType": "Numeric", "lowInformation": false, "name": "feature",
                     "uniqueCount": 200, "importance": 1, "id": 34, "naCount": 0,
                     "mean": 1, "median": 1, "stdDev": 1, "min": 1, "max": 10,
                     "timeSeriesEligible": false,
                     "timeSeriesEligibilityReason": "notADate",
                     "timeStep": null, "timeUnit": null, "targetLeakage": "SKIPPED_DETECTION"}'
dateFeatureInfoJson <- '{"featureType": "Date", "lowInformation": false,
                         "name": "fake feature date", "uniqueCount": 200, "importance": 1,
                         "id": 35, "naCount": 0, "mean": "2015-05-27",
                         "median": "2015-05-29", "stdDev": "200.504 days",
                         "min": "2014-06-10", "max": "2016-05-06",
                         "timeSeriesEligible": true,
                         "timeSeriesEligibilityReason": "suitable",
                         "timeStep": 1, "timeUnit": "DAY", "targetLeakage": "SKIPPED_DETECTION"}'
leakageFeatureInfoJson <- '{"featureType": "Numeric", "lowInformation": true, "name": "feature",
                            "uniqueCount": 200, "importance": 1, "id": 34, "naCount": 0,
                            "mean": 1, "median": 1, "stdDev": 1, "min": 1, "max": 10,
                            "timeSeriesEligible": false,
                            "timeSeriesEligibilityReason": "notADate",
                            "timeStep": null, "timeUnit": null, "targetLeakage": "HIGH_RISK"}'

test_that("ListFeatureInfo returns correct features", {
  getStub <- stub(httr::GET)
  featuresInfoUrl <- UrlJoin(projectUrl, "features")
  featuresInfoResponse <- httr:::response(url = featuresInfoUrl,
                                          status_code = 200L,
                                          content = charToRaw(featuresInfoJson))
  getStub$onCall(1)$returns(featuresInfoResponse)
  featuresInfo <- with_mock("httr::GET" = getStub$f,
                            "datarobot:::Endpoint" = function() fakeEndpoint,
                            "datarobot:::Token" = function() fakeToken,
                            ListFeatureInfo(fakeProject))
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(length(featuresInfo), 1)
  featureInfo <- featuresInfo[[1]]
  expect_equal(featureInfo$id, 34)
  expect_equal(featureInfo$name, "feature")
  expect_equal(featureInfo$featureType, "Numeric")
  expect_equal(featureInfo$importance, 1)
  expect_false(featureInfo$lowInformation)
  expect_equal(featureInfo$uniqueCount, 200)
  expect_equal(featureInfo$naCount, 0)
  expect_equal(featureInfo$mean, 1)
  expect_equal(featureInfo$median, 1)
  expect_equal(featureInfo$stdDev, 1)
  expect_equal(featureInfo$min, 1)
  expect_equal(featureInfo$max, 10)
  expect_false(featureInfo$timeSeriesEligible)
  expect_equal(featureInfo$timeSeriesEligibilityReason, "notADate")
  expect_true(is.null(featureInfo$timeStep))
  expect_true(is.null(featureInfo$timeUnit))
  expect_equal(featureInfo$targetLeakage, "SKIPPED_DETECTION")
})

test_that("GetFeatureInfo returns correct feature with leakage", {
  getStub <- stub(httr::GET)
  featureInfoUrl <- UrlJoin(projectUrl, "features", leakageFeatureInfoJson)
  featureInfoResponse <- httr:::response(url = featureInfoUrl,
                                         status_code = 200L,
                                         content = charToRaw(leakageFeatureInfoJson))
  getStub$onCall(1)$returns(featureInfoResponse)
  featureInfo <- with_mock("httr::GET" = getStub$f,
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken,
                           GetFeatureInfo(fakeProject, leakageFeatureInfoJson))
  expect_true(featureInfo$lowInformation)
  expect_equal(featureInfo$targetLeakage, "HIGH_RISK")
})

test_that("GetFeatureInfo returns correct feature", {
  getStub <- stub(httr::GET)
  featureInfoUrl <- UrlJoin(projectUrl, "features", fakeFeature)
  featureInfoResponse <- httr:::response(url = featureInfoUrl,
                                         status_code = 200L,
                                         content = charToRaw(featureInfoJson))
  getStub$onCall(1)$returns(featureInfoResponse)
  featureInfo <- with_mock("httr::GET" = getStub$f,
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken,
                           GetFeatureInfo(fakeProject, fakeFeature))
  expect_equal(featureInfo$id, 34)
  expect_equal(featureInfo$name, "feature")
  expect_equal(featureInfo$featureType, "Numeric")
  expect_equal(featureInfo$importance, 1)
  expect_false(featureInfo$lowInformation)
  expect_equal(featureInfo$uniqueCount, 200)
  expect_equal(featureInfo$naCount, 0)
  expect_equal(featureInfo$mean, 1)
  expect_equal(featureInfo$median, 1)
  expect_equal(featureInfo$stdDev, 1)
  expect_equal(featureInfo$min, 1)
  expect_equal(featureInfo$max, 10)
  expect_false(featureInfo$timeSeriesEligible)
  expect_equal(featureInfo$timeSeriesEligibilityReason, "notADate")
  expect_true(is.null(featureInfo$timeStep))
  expect_true(is.null(featureInfo$timeUnit))
  expect_equal(featureInfo$targetLeakage, "SKIPPED_DETECTION")
})

test_that("GetFeatureInfo works on a date", {
  getStub <- stub(httr::GET)
  featureInfoUrl <- UrlJoin(projectUrl, "features", fakeFeature)
  featureInfoResponse <- httr:::response(url = featureInfoUrl,
                                         status_code = 200L,
                                         content = charToRaw(dateFeatureInfoJson))
  getStub$onCall(1)$returns(featureInfoResponse)
  featureInfo <- with_mock("httr::GET" = getStub$f,
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken,
                           GetFeatureInfo(fakeProject, fakeDateFeature))
  expect_equal(featureInfo$id, 35)
  expect_equal(featureInfo$name, "fake feature date")
  expect_equal(featureInfo$featureType, "Date")
  expect_equal(featureInfo$importance, 1)
  expect_false(featureInfo$lowInformation)
  expect_equal(featureInfo$uniqueCount, 200)
  expect_equal(featureInfo$naCount, 0)
  expect_equal(featureInfo$mean, "2015-05-27")
  expect_equal(featureInfo$median, "2015-05-29")
  expect_equal(featureInfo$stdDev, "200.504 days")
  expect_equal(featureInfo$min, "2014-06-10")
  expect_equal(featureInfo$max, "2016-05-06")
  expect_true(featureInfo$timeSeriesEligible)
  expect_equal(featureInfo$timeSeriesEligibilityReason, "suitable")
  expect_equal(featureInfo$timeStep, 1)
  expect_equal(featureInfo$timeUnit, "DAY")
  expect_equal(featureInfo$targetLeakage, "SKIPPED_DETECTION")
})


test_that("GetFeatureHistogram works", {
  getStub <- stub(httr::GET)
  featureHistogramUrl <- UrlJoin(projectUrl, "featureHistograms", fakeFeature)
  featureHistogramJson <- fileToChar("responses/GetFeatureHistogram.json")
  featureHistogramResponse <- httr:::response(url = featureHistogramUrl,
                                              status_code = 200L,
                                              content = charToRaw(featureHistogramJson))
  getStub$onCall(1)$returns(featureHistogramResponse)
  histogram <- with_mock("httr::GET" = getStub$f,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         GetFeatureHistogram(fakeProject, fakeDateFeature))
  expect_is(histogram, "list")
  expect_equal(length(histogram), 58)
  expect_is(histogram[[1]], "list")
  ExpectHasKeys(histogram[[1]], c("count", "target", "label"))
  expect_is(histogram[[1]]$count, "integer")
  expect_true(is.null(histogram[[1]]$target))
  expect_is(histogram[[1]]$label, "character")
})

test_that("GetFeatureHistogram works with binLimit", {
  getStub <- stub(httr::GET)
  featureHistogramUrl <- UrlJoin(projectUrl, "featureHistograms", fakeFeature)
  featureHistogramJson <- fileToChar("responses/GetFeatureHistogram.json")
  featureHistogramResponse <- httr:::response(url = featureHistogramUrl,
                                              status_code = 200L,
                                              content = charToRaw(featureHistogramJson))
  getStub$onCall(1)$returns(featureHistogramResponse)
  histogram <- with_mock("httr::GET" = getStub$f,
                         "datarobot:::DataRobotGET" = function(routeString,
                                                               addUrl = TRUE,
                                                               query = NULL,
                                                               returnRawResponse = FALSE, ...) {
                            queryForInspect <<- query
                            datarobot:::MakeDataRobotRequest(httr::GET, routeString,
                                                             addUrl = addUrl,
                                                             returnRawResponse = returnRawResponse,
                                                             query = query, ...)
                        },
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         GetFeatureHistogram(fakeProject, fakeDateFeature, binLimit = 2))
  expect_is(histogram, "list")
  expect_equal(length(histogram), 58)
  expect_is(histogram[[1]], "list")
  ExpectHasKeys(histogram[[1]], c("count", "target", "label"))
  expect_is(histogram[[1]]$count, "integer")
  expect_true(is.null(histogram[[1]]$target))
  expect_is(histogram[[1]]$label, "character")
  expect_equal(queryForInspect$binLimit, 2)
})


describe("BatchFeaturesTypeTransform", {
  test_that("can feature type transform one feature", {
    getStub <- stub(httr::GET)
    featuresInfoUrl <- UrlJoin(projectUrl, "features")
    featuresInfoResponse <- httr:::response(url = featuresInfoUrl,
                                            status_code = 200L,
                                            content = charToRaw(featuresInfoJson))
    getStub$onCall(1)$returns(featuresInfoResponse)
    featuresInfo <- with_mock("httr::GET" = getStub$f,
                              "datarobot:::DataRobotPOST" = function(routeString,
                                                                   addUrl = TRUE,
                                                                   body = NULL,
                                                                   returnRawResponse = FALSE, ...) {
                                 bodyForInspect <<- body
                                 "NOOP"
                              },
                              "datarobot:::Endpoint" = function() fakeEndpoint,
                              "datarobot:::Token" = function() fakeToken,
                              "datarobot:::WaitForAsyncReturn" = function(...) { "NOOP" },
                              BatchFeaturesTypeTransform(fakeProject, "var",
                                                         "categorical", "transform"))
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(length(featuresInfo), 1)
    featureInfo <- featuresInfo[[1]]
    expect_equal(featureInfo$id, 34)
    expect_equal(as.character(bodyForInspect$parentNames[[1]]), "var")
    expect_equal(as.character(bodyForInspect$variableType), "categorical")
    expect_equal(as.character(bodyForInspect$prefix), "transform")
  })

  test_that("can feature type transform two features", {
    getStub <- stub(httr::GET)
    featuresInfoUrl <- UrlJoin(projectUrl, "features")
    featuresInfoResponse <- httr:::response(url = featuresInfoUrl,
                                            status_code = 200L,
                                            content = charToRaw(featuresInfoJson))
    getStub$onCall(1)$returns(featuresInfoResponse)
    featuresInfo <- with_mock("httr::GET" = getStub$f,
                              "datarobot:::DataRobotPOST" = function(routeString,
                                                                   addUrl = TRUE,
                                                                   body = NULL,
                                                                   returnRawResponse = FALSE, ...) {
                                 bodyForInspect <<- body
                                 "NOOP"
                              },
                              "datarobot:::Endpoint" = function() fakeEndpoint,
                              "datarobot:::Token" = function() fakeToken,
                              "datarobot:::WaitForAsyncReturn" = function(...) { "NOOP" },
                              BatchFeaturesTypeTransform(fakeProject, c("var1", "var2"),
                                                         "categorical", "transform"))
    expect_equal(getStub$calledTimes(), 1)
    expect_equal(length(featuresInfo), 1)
    featureInfo <- featuresInfo[[1]]
    expect_equal(featureInfo$id, 34)
    expect_equal(as.character(bodyForInspect$parentNames[[1]]), "var1")
    expect_equal(as.character(bodyForInspect$parentNames[[2]]), "var2")
    expect_equal(as.character(bodyForInspect$variableType), "categorical")
    expect_equal(as.character(bodyForInspect$prefix), "transform")
  })
})
