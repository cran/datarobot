context("LiftChart")
library(stubthat)
library(testthat)

fakeSource <- "validation"
liftChartUrl <- UrlJoin(projectUrl, "models", fakeModelId, "liftChart", fakeSource)
liftChartAllUrl <- UrlJoin(projectUrl, "models", fakeModelId, "liftChart")

liftChartJson <- fileToChar("responses/liftChart.json")
liftChartResponse <- httr:::response(url = liftChartUrl,
                                     status_code = 200L,
                                     content = charToRaw(liftChartJson))
errorMsg <- jsonlite::toJSON(list(message = list("404 whatever")))
noChartResponse <- httr:::response(url = liftChartUrl,
                                   status_code = 404L,
                                   content = charToRaw(errorMsg))
liftChartAllJson <- fileToChar("responses/liftChartAll.json")
liftChartAllResponse <- httr:::response(url = liftChartAllUrl,
                                        status_code = 200L,
                                        content = charToRaw(liftChartAllJson))


test_that("GetLiftChart succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(liftChartResponse)
  liftChart <- with_mock("httr::GET" = getStub$f,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         GetLiftChart(fakeModel))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(liftChart, "data.frame")
  expect_is(liftChart$binWeight, "numeric")
  expect_is(liftChart$actual, "numeric")
  expect_is(liftChart$predicted, "numeric")
})

test_that("GetLiftChart succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(liftChartResponse)
  liftChart <- with_mock("httr::GET" = getStub$f,
                         "datarobot::GetFrozenModel" = function(...) fakeModel,
                         "datarobot::GetModel" = function(...) fakeModel,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         GetLiftChart(fakeModel, fallbackToParentInsights = TRUE))
  expect_equal(getStub$calledTimes(), 2)
  expect_is(liftChart, "data.frame")
  expect_is(liftChart$binWeight, "numeric")
  expect_is(liftChart$actual, "numeric")
  expect_is(liftChart$predicted, "numeric")
})

test_that("GetLiftChart fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(liftChartResponse)
  expect_error(with_mock("httr::GET" = getStub$f,
                         "datarobot::GetFrozenModel" = function(...) fakeModel,
                         "datarobot::GetModel" = function(...) fakeModel,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         GetLiftChart(fakeModel, fallbackToParentInsights = FALSE)), "404")
})


test_that("ListLiftCharts succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(liftChartAllResponse)
  liftChartAll <- with_mock("httr::GET" = getStub$f,
                            "datarobot:::Endpoint" = function() fakeEndpoint,
                            "datarobot:::Token" = function() fakeToken,
                            ListLiftCharts(fakeModel))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(liftChartAll, "list")
  expect_is(liftChartAll$validation, "data.frame")
  expect_is(liftChartAll$validation$binWeight, "numeric")
  expect_is(liftChartAll$validation$actual, "numeric")
  expect_is(liftChartAll$validation$predicted, "numeric")
  expect_is(liftChartAll$holdout, "data.frame")
  expect_is(liftChartAll$holdout$binWeight, "numeric")
  expect_is(liftChartAll$holdout$actual, "numeric")
  expect_is(liftChartAll$holdout$predicted, "numeric")
  expect_is(liftChartAll$crossValidation, "data.frame")
  expect_is(liftChartAll$crossValidation$binWeight, "numeric")
  expect_is(liftChartAll$crossValidation$actual, "numeric")
  expect_is(liftChartAll$crossValidation$predicted, "numeric")
})

test_that("ListLiftCharts succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(liftChartAllResponse)
  liftChartAll <- with_mock("httr::GET" = getStub$f,
                            "datarobot::GetFrozenModel" = function(...) fakeModel,
                            "datarobot::GetModel" = function(...) fakeModel,
                            "datarobot:::Endpoint" = function() fakeEndpoint,
                            "datarobot:::Token" = function() fakeToken,
                            ListLiftCharts(fakeModel, fallbackToParentInsights = TRUE))
  expect_equal(getStub$calledTimes(), 2)
  expect_is(liftChartAll, "list")
})

test_that("ListLiftCharts fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(liftChartAllResponse)
  expect_error(with_mock("httr::GET" = getStub$f,
                         "datarobot::GetFrozenModel" = function(...) fakeModel,
                         "datarobot::GetModel" = function(...) fakeModel,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         ListLiftCharts(fakeModel, fallbackToParentInsights = FALSE)), "404")
})
