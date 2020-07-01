context("GetValidMetrics")
library(testthat)
library(stubthat)

test_that("Required parameters are present", {
  expect_error(GetValidMetrics())
  expect_error(GetValidMetrics(fakeProject))
})

validMetricsUrl <- UrlJoin(projectUrl, "features", "metrics")
validMetricsJson <- fileToChar("responses/GetValidMetrics.json")

test_that("It can get valid metrics", {
  metricsResponse <- httr:::response(url = validMetricsUrl,
                                     status_code = 200L,
                                     content = charToRaw(validMetricsJson))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(metricsResponse)
  metrics <- with_mock("httr::POST" = function(...) stop("Should not be called!"),
                       "httr::GET" = getStub$f,
                       "datarobot:::Endpoint" = function() fakeEndpoint,
                       "datarobot:::Token" = function() fakeToken,
                       GetValidMetrics(fakeProject, fakeTarget))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(metrics, "character")
  expect_true(length(metrics) > 1)
})
