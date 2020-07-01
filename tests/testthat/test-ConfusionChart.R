context("Confusion chart")
library(stubthat)
library(testthat)

describe("it can get a confusion chart", {
  getStub <- stub(httr::GET)
  getConfusionChartUrl <- UrlJoin(projectUrl, "models", fakeModelId,
                                  "confusionCharts", "validation")
  getConfusionChartJson <- fileToChar("responses/getConfusionChart.json")
  confusionChartResponse <- httr:::response(url = getConfusionChartUrl,
                                            status_code = 200L,
                                            content = charToRaw(getConfusionChartJson))
  getStub$onCall(1)$returns(confusionChartResponse)
  confusionChart <- with_mock("httr::GET" = getStub$f,
                              "datarobot:::Endpoint" = function() fakeEndpoint,
                              "datarobot:::Token" = function() fakeToken,
                              GetConfusionChart(fakeModel))
  expect_equal(getStub$calledTimes(), 1)

  test_that("the chart is a list", {
    expect_is(confusionChart, "list")
  })

  test_that("the chart has a source", {
    expect_equal(confusionChart$source, "validation")
  })

  test_that("data has classes", {
    expect_equal(sort(confusionChart$data$classes), sort(unique(as.character(iris$Species))))
  })

  test_that("confusionMatrix", {
    expect_is(confusionChart$data$confusionMatrix, "matrix")
    expect_equal(nrow(confusionChart$data$confusionMatrix), 3)
    expect_equal(ncol(confusionChart$data$confusionMatrix), 3)
    # Ensure that the confusion matrix has the correct number of samples for the validation
    # partition, which is 20% of 80% of the original dataframe.
    expect_equal(sum(confusionChart$data$confusionMatrix)  * (1 / 0.2) * (1 / 0.8), nrow(iris))
  })

  test_that("classMetrics", {
    expect_is(confusionChart$data$classMetrics, "list")
    expect_is(confusionChart$data$classMetrics$wasActualPercentages, "list")
    expect_is(confusionChart$data$classMetrics$wasActualPercentages[[1]], "data.frame")
    expect_equal(length(confusionChart$data$classMetrics$wasActualPercentages),
                 length(unique(iris$Species)))
    expect_equal(sum(confusionChart$data$classMetrics$wasActualPercentages[[1]]$percentage), 1)
    expect_equal(sum(confusionChart$data$classMetrics$wasActualPercentages[[2]]$percentage), 1)
    expect_equal(sum(confusionChart$data$classMetrics$wasActualPercentages[[3]]$percentage), 1)
    expect_equal(sort(confusionChart$data$classMetrics$wasActualPercentages[[1]]$otherClassName),
                 sort(unique(as.character(iris$Species))))
    expect_is(confusionChart$data$classMetrics$f1, "numeric")
    expect_equal(length(confusionChart$data$classMetrics$f1), length(unique(iris$Species)))
    expect_true(all(confusionChart$data$classMetrics$f1 >= 0))
    expect_true(all(confusionChart$data$classMetrics$f1 <= 1))
    expect_is(confusionChart$data$classMetrics$recall, "numeric")
    expect_equal(length(confusionChart$data$classMetrics$recall), length(unique(iris$Species)))
    expect_true(all(confusionChart$data$classMetrics$recall >= 0))
    expect_true(all(confusionChart$data$classMetrics$recall <= 1))
    expect_is(confusionChart$data$classMetrics$actualCount, "integer")
    expect_equal(length(confusionChart$data$classMetrics$actualCount), length(unique(iris$Species)))
    expect_is(confusionChart$data$classMetrics$precision, "numeric")
    expect_equal(length(confusionChart$data$classMetrics$precision), length(unique(iris$Species)))
    expect_true(all(confusionChart$data$classMetrics$precision >= 0))
    expect_true(all(confusionChart$data$classMetrics$precision <= 1))
    expect_is(confusionChart$data$classMetrics$wasPredictedPercentages[[1]], "data.frame")
    expect_equal(length(confusionChart$data$classMetrics$wasPredictedPercentages),
                 length(unique(iris$Species)))
    expect_equal(sum(confusionChart$data$classMetrics$wasPredictedPercentages[[1]]$percentage), 1)
    expect_equal(sum(confusionChart$data$classMetrics$wasPredictedPercentages[[2]]$percentage), 1)
    expect_equal(sum(confusionChart$data$classMetrics$wasPredictedPercentages[[3]]$percentage), 1)
    expect_equal(sort(confusionChart$data$classMetrics$wasPredictedPercentages[[1]]$otherClassName),
                 sort(unique(as.character(iris$Species))))
    expect_is(confusionChart$data$classMetrics$predictedCount, "integer")
    expect_equal(length(confusionChart$data$classMetrics$predictedCount),
                 length(unique(iris$Species)))
    expect_equal(sort(confusionChart$data$classMetrics$className),
                 sort(unique(as.character(iris$Species))))
  })
})

test_that("get confusion chart succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  getConfusionChartUrl <- UrlJoin(projectUrl, "models", fakeModelId,
                                  "confusionCharts", "validation")
  getConfusionChartJson <- fileToChar("responses/getConfusionChart.json")
  confusionChartResponse <- httr:::response(url = getConfusionChartUrl,
                                            status_code = 200L,
                                            content = charToRaw(getConfusionChartJson))
  errorMsg <- jsonlite::toJSON(list(message = list("404 whatever")))
  noChartResponse <- httr:::response(url = getConfusionChartUrl,
                                     status_code = 404L,
                                     content = charToRaw(errorMsg))
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(confusionChartResponse)
  confusionChart <- with_mock("httr::GET" = getStub$f,
                              "datarobot::GetFrozenModel" = function(...) fakeModel,
                              "datarobot::GetModel" = function(...) fakeModel,
                              "datarobot:::Endpoint" = function() fakeEndpoint,
                              "datarobot:::Token" = function() fakeToken,
                              GetConfusionChart(fakeModel, fallbackToParentInsights = TRUE))
  expect_equal(getStub$calledTimes(), 2)
  expect_is(confusionChart, "list")
})

test_that("get confusion chart fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  getConfusionChartUrl <- UrlJoin(projectUrl, "models", fakeModelId,
                                  "confusionCharts", "validation")
  getConfusionChartJson <- fileToChar("responses/getConfusionChart.json")
  confusionChartResponse <- httr:::response(url = getConfusionChartUrl,
                                            status_code = 200L,
                                            content = charToRaw(getConfusionChartJson))
  errorMsg <- jsonlite::toJSON(list(message = list("404 whatever")))
  noChartResponse <- httr:::response(url = getConfusionChartUrl,
                                     status_code = 404L,
                                     content = charToRaw(errorMsg))
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(confusionChartResponse)
  expect_error(with_mock("httr::GET" = getStub$f,
                         "datarobot::GetFrozenModel" = function(...) fakeModel,
                         "datarobot::GetModel" = function(...) fakeModel,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         GetConfusionChart(fakeModel, fallbackToParentInsights = FALSE)),
               "404")
})


test_that("it can list confusion charts", {
  getStub <- stub(httr::GET)
  listConfusionChartUrl <- UrlJoin(projectUrl, "models", fakeModelId, "confusionCharts")
  listConfusionChartJson <- fileToChar("responses/listConfusionCharts.json")
  confusionChartResponse <- httr:::response(url = listConfusionChartUrl,
                                           status_code = 200L,
                                           content = charToRaw(listConfusionChartJson))
  getStub$onCall(1)$returns(confusionChartResponse)
  confusionCharts <- with_mock("httr::GET" = getStub$f,
                               "datarobot:::Endpoint" = function() fakeEndpoint,
                               "datarobot:::Token" = function() fakeToken,
                               ListConfusionCharts(fakeModel))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(confusionCharts, "list")
  expect_equal(length(confusionCharts), 3)
  expect_equal(confusionCharts[[1]]$source, "holdout")
  expect_equal(confusionCharts[[2]]$source, "validation")
  expect_equal(confusionCharts[[3]]$source, "crossValidation")
  expect_equal(names(confusionCharts[[1]]), c("source", "data"))
  expect_equal(names(confusionCharts[[2]]), c("source", "data"))
  expect_equal(names(confusionCharts[[3]]), c("source", "data"))
  expect_equal(names(confusionCharts[[1]]$data), c("classMetrics", "classes", "confusionMatrix"))
  expect_equal(names(confusionCharts[[2]]$data), c("classMetrics", "classes", "confusionMatrix"))
  expect_equal(names(confusionCharts[[3]]$data), c("classMetrics", "classes", "confusionMatrix"))
})

test_that("list confusion charts succeeds with fallback to parent insights", {
  getStub <- stub(httr::GET)
  listConfusionChartUrl <- UrlJoin(projectUrl, "models", fakeModelId, "confusionCharts")
  listConfusionChartJson <- fileToChar("responses/listConfusionCharts.json")
  confusionChartResponse <- httr:::response(url = listConfusionChartUrl,
                                           status_code = 200L,
                                           content = charToRaw(listConfusionChartJson))
  errorMsg <- jsonlite::toJSON(list(message = list("404 whatever")))
  noChartResponse <- httr:::response(url = listConfusionChartUrl,
                                     status_code = 404L,
                                     content = charToRaw(errorMsg))
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(confusionChartResponse)
  confusionCharts <- with_mock("httr::GET" = getStub$f,
                               "datarobot::GetFrozenModel" = function(...) fakeModel,
                               "datarobot::GetModel" = function(...) fakeModel,
                               "datarobot:::Endpoint" = function() fakeEndpoint,
                               "datarobot:::Token" = function() fakeToken,
                               ListConfusionCharts(fakeModel, fallbackToParentInsights = TRUE))
  expect_equal(getStub$calledTimes(), 2)
  expect_is(confusionCharts, "list")
})

test_that("list confusion charts fails without fallback to parent insights", {
  getStub <- stub(httr::GET)
  listConfusionChartUrl <- UrlJoin(projectUrl, "models", fakeModelId, "confusionCharts")
  listConfusionChartJson <- fileToChar("responses/listConfusionCharts.json")
  confusionChartResponse <- httr:::response(url = listConfusionChartUrl,
                                           status_code = 200L,
                                           content = charToRaw(listConfusionChartJson))
  errorMsg <- jsonlite::toJSON(list(message = list("404 whatever")))
  noChartResponse <- httr:::response(url = listConfusionChartUrl,
                                     status_code = 404L,
                                     content = charToRaw(errorMsg))
  getStub$onCall(1)$returns(noChartResponse)
  getStub$onCall(2)$returns(confusionChartResponse)
  expect_error(with_mock("httr::GET" = getStub$f,
                         "datarobot::GetFrozenModel" = function(...) fakeModel,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         ListConfusionCharts(fakeModel, fallbackToParentInsights = FALSE)),
               "404")
})
