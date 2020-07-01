context("Test plot.listOfModels")
library(testthat)
library(stubthat)

GetListOfModels <- function() {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  listModelsUrl <- UrlJoin(projectUrl, "models")
  listModelsJson <- fileToChar("responses/ListModels.json")
  modelsResponse <- httr:::response(url = listModelsUrl,
                                    status_code = 200L,
                                    content = charToRaw(listModelsJson))
  getStub$onCall(2)$returns(modelsResponse)
  listModelJobsUrl <- UrlJoin(projectUrl, "modelJobs")
  modelsResponse <- httr:::response(url = listModelJobsUrl,
                                    status_code = 200L,
                                    content = charToRaw("[]"))
  getStub$onCall(3)$returns(modelsResponse)
  models <- with_mock("httr::GET" = getStub$f,
                      "datarobot:::Endpoint" = function() fakeEndpoint,
                      "datarobot:::Token" = function() fakeToken,
                      ListModels(fakeProjectId))
  expect_equal(getStub$calledTimes(), 3)
  expect_is(models, c("listOfModels", "listSubclass"))
  models
}


test_that("Simple invocation works", {
  models <- GetListOfModels()
  simpleTest <- evaluate_promise(plot(models))
  expect_equivalent(simpleTest$result, NULL)
  expect_equivalent(simpleTest$output, "")
  expect_equivalent(simpleTest$warnings, character(0))
  expect_equivalent(simpleTest$messages, character(0))
})

test_that("metric specification works", {
  models <- GetListOfModels()
  simpleTest <- evaluate_promise(plot(models,
                                      metric = "Gini Norm.validation"))
  expect_equivalent(simpleTest$result, NULL)
  expect_equivalent(simpleTest$output, "")
  expect_equivalent(simpleTest$warnings, character(0))
  expect_equivalent(simpleTest$messages, character(0))
})

test_that("pct specification works", {
  models <- GetListOfModels()
  simpleTest <- evaluate_promise(plot(models, pct = 79.911))
  expect_equivalent(simpleTest$result, NULL)
  expect_equivalent(simpleTest$output, "")
  expect_equivalent(simpleTest$warnings, character(0))
  expect_equivalent(simpleTest$messages, character(0))
})

test_that("pct specification errors when not found", {
  models <- GetListOfModels()
  expect_error(plot(models, pct = 11), "not found")
})

test_that("selectRecords specification works without pct specification", {
  models <- GetListOfModels()
  simpleTest <- evaluate_promise(plot(models,
                                      selectRecords = seq(1, 10, 1)))
  expect_equivalent(simpleTest$result, NULL)
  expect_equivalent(simpleTest$output, "")
  expect_equivalent(simpleTest$warnings, character(0))
  expect_equivalent(simpleTest$messages, character(0))
})

test_that("selectRecords specification works with pct specification", {
  models <- GetListOfModels()
  simpleTest <- evaluate_promise(plot(models,
                                      selectRecords = seq(1, 10, 1),
                                      pct = 79.911))
  expect_equivalent(simpleTest$result, NULL)
  expect_equivalent(simpleTest$output, "")
  expect_equivalent(simpleTest$warnings, character(0))
  expect_equivalent(simpleTest$messages, character(0))
})

test_that("orderDecreasing specification works", {
  models <- GetListOfModels()
  simpleTest <- evaluate_promise(plot(models, orderDecreasing = TRUE))
  expect_equivalent(simpleTest$result, NULL)
  expect_equivalent(simpleTest$output, "")
  expect_equivalent(simpleTest$warnings, character(0))
  expect_equivalent(simpleTest$messages, character(0))
})

test_that("Can plot list of models with multiple featurelists", {
  models <- GetListOfModels()
  models[[1]]$featurelistName <- NULL
  models[[1]]$featurelistId <- NULL
  simpleTest <- evaluate_promise(plot(models))
  expect_equivalent(simpleTest$result, NULL)
  expect_equivalent(simpleTest$output, "")
  expect_equivalent(simpleTest$warnings, character(0))
  expect_equivalent(simpleTest$messages, character(0))
})
