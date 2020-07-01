context("Test ListModelJobs")
library(stubthat)
library(testthat)

modelJobUrl <- UrlJoin(projectUrl, "modelJobs", fakeJobId)
response <- list()
expectedKeys <- c("status", "processes", "projectId", "samplePct",
                  "trainingRowCount", "modelType", "featurelistId",
                  "modelCategory", "blueprintId", "modelJobId",
                  "modelJobId", "isBlocked")

test_that("Query with no arguments is correct", {
  expectedRoute <- sprintf("projects/%s/modelJobs/", fakeProjectId)
  modelJobs <- with_mock(
    "datarobot::DataRobotGET" = function(RouteString, AddURL, ...) {
      expect_equivalent(RouteString, expectedRoute)
      query <- list(...)$query
      for (param in query) {
        expect_null(param)
      }
      response
    }, ListModelJobs(fakeProjectId))
  expect_is(modelJobs, "data.frame")
  ExpectHasKeys(modelJobs, expectedKeys)
})


test_that("Query with jobStatus argument is correct", {
  expectedRoute <- sprintf("projects/%s/modelJobs/", fakeProjectId)
  modelJobs <- with_mock(
    "datarobot::DataRobotGET" = function(RouteString, AddURL, ...) {
      expect_equivalent(RouteString, expectedRoute)
      expect_equivalent(list(...)$query$status, "queue")
      response
    }, ListModelJobs(fakeProjectId, status = "queue"))
  ExpectHasKeys(modelJobs, expectedKeys)
})

context("Test GetModelJob")
modelJobJson <- fileToChar("responses/modelJobInfo.json")
completeModelJobResponse <- httr:::response(url = modelJobUrl,
                                              status_code = 303L,
                                              headers = list(location = UrlJoin(projectUrl,
                                                                                "someRedirect")),
                                              content = charToRaw(modelJobJson))

test_that("GetModelJob succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeModelJobResponse)
  response <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetModelJob(fakeProject, fakeJobId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(response, "list")
  ExpectHasKeys(response, expectedKeys)
  expect_is(response$status, "character")
  expect_is(response$projectId, "character")
  expect_is(response$modelJobId, "character")
  expect_is(response$processes, "character")
  expect_is(response$samplePct, "numeric")
  expect_is(response$trainingRowCount, "integer")
  expect_is(response$modelType, "character")
  expect_is(response$featurelistId, "character")
  expect_is(response$modelCategory, "character")
  expect_is(response$blueprintId, "character")
  expect_is(response$isBlocked, "logical")

  tmp <- fromJSON(modelJobJson)
  tmp$status <- JobStatus$Queue
  modelJobJson <- toJSON(tmp)

  completeModelJobResponse <- httr:::response(url = modelJobUrl,
                                              status_code = 200L,
                                              content = charToRaw(modelJobJson))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeModelJobResponse)
  response <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetModelJob(fakeProject, fakeJobId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(response, "list")
  ExpectHasKeys(response, expectedKeys)
  expect_is(response$status, "character")
  expect_is(response$projectId, "character")
  expect_is(response$modelJobId, "character")
  expect_is(response$processes, "character")
  expect_is(response$samplePct, "numeric")
  expect_is(response$trainingRowCount, "integer")
  expect_is(response$modelType, "character")
  expect_is(response$featurelistId, "character")
  expect_is(response$modelCategory, "character")
  expect_is(response$blueprintId, "character")
  expect_is(response$isBlocked, "logical")

  completeModelJobResponse <- httr:::response(url = modelJobUrl,
                                              status_code = 400L,
                                              content = charToRaw(modelJobJson))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeModelJobResponse)
  expect_error(with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetModelJob(fakeProject, fakeJobId)))
})
