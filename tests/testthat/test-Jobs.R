context("Test ListJobs")
library(jsonlite)
library(stubthat)
library(testthat)

listJobsJson <- fileToChar("responses/jobs.json")
listErroredJobsJson <- fileToChar("responses/jobs_errored.json")
datarobot:::SaveConnectionEnvironmentVars("fake_endpoint", "fake_token")

test_that("We can list and delete jobs", {
  jobs <- with_mock(
    "httr::GET" = function(url, ...) {
      expect_equal(url, datarobot:::BuildPath(datarobot:::UrlJoin("projects",
                                                                  fakeProjectId,
                                                                  "jobs"))$fullPath)
      httr:::response(url = url, status_code = 200L,
                      content = charToRaw(listJobsJson))
    },
    ListJobs(fakeProjectId)
 )
  expect_equal(jobs, jsonlite::fromJSON(listJobsJson, simplifyDataFrame = FALSE)$jobs)
  ExpectHasKeys(jobs[[1]], c("status", "url", "id", "projectId", "jobType", "isBlocked"))
  jobToDelete <- jobs[[1]]
  httrDeleteCallCount <- 0
  with_mock(
    "httr::DELETE" = function(url, ...) {
      httrDeleteCallCount <<- httrDeleteCallCount + 1
      expect_equal(url, jobToDelete$url)
      httr:::response(url = url, status_code = 200L, content = raw(0))
    },
    DeleteJob(jobToDelete)
 )
  expect_equal(httrDeleteCallCount, 1)
})

test_that("Get errored jobs uses the right query parameter", {
  jobs <- with_mock(
    "httr::GET" = function(url, query, ...) {
      expect_equal(url, datarobot:::BuildPath(datarobot:::UrlJoin("projects",
                                                                  fakeProjectId,
                                                                  "jobs"))$fullPath)
      expect_equal(query, list(status = JobStatus$Error))
      httr:::response(url = url, status_code = 200L,
                      content = charToRaw(listErroredJobsJson))
    },
    ListJobs(fakeProjectId, status = "error"
   )
 )
})

test_that("Deleting a non-job is an error", {
  jobToDelete <- list(not = 1, a = 2, job = 3)
  httrDeleteCallCount <- 0
  with_mock(
    "httr::DELETE" = function(url, ...) {
      httrDeleteCallCount <<- httrDeleteCallCount + 1
      expect_equal(url, jobToDelete$url)
      httr:::response(url = url, status_code = 200L, content = raw(0))
    },
    expect_error(DeleteJob(jobToDelete))
 )
  expect_equal(httrDeleteCallCount, 0)
})


context("Test GetJob")
jobJson <- fileToChar("responses/jobInfo.json")
completeJobResponse <- httr:::response(url = jobUrl,
                                       status_code = 303L,
                                       content = charToRaw(jobJson))

test_that("GetJob succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeJobResponse)
  response <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetJob(fakeProject, fakeJobId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(response, "list")
  ExpectHasKeys(response, c("status", "url", "id", "projectId", "jobType", "isBlocked"))
  expect_is(response$status, "character")
  expect_is(response$projectId, "character")
  expect_is(response$id, "character")
  expect_is(response$url, "character")
  expect_is(response$jobType, "character")
  expect_is(response$isBlocked, "logical")

  tmp <- fromJSON(jobJson)
  tmp$status <- JobStatus$Queue
  jobJson <- toJSON(tmp)

  completeJobResponse <- httr:::response(url = jobUrl,
                                         status_code = 200L,
                                         content = charToRaw(jobJson))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeJobResponse)
  response <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetJob(fakeProject, fakeJobId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(response, "list")
  ExpectHasKeys(response, c("status", "url", "id", "projectId", "jobType", "isBlocked"))
  expect_is(response$status, "character")
  expect_is(response$projectId, "character")
  expect_is(response$id, "character")
  expect_is(response$url, "character")
  expect_is(response$jobType, "character")
  expect_is(response$isBlocked, "logical")

  completeJobResponse <- httr:::response(url = jobUrl,
                                         status_code = 400L,
                                         content = charToRaw(jobJson))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completeJobResponse)
  expect_error(with_mock("httr::GET" = getStub$f,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         GetJob(fakeProject, fakeJobId)))
  })
