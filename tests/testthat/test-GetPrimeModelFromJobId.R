context("Test GetPrimeModelFromJobId")
library(stubthat)
library(testthat)

primeModelUrl <- UrlJoin(projectUrl, "primeModels", fakeModelId)
primeModelJson <- fileToChar("responses/primeModel.json")
completedPrimeModelResponse <- httr:::response(url = primeModelUrl,
                                               status_code = 200L,
                                               content = charToRaw(primeModelJson))

test_that("GetPrimeModelFromJobId succeeds", {
  jobDataInprogress <- list(
    status = JobStatus$InProgress,
    url = "https://host_name.com/projects/p-id/jobs/1/",
    id = fakeJobId,
    jobType = JobType$PrimeModel,
    projectId = fakeProjectId
 )

  jobDataComplete <- jobDataInprogress
  jobDataComplete$status <- JobStatus$Completed

  jobInprogressResponse <-
    httr:::response(url = primeModelUrl,
                    status_code = 200L,
                    content = charToRaw(jsonlite::toJSON(jobDataInprogress)))

  jobCompleteResponse <-
    httr:::response(url = primeModelUrl,
                    status_code = 303L,
                    headers = list(location = primeModelUrl),
                    content = charToRaw(jsonlite::toJSON(jobDataComplete)))

  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(jobInprogressResponse)
  getStub$onCall(2)$returns(jobCompleteResponse)
  getStub$onCall(3)$returns(jobCompleteResponse)
  getStub$onCall(4)$returns(completedPrimeModelResponse)
  primeModel <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetPrimeModelFromJobId(fakeProjectId, fakeJobId, maxWait = 1))
  expect_equal(getStub$calledTimes(), 4)
  expect_is(primeModel, "dataRobotPrimeModel")
  ExpectHasKeys(primeModel, c("samplePct", "parentModelId", "id", "trainingRowCount"),
                allowAdditional = TRUE)
})
