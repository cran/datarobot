library(stubthat)
library(testthat)
context("Test RequestNewDatetimeModel")

fakeTrainingRowCount <- 200

test_that("Required parameters are present", {
  expect_error(RequestNewDatetimeModel(),
               'argument "project" is missing, with no default')
  expect_error(RequestNewDatetimeModel(fakeProjectId),
               'argument "blueprint" is missing, with no default')
  expect_error(RequestNewDatetimeModel(blueprint = blueprint),
               'argument "project" is missing, with no default')
})

test_that("RequestNewDatetimeModel works", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(url = postDatetimeModelUrl,
                                           status_code = 200L,
                                           content = NULL)
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock("httr::POST" = postStub$f,
                     "httr::GET" = function() stop("Should not be called!"),
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken,
                     "datarobot:::JobIdFromResponse" = identity,
                     RequestNewDatetimeModel(fakeProject, fakeBlueprint))
  expect_equal(postStub$calledTimes(), 1)
  expect_is(jobId, "response")
})

test_that("RequestNewDatetimeModel works with trainingRowCount", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(url = postDatetimeModelUrl,
                                           status_code = 202L,
                                           headers = list(location = jobUrl),
                                           content = raw(0))
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock("httr::POST" = postStub$f,
                     "httr::GET" = function() stop("Should not be called!"),
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken,
                     "datarobot:::JobIdFromResponse" = identity,
                     RequestNewDatetimeModel(fakeProject, fakeBlueprint, trainingRowCount = 200))
  expect_equal(postStub$calledTimes(), 1)
  expect_is(jobId, "response")
})

test_that("RequestNewDatetimeModel works with featurelist", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(url = postDatetimeModelUrl,
                                           status_code = 202L,
                                           headers = list(location = jobUrl),
                                           content = raw(0))
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock("httr::POST" = postStub$f,
                     "httr::GET" = function() stop("Should not be called!"),
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken,
                     "datarobot:::JobIdFromResponse" = identity,
                     RequestNewDatetimeModel(fakeProject,
                                             fakeBlueprint,
                                             featurelist = fakeFeaturelist))
  expect_equal(postStub$calledTimes(), 1)
  expect_is(jobId, "response")
})

test_that("RequestNewDatetimeModel works with trainingDuration", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(url = postDatetimeModelUrl,
                                           status_code = 202L,
                                           headers = list(location = jobUrl),
                                           content = raw(0))
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock("httr::POST" = postStub$f,
                     "httr::GET" = function() stop("Should not be called!"),
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken,
                     "datarobot:::JobIdFromResponse" = identity,
                     RequestNewDatetimeModel(fakeProject,
                                             fakeBlueprint,
                                             trainingDuration = ConstructDurationString(days = 20)))
  expect_equal(postStub$calledTimes(), 1)
  expect_is(jobId, "response")
})

test_that("RequestNewDatetimeModel works with trainingDuration and timeWindowSamplePct", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(url = postDatetimeModelUrl,
                                           status_code = 202L,
                                           headers = list(location = jobUrl),
                                           content = raw(0))
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock("httr::POST" = postStub$f,
                     "httr::GET" = function() stop("Should not be called!"),
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken,
                     "datarobot:::JobIdFromResponse" = identity,
                     RequestNewDatetimeModel(fakeProject,
                                             fakeBlueprint,
                                             trainingDuration = ConstructDurationString(days = 20),
                                             timeWindowSamplePct = 90))
  expect_equal(postStub$calledTimes(), 1)
  expect_is(jobId, "response")
})

test_that("Required parameters are present", {
  expect_error(RequestFrozenDatetimeModel(trainingRowCount = 20),
               'argument "model" is missing, with no default')
  expect_error(RequestFrozenDatetimeModel(trainingRowCount = fakeTrainingRowCount),
               'argument "model" is missing, with no default')
})

context("Test RequestFrozenDatetimeModel")
test_that("RequestNewFrozenDatetimeModel works", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "frozenDatetimeModels")
  datetimeModelResponse <- httr:::response(url = postDatetimeModelUrl,
                                           status_code = 202L,
                                           headers = list(location = jobUrl),
                                           content = raw(0))
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock("httr::POST" = postStub$f,
                     "httr::GET" = function() stop("Should not be called!"),
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken,
                     "datarobot:::JobIdFromResponse" = identity,
                     RequestFrozenDatetimeModel(fakeModel, fakeTrainingRowCount))
  expect_equal(postStub$calledTimes(), 1)
  expect_is(jobId, "response")
})

test_that("RequestNewFrozenDatetimeModel works with samplePct", {
  postStub <- stub(httr::POST)
  postDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels")
  datetimeModelResponse <- httr:::response(url = postDatetimeModelUrl,
                                           status_code = 202L,
                                           headers = list(location = jobUrl),
                                           content = raw(0))
  postStub$onCall(1)$returns(datetimeModelResponse)
  jobId <- with_mock("httr::POST" = postStub$f,
                     "httr::GET" = function() stop("Should not be called!"),
                     "datarobot:::Endpoint" = function() fakeEndpoint,
                     "datarobot:::Token" = function() fakeToken,
                     "datarobot:::JobIdFromResponse" = identity,
                     RequestFrozenDatetimeModel(fakeModel,
                       trainingDuration = ConstructDurationString(days = 20),
                       timeWindowSamplePct = 90))
  expect_equal(postStub$calledTimes(), 1)
  expect_is(jobId, "response")
})


context("Test GetDatetimeModel")
test_that("Required parameters are present", {
  expect_error(GetDatetimeModel(),
               'argument "modelId" is missing, with no default')
  expect_error(GetDatetimeModel(fakeProject),
               'argument "modelId" is missing, with no default')
  expect_error(GetDatetimeModel(modelId = fakeModelId),
               'argument "project" is missing, with no default')
})

test_that("Message if modelId is blank", {
  expect_error(GetDatetimeModel(fakeProject, ""))
})

test_that("it can get a datetime model", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse) # GetDatetimeModel calls GetProject
  getDatetimeModelUrl <- UrlJoin(projectUrl, "datetimeModels", fakeModelId)
  getDatetimeModelJson <- fileToChar("responses/getDatetimeModelObject.json")
  datetimeModelResponse <- httr:::response(url = getDatetimeModelUrl,
                                           status_code = 200L,
                                           content = charToRaw(getDatetimeModelJson))
  getStub$onCall(2)$returns(datetimeModelResponse)
  datetimeModel <- with_mock("httr::GET" = getStub$f,
                             "datarobot:::Endpoint" = function() fakeEndpoint,
                             "datarobot:::Token" = function() fakeToken,
                             GetDatetimeModel(fakeProject, fakeModelId))
  expect_equal(getStub$calledTimes(), 2)
  expect_is(datetimeModel, "dataRobotDatetimeModel")
})


describe("ScoreBacktests", {
  context("ScoreBacktests")
  test_that("Required parameters are present", {
    expect_error(ScoreBacktests())
  })

  test_that("Function works with project and blueprint", {
    postStub <- stub(httr::POST)
    postBacktestUrl <- UrlJoin(projectUrl, "datetimeModels", fakeModelId, "backtests")
    postBacktestJson <- fileToChar("responses/scoreBacktests.json")
    datetimeModelResponse <- httr:::response(url = postBacktestUrl,
                                             status_code = 200L,
                                             content = charToRaw(postBacktestJson))
    postStub$onCall(1)$returns(datetimeModelResponse)
    jobId <- with_mock("httr::POST" = postStub$f,
                       "httr::GET" = function() stop("Should not be called!"),
                       "datarobot:::Endpoint" = function() fakeEndpoint,
                       "datarobot:::Token" = function() fakeToken,
                       "datarobot:::JobIdFromResponse" = identity,
                       ScoreBacktests(fakeModel))
    expect_equal(postStub$calledTimes(), 1)
    expect_is(jobId, "response")
  })

  test_that("Function works with wait", {
    postStub <- stub(httr::POST)
    postBacktestUrl <- UrlJoin(projectUrl, "datetimeModels", fakeModelId, "backtests")
    postBacktestJson <- fileToChar("responses/scoreBacktests.json")
    datetimeModelResponse <- httr:::response(url = postBacktestUrl,
                                             status_code = 200L,
                                             content = charToRaw(postBacktestJson))
    postStub$onCall(1)$returns(datetimeModelResponse)
    response <- with_mock("httr::POST" = postStub$f,
                          "httr::GET" = function() stop("Should not be called!"),
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          "datarobot:::WaitForJobToComplete" = function(...) NULL,
                          "datarobot:::JobIdFromResponse" = identity,
                          ScoreBacktests(fakeModel, wait = TRUE))
    expect_equal(postStub$calledTimes(), 1)
    expect_true(is.null(response))
  })
})


context("Test GenerateDatetimePartition")
datetimePartitionUrl <- UrlJoin(projectUrl, "datetimePartitioning")
datetimePartitionJson <- fileToChar("responses/datetimePartition.json")
completedDatetimePartitionResponse <- httr:::response(url = datetimePartitionUrl,
                                                      status_code = 200L,
                                                      content = charToRaw(datetimePartitionJson))

test_that("Required parameters are present", {
  expect_error(GenerateDatetimePartition())
})

datetimePartNames <- c("primaryTrainingEndDate", "primaryTrainingDuration", "backtests",
                       "datetimePartitionColumn", "primaryTrainingStartDate",
                       "availableTrainingStartDate", "availableTrainingDuration",
                       "dateFormat", "autopilotDataSelectionMethod",
                       "validationDuration", "holdoutEndDate", "numberOfBacktests", "gapStartDate",
                       "availableTrainingEndDate", "holdoutStartDate", "gapEndDate",
                       "holdoutDuration", "gapDuration", "cvMethod", "backtests",
                       "featureSettings", "isTimeSeries", "isMultiSeries", "isCrossSeries")

test_that("Function works with fakeProjectId", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(completedDatetimePartitionResponse)
  testReturn <- with_mock("httr::POST" = postStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GenerateDatetimePartition(fakeProjectId,
                                                    spec = list(someSpec = "dummy_spec")))
  expect_is(testReturn, "list")
  expect_is(testReturn$backtests, "data.frame")
  ExpectHasKeys(testReturn, datetimePartNames)
})


context("Test GetDatetimePartition")

test_that("Required parameters are present", {
  expect_error(GetDatetimePartition(), 'argument "project" is missing, with no default')
})

test_that("Function works with fakeProjectId", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedDatetimePartitionResponse)
  testReturn <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetDatetimePartition(fakeProjectId))
  expect_is(testReturn, "list")
  expect_is(testReturn$backtests, "data.frame")
  ExpectHasKeys(testReturn, datetimePartNames)
})

dtPartitionNoHoldoutJson <- fileToChar("responses/datetimePartitionWithoutHoldout.json")
dtPartitionNoHoldoutResponse <- httr:::response(url = datetimePartitionUrl,
                                                status_code = 200L,
                                                content = charToRaw(dtPartitionNoHoldoutJson))

test_that("Function works with datetime partitioning without holdout", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(dtPartitionNoHoldoutResponse)

  testReturn <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetDatetimePartition(fakeProjectId))

  expect_is(testReturn, "list")
  expect_is(testReturn$backtests, "data.frame")
  ExpectHasKeys(testReturn, datetimePartNames, allowAdditional = TRUE)
  # Also has availableTrainingRowCount, gapRowCount, holdoutRowCount,
  # primaryTrainingRowCount, projectId, totalRowCount
})


test_that("Function works with time series", {
  datetimePartitionJson <- fileToChar("responses/datetimePartition2.json")
  completedDatetimePartitionResponse <- httr:::response(url = datetimePartitionUrl,
                                                        status_code = 200L,
                                                        content = charToRaw(datetimePartitionJson))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedDatetimePartitionResponse)
  testReturn <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetDatetimePartition(fakeProjectId))
  expect_is(testReturn, "list")
  expect_true(testReturn$isTimeSeries)
  expect_false(testReturn$isMultiSeries)
  expect_false(testReturn$isCrossSeries)
})


test_that("Function works with multiseries", {
  datetimePartitionJson <- fileToChar("responses/multiseriesPartition.json")
  completedDatetimePartitionResponse <- httr:::response(url = datetimePartitionUrl,
                                                        status_code = 200L,
                                                        content = charToRaw(datetimePartitionJson))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedDatetimePartitionResponse)
  testReturn <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetDatetimePartition(fakeProjectId))
  expect_is(testReturn, "list")
  expect_true(testReturn$isTimeSeries)
  expect_true(testReturn$isMultiSeries)
  expect_false(testReturn$isCrossSeries)
})


test_that("Function works with cross series", {
  datetimePartitionJson <- fileToChar("responses/crossSeriesPartition.json")
  completedDatetimePartitionResponse <- httr:::response(url = datetimePartitionUrl,
                                                        status_code = 200L,
                                                        content = charToRaw(datetimePartitionJson))
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedDatetimePartitionResponse)
  testReturn <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetDatetimePartition(fakeProjectId))
  expect_is(testReturn, "list")
  expect_true(testReturn$isTimeSeries)
  expect_true(testReturn$isMultiSeries)
  expect_true(testReturn$isCrossSeries)
})
