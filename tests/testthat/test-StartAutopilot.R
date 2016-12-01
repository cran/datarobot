library(stubthat)

context("Test SetTarget")

project <- list(projectName = "project for test", projectId = "56817c58c80891519c90a248",
                fileName = "my_data.csv", created = "2015-12-28T18:15:55.892525Z")
target <- "medv"
response <- "my_target"

# TODO: Add test that we fail appropriately when the status response includes a status
#       field that indicates failure.
# Note: That will require slightly reorganizing the tests, since at the moment
#       `withSetTargetMocks` expects the same assertions for each test.

withSetTargetMocks <- function(...) {

  # Set up stubs for PATCH and GET. We expect PATCH is called once (to request setting the target),
  # and GET is called three times:
  #  (1) get status of the async request (not done yet)
  #  (2) get status of the async request (now we're done)
  #  (3) get updated project data (we don't use this for anything, but it's part of the async
  #      workflow, so it's expected to happen)
  #
  # Note: This does not include the GET request to confirm that the project status is 'aim'
  #       (ready for target-setting), since that is mocked separately via GetProjectStatus

  fakeEndpoint <- "fake_endpoint"
  fakeToken <- "fake_token"

  patchStub <- stub(httr::PATCH)
  getStub <- stub(httr::GET)

  projectUrl <- datarobot:::UrlJoin(fakeEndpoint, 'projects', project$projectId)
  aimUrl <- datarobot:::UrlJoin(projectUrl, 'aim')
  statusUrl <- datarobot:::UrlJoin(fakeEndpoint, 'status', 'some-status')

  patchStub$onCall(1)$expects(url = aimUrl)
  patchStub$onCall(1)$returns(httr:::response(url = aimUrl,
                                               status_code = 202L,
                                               headers = list(location = statusUrl),
                                               content = raw(0)))

  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(url = statusUrl,
                                             status_code = 200L,
                                             content = charToRaw('{"status": "someStatus"}')))

  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(url = statusUrl,
                                             status_code = 303L,
                                             headers = list(location = projectUrl),
                                             content = raw(0)))

  getStub$onCall(3)$expects(url = projectUrl)
  getStub$onCall(3)$returns(httr:::response(url = projectUrl,
                                             status_code = 200L,
                                             content = raw(0))) # put response body here if needed

  with_mock("httr::PATCH" = patchStub$f,
            "httr::GET" = getStub$f,
            "datarobot:::Endpoint" = function() return(fakeEndpoint),
            "datarobot:::Token" = function() return(fakeToken),
            GetProjectStatus = function(...) return(list(stage = 'aim')),
            ...)

  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 3)

}

withSetTargetMocks(
  SetTarget(project = project$projectId, target = target)
)


test_that("Required parameters are present", {
  # TODO: Fix this.
  # (These don't use mocks and don't look for specific errors, so are likely to spuriously passs.)
  expect_error(SetTarget())
  expect_error(SetTarget(target = target))
  expect_error(SetTarget(project, target = NULL))
})


test_that("Use projectId only", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project$projectId, target = target),
    "Autopilot started"))
})

test_that("Use full project list only", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target),
    "Autopilot started"))
})

test_that("Use non-null metric", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, metric = "testMetric"),
    "Autopilot started"))
})

test_that("Use non-null weights", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, weights = "testWeights"),
    "Autopilot started"))
})

test_that("Use non-null featurelistId", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, featurelistId  = "56bb612d100d2b541ac5482f"),
    "Autopilot started"))
})

test_that("Use non-null mode", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, mode = "testMode"),
    "Autopilot started"))
})

test_that("Use non-null seed", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, seed = 19),
    "Autopilot started"))
})

test_that("Use non-null positiveClass", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, positiveClass = "testClass"),
    "Autopilot started"))
})

test_that("Use non-null blueprintThreshold", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, blueprintThreshold = 8),
    "Autopilot started"))
})

test_that("Use non-null responseCap", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, responseCap = .8),
    "Autopilot started"))
})

test_that("Use non-null recommenderUserId", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, recommenderUserId = "testUserId"),
    "Autopilot started"))
})

test_that("Use non-null recommenderItemId", {
  withSetTargetMocks(expect_message(
    SetTarget(project = project, target = target, recommenderItemId = "testItemId"),
    "Autopilot started"))
})

test_that("Use semi mode", {
  withSetTargetMocks(expect_warning(
    SetTarget(project = project, target = target, mode = "semi")))
})

test_that("Use quickrun flag", {
  withSetTargetMocks(expect_warning(
    SetTarget(project = project, target = target, mode = "auto", quickrun = TRUE)))
})
