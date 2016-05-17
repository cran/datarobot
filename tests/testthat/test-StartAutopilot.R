library(stubthat)

context("Test SetTarget")

project <- list(projectName = "project for test", projectId = "56817c58c80891519c90a248",
                fileName = "my_data.csv", created = "2015-12-28T18:15:55.892525Z")
target <- "medv"
response <- "my_target"

# TODO: Add test that we fail appropriately when the status response includes a status
#       field that indicates failure.
# Note: That will require slightly reorganizing the tests, since at the moment
#       `with_set_target_mocks` expects the same assertions for each test.

with_set_target_mocks <- function(...) {

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

  patch_stub <- stub(httr::PATCH)
  get_stub <- stub(httr::GET)

  projectUrl <- datarobot:::UrlJoin(fakeEndpoint, 'projects', project$projectId)
  aimUrl <- datarobot:::UrlJoin(projectUrl, 'aim')
  statusUrl <- datarobot:::UrlJoin(fakeEndpoint, 'status', 'some-status')

  patch_stub$onCall(1)$expects(url = aimUrl)
  patch_stub$onCall(1)$returns(httr:::response(url = aimUrl,
                                               status_code = 202L,
                                               headers = list(location = statusUrl),
                                               content = raw(0)))

  get_stub$onCall(1)$expects(url = statusUrl)
  get_stub$onCall(1)$returns(httr:::response(url = statusUrl,
                                             status_code = 200L,
                                             content = charToRaw('{"status": "someStatus"}')))

  get_stub$onCall(2)$expects(url = statusUrl)
  get_stub$onCall(2)$returns(httr:::response(url = statusUrl,
                                             status_code = 303L,
                                             headers = list(location = projectUrl),
                                             content = raw(0)))

  get_stub$onCall(3)$expects(url = projectUrl)
  get_stub$onCall(3)$returns(httr:::response(url = projectUrl,
                                             status_code = 200L,
                                             content = raw(0))) # put response body here if needed

  with_mock("httr::PATCH" = patch_stub$f,
            "httr::GET" = get_stub$f,
            "datarobot:::Endpoint" = function() return(fakeEndpoint),
            "datarobot:::Token" = function() return(fakeToken),
            GetProjectStatus = function(...) return(list(stage = 'aim')),
            ...)

  expect_equal(patch_stub$calledTimes(), 1)
  expect_equal(get_stub$calledTimes(), 3)

}

with_set_target_mocks(
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
  with_set_target_mocks(expect_message(
    SetTarget(project = project$projectId, target = target),
    "Autopilot started"))
})

test_that("Use full project list only", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target),
    "Autopilot started"))
})

test_that("Use non-null metric", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, metric = "testMetric"),
    "Autopilot started"))
})

test_that("Use non-null weights", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, weights = "testWeights"),
    "Autopilot started"))
})

test_that("Use non-null featurelistId", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, featurelistId  = "56bb612d100d2b541ac5482f"),
    "Autopilot started"))
})

test_that("Use non-null mode", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, mode = "testMode"),
    "Autopilot started"))
})

test_that("Use non-null seed", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, seed = 19),
    "Autopilot started"))
})

test_that("Use non-null positiveClass", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, positiveClass = "testClass"),
    "Autopilot started"))
})

test_that("Use non-null blueprintThreshold", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, blueprintThreshold = 8),
    "Autopilot started"))
})

test_that("Use non-null responseCap", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, responseCap = .8),
    "Autopilot started"))
})

test_that("Use non-null recommenderUserId", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, recommenderUserId = "testUserId"),
    "Autopilot started"))
})

test_that("Use non-null recommenderItemId", {
  with_set_target_mocks(expect_message(
    SetTarget(project = project, target = target, recommenderItemId = "testItemId"),
    "Autopilot started"))
})
