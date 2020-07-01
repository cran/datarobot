context("Test ListProjects")
library(testthat)
library(stubthat)

expectedProjectNames <- c("projectId", "projectName", "fileName", "stage",
                          "autopilotMode", "created", "target", "metric",
                          "partition", "recommender", "advancedOptions",
                          "positiveClass", "maxTrainPct", "maxTrainRows",
                          "scaleoutMaxTrainPct", "scaleoutMaxTrainRows",
                          "holdoutUnlocked", "targetType")

getProjectJson <- fileToChar("responses/ListProjects.json")

test_that("it can list projects", {
  getStub <- stub(httr::GET)
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        ListProjects())
  expect_equal(getStub$calledTimes(), 1)
  expect_is(projects, "projectSummaryList")
  ExpectHasKeys(projects, expectedProjectNames)
})

test_that("it returns an empty list when there are no projects", {
  project <- with_mock("datarobot:::DataRobotGET" = function(...) list(),
                       "datarobot:::Endpoint" = function() fakeEndpoint,
                       "datarobot:::Token" = function() fakeToken,
                       ListProjects())
  expect_is(project, "projectSummaryList")
  expect_equal(length(project$projectId), 0)
  ExpectHasKeys(project, expectedProjectNames)
})

test_that("filter parameter must be a list", {
  expect_error(ListProjects(filter = "not-a-list"), "must be a list")
})

test_that("it can filter projects by name", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        ListProjects(filter = list("projectName" = "TimeSeries")))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(projects, "projectSummaryList")
})

test_that("extraneous filters are silently ignored", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        ListProjects(filter = list("tag" = "my-project")))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(projects, "projectSummaryList")
})

test_that("summary for 'projectSummaryList' object", {
  getStub <- stub(httr::GET)
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        ListProjects())
  expect_equal(getStub$calledTimes(), 1)
  expect_is(projects, "projectSummaryList")
  ExpectHasKeys(projects, expectedProjectNames)
  summaryList <- summary(projects)
  expect_is(summaryList, "list")
  ExpectHasKeys(summaryList, c("generalSummary", "detailedSummary"))
  expect_is(summaryList$generalSummary, "character")
  expect_equal(length(summaryList$generalSummary), 1)
  expect_is(summaryList$detailedSummary, "data.frame")
  ExpectHasKeys(summaryList$detailedSummary,
                setdiff(expectedProjectNames, c("partition", "recommender", "advancedOptions",
                                                "maxTrainRows", "scaleoutMaxTrainPct",
                                                "scaleoutMaxTrainRows")))
})

test_that("summary for 'projectSummaryList' object - nList", {
  getStub <- stub(httr::GET)
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        ListProjects())
  summaryList <- summary(projects)
  # Default case is 6
  expect_equal(nrow(summary(projects)$detailedSummary), 6)
  # Can restrict
  expect_equal(nrow(summary(projects, nList = 1)$detailedSummary), 1)
  # More than number of projects -> number of projects
  expect_equal(nrow(summary(projects, nList = 20)$detailedSummary), length(projects$projectId))
})

test_that("as.data.frame for project list", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/ListProjects.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        ListProjects())
  projectsDf <- as.data.frame(projects)
  expect_is(projectsDf, "data.frame")
  ExpectHasKeys(projectsDf, c("projectName", "projectId", "created", "fileName", "target",
                              "targetType", "positiveClass", "metric", "autopilotMode", "stage",
                              "maxTrainPct", "holdoutUnlocked"))
  expect_equal(length(projects$projectId), nrow(projectsDf))
})

test_that("as.data.frame for project list with full summary", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/ListProjects.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        ListProjects())
  projectsDf <- as.data.frame(projects, simple = FALSE)
  expect_is(projectsDf, "data.frame")
  ExpectHasKeys(projectsDf, c("projectName", "projectId", "created", "fileName", "target",
                              "targetType", "positiveClass", "metric", "autopilotMode", "stage",
                              "maxTrainPct", "holdoutUnlocked", "datetimeCol", "cvMethod",
                              "validationPct", "reps", "cvHoldoutLevel", "holdoutLevel",
                              "userPartitionCol", "validationType", "trainingLevel",
                              "partitionKeyCols", "holdoutPct", "validationLevel",
                              "datetimePartitionColumn", "recommenderItemId",
                              "isRecommender", "recommenderUserId", "scaleoutModelingMode",
                              "responseCap", "downsampledMinorityRows", "downsampledMajorityRows",
                              "blueprintThreshold", "seed", "weights", "smartDownsampled",
                              "majorityDownsamplingRate"))
  expect_equal(length(projects$projectId), nrow(projectsDf))
})

test_that("as.data.frame for project list with custom row names", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/ListProjects.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  projects <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        ListProjects())
  projectsDf <- as.data.frame(projects)
  expect_is(projectsDf, "data.frame")
  ExpectHasKeys(projectsDf, c("projectName", "projectId", "created", "fileName", "target",
                              "targetType", "positiveClass", "metric", "autopilotMode", "stage",
                              "maxTrainPct", "holdoutUnlocked"))
  expect_equal(rownames(projectsDf), as.character(seq(7)))
  expect_equal(length(projects$projectId), nrow(projectsDf))
  newRowNames <- paste0("row_", as.character(seq(7)))
  projectsDf <- as.data.frame(projects, row.names = newRowNames)
  expect_is(projectsDf, "data.frame")
  ExpectHasKeys(projectsDf, c("projectName", "projectId", "created", "fileName", "target",
                              "targetType", "positiveClass", "metric", "autopilotMode", "stage",
                              "maxTrainPct", "holdoutUnlocked"))
  expect_equal(rownames(projectsDf), newRowNames)
  expect_equal(length(projects$projectId), nrow(projectsDf))
})


context("Test GetProject")

test_that("Required parameters are present", {
  expect_error(GetProject())
})

expectedProjectNames <- c("projectId", "projectName", "fileName", "stage",
                          "autopilotMode", "created", "target", "metric",
                          "partition", "recommender", "advancedOptions",
                          "positiveClass", "maxTrainPct", "maxTrainRows",
                          "scaleoutMaxTrainPct", "scaleoutMaxTrainRows",
                          "holdoutUnlocked", "targetType")


test_that("it can get a project from a projectId", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  project <- with_mock("httr::GET" = getStub$f,
                       "datarobot:::Endpoint" = function() fakeEndpoint,
                       "datarobot:::Token" = function() fakeToken,
                       GetProject(fakeProjectId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(project, "dataRobotProject")
  ExpectHasKeys(project, expectedProjectNames)
})

test_that("it can get a project from a project", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  project <- with_mock("httr::GET" = getStub$f,
                       "datarobot:::Endpoint" = function() fakeEndpoint,
                       "datarobot:::Token" = function() fakeToken,
                       GetProject(fakeProject))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(project, "dataRobotProject")
  ExpectHasKeys(project, expectedProjectNames)
})

test_that("GetProject returns message if projectId missing from project", {
  expect_error(GetProject(list(id = 'lol')), "does not contain a valid project")
})

test_that("it can summarize a project", {
  getStub <- stub(httr::GET)
  getProjectJson <- fileToChar("responses/GetProject.json")
  projectResponse <- httr:::response(url = projectUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectJson))
  getStub$onCall(1)$returns(projectResponse)
  project <- with_mock("httr::GET" = getStub$f,
                       "datarobot:::Endpoint" = function() fakeEndpoint,
                       "datarobot:::Token" = function() fakeToken,
                       GetProject(fakeProjectId))
  ExpectHasKeys(project, expectedProjectNames)
  expect_is(project, "dataRobotProject")
  projectSummary <- summary(project)
  ExpectHasKeys(projectSummary, c("projectName", "projectId", "created", "fileName", "target",
                                  "targetType", "metric"))
  expect_is(projectSummary, "character")
})


context("Test DeleteProject")
test_that("Required parameters are present", {
  expect_error(DeleteProject())
})

test_that("It can delete a project", {
  expect_message(with_mock("datarobot:::DataRobotDELETE" = function(routeString,
                                                                    addUrl = TRUE,
                                                                    body = NULL,
                                                                    returnRawResponse = FALSE,
                                                                    ...) {
                             routeForInspect <<- routeString
                             ""
                           },
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken, {
                   DeleteProject(fakeProject$projectId)
                 }), "deleted")
    expect_equal(routeForInspect, paste0("projects/", fakeProjectId, "/"))
})


context("Test UpdateProject")
test_that("Required parameters are present", {
  expect_error(UpdateProject())
})

test_that("No update without information", {
  expect_error(UpdateProject(fakeProject), "No update data is provided")
})

test_that("It can update a project", {
  newName <- "TestOfProjectUpdate"
  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$returns(httr:::response(url = projectUrl,
                                              status_code = 202L,
                                              headers = list(location = projectUrl),
                                              content = raw(0)))
  expect_message(with_mock("httr::PATCH" = patchStub$f,
                           "datarobot:::DataRobotPATCH" = function(routeString,
                                                                   addUrl = TRUE,
                                                                   body = NULL,
                                                                   returnRawResponse = FALSE, ...) {
                             bodyForInspect <<- body
                             datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
                                                              addUrl = addUrl,
                                                              returnRawResponse = returnRawResponse,
                                                              body = body, ...)
                           },
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken, {
                   UpdateProject(fakeProject$projectId,
                                newProjectName = newName,
                                holdoutUnlocked = TRUE,
                                workerCount = 8)
                 }), "updated")
    expect_equal(bodyForInspect$workerCount, 8)
    expect_true(bodyForInspect$holdoutUnlocked)
    expect_equal(as.character(bodyForInspect$projectName), newName)
})


test_that("It can update a project to set workers to max", {
  newName <- "TestOfProjectUpdate"
  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$returns(httr:::response(url = projectUrl,
                                              status_code = 202L,
                                              headers = list(location = projectUrl),
                                              content = raw(0)))
  expect_message(with_mock("httr::PATCH" = patchStub$f,
                           "datarobot:::DataRobotPATCH" = function(routeString,
                                                                   addUrl = TRUE,
                                                                   body = NULL,
                                                                   returnRawResponse = FALSE, ...) {
                             bodyForInspect <<- body
                             datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
                                                              addUrl = addUrl,
                                                              returnRawResponse = returnRawResponse,
                                                              body = body, ...)
                           },
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken, {
                   UpdateProject(fakeProject$projectId, workerCount = "max")
                 }), "updated")
    expect_equal(bodyForInspect$workerCount, -1)
})


context("Test GetProjectStatus")
projectStatusUrl <- UrlJoin(projectUrl, "status")

test_that("Required parameters are present", {
  expect_error(GetProjectStatus())
})

test_that("it can get project status", {
  getStub <- stub(httr::GET)
  getProjectStatusJson <- fileToChar("responses/GetProjectStatus.json")
  projectStatusResponse <- httr:::response(url = projectStatusUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectStatusJson))
  getStub$onCall(1)$returns(projectStatusResponse)
  projectStatus <- with_mock("httr::GET" = getStub$f,
                             "datarobot:::Endpoint" = function() fakeEndpoint,
                             "datarobot:::Token" = function() fakeToken,
                             GetProjectStatus(fakeProjectId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(projectStatus, "list")
  ExpectHasKeys(projectStatus, c("autopilotDone", "stageDescription", "stage"))
})

test_that("Use project list", {
  getStub <- stub(httr::GET)
  getProjectStatusJson <- fileToChar("responses/GetProjectStatus.json")
  projectStatusResponse <- httr:::response(url = projectStatusUrl,
                                     status_code = 200L,
                                     content = charToRaw(getProjectStatusJson))
  getStub$onCall(1)$returns(projectStatusResponse)
  projectStatus <- with_mock("httr::GET" = getStub$f,
                             "datarobot:::Endpoint" = function() fakeEndpoint,
                             "datarobot:::Token" = function() fakeToken,
                             GetProjectStatus(fakeProject))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(projectStatus, "list")
  ExpectHasKeys(projectStatus, c("autopilotDone", "stageDescription", "stage"))
})
