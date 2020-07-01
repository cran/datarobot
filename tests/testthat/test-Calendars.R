context("Test Calendars")
library(stubthat)
library(testthat)


getCalendarUrl <- UrlJoin("calendar", fakeCalendarId)
createCalendarUrl <- UrlJoin("calendars", "fileUpload")
listCalendarsUrl <- "calendar"
postCalendarUrl <- UrlJoin("calendar", fakeCalendarId)
expectedCols <- c("name", "created", "calendarStartDate", "calendarEndDate",
                  "numEventTypes", "source", "projectIds", "id")


test_that("it can get a calendar", {
  getStub <- stub(httr::GET)
  getCalendarJson <- fileToChar("responses/getCalendar.json")
  calendarResponse <- httr:::response(url = getCalendarUrl,
                                      status_code = 200L,
                                      content = charToRaw(getCalendarJson))
  getStub$onCall(1)$returns(calendarResponse)
  calendar <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetCalendar(fakeCalendarId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(calendar, "dataRobotCalendar")
  ExpectHasKeys(calendar, expectedCols)
})


test_that("it can list all calendars", {
  getStub <- stub(httr::GET)
  listCalendarsJson <- fileToChar("responses/listCalendars.json")
  calendarResponse <- httr:::response(url = listCalendarsUrl,
                                      status_code = 200L,
                                      content = charToRaw(listCalendarsJson))
  getStub$onCall(1)$returns(calendarResponse)
  calendars <- with_mock("httr::GET" = getStub$f,
                         "datarobot:::Endpoint" = function() fakeEndpoint,
                         "datarobot:::Token" = function() fakeToken,
                         ListCalendars())
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(length(calendars), 23)
  expect_is(calendars, "listOfCalendars")
  expect_is(calendars[[1]], "dataRobotCalendar")
})


test_that("it can create a calendar", {
  postStub <- stub(httr::POST)
  createCalendarJson <- fileToChar("responses/createCalendar.json")
  createCalendarResponse <- httr:::response(url = createCalendarUrl,
                                            status_code = 202L,
                                            content = charToRaw(createCalendarJson))
  postStub$onCall(1)$returns(createCalendarResponse)
  calendar <- with_mock(`httr::POST` = postStub$f,
                        `httr::GET` = function() stop("Should not be called!"),
                        `datarobot::WaitForAsyncReturn` = function(...) {
                          ParseReturnResponse(createCalendarResponse)
                        },
                        `datarobot:::UploadData` = function(file) file,
                        `datarobot:::Endpoint` = function() fakeEndpoint,
                        `datarobot:::Token` = function() fakeToken,
                        CreateCalendar("calendar.csv"))
  expect_equal(postStub$calledTimes(), 1)
  expect_is(calendar, "dataRobotCalendar")
  ExpectHasKeys(calendar, expectedCols)
})


test_that("it can rename a calendar", {
  patchStub <- stub(httr::PATCH)
  createCalendarJson <- fileToChar("responses/createCalendar.json")
  calendarResponse <- httr:::response(url = postCalendarUrl,
                                      status_code = 202L,
                                      content = charToRaw(createCalendarJson))
  patchStub$onCall(1)$returns(calendarResponse)
  getStub <- stub(httr::GET)
  getCalendarJson <- fileToChar("responses/getCalendar.json")
  calendarResponse <- httr:::response(url = getCalendarUrl,
                                      status_code = 200L,
                                      content = charToRaw(getCalendarJson))
  getStub$onCall(1)$returns(calendarResponse)
  calendar <- with_mock("httr::GET" = getStub$f,
                        "httr::PATCH" = patchStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        UpdateCalendar(fakeCalendarId, "ThrowawayName"))
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 1)
  expect_is(calendar, "dataRobotCalendar")
  ExpectHasKeys(calendar, expectedCols)
})


test_that("It can delete a calendar", {
  response <- with_mock("datarobot:::DataRobotDELETE" = function(routeString,
                                                                 addUrl = TRUE,
                                                                 body = NULL,
                                                                 returnRawResponse = FALSE,
                                                                 ...) {
                          routeForInspect <<- routeString
                          ""
                        },
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        DeleteCalendar(fakeCalendar))
  expect_is(response, "NULL")
  expect_equal(routeForInspect, paste0("calendars/", fakeCalendarId, "/"))
})


test_that("it can get a calendar from a project", {
  getStub <- stub(httr::GET)
  getCalendarJson <- fileToChar("responses/getCalendar.json")
  calendarResponse <- httr:::response(url = getCalendarUrl,
                                      status_code = 200L,
                                      content = charToRaw(getCalendarJson))
  getStub$onCall(1)$returns(calendarResponse)
  calendar <- with_mock("httr::GET" = getStub$f,
                        "datarobot::GetDatetimePartition" = function(...) {
                          list(calendarId = fakeCalendarId)
                        },
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetCalendarFromProject(fakeProject))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(calendar, "dataRobotCalendar")
  ExpectHasKeys(calendar, expectedCols)
})
