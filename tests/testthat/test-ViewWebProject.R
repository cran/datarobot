context("ViewWebProject")
library(testthat)
library(stubthat)

test_that("Required parameters are present", {
  expect_error(ViewWebProject())
})

test_that("It gets the correct URL", {
  dataRobotUrl <- Sys.getenv("DATAROBOT_API_ENDPOINT")
  parsedUrl <- httr::parse_url(dataRobotUrl)
  urlString <- MakeUrl(parsedUrl, project = fakeProject)
  expect_equal(urlString,
               UrlJoin(paste0(parsedUrl$scheme, "://", parsedUrl$hostname),
               "projects", fakeProjectId, "eda"))
})

test_that("It can view a web project", {
  testReturn <- with_mock("datarobot:::DataRobotBrowse" = function(...) "NOOP", {
                          suppressMessages(expect_message(ViewWebProject(fakeProject),
                                                          "Opened URL"))
                          })
  expect_null(testReturn$returnValue)
})
