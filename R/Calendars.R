#' Create a calendar from an uploaded CSV.
#'
#' @param file character. The filename containing the calendar CSV to upload.
#' @param name character. Optional. The name of the calendar.
#' @param maxWait integer. The maximum time (in seconds) to wait for the retrieve to complete.
#' @return An S3 object of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'    CreateCalendar("myRatingTable.csv", name = "myCalendar")
#' }
#' @export
CreateCalendar <- function(file, name = NULL, maxWait = 600) {
  if (is.null(name)) { name <- file }
  routeString <- UrlJoin("calendars", "fileUpload")
  body <- list(name = name,
               file = UploadData(file))
  rawReturn <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  location <- httr::headers(rawReturn)$location
  calendar <- WaitForAsyncReturn(location, maxWait = maxWait, addUrl = FALSE,
                                 failureStatuses = JobFailureStatuses)
  as.dataRobotCalendar(calendar)
}

as.dataRobotCalendar <- function(inList) {
  inList$id <- inList$Id
  elements <- c("name", "created", "calendarStartDate", "calendarEndDate",
                "numEventTypes", "source", "projectIds", "id")
  outList <- ApplySchema(inList, elements)
  class(outList) <- "dataRobotCalendar"
  outList
}


#' Retrieve a calendar
#'
#' @param calendarId character. The ID of the calendar to retrieve.
#' @return An S3 object of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'    calendarId <- "5da75da31fb4a45b8a815a53"
#'    GetCalendar(calendarId)
#' }
#' @export
GetCalendar <- function(calendarId) {
  routeString <- UrlJoin("calendars", calendarId)
  as.dataRobotCalendar(DataRobotGET(routeString))
}


#' Retrieve the calendar for a particular project.
#'
#' @inheritParams GetProject
#' @return An S3 object of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'    GetCalendar(projectId)
#' }
#' @export
GetCalendarFromProject <- function(project) {
  GetCalendar(GetDatetimePartition(project)$calendarId)
}


#' List all available calendars.
#'
#' @return A list of S3 objects of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'    ListCalendars()
#' }
#' @export
ListCalendars <- function() {
  calendars <- DataRobotGET("calendars", simplifyDataFrame = FALSE)
  calendars <- GetServerDataInRows(calendars)
  calendars <- lapply(calendars, as.dataRobotCalendar)
  class(calendars) <- c("listOfCalendars", "listSubclass")
  calendars
}


#' Delete a calendar
#'
#' @inheritParams GetCalendar
#' @return NULL
#' @examples
#' \dontrun{
#'    calendarId <- "5da75da31fb4a45b8a815a53"
#'    DeleteCalendar(calendarId)
#' }
#' @export
DeleteCalendar <- function(calendarId) {
  calendarId <- ValidateCalendar(calendarId)
  routeString <- UrlJoin("calendars", calendarId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}


#' Update a calendar
#'
#' Currently supports changing the name of a calendar.
#'
#' @inheritParams GetCalendar
#' @param name character. The new name to name the calendar.
#' @return An S3 object of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'    calendarId <- "5da75da31fb4a45b8a815a53"
#'    UpdateCalendar(calendarId, name = "New name for calendar")
#' }
#' @export
UpdateCalendar <- function(calendarId, name = NULL) {
  calendarId <- ValidateCalendar(calendarId)
  routeString <- UrlJoin("calendars", calendarId)
  DataRobotPATCH(routeString, body = list(name = name))
  GetCalendar(calendarId)
}
