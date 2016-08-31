#' Function to set up a new DataRobot project
#'
#' This function uploads a modeling dataset defined by the dataSource parameter
#' and allows specification of the optional project name projectName. The
#' dataSource parameter can be either the name of a CSV file or a dataframe;
#' in the latter case, it is saved as a CSV file whose name is described in
#' the Details section. This function returns the projectName specified in the
#' calling sequence, the unique alphanumeric identifier projectId for the new
#' project, the name of the modeling dataset uploaded to create this project,
#' and the project creation time and date.
#'
#' The DataRobot modeling engine requires a CSV file containing the data to be
#' used in fitting models, and this has been implemented here in two ways.
#' The first and simpler is to specify dataSource as the name of this CSV file,
#' but for the convenience of those who wish to work with dataframes, this
#' function also provides the option of specifying a dataframe, which is then
#' written to a CSV file and uploaded to the DataRobot server. In this case, the
#' file name is either specified directly by the user through the saveFile
#' parameter, or indirectly from the name of the dataSource dataframe if
#' saveFile = NULL (the default).  In this second case, the file name consists
#' of the name of the dataSource dataframe with the string csvExtension appended.
#'
#' @param dataSource Either (a) the name of a CSV file or (b) a dataframe;
#' in either case, this parameter identifies the source of the data from which
#' all project models will be built.  See Details.
#' @param projectName Optional character string specifying a project name.
#' @param maxWait The maximum time to wait for each of two steps: (1) The initial project creation
#' request, and (2) data processing that occurs after receiving the response to this initial
#' request.
#' @return This function returns a list with the following four components:
#' \describe{
#'   \item{projectName}{The name assigned to the DataRobot project}
#'   \item{projectId}{The unique alphanumeric project identifier for this DataRobot project}
#'   \item{fileName}{The name of the CSV modeling file uploaded for this project}
#'   \item{created}{Character string containing the time and date of project creation}
#' }
#' @export
#'
SetupProject <- function(dataSource, projectName = NULL,
                         maxWait = 60 * 60) {
  dataPath <- DataPathFromDataArg(dataSource)

  routeString <- "projects/"
  dataList <- list(projectName = projectName, file = httr::upload_file(dataPath))
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList, returnRawResponse = TRUE,
                             httr::timeout(maxWait))
  message(paste("Project", projectName,
                "creation requested, awaiting creation"))
  project <- ProjectFromAsyncUrl(httr::headers(rawReturn)$location, maxWait = maxWait)
  message(sprintf("Project %s (%s) created", project$projectId, project$projectName))
  return(project)
}

#' Retrieve a project from the project-creation URL
#'
#' If project creation times out, the error message includes a URL corresponding to the project
#' creation task. That URL can be passed to this function (which will return the completed project
#' details when finished) to resume waiting for project creation.
#'
#' @param asyncUrl The temporary status URL
#' @param maxWait The maximum time to wait (in seconds) for project creation before aborting.
#' @export
#'
ProjectFromAsyncUrl <- function(asyncUrl, maxWait = 60) {
  timeoutMessage <-
    paste(sprintf("Project creation did not complete before timeout (%ss).", maxWait),
          "To query its status and (if complete) retrieve the completed project, use:\n  ",
          sprintf("%s('%s')", "ProjectFromAsyncUrl", asyncUrl))
  projectInfo <- tryCatch(WaitForAsyncReturn(asyncUrl,
                                               addUrl = FALSE,
                                               maxWait = maxWait,
                                               failureStatuses = "ERROR"),
                            AsyncTimeout = function(e) stop(timeoutMessage))
  return(list(projectName = projectInfo$projectName,
              projectId = projectInfo$id,
              fileName = projectInfo$fileName,
              created = projectInfo$created))
}

DataPathFromDataArg <- function(dataSource, saveFile = NULL, csvExtension = NULL) {
  # Can remove last two arguments after 2.3

  #  Verify that newdata is either an existing datafile or a dataframe
  #    If a dataframe, save as a CSV file
  #    If neither an existing datafile nor a dataframe, halt with error
  #
  if (is(dataSource, "character")) {
    if (file.exists(dataSource)) {
      dataPath <- dataSource
    } else {
      errorMsg <- paste("No file named", dataSource,
                        "exists in the working directory", getwd())
      stop(strwrap(errorMsg))
    }
  } else {
    if (is.data.frame(dataSource)) {
      #
      #  If dataSource is a dataframe, save as a CSV file
      #
      if (is.null(saveFile)) {
        dataPath <- tempfile(fileext = "_autoSavedDF.csv")
      } else {
        dataPath <- saveFile
      }
      write.csv(dataSource, dataPath, row.names = FALSE)
    } else {
      errorMsg <- paste(deparse(substitute(dataSource)),
                        "is not a valid data file name or dataframe")
      stop(strwrap(errorMsg))
    }
  }
  return(dataPath)
}
