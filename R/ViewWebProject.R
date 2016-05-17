#' Retrieve a DataRobot web page that displays detailed project information
#'
#' This function brings up a web page that displays detailed project
#' information like that available from the standard DataRobot user interface.
#'
#' @inheritParams DeleteProject
#' @export
#'
ViewWebProject <- function(project) {
  projectId <- ValidateProject(project)
  #
  #  GET Datarobot endpoint URL
  #
  dataRobotUrl <- Sys.getenv("DataRobot_URL")
  #
  parsedUrl <- httr::parse_url(dataRobotUrl)
  #
  #  Specify route for web-browser
  #
  #    This statement gives an absolute_paths_linter false positive:
  #
  urlString <- paste(parsedUrl['scheme'], '://', parsedUrl['hostname'],
                     '/', sep = "")
  routeString <- paste(urlString, "projects/", projectId,
                       "/models", sep = "")  # nolint
  #
  #  Invoke browser to open specified route
  #
  browseURL(routeString)
  #
  #  Display user message and exit
  #
  if (is.list(project)) {
    projectName <- project$projectName
  } else {
    projectName <- NULL
  }
  if (is.null(projectName)) {
    message(paste("Opened URL", urlString, "for selected project"))
  } else {
    message(paste("Opened URL", urlString, "for project:", projectName))
  }
}
