#' Delete a specified element from the DataRobot project list
#'
#' This function deletes the project defined by project,
#' described under Arguments. This parameter may be obtained
#' in several ways, including: (1), as one of the projectId
#' elements of the list returned by GetProjectList; (2), as
#' the S3 object returned by the GetProject function; or (3),
#' as the list returned by the SetupProject function.
#'
#' @param project Either (1) a character string giving the unique alphanumeric
#' identifier for the project, or (2) a list containing the element projectId with this identifier.
#' @export
#'
DeleteProject <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId)
  response <- DataRobotDELETE(routeString, addUrl = TRUE)
  if (is.list(project)) {
    projectName <- project$projectName
  } else {
    projectName <- ""
  }
  message(paste("Project", projectName, "with projectId = ",
                projectId, "deleted"))
}
