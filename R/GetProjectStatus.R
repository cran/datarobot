#' Request Autopilot status for a specified DataRobot project
#'
#' This function polls the DataRobot Autopilot for the status
#' of the project specified by the project parameter.
#'
#' @inheritParams DeleteProject
#' @return List with the following three components:
#' \describe{
#'   \item{autopilotDone}{Logical flag indicating whether the Autopilot has completed}
#'   \item{stage}{Character string specifying the Autopilot stage}
#'   \item{stageDescription}{Character string interpreting the Autopilot stage value}
#' }
#' @export
#'
GetProjectStatus <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "status")
  autopilotStatus <- DataRobotGET(routeString, addUrl = TRUE)
  return(autopilotStatus)
}
