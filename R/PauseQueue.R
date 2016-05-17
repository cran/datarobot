#' Pause the DataRobot modeling queue
#'
#' This function pauses the DataRobot modeling queue for a specified project
#'
#' @inheritParams DeleteProject
#' @export
#'
PauseQueue <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "autopilot")
  bodyList <- list(command = 'stop')
  response <- DataRobotPOST(routeString, addUrl = TRUE, body = bodyList)
  message(paste("Queue for project", projectId, "paused"))
}
