#' Update parameters for an existing project
#'
#' This function updates parameters for the project defined by project.
#'
#' @inheritParams DeleteProject
#' @param newProjectName Updated value for the projectName parameter
#' associated with the project.
#' @param holdoutUnlocked Either NULL (the default) or logical TRUE;
#' if TRUE, this function requests the DataRobot Autopilot to unlock
#' the holdout data subset.
#' @param workerCount Integer; sets the number of workers requested
#' for the associated project.
#' @export
#'
UpdateProject <- function(project, newProjectName = NULL, workerCount = NULL,
                          holdoutUnlocked = NULL) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId)
  bodyList <- list()
  bodyList$workerCount <- workerCount
  bodyList$holdoutUnlocked <- holdoutUnlocked
  bodyList$projectName <- newProjectName
  if (all(unlist(Map(is.null, bodyList)))) {
    stop("No update data is provided")
  }
  body <- jsonlite::unbox(as.data.frame(bodyList))
  response <- DataRobotPATCH(routeString, addUrl = TRUE,
                             body = body, encode = "json")
  message(paste("Project", projectId, "updated"))
}
