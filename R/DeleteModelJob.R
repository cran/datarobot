#' Delete a model job from the modeling queue
#'
#' This function deletes the modeling job specified by modelJobId from
#' the DataRobot modeling queue.
#'
#' @param project Either (1) a character string giving the unique alphanumeric
#' identifier for the project, or (2) a list containing the element projectId
#' with this identifier.
#' @param modelJobId Integer, identifier for the modeling job to be
#' deleted; can be obtained from the results returned by the function
#' GetModelJobs.
#' @export
#'
DeleteModelJob <- function(project, modelJobId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  response <- DataRobotDELETE(routeString, addUrl = TRUE)
  message(paste("Job", modelJobId, "deleted from project", projectId))
}

#' Delete a model job from the modeling queue
#'
#' (Deprecated in 2.1, will be removed in 2.3. Use GetModelJobs instead.)
#'
#' @inheritParams DeleteModelJob
#'
#' @export
DeletePendingJob <- function(project, modelJobId) {
  Deprecated("DeletePendingJob (use DeleteModelJob instead)", "2.1", "2.3")
  return(DeleteModelJob(project, modelJobId))
}
