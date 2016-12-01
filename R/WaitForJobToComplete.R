#' Wait for specified job to complete
#'
#' @inheritParams DeleteProject
#' @param jobId integer identifier (returned for example by RequestPrimeModel)
#' @param maxWait maximum time to wait (in seconds) for the job to complete
#' @return NULL
#' @export
#'
WaitForJobToComplete <- function(project, jobId, maxWait = 60) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  WaitForAsyncReturn(routeString, maxWait, failureStatuses = JobFailureStatuses)
  return(invisible(NULL))
}
