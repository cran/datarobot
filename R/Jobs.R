#' Retrieve information about jobs
#'
#' This function requests information about the jobs that go through the DataRobot queue.
#'
#' @inheritParams GetPredictJobs
#'
#' @return A list of lists with one element for each job. The named list for
#' each job contains:
#' \describe{
#'   \item{status}{job status ("inprogress", "queue", or "error")}
#'   \item{url}{URL to request more detail about the job (character)}
#'   \item{id}{job id (character).}
#'   \item{jobType}{Job type. See JobType for valid values}
#'   \item{projectId}{the id of the project that contains the model (character).}
#' }
#' @export
#'
ListJobs <- function(project, status = NULL) {
  projectId <- ValidateProject(project)
  query <- if (is.null(status)) NULL else list(status = status)
  routeString <- UrlJoin("projects", projectId, "jobs")
  jobsResponse <- DataRobotGET(routeString, addUrl = TRUE, query = query, simplifyDataFrame = FALSE)
  jobs <- jobsResponse$jobs
  return(jobs)
}

#' Cancel a running job
#'
#' @param job The job you want to cancel (one of the items in the list returned from ListJobs)
#'
#' @export
#'
DeleteJob <- function(job) {
  if (!("url" %in% names(job))) {
    stop("The job has no `url` field. This function requires a job like from ListJobs.")
  }
  return(invisible(DataRobotDELETE(job$url, addUrl = FALSE)))
}

JobIdFromJobLink <- function(jobLink) {
  # Same logic as used in our Python package to get the id from the link
  pathSplit <- unlist(strsplit(jobLink, "/"))
  return(pathSplit[length(pathSplit)])
}

JobIdFromResponse <- function(rawResponse) {
  # Gets the job id from the response to any request that puts a job in the project queue.
  rawHeaders <- httr::headers(rawResponse)
  predictJobPath <- rawHeaders$location
  return(JobIdFromJobLink(predictJobPath))
}
