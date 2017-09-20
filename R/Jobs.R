#' Retrieve information about jobs
#'
#' This function requests information about the jobs that go through the DataRobot queue.
#'
#' @inheritParams GetPredictJobs
#' @return A list of lists with one element for each job. The named list for
#' each job contains:
#' \describe{
#'   \item{status}{Model job status; an element of JobStatus, e.g. JobStatus$Queue}
#'   \item{url}{Character string: URL to request more detail about the job}
#'   \item{id}{Character string specifying the job id}
#'   \item{jobType}{Job type. See JobType for valid values}
#'   \item{projectId}{Character string specifying the project that contains the model}
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListJobs(projectId)
#' }
#' @export
ListJobs <- function(project, status = NULL) {
  projectId <- ValidateProject(project)
  query <- if (is.null(status)) NULL else list(status = status)
  routeString <- UrlJoin("projects", projectId, "jobs")
  jobsResponse <- DataRobotGET(routeString, addUrl = TRUE, query = query, simplifyDataFrame = FALSE)
  jobs <- jobsResponse$jobs
  return(lapply(jobs, as.dataRobotJob))
}


#' Request information about a job
#'
#' @inheritParams DeleteProject
#' @param jobId Character string specifying the job id
#' @return list with following elements:
#' \describe{
#'   \item{status}{job status; an element of JobStatus, e.g. JobStatus$Queue}
#'   \item{url}{Character string: URL to request more detail about the job}
#'   \item{id}{Character string specifying the job id}
#'   \item{jobType}{Job type. See JobType for valid values}
#'   \item{projectId}{Character string specifying the project that contains the model}
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- GetModelJobs(project)
#'   job <- initialJobs[[1]]
#'   jobId <- job$modelJobId
#'   GetJob(projectId, jobId)
#' }
#' @export
GetJob <- function(project, jobId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  response <- DataRobotGET(routeString, addUrl = TRUE, config = httr::config(followlocation = 0))
  return(as.dataRobotJob(response))
}


as.dataRobotJob <- function(inList) {
  elements <- c("status",
                "url",
                "id",
                "jobType",
                "projectId"
 )
  return(ApplySchema(inList, elements))
}

#' Cancel a running job
#'
#' @param job object. The job you want to cancel (one of the items in the list returned from
#'   \code{ListJobs})
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- GetModelJobs(project)
#'   job <- initialJobs[[1]]
#'   DeleteJob(job)
#' }
#' @export
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
