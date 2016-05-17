#' Retrieve status of Autopilot modeling jobs that are not complete
#'
#' This function requests information on DataRobot Autopilot modeling
#' tasks that are not complete, for one of three reasons: the task is
#' running and has not yet completed; the task is queued and has not
#' yet been started; or, the task has terminated due to an error.
#'
#' The jobStatus variable specifies which of the three groups of
#' modeling tasks is of interest. Specifically, if jobStatus has the
#' value 'inprogress', the request returns information about modeling
#' tasks that are running but not yet complete; if jobStatus has the
#' value 'queue', the request returns information about modeling tasks
#' that are scheduled to run but have not yet started; if jobStatus
#' has the value 'error', the request returns information about modeling
#' tasks that have terminated due to an error. By default, jobStatus is
#' NULL, which means jobs with status "inprogress" or "queue" are returned,
#' but not those with status "error".
#'
#' @inheritParams GetPredictJobs
#' @param jobStatus (deprecated - use status instead) The status of the desired jobs: one of
#' JobStatus$Queue, JobStatus$InProgress, or JobStatus$Error. If NULL (default), queued and
#' inprogress jobs are returned.
#'
#' @return A list of lists with one element for each modeling task
#' in the group being queried; if there are no tasks in the class
#' being queried, an empty list is returned. If the group is not empty,
#' a list is returned with the following nine elements:
#' \describe{
#'   \item{status}{Prediction job status; one of JobStatus$Queue, JobStatus$InProgress, or
#'   JobStatus$Error}
#'   \item{processes}{List of character vectors describing any preprocessing applied, possibly along with the model type}
#'   \item{projectId}{Character string giving the unique identifier for the project}
#'   \item{samplePct}{Numeric: the percentage of the dataset used in constructing the training dataset for model building}
#'   \item{modelType}{Character string specifying the model type}
#'   \item{featurelistId}{Character string identifying the featurelist used in fitting the model (derived from the modeling dataset)}
#'   \item{blueprintId}{Character string identifying the DataRobot blueprint on which the model is based}
#'   \item{modelJobId}{Character: id of the job}
#'   \item{modelId}{Character string uniquely identifying the model}
#' }
#' @export
#'
GetModelJobs <- function(project, status = NULL, jobStatus = NULL) {
  if (!is.null(jobStatus)) {
    if (!is.null(status)) {
      stop("You provided `jobStatus` (deprecated) and `status`. Please only provide `status`.")
    }
    Deprecated("jobStatus argument (use status instead)", "2.1", "2.3")
    status <- jobStatus
  }
  projectId <- ValidateProject(project)
  query <- if (is.null(status)) NULL else list(status = status)
  routeString <- UrlJoin("projects", projectId, "modelJobs")
  pendingList <- DataRobotGET(routeString, addUrl = TRUE, query = query)
  if (length(pendingList) == 0) {
    return(data.frame(status = character(0), processes = I(list()), projectId = character(0),
                      samplePct = numeric(0), modelType = character(0),
                      featurelistId = character(0),  modelCategory = character(0),
                      blueprintId = character(0), modelJobId = character(0)))
  }
  idIndex <- which(names(pendingList) == 'id')
  names(pendingList)[idIndex] <- 'modelJobId'
  return(pendingList)
}

#' Retrieve status of Autopilot modeling jobs that are not complete
#'
#' (Deprecated in 2.1, will be removed in 2.3. Use GetModelJobs instead.)
#'
#' @inheritParams GetModelJobs
#'
#' @export
GetPendingJobs <- function(project, jobStatus = NULL) {
  Deprecated("GetPendingJobs (use GetModelJobs instead)", "2.1", "2.3")
  return(GetModelJobs(project, jobStatus = jobStatus))
}
