#' Function to list all prediction jobs in a project
#'
#' @param project Either (1) a character string giving the unique alphanumeric identifier for the
#' project, or (2) a list containing the element projectId with this identifier.
#' @param status The status of the desired jobs: one of JobStatus$Queue, JobStatus$InProgress, or
#' JobStatus$Error. If NULL (default), queued and inprogress jobs are returned.
#'
#' @return Dataframe with one row for each prediction job in the queue,
#' with the following columns:
#' \describe{
#'   \item{status}{Prediction job status; one of JobStatus$Queue, JobStatus$InProgress, or
#' JobStatus$Error}
#'   \item{predictJobId}{Character string specifying the job id}
#'   \item{modelId}{Character string specifying the model from which
#'   predictions have been requested.}
#'   \item{projectId}{Character string specifying the project that contains the model.}
#' }
#' @export
#'
GetPredictJobs <- function(project, status = NULL) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictJobs")
  query <- if (is.null(status)) NULL else list(status = status)
  rawPredictJobStatus <- DataRobotGET(routeString, addUrl = TRUE, query = query)
  predictJobStatus <- rawPredictJobStatus
  predictJobId <- rawPredictJobStatus$id
  predictJobStatus$id <- NULL
  predictJobStatus$predictJobId <- predictJobId
  return(as.dataRobotPredictJobStatus(predictJobStatus))
}

PredictJobRoute <- function(projectId, predictJobId) {
  return(UrlJoin("projects", projectId, "predictJobs", predictJobId))
}

GetPredictJob <- function(project, predictJobId) {
  projectId <- ValidateProject(project)
  routeString <- PredictJobRoute(projectId, predictJobId)
  rawResponse <- DataRobotGET(routeString, addUrl = TRUE, returnRawResponse = TRUE,
                              config = httr::config(followlocation = 0))
  if (ResponseIsRedirection(rawResponse)) {
    Raise(Exceptions$PendingJobFinished())
  } else {
    return(ParseReturnResponse(rawResponse))
  }
}


as.dataRobotPredictJobStatus <- function(inList){
  elements <- c("status",
                "projectId",
                "modelId",
                "predictJobId"
                )
  return(ApplySchema(inList, elements))
}
