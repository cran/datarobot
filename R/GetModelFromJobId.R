#' Retrieve a new or updated model defined by modelJobId
#'
#' The functions RequestNewModel and RequestSampleSizeUpdate
#' initiate the creation of new models in a DataRobot project.
#' Both functions submit requests to the DataRobot modeling
#' engine and return an integer-valued modelJobId.  The
#' GetModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete.  Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' @inheritParams DeleteProject
#' @param modelJobId The integer returned by either RequestNewModel
#' or RequestSampleSizeUpdate.
#' @param maxWait Integer, The maximum time (in seconds) to wait for the model job to complete
#' @return An S3 object of class 'dataRobotModel' summarizing all
#' available information about the model.
#' @export
#'
GetModelFromJobId <- function(project, modelJobId, maxWait = 60) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                     failureStatuses = JobFailureStatuses)
  modelId <- modelDetails$id
  returnModel <- GetModelObject(projectId, modelId)
  message("Model ", modelId, " retrieved")
  class(returnModel) <- 'dataRobotModel'
  return(returnModel)
}
