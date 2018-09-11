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
#' @return A list of lists with one element for each modeling task
#' in the group being queried; if there are no tasks in the class
#' being queried, an empty list is returned. If the group is not empty,
#' a list is returned with the following nine elements:
#' \itemize{
#'   \item status. Prediction job status; an element of JobStatus, e.g. JobStatus$Queue.
#'   \item processes. List of character vectors describing any preprocessing applied.
#'   \item projectId. Character string giving the unique identifier for the project.
#'   \item samplePct. Numeric: the percentage of the dataset used for model building.
#'   \item modelType. Character string specifying the model type.
#'   \item modelCategory. Character string: what kind of model this is - 'prime' for DataRobot
#'     Prime models, 'blend' for blender models, and 'model' for other models.
#'   \item featurelistId. Character string: id of the featurelist used in fitting the model.
#'   \item blueprintId. Character string: id of the DataRobot blueprint on which the model is based.
#'   \item modelJobId. Character: id of the job.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetModelJobs(projectId)
#' }
#' @export
GetModelJobs <- function(project, status = NULL) {
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
  return(as.dataRobotModelJob(pendingList))
}

#' Request information about a single model job
#'
#' @inheritParams DeleteProject
#' @param modelJobId Character string specifying the job id
#' @return list with following elements:
#' \itemize{
#'   \item status. Model job status; an element of JobStatus, e.g. JobStatus$Queue.
#'   \item processes. List of character vectors describing any preprocessing applied.
#'   \item projectId. Character string giving the unique identifier for the project.
#'   \item samplePct. Numeric: the percentage of the dataset used for model building.
#'   \item trainingRowCount. Integer. The number of rows of the project dataset used in training
#'     the model.
#'   \item modelType. Character string specifying the model this job builds.
#'   \item modelCategory. Character string: what kind of model this is - 'prime' for DataRobot Prime
#'     models, 'blend' for blender models, and 'model' for other models.
#'   \item featurelistId. Character string: id of the featurelist used in fitting the model.
#'   \item blueprintId. Character string: id of the DataRobot blueprint on which the model is based.
#'   \item modelJobId. Character: id of the job.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- GetModelJobs(project)
#'   job <- initialJobs[[1]]
#'   modelJobId <- job$modelJobId
#'   GetModelJob(projectId, modelJobId)
#' }
#' @export
GetModelJob <- function(project, modelJobId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  response <- DataRobotGET(routeString, addUrl = TRUE, config = httr::config(followlocation = 0))
  idIndex <- which(names(response) == 'id')
  names(response)[idIndex] <- 'modelJobId'
  return(as.dataRobotModelJob(response))
}

as.dataRobotModelJob <- function(inList) {
  elements <- c("status",
                "processes",
                "projectId",
                "samplePct",
                "trainingRowCount",
                "modelType",
                "featurelistId",
                "modelCategory",
                "blueprintId",
                "modelJobId")
  return(ApplySchema(inList, elements))
}
