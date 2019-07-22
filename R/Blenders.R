#' Retrieve the details of a specified blender model
#'
#' This function returns a DataRobot S3 object of class
#' dataRobotModel for the model defined by project and modelId.
#'
#' @inheritParams DeleteProject
#' @param modelId character. Unique alphanumeric identifier for the blender model of interest.
#' @return An S3 object of class `dataRobotBlenderModel', which is a list
#' with the following components:
#' \itemize{
#'  \item featurelistId. Character string: unique alphanumeric identifier for the =
#'    featurelist on which the model is based.
#'  \item processes. Character vector with components describing preprocessing; may
#'    include modelType.
#'  \item featurelistName. Character string giving the name of the featurelist on which
#'    the model is based.
#'  \item projectId. Character string giving the unique alphanumeric identifier for the project.
#'  \item samplePct. Numeric: percentage of the dataset used to form the training dataset
#"    for model fitting.
#'  \item isFrozen. Logical : is model created with frozen tuning parameters.
#'  \item modelType. Character string describing the model type.
#'  \item metrics. List with one element for each valid metric associated with the model.
#'    Each element is a list with elements for each possible evaluation type
#'    (holdout, validation, and crossValidation).
#'  \item modelCategory. Character string giving model category (e.g., blend, model).
#'  \item blueprintId. Character string giving the unique DataRobot blueprint identifier
#'    on which the model is based.
#'  \item modelIds. Character string giving the unique alphanumeric model identifier of
#'    blended models.
#'  \item blenderMethod. Character string describing blender method.
#'  \item modelId. Character string giving the unique alphanumeric blender model identifier.
#'  \item projectName. Character string: name of project defined by projectId.
#'  \item projectTarget. Character string defining the target variable predicted by all
#'    models in the project.
#'  \item projectMetric. Character string defining the fitting metric optimized by all
#'    project models.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetBlenderModel(projectId, modelId)
#' }
#' @export
GetBlenderModel <- function(project, modelId) {
  # Fail if modelId is an empty string
  if (modelId == "") {
   stop("Invalid modelId specified")
  } else {
   projectId <- ValidateProject(project)
   fullProject <- GetProject(projectId)
   projectName <- fullProject$projectName
   projectTarget <- fullProject$target
   projectMetric <- fullProject$metric
   routeString <- UrlJoin("projects", projectId, "blenderModels", modelId)
   modelDetails <- DataRobotGET(routeString)
   listNames <- names(modelDetails)
   idIndex <- which(listNames == "id")
   names(modelDetails)[idIndex] <- "modelId"
   modelDetails$metrics <- ReformatMetrics(modelDetails$metrics)
   modelDetails$projectName <- projectName
   modelDetails$projectTarget <- projectTarget
   modelDetails$projectMetric <- projectMetric
   if (length(modelDetails$processes) == 0) {
    modelDetails$processes <- character(0)
   }
   as.dataRobotBlenderModel(modelDetails)
  }
}

#' Submit a job for creating blender model. Upon success, the new job will
#' be added to the end of the queue.
#'
#' This function requests the creation of a blend of several models in specified
#' DataRobot project. The function also allows the user to specify method used
#' for blending. This function returns an integer modelJobId value,
#' which can be used by the GetBlenderModelFromJobId function to return the full
#" blender model object.
#'
#' @inheritParams DeleteProject
#' @param modelIds character. Vector listing the model Ids to be blended.
#' @param blendMethod character. Parameter specifying blending method.
#'   See acceptable values within BlendMethods.
#' @return An integer value that can be used as the modelJobId parameter
#' in subsequent calls to the GetBlenderModelFromJobId function.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelsToBlend <- c("5996f820af07fc605e81ead4", "59a5ce3301e9f0296721c64c")
#'   RequestBlender(projectId, modelId, "GLM")
#' }
#' @export
RequestBlender <- function(project, modelIds, blendMethod) {
 projectId <- ValidateProject(project)
 routeString <- UrlJoin("projects", projectId, "blenderModels")
 body <- list(modelIds = I(modelIds), blenderMethod = blendMethod)
 rawReturn <- DataRobotPOST(routeString, body = body,
                            returnRawResponse = TRUE, encode = "json")
 message("New blender request received")
 JobIdFromResponse(rawReturn)
}

#' Retrieve a new or updated blender model defined by modelJobId
#'
#' The function RequestBlender initiates the creation of new blender models in a
#' DataRobot project.
#"
#' It submits requests to the DataRobot modeling
#' engine and returns an integer-valued modelJobId. The
#' GetBlenderModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotBlenderModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete. Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' @inheritParams DeleteProject
#' @param modelJobId integer. The integer returned by RequestBlender.
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to
#"   complete.
#' @return An S3 object of class 'dataRobotBlenderModel' summarizing all
#' available information about the model. It is a list
#' with the following components:
#' \itemize{
#'  \item featurelistId. Character string: unique alphanumeric identifier for the
#'   featurelist on which the model is based.
#'  \item processes. Character vector with components describing preprocessing; may
#'   include modelType.
#'  \item featurelistName. Character string giving the name of the featurelist on which
#'   the model is based.
#'  \item projectId. Character string giving the unique alphanumeric identifier for the
#'   project.
#'  \item samplePct. Numeric: percentage of the dataset used to form the training dataset
#'   for model fitting.
#'  \item trainingRowCount. Integer. The number of rows of the project dataset used in training
#'     the model. In a datetime partitioned project, if specified, defines the number of
#'     rows used to train the model and evaluate backtest scores; if unspecified, either
#'     \code{trainingDuration} or \code{trainingStartDate} and \code{trainingEndDate} was used to
#'  \item isFrozen. Logical : is model created with frozen tuning parameters.
#'  \item modelType. Character string describing the model type.
#'  \item metrics. List with one element for each valid metric associated with the model.
#'   Each element is a list with elements for each possible evaluation type (holdout,
#"   validation, and crossValidation).
#'  \item modelCategory. Character string giving model category (e.g., blend, model).
#'  \item blueprintId. Character string giving the unique DataRobot blueprint identifier
#'   on which the model is based.
#'  \item modelIds. Character string giving the unique alphanumeric model identifier of
#'   blended models.
#'  \item blenderMethod. Character string describing blender method.
#'  \item id. Character string giving the unique alphanumeric blender model identifier.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelsToBlend <- c("5996f820af07fc605e81ead4", "59a5ce3301e9f0296721c64c")
#'   blendJobId <- RequestBlender(projectId, modelId, "GLM")
#'   GetBlenderModelFromJobId(projectId, blendJobId)
#' }
#' @export
GetBlenderModelFromJobId <- function(project, modelJobId, maxWait = 600) {
 projectId <- ValidateProject(project)
 routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
 message("Blender Model request issued: awaiting response")
 modelDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                   failureStatuses = JobFailureStatuses)
 modelId <- modelDetails$id
 returnModel <- GetBlenderModel(projectId, modelId)
 message("Blender Model ", modelId, " retrieved")
 class(returnModel) <- "dataRobotBlenderModel"
 returnModel
}

as.dataRobotBlenderModel <- function(inList) {
 elements <- c("featurelistId",
               "processes",
               "featurelistName",
               "projectId",
               "projectName",
               "projectTarget",
               "samplePct",
               "trainingRowCount",
               "isFrozen",
               "modelType",
               "projectMetric",
               "metrics",
               "modelCategory",
               "blueprintId",
               "modelIds",
               "blenderMethod",
               "modelId")
 outList <- ApplySchema(inList, elements)
 class(outList) <- "dataRobotBlenderModel"
 outList
}


#' Check whether individual models can be blended together
#'
#' @inheritParams DeleteProject
#' @param modelIds list. A list of model ids corresponding to the models to check.
#' @param blendMethod character. The blender method to check. See \code{BlendMethods}.
#' @return List with:
#'   \itemize{
#'     \item blendable logical. Whether or not the models can be blended.
#'     \item reason character. An explanation for why the models cannot be blended, if not
#'       blendable. Otherwise \code{""}.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelsToBlend <- c("5996f820af07fc605e81ead4", "59a5ce3301e9f0296721c64c")
#'   IsBlenderEligible(projectId, modelId, "GLM")
#' }
#' @export
IsBlenderEligible <- function(project, modelIds, blendMethod) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "blenderModels", "blendCheck")
  body <- list(modelIds = modelIds, blenderMethod = blendMethod)
  response <- DataRobotPOST(routeString, body = body, encode = "json")
  ApplySchema(response, c("reason", "blendable"))
}
