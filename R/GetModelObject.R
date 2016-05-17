#' Retrieve the details of a specified model
#'
#' This function returns a DataRobot S3 object of class
#' dataRobotModel for the model defined by project and modelId.
#'
#' The S3 object returned by this function is required by the
#' functions DeleteModel, ListModelFeatures, and RequestSampleSizeUpdate.
#'
#' @inheritParams DeleteProject
#' @param modelId Unique alphanumeric identifier for the model of interest.
#' @return An S3 object of class `dataRobotModel', which is a list
#' with the following 13 components:
#' \describe{
#'   \item{featurelistId}{Character string: unique alphanumeric identifier for the featurelist on which the model is based}
#'   \item{processes}{Character vector with components describing preprocessing; may include modelType}
#'   \item{featurelistName}{Character string giving the name of the featurelist on which the model is based}
#'   \item{projectId}{Character string giving the unique alphanumeric identifier for the project}
#'   \item{samplePct}{Numeric: percentage of the dataset used to form the training dataset for model fitting}
#'   \item{modelType}{Character string describing the model type}
#'   \item{metrics}{List with one element for each valid metric associated with the model. Each element is a list with elements for each possible evaluation type (holdout, validation, and crossValidation)}
#'   \item{modelCategory}{Character string giving model category (e.g., blend, model)}
#'   \item{blueprintId}{Character string giving the unique DataRobot blueprint identifier on which the model is based}
#'   \item{modelId}{Character string giving the unique alphanumeric model identifier}
#'   \item{projectName}{Character string: optional description of project defined by projectId}
#'   \item{projectTarget}{Character string defining the target variable predicted by all models in the project}
#'   \item{projectMetric}{Character string defining the fitting metric optimized by all project models}
#' }
#' @export
#'
GetModelObject <- function(project, modelId) {
  #  Fail if modelId is an empty string
  if (modelId == "") {
    stop("Invalid modelId specified")
  } else {
    projectId <- ValidateProject(project)
    fullProject <- GetProject(projectId)
    projectName <- fullProject$projectName
    projectTarget <- fullProject$target
    projectMetric <- fullProject$metric
    routeString <- UrlJoin("projects", projectId, "models", modelId)
    modelDetails <- DataRobotGET(routeString, addUrl = TRUE)
    #
    #  Request successful - extract data from $content element of
    #  Reformat results: (1) change name "id" to "modelId";
    #  (2) reformat $metrics list component to replace NULL
    #  representation of missing values with NA
    #
    #  Also, add projectName, projectTarget, and projectMetric
    #
    #  NOTE: if the $processes list is empty, it is represented
    #        as an empty list rather than an empty character vector,
    #        while GetAllModels returns an empty character vector
    #        for this case; for compatability, check
    #        for this case and reformat if detected
    #
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
    class(modelDetails) <- 'dataRobotModel'
    return(modelDetails)
  }
}
