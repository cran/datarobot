#' Returns the list of features (i.e., variables) on which a specified model is based
#'
#' This function returns the list of features (typically, response variable
#' and raw covariates) used in building the model specified by model, an S3
#' object of class 'dataRobotModel'.
#'
#' @inheritParams DeleteModel
#'
#' @return A character vector of feature names, with one component for
#' each model feature.
#' @export
#'
ListModelFeatures <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "features")
  featurelist <- DataRobotGET(routeString, addUrl = TRUE)
  features <- featurelist$featureNames
  return(features)
}

#' Deprecated (use ListModelFeatures instead)
#' @inheritParams DeleteModel
#' @export
#'
GetFeatures <- function(model) {
  Deprecated("GetFeatures (use ListModelFeatures instead)", "2.2", "2.3")
  return(ListModelFeatures(model))
}

#' Details about all features for this project
#'
#' @inheritParams DeleteProject
#' @return A list of lists with one element for each feature The named list for
#' each feature contains:
#' \describe{
#'	 \item{id}{feature id}
#'	 \item{name}{feature name}
#'	 \item{feature_type}{feature type: 'Numeric', 'Categorical', etc.}
#'	 \item{importance}{numeric measure of the strength of relationship between the feature and
#'	 target (independent of any model or other features).}
#'	 \item{low_information}{whether feature has too few values to be informative}
#'	 \item{unique_count}{number of unique values}
#'	 \item{na_count}{number of missing values}}
#' @export
#'
ListFeatureInfo <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "features")
  return(DataRobotGET(routeString, addUrl = TRUE, simplifyDataFrame = FALSE))
}

#' Details about a feature
#'
#' @inheritParams DeleteProject
#' @param featureId id of the feature to be retrieved
#' @return A named list which contains:
#' \describe{
#'	 \item{id}{feature id}
#'	 \item{name}{feature name}
#'	 \item{feature_type}{feature type: 'Numeric', 'Categorical', etc.}
#'	 \item{importance}{numeric measure of the strength of relationship between the feature and
#'	 target (independent of any model or other features).}
#'	 \item{low_information}{whether feature has too few values to be informative}
#'	 \item{unique_count}{number of unique values}
#'	 \item{na_count}{number of missing values}}
#' @export
#'
GetFeatureInfo <- function(project, featureId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "features", featureId)
  return(DataRobotGET(routeString, addUrl = TRUE, simplifyDataFrame = FALSE))
}
