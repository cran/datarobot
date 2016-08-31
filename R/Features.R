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

#' Details about all features for this project
#'
#' @inheritParams DeleteProject
#' @return A list of lists with one element for each feature The named list for
#' each feature contains:
#' \describe{
#'	 \item{id}{feature id - note: Throughout the API, features are specified using their names,
#'	 not this ID.}
#'	 \item{name}{feature name}
#'	 \item{featureType}{feature type: 'Numeric', 'Categorical', etc.}
#'	 \item{importance}{numeric measure of the strength of relationship between the feature and
#'	 target (independent of any model or other features).}
#'	 \item{lowInformation}{whether feature has too few values to be informative}
#'	 \item{uniqueCount}{number of unique values}
#'	 \item{naCount}{number of missing values}}
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
#' @param featureName id of the feature to be retrieve. Note: DataRobot renames some features, so
#' the feature name may not be the one from your original data. You can use ListFeatureInfo to list
#' the features and check the name. Deprecation note: If the name given does not match any feature
#' names, we will treat it as an integer feature ID, and return the feature with the matching ID
#' (if any). This is for backwards-compatibility and will be removed in v3.0.

#' @return A named list which contains:
#' \describe{
#'	 \item{id}{feature id - note: Throughout the API, features are specified using their names,
#'	 not this ID.}
#'	 \item{name}{feature name}
#'	 \item{feature_type}{feature type: 'Numeric', 'Categorical', etc.}
#'	 \item{importance}{numeric measure of the strength of relationship between the feature and
#'	 target (independent of any model or other features).}
#'	 \item{low_information}{whether feature has too few values to be informative}
#'	 \item{unique_count}{number of unique values}
#'	 \item{na_count}{number of missing values}}
#' @export
#'
GetFeatureInfo <- function(project, featureName) {
  projectId <- ValidateProject(project)
  featureForUrl <- if (is.character(featureName)) URLencode(enc2utf8(featureName)) else featureName
  routeString <- UrlJoin("projects", projectId, "features", featureForUrl)
  return(DataRobotGET(routeString, addUrl = TRUE, simplifyDataFrame = FALSE))
}


CreateDerivedFeatureFunctionMaker <- function(variableType) {
  featureRequester <- function(project, parentName, name=NULL, dateExtraction=NULL,
                               replacement=NULL, maxWait=60) {
    projectId <- ValidateProject(project)
    routeString <- UrlJoin("projects", projectId, "typeTransformFeatures")
    if (is.null(name)) name <- sprintf("%s (%s)", parentName, variableType)
    body <- list(name = name, parentName = parentName, variableType = variableType,
                 replacement = replacement, dateExtraction = dateExtraction)
    creationRequestResponse <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                                             returnRawResponse = TRUE, encode = "json")
    return(FeatureFromAsyncUrl(httr::headers(creationRequestResponse)$location, maxWait = maxWait))
  }
  return(featureRequester)
}

##' @name CreateDerivedFeatures
##' @rdname CreateDerivedFeatures
##'
##' @title Derived Features
##'
##' @description These functions request that new features be created as transformations of existing
##' features and wait for the new feature to be created.
##'
##' @inheritParams DeleteProject
##' @param parentName The name of the parent feature.
##' @param name The name of the new feature.
##' @param dateExtraction dateExtraction: The value to extract from the date column:
##' 'year', 'yearDay', 'month', 'monthDay', 'week', or 'weekDay'. Required for transformation of a
##' date column. Otherwise must not be provided.
##' @param replacement The replacement in case of a failed transformation. Optional.
##' @param maxWait The maximum time (in seconds) to wait for feature creation.
##'
##' @return Details for the created feature; same schema as the object returned from GetFeatureInfo.
NULL

##' @rdname CreateDerivedFeatures
##' @export
CreateDerivedFeatureAsCategorical <- CreateDerivedFeatureFunctionMaker("categorical")

##' @rdname CreateDerivedFeatures
##' @export
CreateDerivedFeatureAsText <- CreateDerivedFeatureFunctionMaker("text")

##' @rdname CreateDerivedFeatures
##' @export
CreateDerivedFeatureAsNumeric <- CreateDerivedFeatureFunctionMaker("numeric")


#' Retrieve a feature from the creation URL
#'
#' If feature creation times out, the error message includes a URL corresponding to the
#' creation task. That URL can be passed to this function (which will return the feature
#' details when finished) to resume waiting for feature creation.
#'
#' @inheritParams ProjectFromAsyncUrl
#' @export
#'
FeatureFromAsyncUrl <- function(asyncUrl, maxWait = 60) {
  timeoutMessage <-
    paste(sprintf("Feature creation did not complete before timeout (%ss).", maxWait),
          "To query its status and (if complete) retrieve the completed feature info, use:\n  ",
          sprintf("%s('%s')", "FeatureFromAsyncUrl", asyncUrl))
  return(tryCatch(WaitForAsyncReturn(asyncUrl,
                                     addUrl = FALSE,
                                     maxWait = maxWait,
                                     failureStatuses = "ERROR"),
                  AsyncTimeout = function(e) stop(timeoutMessage)))
}
