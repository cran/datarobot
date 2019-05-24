#' Returns the list of features (i.e., variables) on which a specified model is based
#'
#' This function returns the list of features (typically, response variable
#' and raw covariates) used in building the model specified by model, an S3
#' object of class 'dataRobotModel'.
#'
#' @inheritParams DeleteModel
#' @return A character vector of feature names, with one component for
#' each model feature.
#' @examples
#' \dontrun{
#'   modelId <- "5996f820af07fc605e81ead4"
#'   ListModelFeatures(modelId)
#' }
#' @export
ListModelFeatures <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "features")
  featurelist <- DataRobotGET(routeString, addUrl = TRUE)
  featurelist$featureNames
}

#' Details about all features for this project
#'
#' @inheritParams DeleteProject
#' @return A named list which contains:
#' \itemize{
#'   \item id nummeric. feature id. Note that throughout the API, features are specified using
#'     their names, not this ID.
#'   \item name character. The name of the feature.
#'   \item featureType character. Feature type: 'Numeric', 'Categorical', etc.
#'   \item importance numeric. numeric measure of the strength of relationship between the
#'     feature and target (independent of any model or other features).
#'   \item lowInformation logical. Whether the feature has too few values to be informative.
#'   \item uniqueCount numeric. The number of unique values in the feature.
#'   \item naCount numeric. The number of missing values in the feature.
#'   \item dateFormat character. The format of the feature if it is date-time feature.
#'   \item projectId character. Character id of the project the feature belonges to.
#'   \item max. The maximum value in the dataset, formatted in the same format as the data.
#'   \item min. The minimum value in the dataset, formatted in the same format as the data.
#'   \item mean. The arithmetic mean of the dataset, formatted in the same format as the data.
#'   \item median. The median of the dataset, formatted in the same format as the data.
#'   \item stdDev. The standard deviation of the dataset, formatted in the same format as the data.
#'   \item timeSeriesEligible logical. Whether this feature can be used as the datetime partition
#'     column in a time series project.
#'   \item timeSeriesEligibilityReason character. Why the feature is ineligible for the
#'     datetime partition column in a time series project, "suitable" when it is eligible.
#'   \item timeStep numeric. For time-series eligible features, a positive integer determining
#'     the interval at which windows can be specified. If used as the datetime partition column
#'     on a time series project, the feature derivation and forecast windows must start and end
#'     at an integer multiple of this value. NULL for features that are not time series eligible.
#'   \item timeUnit character. For time series eligible features, the time unit covered by a
#'     single time step, e.g. "HOUR", or NULL for features that are not time series eligible.
#'   \item targetLeakage character. Whether a feature is considered to have target leakage or not.
#'     A value of "SKIPPED_DETECTION" indicates that target leakage detection was not run on
#'     the feature.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListFeatureInfo(projectId)
#' }
#' @export
ListFeatureInfo <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "features")
  lapply(DataRobotGET(routeString, addUrl = TRUE, simplifyDataFrame = FALSE),
         as.dataRobotFeatureInfo)
}

#' Details about a feature
#'
#' @inheritParams DeleteProject
#' @param featureName Name of the feature to retrieve. Note: DataRobot renames some features, so
#' the feature name may not be the one from your original data. You can use ListFeatureInfo to list
#' the features and check the name.
#' @inherit ListFeatureInfo return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetFeatureInfo(projectId, "myFeature")
#' }
#' @export
GetFeatureInfo <- function(project, featureName) {
  projectId <- ValidateProject(project)
  featureForUrl <- if (is.character(featureName)) URLencode(enc2utf8(featureName)) else featureName
  routeString <- UrlJoin("projects", projectId, "features", featureForUrl)
  as.dataRobotFeatureInfo(DataRobotGET(routeString, addUrl = TRUE,
                                       simplifyDataFrame = FALSE))
}

as.dataRobotFeatureInfo <- function(inList) {
  elements <- c("id",
                "name",
                "featureType",
                "importance",
                "lowInformation",
                "uniqueCount",
                "naCount",
                "dateFormat",
                "projectId",
                "max",
                "min",
                "mean",
                "median",
                "stdDev",
                "timeSeriesEligible",
                "timeSeriesEligibilityReason",
                "timeStep",
                "timeUnit",
                "targetLeakage")
  ApplySchema(inList, elements)
}


CreateDerivedFeatureFunctionMaker <- function(variableType) {
  featureRequester <- function(project, parentName, name = NULL, dateExtraction = NULL,
                               replacement = NULL, maxWait = 600) {
    projectId <- ValidateProject(project)
    routeString <- UrlJoin("projects", projectId, "typeTransformFeatures")
    if (is.null(name)) name <- sprintf("%s (%s)", parentName, variableType)
    body <- list(name = name, parentName = parentName, variableType = variableType,
                 replacement = replacement, dateExtraction = dateExtraction)
    body <- Filter(Negate(is.null), body)  # Drop NULL values
    creationRequestResponse <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                                             returnRawResponse = TRUE, encode = "json")
    as.dataRobotFeatureInfo(FeatureFromAsyncUrl(
      httr::headers(creationRequestResponse)$location, maxWait = maxWait))
  }
  featureRequester
}

#' @name CreateDerivedFeatures
#' @rdname CreateDerivedFeatures
#'
#' @title Derived Features
#'
#' @description These functions request that new features be created as transformations of existing
#' features and wait for the new feature to be created.
#'
#' @inheritParams DeleteProject
#' @param parentName The name of the parent feature.
#' @param name The name of the new feature.
#' @param dateExtraction dateExtraction: The value to extract from the date column:
#'   'year', 'yearDay', 'month', 'monthDay', 'week', or 'weekDay'. Required for transformation of a
#'   date column. Otherwise must not be provided.
#' @param replacement The replacement in case of a failed transformation. Optional.
#' @param maxWait The maximum time (in seconds) to wait for feature creation.
#'
#' @return Details for the created feature; same schema as the object returned from GetFeatureInfo.
NULL

#' @rdname CreateDerivedFeatures
#' @export
CreateDerivedFeatureAsCategorical <- CreateDerivedFeatureFunctionMaker("categorical")

#' @rdname CreateDerivedFeatures
#' @export
CreateDerivedFeatureAsText <- CreateDerivedFeatureFunctionMaker("text")

#' @rdname CreateDerivedFeatures
#' @export
CreateDerivedFeatureAsNumeric <- CreateDerivedFeatureFunctionMaker("numeric")

#' @rdname CreateDerivedFeatures
#' @export
CreateDerivedFeatureIntAsCategorical <- CreateDerivedFeatureFunctionMaker("categoricalInt")

#' Retrieve a feature from the creation URL
#'
#' If feature creation times out, the error message includes a URL corresponding to the
#' creation task. That URL can be passed to this function (which will return the feature
#' details when finished) to resume waiting for feature creation.
#'
#' @inheritParams ProjectFromAsyncUrl
#' @export
FeatureFromAsyncUrl <- function(asyncUrl, maxWait = 600) {
  timeoutMessage <-
    paste(sprintf("Feature creation did not complete before timeout (%ss).", maxWait),
          "To query its status and (if complete) retrieve the completed feature info, use:\n  ",
          sprintf("%s('%s')", "FeatureFromAsyncUrl", asyncUrl))
  tryCatch(WaitForAsyncReturn(asyncUrl,
                              addUrl = FALSE,
                              maxWait = maxWait,
                             failureStatuses = "ERROR"),
                  AsyncTimeout = function(e) stop(timeoutMessage))
}
