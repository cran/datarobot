#' Function to upload new data to a DataRobot project for predictions
#'
#' The DataRobot prediction engine requires a CSV file containing the data to be
#' used in prediction, and this has been implemented here in two ways.
#' The first and simpler is to specify dataSource as the name of this CSV file,
#' but for the convenience of those who wish to work with dataframes, this
#' function also provides the option of specifying a dataframe, which is then
#' written to a CSV file and uploaded to the DataRobot server.
#'
#' @inheritParams DeleteProject
#' @param dataSource object. Either (a) the name of a CSV file (b) a dataframe or
#'   (c) url to publicly available file;
#'   in each case, this parameter identifies the source of the data for which
#'   predictions will be calculated.
#' @param forecastPoint character. Optional. The point relative to which predictions will be
#'     generated, based on the forecast window of the project. Only specified in time series
#'     projects.
#' @param maxWait integer. The maximum time (in seconds) to wait for each of two steps:
#'   (1) The initial dataset upload request, and
#'   (2) data processing that occurs after receiving the response to this initial request.
#' @return list with the following components:
#' \itemize{
#'   \item id character. The unique alphanumeric identifier for the dataset.
#'   \item numColumns numeric. Number of columns in dataset.
#'   \item name character. Name of dataset file.
#'   \item created character. time of upload.
#'   \item projectId character. String giving the unique alphanumeric identifier for the project.
#'   \item numRows numeric. Number of rows in dataset.
#'   \item forecastPoint character. The point relative to which predictions will be generated,
#'     based on the forecast window of the project. Only specified in time series projects,
#'     otherwise will be NULL.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   UploadPredictionDataset(projectId, iris)
#' }
#' @export
UploadPredictionDataset <- function(project, dataSource, forecastPoint = NULL, maxWait = 600) {
  projectId <- ValidateProject(project)
  if (isURL(dataSource)) {
    routeString <- UrlJoin("projects", projectId, "predictionDatasets", "urlUploads")
    dataList <- list(url = dataSource)
  } else {
    dataPath <- DataPathFromDataArg(dataSource)
    routeString <- UrlJoin("projects", projectId, "predictionDatasets", "fileUploads")
    dataList <- list(file = httr::upload_file(dataPath))
  }
  if (!is.null(forecastPoint)) {
    dataList$forecastPoint <- forecastPoint
  }
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList,
                             returnRawResponse = TRUE, timeout = maxWait)
  asyncUrl <- httr::headers(rawReturn)$location
  PredictionDatasetFromAsyncUrl(asyncUrl, maxWait = maxWait)
}

#' Retrieve prediction dataset info from the dataset creation URL
#'
#' If dataset creation times out, the error message includes a URL corresponding to the creation
#' task. That URL can be passed to this function (which will return the completed dataset info
#' details when finished) to resume waiting for creation.
#'
#' @param asyncUrl The temporary status URL
#' @param maxWait The maximum time to wait (in seconds) for creation before aborting.
#' @export
PredictionDatasetFromAsyncUrl <- function(asyncUrl, maxWait = 600) {
  timeoutMessage <-
    paste(sprintf("Dataset creation did not complete before timeout (%ss).", maxWait),
          "To query its status and (if complete) retrieve the completed dataset info, use:\n  ",
          sprintf("%s('%s')", "PredictionDatasetFromAsyncUrl", asyncUrl))
  datasetInfo <- tryCatch(WaitForAsyncReturn(asyncUrl,
                                             addUrl = FALSE,
                                             maxWait = maxWait,
                                             failureStatuses = "ERROR"),
                          AsyncTimeout = function(e) stop(timeoutMessage))
  datasetInfo
}

#' Retrieve all prediction datasets associated with a project
#'
#' This function returns an S3 object of class listDataRobotPredictionDataset that
#' describes all prediction datasets
#' available for the project specified by the project parameter.
#' This list may be converted to a dataframe with the as.data.frame
#' method for objects of class listDataRobotPredictionDataset.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class 'listDataRobotPredictionDataset', which is a
#' list of dataframes: each element of the list corresponds to one
#' prediction dataset associated with the project, and each dataframe has
#' one row and the following columns:
#' \itemize{
#'   \item id character. The unique alphanumeric identifier for the dataset.
#'   \item numColumns numeric. Number of columns in dataset.
#'   \item name character. Name of dataset file.
#'   \item created character. time of upload.
#'   \item projectId character. String giving the unique alphanumeric identifier for the project.
#'   \item numRows numeric. Number of rows in dataset.
#'   \item forecastPoint. The point relative to which predictions will be generated, based on the
#'     forecast window of the project. Only specified in time series projects, otherwise
#'     will be NULL.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListPredictionDatasets(projectId)
#' }
#' @export
ListPredictionDatasets <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionDatasets")
  datasetInfo <- DataRobotGET(routeString, addUrl = TRUE, simplifyDataFrame = FALSE)
  datasetlist <- datasetInfo$data
  class(datasetlist) <- c("listOfDataRobotPredictionDatasets", "listSubclass")
  datasetlist
}

#' Delete a specified prediction dataset
#'
#' This function removes a prediction dataset
#'
#' @inheritParams DeleteProject
#' @param datasetId The id of the dataset to delete
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   DeletePredictionDataset(projectId, datasetId)
#' }
#' @export
DeletePredictionDataset <- function(project, datasetId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictionDatasets", datasetId)
  DataRobotDELETE(routeString, addUrl = TRUE)
  invisible(NULL)
}

#' Request predictions against a previously uploaded dataset
#'
#' @inheritParams DeleteProject
#' @param modelId numeric. The ID of the model to use to make predictions
#' @param datasetId numeric. The ID of the dataset to make predictions against (as uploaded from
#' UploadPredictionDataset)
#' @return predictJobId to be used by GetPredictions function to retrieve
#' the model predictions.
#'
#' @examples
#' \dontrun{
#'   dataset <- UploadPredictionDataset(project, diamonds_small)
#'   model <- ListModels(project)[[1]]
#'   modelId <- model$modelId
#'   predictJobId <- RequestPredictionsForDataset(project, modelId, dataset$id)
#'   predictions <- GetPredictions(project, predictJobId)
#' }
#' @export
RequestPredictionsForDataset <- function(project, modelId, datasetId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "predictions")
  dataList <- list(modelId = modelId, datasetId = datasetId)
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList,
                             returnRawResponse = TRUE)
  JobIdFromResponse(rawReturn)
}
