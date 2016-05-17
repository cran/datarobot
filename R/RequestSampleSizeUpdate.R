#' Refits an existing model to a different fraction of the training dataset
#'
#' This function requests a refit of the model defined by the model parameter
#' to the same training dataset used in building it originally, but with a
#' different fraction of the data, specified by the samplePct parameter.
#' The function returns an integer value that may be used with the function
#' GetModelFromJobId to retrieve the model after fitting is complete.
#'
#' Motivation for this function is the fact that some models - e.g., very
#' complex machine learning models fit to large datasets - may take a long
#' time to complete.  Splitting the model creation request from model retrieval
#' in these cases allows the user to perform other interactive R session tasks
#' between the time the model creation/update request is made and the time the
#' final model is available.
#'
#' @inheritParams DeleteModel
#' @param samplePct Numeric, specifying the percentage of the training dataset
#' to be used in building the new model.
#' @return Integer, value to be used as the modelJobId parameter in calling the
#' function GetModelFromJobId to retrieve the updated model.
#' @export
#'
RequestSampleSizeUpdate <- function(model, samplePct) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  routeString <- UrlJoin("projects", projectId, "models")
  blueprintId <- model$blueprintId
  bodyFrame <- data.frame(blueprintId = blueprintId, samplePct = samplePct)
  body <- jsonlite::unbox(bodyFrame)
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                             returnRawResponse = TRUE, encode = "json")
  message("Model creation request submitted - retrieve via modelJobId
          value returned")
  rawHeaders <- httr::headers(rawReturn)
  modelJobPath <- rawHeaders$location
  pathSplit <- unlist(strsplit(modelJobPath, "modelJobs/"))
  modelJobId <- gsub("/", "", pathSplit[2])
  return(modelJobId)
}
