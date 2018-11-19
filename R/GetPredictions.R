#' Retrieve model predictions from predictJobId
#'
#' This function is called with a project descriptor and an
#' integer predictJobId, obtained from an earlier call to
#' \code{RequestPredictionsForDataset}. It returns the predictions generated
#' for the model and data specified in this prior function
#' call.
#'
#' The contents of the return vector depends on both the modeling
#' task - binary classification, multiclass classification, or regression
#' - and the value of the type parameter.  For regression tasks, the type
#' parameter is ignored and a vector of numerical predictions of the response
#' variable is returned.
#'
#' For binary classification tasks, either
#' a vector of predicted responses is returned if type has the
#' value "response" (the default), or a vector of probabilities
#' for the positive class is returned, if type is "probability".
#'
#' For multiclass classification tasks, "response" will return the predicted
#' class and "probability" will return the probability of each class.
#'
#' This function will error if the requested job has errored, or
#' if it isn't complete within maxWait seconds.
#'
#' @inheritParams DeleteProject
#' @param predictJobId integer. Id identifying the prediction job
#'   created by the call to \code{RequestPredictionsForDataset}.
#' @param type character. String specifying the type of response for
#'   binary classifiers; see Details.
#' @param classPrefix character. For multiclass projects returning prediction probabilities,
#'   this prefix is prepended to each class in the header of the dataframe. Defaults to
#'   "class_".
#' @param maxWait integer. The maximum time (in seconds) to wait for the prediction job
#'   to complete.
#' @return Vector of predictions, depending on the modeling task
#' ("Binary", "Multiclass", or "Regression") and the value of the type parameter;
#' see Details.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- GetPredictJobs(project)
#'   job <- initialJobs[[1]]
#'   predictJobId <- job$predictJobId
#'   GetPredictions(projectId, predictJobId)
#' }
#' @export
GetPredictions <- function(project, predictJobId, type = "response",
                           classPrefix = "class_", maxWait = 600) {
  ValidateParameterIn(type, c("response", "probability"), allowNULL = FALSE)
  message("request issued, waiting for predictions")
  projectId <- ValidateProject(project)
  predictJobRoute <- PredictJobRoute(projectId, predictJobId)
  timeoutMessage <-
    paste(sprintf("Retrieving predictions did not complete before timeout (%ss).", maxWait),
          "Try increasing the", sQuote("maxWait"), "parameter to increase the amount of time",
          "to wait for predictions.")
  predictionResponse <- tryCatch(WaitForAsyncReturn(predictJobRoute, maxWait = maxWait,
                                                    failureStatuses = JobFailureStatuses),
                                 AsyncTimeout = function(e) stop(timeoutMessage))
  SelectDesiredPredictions(predictionResponse, type, classPrefix = classPrefix)
}

SelectDesiredPredictions <- function(parsedPredictionResponse, type, classPrefix = "class_") {
  predictDF <- parsedPredictionResponse$predictions
  task <- parsedPredictionResponse$task
  if (task == "Regression") {
    predictDF$prediction
  } else if (task == "Multiclass") {
    message("Multiclass with labels ", paste0(unique(predictDF$prediction), collapse = ", "))
    if (type == "response") { predictDF$prediction }
    else {
      m <- Reduce(rbind,
                  lapply(predictDF$predictionValues,
                         function(x) stats::setNames(x$value,
                                                     paste0(classPrefix, x$label))))
      rownames(m) <- NULL
      as.data.frame(m)
    }
  } else {
    message("Binary classifier with positiveClass = ", parsedPredictionResponse$positiveClass)
    if (type == "response") { predictDF$prediction } else { predictDF$positiveProbability }
  }
}
