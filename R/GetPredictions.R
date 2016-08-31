#' Retrieve model predictions from predictJobId
#'
#' This function is called with a project descriptor and an
#' integer predictJobId, obtained from an earlier call to
#' RequestPredictions.  It returns the predictions generated
#' for the model and data specified in this prior function
#' call.
#'
#' The contents of the return vector depends on both the modeling
#' task - binary classification or regression - and the value of
#' the type parameter.  For regression tasks, the type parameter
#' is ignored and a vector of numerical predictions of the response
#' variable is returned.  For binary classification tasks, either
#' a vector of predicted responses is returned if type has the
#' value "response" (the default), or a vector of probabilities
#' for the positive class is returned, if type is "probability".
#'
#' This function will error if the requested job has errored, or
#' if it isn't complete within maxWait seconds.
#'
#' @inheritParams DeleteProject
#' @param predictJobId Integer, identifying the prediction job
#' created by the call to RequestPredictions.
#' @param type Character string, specifying the type of response for
#' binary classifiers; see Details.
#' @inheritParams SetupProject
#' @return Vector of predictions, depending on the modeling task
#' ("Binary" or "Regression") and the value of the type parameter;
#' see Details.
#' @export
#'
GetPredictions <- function(project, predictJobId,
                           type = "response",
                           maxWait = 60) {
  validOptions <- c("response", "probability")
  if (!(tolower(type) %in% validOptions)) {
    stop(sprintf("type parameter %s is invalid - please choose one of the following: \n%s",
                 paste(validOptions, collapse = ", ")))
  } else {
    message("request issued, waiting for predictions")
    projectId <- ValidateProject(project)
    predictJobRoute <- PredictJobRoute(projectId, predictJobId)
    predictionResponse <- WaitForAsyncReturn(predictJobRoute, maxWait = maxWait,
                                             failureStatuses = JobFailureStatuses)
    desiredPredictionColumn <- SelectDesiredPredictions(predictionResponse, type)
    return(desiredPredictionColumn)
  }
}

SelectDesiredPredictions <- function(parsedPredictionResponse, type) {
  predictDF <- parsedPredictionResponse$predictions
  task <- parsedPredictionResponse$task
  if (task == "Regression") {
    return(predictDF$prediction)
  } else {
    message("Binary classifier with positiveClass = ", parsedPredictionResponse$positiveClass)
    if (type == "response") {
      return(predictDF$prediction)
    } else {
      return(predictDF$positiveProbability)
    }
  }
}
