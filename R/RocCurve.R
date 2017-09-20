#' Retrieve ROC curve data for a model for a particular data partition (see DataPartition)
#'
#' @param model An S3 object of class dataRobotModel like that returned by the function
#'   GetModelObject, or each element of the list returned by the function GetAllModels.
#' @param source character. Data partition to retrieve ROC curve data.Default is
#"   DataPartition$VALIDATION (see DataPartition)
#' @return list with the following components:
#' \itemize{
#'   \item source. Character: data partition for which ROC curve data is returned
#'     (see DataPartition).
#'   \item negativeClassPredictions. Numeric: example predictions for the negative class.
#'   \item rocPoints. data.frame: each row represents pre-calculated metrics (accuracy,
#'     f1_score, false_negative_score, true_negative_score, true_positive_score,
#'     false_positive_score, true_negative_rate, false_positive_rate, true_positive_rate,
#'     matthews_correlation_coefficient, positive_predictive_value, negative_predictive_value,
#'     threshold) associated with different thresholds for the ROC curve.
#'   \item positiveClassPredictions. Numeric: example predictions for the positve class.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModelObject(projectId, modelId)
#'   GetRocCurve(model)
#' }
#' @export
GetRocCurve <- function(model, source = DataPartition$VALIDATION) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "rocCurve", source)
  response <- DataRobotGET(routeString, addUrl = TRUE, returnRawResponse = FALSE)
  return(as.dataRobotRocCurve(response))
}

as.dataRobotRocCurve <- function(inList) {
  elements <- c("source",
                "negativeClassPredictions",
                "rocPoints",
                "positiveClassPredictions")
  outList <- ApplySchema(inList, elements)
  return(outList)
}


#' Retrieve ROC curve data for a model for all available data partitions (see DataPartition)
#'
#' @param model An S3 object of class dataRobotModel like that returned by the function
#'   GetModelObject, or each element of the list returned by the function GetAllModels.
#' @return list of lists where each list is renamed as the data partitions source and returns the
#'   following components:
#' \itemize{
#'   \item source. Character: data partitions for which ROC curve data is returned
#'     (see DataPartition).
#'   \item negativeClassPredictions. Numeric: example predictions for the negative class for each
#"     data partition source.
#'   \item rocPoints. data.frame: each row represents pre-calculated metrics (accuracy, f1_score,
#'     false_negative_score, true_negative_score, true_positive_score, false_positive_score,
#'   true_negative_rate, false_positive_rate, true_positive_rate, matthews_correlation_coefficient,
#'     positive_predictive_value, negative_predictive_value, threshold) associated with different
#'     thresholds for the ROC curve.
#'   \item positiveClassPredictions. Numeric: example predictions for the positive class for each
#'     data partition source.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModelObject(projectId, modelId)
#'   GetAllRocCurves(model)
#' }
#' @export
GetAllRocCurves <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "rocCurve")
  response <- DataRobotGET(routeString, addUrl = TRUE, returnRawResponse = FALSE)
  temp <- list()
  for (i in 1:nrow(response$charts)) {
    temp[[i]] <- list(source = response$charts$source[i],
                      negativeClassPredictions = response$charts$negativeClassPredictions[[i]],
                      rocPoints = response$charts$rocPoints[[i]],
                      positiveClassPredictions = response$charts$positiveClassPredictions[[i]])
  }
  names(temp) <- response$charts$source
  response$charts <- temp
  return(lapply(response$charts, as.dataRobotRocCurve))
}
