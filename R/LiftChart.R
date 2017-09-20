#' Retrieve lift chart data for a model for a data partition (see DataPartition)
#'
#' @param model An S3 object of class dataRobotModel like that returned by the function
#'   GetModelObject, or each element of the list returned by the function GetAllModels.
#' @param source Data partition for which lift chart data would be returned. Default is
#'   DataPartition$VALIDATION (see DataPartition)
#' @return data.frame with the following components:
#' \itemize{
#'   \item binWeight. Numeric: weight of the bin.  For weighted projects, the sum of the weights of
#'     all rows in the bin; otherwise, the number of rows in the bin.
#'   \item actual. Numeric: sum of actual target values in bin.
#'   \item predicted. Numeric: sum of predicted target values in bin.
#' }
#' @examples
#' \dontrun{
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetLiftChart(modelId)
#' }
#' @export
GetLiftChart <- function(model, source = DataPartition$VALIDATION) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "liftChart", source)
  response <- DataRobotGET(routeString, addUrl = TRUE, returnRawResponse = FALSE)
  return(as.dataRobotLiftChart(response$bins))
}

as.dataRobotLiftChart <- function(inList) {
  elements <- c("binWeight",
                "actual",
                "predicted")
  outList <- ApplySchema(inList, elements)
  return(outList)
}

#' Retrieve lift chart data for a model for all available data partitions (see DataPartition)
#'
#' @inheritParams GetLiftChart
#' @inherit GetLiftChart return
#' @examples
#' \dontrun{
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetAllLiftCharts(modelId)
#' }
#' @export
GetAllLiftCharts <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "liftChart")
  response <- DataRobotGET(routeString, addUrl = TRUE, returnRawResponse = FALSE)
  names(response$charts$bins) <- response$charts$source
  return(lapply(response$charts$bins, as.dataRobotLiftChart))
}
