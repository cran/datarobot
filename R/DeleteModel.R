#' Delete a specified DataRobot model
#'
#' This function removes the model specified by the parameter model from its
#' associated project.
#'
#' @param model An S3 object of class dataRobotModel like that returned by
#' the function GetModelObject, or each element of the list returned by
#' the function GetAllModels.
#' @export
#'
DeleteModel <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId)
  response <- DataRobotDELETE(routeString, addUrl = TRUE)
  modelName <- validModel$modelType
  message(paste("Model", modelName,
                "(modelId = ", modelId, ") deleted from project", projectId))
}
