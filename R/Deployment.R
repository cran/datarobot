#' Create a deployment.
#'
#' @inheritParams RequestPredictionExplanationsInitialization
#' @param label character. The name of the deployment.
#' @param description character. Optional. A longer description of the deployment.
#' @param defaultPredictionServerId character. The ID of the prediction server to connect to.
#'   Can also be a prediction server object.
#' @inherit GetDeployment return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   predictionServer <- ListPredictionServers()[[1]]
#'   CreateDeployment(model,
#'                    label = "myDeployment",
#'                    description = "this is my deployment",
#'                    defaultPredictionServerId = predictionServer)
#' }
#' @export
CreateDeployment <- function(model, label = "", description = "",
                             defaultPredictionServerId = NULL) {
  if (is(model, "dataRobotModel")) {
    modelId <- ValidateModel(model)$modelId
  } else {
    modelId <- model
  }
  if (is(defaultPredictionServerId, "dataRobotPredictionServer")) {
    defaultPredictionServerId <- defaultPredictionServerId$id
  }
  body <- list(modelId = modelId,
               label = label,
               description = description)
  body$defaultPredictionServerId <- defaultPredictionServerId
  routeString <- UrlJoin("deployments", "fromLearningModel")
  deploymentId <- DataRobotPOST(routeString, body = body)$id
  GetDeployment(deploymentId)
}


#' List all current model deployments.
#'
#' @return A list of DataRobotDeployment objects containing:
#' \itemize{
#'  \item id character. The ID of the deployment.
#'  \item label character. The label of the deployment.
#'  \item description character. The description of the deployment.
#'  \item defaultPredictionServer list. Information on the default prediction
#'    server connected with the deployment. See \code{ListPredictionServers}
#'    for details.
#'  \item model dataRobotModel. The model associated with the deployment.
#'    See \code{GetModel} for details.
#'  \item capabilities list. Information on the capabilities of the deployment.
#'  \item predictionUsage list. Information on the prediction usage of the deployment.
#'  \item serviceHealth list. Information on the service health of the deployment.
#'  \item modelHealth list. Information on the model health of the deployment.
#'  \item accuracyHealth list. Information on the accuracy health of the deployment.
#' }
#' @examples
#' \dontrun{
#'   ListDeployments()
#' }
#' @export
ListDeployments <- function() {
  response <- DataRobotGET("deployments", simplifyDataFrame = FALSE)
  response <- GetServerDataInRows(response)
  response <- lapply(response, as.dataRobotDeployment)
  class(response) <- c("listOfDeployments", "listSubclass")
  response
}


#' Get information on a particular deployment.
#'
#' @param deploymentId character. The ID of the deployment.
#' @return A DataRobotDeployment object containing:
#' \itemize{
#'  \item id character. The ID of the deployment.
#'  \item label character. The label of the deployment.
#'  \item description character. The description of the deployment.
#'  \item defaultPredictionServer list. Information on the default prediction
#'    server connected with the deployment. See \code{ListPredictionServers}
#'    for details.
#'  \item model dataRobotModel. The model associated with the deployment.
#'    See \code{GetModel} for details.
#'  \item capabilities list. Information on the capabilities of the deployment.
#'  \item predictionUsage list. Information on the prediction usage of the deployment.
#'  \item serviceHealth list. Information on the service health of the deployment.
#'  \item modelHealth list. Information on the model health of the deployment.
#'  \item accuracyHealth list. Information on the accuracy health of the deployment.
#' }
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   GetDeployment(deploymentId)
#' }
#' @export
GetDeployment <- function(deploymentId) {
  routeString <- UrlJoin("deployments", deploymentId)
  deployment <- DataRobotGET(routeString)
  as.dataRobotDeployment(deployment)
}


#' Delete a deployment.
#'
#' @inheritParams GetDeployment
#' @return NULL
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   DeleteDeployment(deploymentId)
#' }
#' @export
DeleteDeployment <- function(deploymentId) {
  if (is(deploymentId, "dataRobotDeployment")) {
    deploymentId <- deploymentId$id
  }
  routeString <- UrlJoin("deployments", deploymentId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}


as.dataRobotDeployment <- function(inList) {
  elements <- c("defaultPredictionServer", "description",
                "modelHealth", "predictionUsage",
                "capabilities", "label", "id", "model",
                "accuracyHealth", "serviceHealth", "permissions")
  out <- ApplySchema(inList, elements)
  class(out) <- "dataRobotDeployment"
  if (!is.null(out$defaultPredictionServer)) {
    out$defaultPredictionServer <- as.dataRobotPredictionServer(out$defaultPredictionServer)
  }
  out$model <- as.dataRobotModel(out$model)
  out$modelHealth <- ApplySchema(out$modelHealth,
                                 c("status", "startDate", "message", "endDate"))
  out$accuracyHealth <- ApplySchema(out$accuracyHealth,
                                    c("status", "startDate", "message", "endDate"))
  out$serviceHealth <- ApplySchema(out$serviceHealth,
                                   c("status", "startDate", "message", "endDate"))
  out$predictionUsage <- ApplySchema(out$predictionUsage,
                                     c("dailyRates", "lastTimestamp"))
  out$capabilities <- ApplySchema(out$capabilities,
                                  c("supportsFeatureDriftTracking",
                                    "supportsTargetDriftTracking",
                                    "supportsModelReplacement"))
  out$permissions <- unlist(as.list(out$permissions))
  out
}


ParseDeploymentCheckFailures <- function(deploymentChecks) {
  failedChecks <- Filter(function(check) check$status != "passing", deploymentChecks)
  failedChecks <- lapply(failedChecks, `[[`, "message")
  failedChecks <- paste(failedChecks, collapse = ", ")
  paste("The following model deployment checks failed:", failedChecks)
}

HandleDeploymentErrors <- function(deploymentResponse) {
  if (httr::status_code(deploymentResponse) == 409L) {
    deploymentResponseContent <- httr::content(deploymentResponse, as = "text")
    deploymentResponseContent <- jsonlite::fromJSON(deploymentResponseContent)
    msg <- ParseDeploymentCheckFailures(deploymentResponseContent$checks)
    stop("Model deployment failure - ", msg)
  } else if (httr::status_code(deploymentResponse) != 202L) {
    deploymentResponseContent <- httr::content(deploymentResponse)
    stop("Model deployment failure - ", deploymentResponseContent$message)
  }
}

#' Replace a model in a deployment with another model.
#'
#' @inheritParams GetDeployment
#' @param newModelId character. The ID of the model to use in the deployment. This model
#'   will replace the old model. You can also pass a dataRobotModel object.
#' @param replacementReason character. Optional. The reason for replacing the deployment.
#'   See \code{ModelReplacementReason} for a list of reasons.
#' @param maxWait integer. How long to wait (in seconds) for the computation to complete
#'   before returning a timeout error? (Default 600 seconds)
#' @inherit GetDeployment return
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   newModelId <- "5996f820af07fc605e81ead4"
#'   ReplaceDeployedModel(deploymentId, newModelId, ModelReplacementReason$Other)
#' }
#' @export
ReplaceDeployedModel <- function(deploymentId, newModelId, replacementReason, maxWait = 600) {
  if (is(deploymentId, "dataRobotDeployment")) {
    deploymentId <- deploymentId$id
  }
  if (is(newModelId, "dataRobotModel")) {
    newModelId <- ValidateModel(newModelId)$modelId
  }
  routeString <- UrlJoin("deployments", deploymentId, "model")
  body <- list(modelId = newModelId, reason = replacementReason)
  response <- DataRobotPATCH(routeString, body = body,
                             returnRawResponse = TRUE, stopOnError = FALSE, encode = "json")
  HandleDeploymentErrors(response)
  response <- WaitForAsyncReturn(httr::headers(response)$location,
                                 addUrl = FALSE,
                                 maxWait = maxWait,
                                 failureStatuses = "ERROR")
  as.dataRobotDeployment(response)
}


#' Validate a potential deployment model replacement.
#'
#' @inheritParams ReplaceDeployedModel
#' @return A validation report with:
#' \itemize{
#'   \item status character. Either PASSED or FAILED depending on whether all checks passed
#'     or not.
#'   \item message character. A message explaining the status failure, if any.
#'   \item checks list. A list of each check and the individual status.
#' }
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   newModelId <- "5996f820af07fc605e81ead4"
#'   ValidateReplaceDeployedModel(deploymentId, newModelId)
#' }
#' @export
ValidateReplaceDeployedModel <- function(deploymentId, newModelId) {
  if (is(deploymentId, "dataRobotDeployment")) {
    deploymentId <- deploymentId$id
  }
  if (is(newModelId, "dataRobotModel")) {
    newModelId <- ValidateModel(newModelId)$modelId
  }
  routeString <- UrlJoin("deployments", deploymentId, "model", "validation")
  body <- list(modelId = newModelId)
  response <- DataRobotPOST(routeString, body = body, encode = "json")
  as.dataRobotDeploymentValidation(response)
}

as.dataRobotDeploymentValidation <- function(inList) {
  elements <- c("status", "message", "checks")
  out <- ApplySchema(inList, elements)
  checks <- c("targetType", "target", "notCurrentModel", "permission", "supported",
              "modelCanBeDeployed", "seriesType", "modelStatus", "featureDataTypes",
              "features")
  checkSchema <- c("status", "message")
  out$checks <- ApplySchema(out$checks, checks)
  for (check in checks) {
    out$checks[[check]] <- ApplySchema(out$checks[[check]], checkSchema)
  }
  out
}


#' Get drift tracking settings for a deployment.
#'
#' @inheritParams GetDeployment
#' @return A list with the following information on drift tracking:
#' \itemize{
#'    \item associationId
#'    \item predictionIntervals list. A list with two keys:
#'      \itemize{
#'         \item enabled. `TRUE` if prediction intervals are enabled and `FALSE` otherwise.
#'         \item percentiles list. A list of percentiles, if prediction intervals are enabled.
#'      }
#'    \item targetDrift list. A list with one key, `enabled`, which is `TRUE` if target
#'      drift is enabled, and `FALSE` otherwise.
#'    \item featureDrift list. A list with one key, `enabled`, which is `TRUE` if feature
#'      drift is enabled, and `FALSE` otherwise.
#' }
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   GetDeploymentDriftTrackingSettings(deploymentId)
#' }
#' @export
GetDeploymentDriftTrackingSettings <- function(deploymentId) {
  if (is(deploymentId, "dataRobotDeployment")) {
    deploymentId <- deploymentId$id
  }
  routeString <- UrlJoin("deployments", deploymentId, "settings")
  as.dataRobotDeploymentDriftTrackingSettings(DataRobotGET(routeString))
}

as.dataRobotDeploymentDriftTrackingSettings <- function(inList) {
  elements <- c("predictionIntervals", "targetDrift", "associationId", "featureDrift")
  out <- ApplySchema(inList, elements)
  out$predictionIntervals <- ApplySchema(out$predictionIntervals, c("percentiles", "enabled"))
  out$targetDrift <- ApplySchema(out$targetDrift, "enabled")
  out$featureDrift <- ApplySchema(out$featureDrift, "enabled")
  out$associationId <- ApplySchema(out$associationId, c("columnNames",
                                                        "requiredInPredictionRequests"))
  out
}


#' Update drift tracking settings for a deployment.
#'
#' @inheritParams ReplaceDeployedModel
#' @param targetDriftEnabled logical. Optional. Set to TRUE to enable target drift. Set to
#'   FALSE to disable.
#' @param featureDriftEnabled logical. Optional. Set to TRUE to enable feature drift. Set to
#'   FALSE to disable.
#' @inherit GetDeploymentDriftTrackingSettings return
#' @examples
#' \dontrun{
#'   deploymentId <- "5e319d2e422fbd6b58a5edad"
#'   UpdateDeploymentDriftTrackingSettings(deploymentId, targetDriftEnabled = TRUE)
#' }
#' @export
UpdateDeploymentDriftTrackingSettings <- function(deploymentId, targetDriftEnabled = NULL,
                                                  featureDriftEnabled = NULL, maxWait = 600) {
  if (is(deploymentId, "dataRobotDeployment")) {
    deploymentId <- deploymentId$id
  }
  body <- list()
  if (!is.null(targetDriftEnabled)) {
    body$targetDrift <- list(enabled = targetDriftEnabled)
  }
  if (!is.null(featureDriftEnabled)) {
    body$featureDrift <- list(enabled = featureDriftEnabled)
  }
  if (identical(body, list())) {
    stop("No changes to deployment drift tracking were found.")
  } else {
    routeString <- UrlJoin("deployments", deploymentId, "settings")
    response <- DataRobotPATCH(routeString, body = body, returnRawResponse = TRUE, encode = "json")
    WaitForAsyncReturn(httr::headers(response)$location,
                       addUrl = FALSE,
                       maxWait = maxWait,
                       failureStatuses = "ERROR")
    GetDeploymentDriftTrackingSettings(deploymentId)
  }
}


#' List all available prediction servers.
#'
#' @return A list of DataRobotPredictionServer objects containing:
#' \itemize{
#'  \item id character. The ID of the prediction server.
#'  \item url character. The URL of the prediction server.
#'  \item dataRobotKey character. The key used to access the prediction server.
#' }
#' @examples
#' \dontrun{
#'   ListPredictionServers()
#' }
#' @export
ListPredictionServers <- function() {
  response <- DataRobotGET("predictionServers", simplifyDataFrame = FALSE)
  response <- GetServerDataInRows(response)
  response <- lapply(response, as.dataRobotPredictionServer)
  class(response) <- c("listOfPredictionServers", "listSubclass")
  response

}

as.dataRobotPredictionServer <- function(inList) {
  elements <- c("id", "url", "dataRobotKey")
  out <- inList
  out$dataRobotKey <- out[["datarobot-key"]]
  out <- ApplySchema(out, elements)
  class(out) <- "dataRobotPredictionServer"
  out
}
