#' Retrieve information about all model deployments throughout your projects.
#'
#' @inheritParams GetModelDeploymentActionLog
#' @param limit integer. At most this many results are returned. Defaults to no limit.
#' @param offset integer. This many results will be skipped. Defaults to 0.
#' @param query character. Filter the model deployments by matching labels and descriptions
#'   with the specified string. Partial matches are included, too. Matches are case insensitive.
#' @param orderBy character. How the model deployments are ordered. Supported attributes for
#'   ordering: label, exportTarget, status, type. Prefix attribute name with dash to sort in
#'   descending order, e.g., \code{orderBy = "-label"}. Only one field can be selected.
#' @param status character. Filter the list of deployments by status. Must be one of:
#'   "active", "inactive", "archived". Valid statuses can be seen in \code{ModelDeploymentStatus}.
#' @return data.frame containing information about each model deployment. See
#'   \code{GetModelDeployment} for details.
#' @examples
#' \dontrun{
#'  ListModelDeployments()
#' }
#' @export
ListModelDeployments <- function(limit = NULL, offset = NULL, query = NULL, orderBy = NULL,
                                 status = NULL) {
  ValidateParameterIn(orderBy, c("label", "exportTarget", "status", "type",
                                 "-label", "-exportTarget", "-status", "-type"), allowNULL = TRUE)
  ValidateParameterIn(status, ModelDeploymentStatus, allowNULL = TRUE)
  body <- list()
  if (!is.null(limit)) { body$limit <- limit }
  if (!is.null(offset)) { body$offset <- offset }
  if (!is.null(query)) { body$query <- query }
  if (!is.null(orderBy)) { body$orderBy <- orderBy }
  if (!is.null(status)) { body$status <- status }
  serverData <- DataRobotGET("modelDeployments",
                             addUrl = TRUE,
                             query = body,
                             simplifyDataFrame = FALSE)
  rows <- GetServerDataInRows(serverData)
  as.dataRobotModelDeploymentsList(rows)
}

as.dataRobotModelDeploymentsList <- function(modelDeployments) {
  out <- lapply(modelDeployments, as.dataRobotModelDeployment)
  class(out) <- "dataRobotModelDeploymentsList"
  out
}

as.dataRobotModelDeployment <- function(deploy) {
  elements <- c("createdAt", "deployed", "description", "id", "instance", "label",
                "model", "modelHealth", "organizationId", "predictionEndpoint",
                "prevRequestCount", "project", "recentRequestCount", "relativeRequestsTrend",
                "requestRates", "serviceHealth", "serviceHealthMessages", "status",
                "trendTimeWindow", "type", "updatedAt", "user")
  deploy <- ApplySchema(deploy, elements)
  deploy$instance <- ApplySchema(deploy$instance,
                                 c("datarobotKey", "hostName", "id", "sslEnabled",
                                   "privateIp", "ormVersion"))
  deploy$serviceHealthMessages <- ApplySchema(deploy$serviceHealthMessages,
                                              c("msgId", "message", "level"))
  deploy$user <- ApplySchema(deploy$user, c("username", "firstName", "lastName", "id"))
  deploy$model <- ApplySchema(deploy$model, c("uid", "id", "modelType", "prevRequestCount"))
  deploy$project <- as.dataRobotProject(deploy$project)
  class(deploy) <- "dataRobotModelDeployment"
  deploy
}

#' Summarize model deployments
#' @param object dataRobotModelDeploymentsList. The output from \code{ListModelDeployments}.
#' @param ... list. Additional arguments to pass from summary. These are ignored.
#' @export
summary.dataRobotModelDeploymentsList <- function(object, ...) {
  data.frame(id = sapply(object, `[[`, "id"),
             label = as.character(sapply(object, `[[`, "label")),
             modelType = sapply(lapply(object, `[[`, "model"), `[[`, "modelType"),
             modelId = sapply(lapply(object, `[[`, "model"), `[[`, "id"),
             createdAt = sapply(object, `[[`, "createdAt"),
             updatedAt = sapply(object, `[[`, "updatedAt"),
             modelHealth = sapply(object, `[[`, "modelHealth"),
             serviceHealth = sapply(object, `[[`, "serviceHealth"),
             recentRequestCount = as.character(sapply(object, `[[`, "requestCount")),
             type = sapply(object, `[[`, "type"))
}


#' Request a model deployment.
#' @inheritParams GetModelParameters
#' @param label character. A name for the model deployment.
#' @param instanceId character. Optional. Id of the instance in DataRobot cloud being deployed to.
#' @param description character. Optional. Description for the model deployment.
#' @param status character. Optional. status for the model deployment. Can be "active",
#'    "inactive" or "archived". Defaults to "active".
#' @return job Id
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   RequestModelDeployment(projectId, modelId, label = "My Deployment")
#' }
#' @export
RequestModelDeployment <- function(project, modelId, label, instanceId = NULL,
                                   description = NULL, status = NULL) {
  projectId <- ValidateProject(project)
  ValidateParameterIn(status, ModelDeploymentStatus, allowNULL = TRUE)
  routeString <- "modelDeployments"
  body <- list(projectId = projectId,
               modelId = modelId,
               label = label)
  if (!is.null(instanceId)) { body$instanceId <- instanceId }
  if (!is.null(description)) { body$description <- description }
  if (!is.null(status)) { body$status <- status }
  response <- DataRobotPOST(routeString, addUrl = TRUE, body = body, returnRawResponse = TRUE)
  message("Requesting deployment for ", label, " (modelId = ", modelId, ")")
  JobIdFromResponse(response)
}


#' Update a model deployment.
#' @param modelDeploymentId character. ID of the model deployment to update
#' @param label character. A name for the model deployment.
#' @param description character. Optional. Description for the model deployment.
#' @param status character. Optional. status for the model deployment. Can be "active",
#'    "inactive" or "archived".
#' @return The model deployment data with the updated fields. See \code{GetModelDeployment}
#'    for details.
#' @examples
#' \dontrun{
#'   modelDeploymentId <- "59a5af20c80891534e3c2bde"
#'   UpdateModelDeployment(modelDeploymentId, label = "My Different Label for Deployment")
#' }
#' @export
UpdateModelDeployment <- function(modelDeploymentId, label = NULL,
                                  description = NULL, status = NULL) {
  ValidateParameterIn(status, ModelDeploymentStatus, allowNULL = TRUE)
  routeString <- UrlJoin("modelDeployments", modelDeploymentId)
  body <- list()
  if (!is.null(label)) { body$label <- label }
  if (!is.null(description)) { body$description <- description }
  if (!is.null(status)) { body$status <- status }
  response <- DataRobotPATCH(routeString, addUrl = TRUE, body = body, returnRawResponse = TRUE)
  GetModelDeployment(modelDeploymentId)
}


#' Retrieve data on a specific model deployment.
#'
#' ModelDeployments provide an interface for tracking the health and activity of predictions
#' made against a deployment model. The \code{GetModelDeploymentServiceStatistics} method can
#' be used to see current and historical trends in requests made and in user and server
#' error rates.
#'
#' @param modelDeploymentId character. ID of the model deployment to retrieve.
#' @return A dataRobotModelDeployment object containing the following data:
#' \itemize{
#'   \item id character. The ID of the model deployment.
#'   \item model list. Details on the model associated with the model deployment. Contains:
#'     \itemize{
#'       \item uid character. The ID of the user who created the model.
#'       \item id character. The ID of the model.
#'       \item modelType character. The type of the model.
#'       \item prevRequestCount integer. The number of requests, within the previous time
#'          window specified in \code{trendTimeWindow}.
#'     }
#'   \item project dataRobotProject. The project object associated with the model. See
#'     \code{GetProject} for details.
#'   \item type character. The type of the model deployment. One of "sse", "dedicated", or
#'     "legacy_dedicated".
#'   \item status character. The status of the model deployment. One of "active", "inactive",
#'     or "archived". Statuses can be accessed via \code{ModelDeploymentStatus} enum list.
#'   \item user list. Details on the user who created the model deployment containing:
#'   \itemize{
#'     \item username character. The username of the user initiating deployment.
#'     \item firstName character. The first name of that user.
#'     \item lastName character. The last name of that user.
#'   }
#'   \item organizationId character. The ID of the organization associated with the model
#'     deployment.
#'   \item instance list. Details on the instance associated with the model deployment.
#'     Contains the following information:
#'     \itemize{
#'       \item id character. The ID of the dedicated prediction instance the model is
#'         deployed to.
#'       \item datarobotKey character. The key for the prediction instance.
#'       \item hostName character. The host name of the dedicated prediction instance.
#'       \item privateIp character. The IP address of the dedicated predicion instance.
#'       \item ormVersion character. The On-demand resource manager version of the dedicated
#'         prediction instance.
#'     }
#'   \item label character. A short label describing the model deployment.
#'   \item description character. A longer description describing the model deployment.
#'   \item predictionEndpoint character. The URL where the model is deployed and available
#'     for serving predictions.
#'   \item deployed logical. Whether the model deployment process has finished or not.
#'   \item createdAt datetime. The timestamp of the creation of the model deployment.
#'   \item updatedAt datetime. The timestamp of when the model deployment was last updated.
#'   \item serviceHealth character. The model health status. One of "passing", "warning", or
#'     "failing". Look at \code{serviceHealthMessages} or use
#'     \code{GetModelDeploymentServiceStatistics} for more information.
#'   \item serviceHealthMessages list. List of health messages detailing service health state.
#'     Contains the following information:
#'   \itemize{
#'     \item level character. The error level, one of "passing", "warning", or "failing".
#'     \item msgId character. The identifier for the message, such as "USER_ERRORS",
#'       "SERVER_ERRORS", or "NO_GOOD_REQUESTS".
#'     \item message character. A message describing the error with more detail.
#'   }
#'   \item recentRequestCount integer. The number of recent requests, within recent time window
#'     specified by \code{trendTimeWindow}.
#'   \item prevRequestCount integer. The number of requests within the previous time window
#'     specified by \code{trendTimeWindow}.
#'   \item relativeRequestsTrend numeric. Relative difference, as a percentage, between the
#'     number of prediction requests performed within the current time window and one time
#'     window before that. The size of the time window is specified by \code{trendTimeWindow}.
#'   \item trendTimeWindow character. The time window, speficied in number of days from "now".
#'   \item requestRates list. A history of request rates per day sorted in chronological order,
#'     with the last entry being the most recent (i.e., today).
#' }
#' @examples
#' \dontrun{
#'   modelDeploymentId <- "59a5af20c80891534e3c2bde"
#'   deployment <- GetTrainingPredictions(modelDeploymentId)
#' }
#' @export
GetModelDeployment <- function(modelDeploymentId) {
  routeString <- UrlJoin("modelDeployments", modelDeploymentId)
  serverData <- CleanServerData(DataRobotGET(routeString, addUrl = TRUE))
  as.dataRobotModelDeployment(serverData)
}


#' Retrieve a health overview of a model deployment.
#' @inheritParams GetModelDeployment
#' @param startDate character. Optional. Filter statistics to be at this datetime or later.
#' @param endDate character. Optional. Filter statistics to be at this datetime or earlier.
#' @return dataRobotModelDeploymentServiceHealth object containing the following information:
#' \itemize{
#'   \item totalRequests integer. The total number of requests performed. 0 if no requests.
#'   \item consumers integer. The total number of unique users performing requests.
#'   \item period list. A list with \code{start} and \code{end} that denote the boundaries
#'     of the time period the stats are reported for. This is a half-open time interval
#'     specifying [start: end).
#'   \item userErrorRate list. A list with \code{current} and \code{previous} that denote
#'     the ratio of user errors to the total number of requests performed for the given
#'     period and one time period before that.
#'   \item serverErrorRate list. A list with \code{current} and \code{previous} that denote
#'     the ratio of server errors to the total number of requests performed for the given
#'     period and one time period before that.
#'   \item load list. A list with \code{peak} and \code{median} that denote the max and the
#'     median request rate (in requests per minute) across all requests for the duration of
#'     the given time period.
#'   \item medianExecutionTime integer. The median of the execution time across all performed
#'     requests, in seconds. Will be \code{NULL} if there have been no requests.
#' }
#' @examples
#' \dontrun{
#'   modelDeploymentId <- "59a5af20c80891534e3c2bde"
#'   serviceHealth <- GetModelDeploymentServiceStatistics(modelDeploymentId)
#'   serviceHealth2 <- GetModelDeploymentServiceStatistics(modelDeploymentId,
#"                                                         startDate = "2017-11-30")
#' }
#' @export
GetModelDeploymentServiceStatistics <- function(modelDeploymentId, startDate, endDate) {
  routeString <- UrlJoin("modelDeployments", modelDeploymentId, "serviceStats")
  serverData <- DataRobotGET(routeString, addUrl = TRUE)
  as.dataRobotServiceHealth(serverData)
}

as.dataRobotServiceHealth <- function(deploy) {
  elements <- c("load", "consumers", "userErrorRate", "totalRequests", "serverErrorRate",
                "period", "medianExecutionTime", "totalPredictionRows")
  deploy <- ApplySchema(deploy, elements)
  deploy$load <- ApplySchema(deploy$load, c("median", "peak"))
  deploy$userErrorRate <- ApplySchema(deploy$userErrorRate, c("current", "previous"))
  deploy$serverErrorRate <- ApplySchema(deploy$serverErrorRate, c("current", "previous"))
  deploy$period <- ApplySchema(deploy$period, c("start", "end"))
  class(deploy) <- "dataRobotModelDeploymentServiceHealth"
  deploy
}


#' Retrieve an action log for a model deployment.
#' @inheritParams GetModelDeployment
#' @param limit integer. At most this many results are returned. Defaults to no limit.
#' @param offset integer. This many results will be skipped. Defaults to 0.
#' @return dataRobotModelDeploymentActionLog object containing the following information:
#' \itemize{
#'   \item action character. The action taken. One of either "deployed" or "created".
#'   \item performedBy list. A list detailing information about the user taking the action.
#'     Contains the following:
#'     \itemize{
#'       \item username character. The username of the user taking the action.
#'       \item firstName character. The first name of that user.
#'       \item lastName character. The last name of that user.
#'     }
#'  \item performedAt datetime. The date and time the action was performed, in ISO-8601 format.
#' }
#' @examples
#' \dontrun{
#'   modelDeploymentId <- "59a5af20c80891534e3c2bde"
#'   serviceHealth <- GetModelDeploymentServiceStatistics(modelDeploymentId)
#'   serviceHealth2 <- GetModelDeploymentServiceStatistics(modelDeploymentId,
#"                                                         startDate = "2017-11-30")
#' }
#' @export
GetModelDeploymentActionLog <- function(modelDeploymentId, limit = NULL, offset = NULL) {
  routeString <- UrlJoin("modelDeployments", modelDeploymentId, "actionLog")
  body <- list()
  if (!is.null(limit)) { body$limit <- limit }
  if (!is.null(offset)) { body$offset <- offset }
  serverData <- DataRobotGET(routeString,
                             addUrl = TRUE,
                             query = body,
                             simplifyDataFrame = FALSE)
  rows <- GetServerDataInRows(serverData)
  as.dataRobotModelDeploymentActionLog(rows)
}

as.dataRobotModelDeploymentActionLog <- function(logs) {
  elements <- c("action", "performedAt", "performedBy")
  performedByElements <- c("username", "firstName", "lastName", "id")
  out <- lapply(logs, function(log) {
    log <- ApplySchema(log, elements)
    log$performedBy <- ApplySchema(log$performedBy, performedByElements)
    log
  })
  class(out) <- "dataRobotModelDeploymentActionLog"
  out
}
