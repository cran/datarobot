#' Compute datetime trend plots for datetime partitioned model.
#'
#' Compute datetime trend plots for datetime partitioned model. This includes
#' Accuracy over Time, Forecast vs Actual, and Anomaly over Time plots.
#' @details
#' \itemize{
#'  \item{
#'    Forecast distance specifies the number of time steps
#'    between the predicted point and the origin point.
#'  }
#'  \item{
#'    For the multiseries models only first 1000 series in alphabetical order
#'    and an average plot for them will be computed.
#'  }
#'  \item{
#'    Maximum 100 forecast distances can be requested for
#'    calculation in time series supervised projects.
#'  }
#' }
#'
#' @inheritParams DeleteModel
#' @param backtest integer or character. Optional. Compute plots for a specific backtest.
#'   Use the backtest index starting from zero.
#'   To compute plots for holdout, use \code{DataSubset$Holdout}.
#' @param source character. Optional. The source of the data for the backtest/holdout.
#'   Must be one of \code{SourceType}.
#' @param forecastDistanceStart integer. Optional. The start of forecast distance range
#'   (forecast window) to compute. If not specified, the first forecast distance
#'   for this project will be used. Only for time series supervised models.
#' @param forecastDistanceEnd integer. Optional. The end of forecast distance range
#'   (forecast window) to compute. If not specified, the last forecast distance
#'   for this project will be used. Only for time series supervised models.
#' @return An integer value that can be used as the jobId parameter in a subsequent call
#'   to \code{WaitForJobToComplete}.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' jobId <- ComputeDatetimeTrendPlots(model)
#' WaitForJobToComplete(projectId, jobId) # optional step
#' }
ComputeDatetimeTrendPlots <- function(model, backtest = 0,
                                      source = SourceType$Validation, forecastDistanceStart = NULL,
                                      forecastDistanceEnd = NULL) {
    validModel <- ValidateModel(model)
    projectId <- validModel$projectId
    modelId <- validModel$modelId
    routeString <- UrlJoin(
        "projects", projectId, "datetimeModels",
        modelId, "datetimeTrendPlots"
    )
    body <- list(
        backtest = backtest, source = source,
        forecastDistanceStart = forecastDistanceStart,
        forecastDistanceEnd = forecastDistanceEnd
    )
    body <- Filter(Negate(is.null), body) # Drop NULL parameters from request
    postResponse <- DataRobotPOST(routeString,
        body = body,
        returnRawResponse = TRUE,
    )
    JobIdFromResponse(postResponse)
}


#' Retrieve Accuracy over Time plots metadata for a model.
#'
#' @inheritParams DeleteModel
#' @param forecastDistance integer. Optional. Forecast distance to retrieve the metadata for.
#'   If not specified, the first forecast distance for this project will be used.
#'   Only available for time series projects.
#' @return list with the following components:
#' \itemize{
#'   \item forecastDistance. integer or NULL:
#'         The forecast distance for which the metadata was retrieved. NULL for OTV projects.
#'   \item resolutions. list: A list of \code{DatetimeTrendPlotsResolutions},
#'         which represents available time resolutions for which plots can be retrieved.
#'   \item backtestStatuses. data.frame: Each row represents a status for the backtest
#'         \code{SourceType}. The row index corresponds to the backtest index via the relation
#'         \code{rowIndex <- backtestIndex + 1}. Status should be one of
#'         \code{DatetimeTrendPlotsStatuses}
#'   \item backtestMetadata. data.frame: Each row represents a metadata for the backtest
#'         \code{SourceType} start and end date. The row index corresponds to the
#'         backtest index via the relation \code{rowIndex <- backtestIndex + 1}.
#'         Each cell contains a POSIXct timestamp for start date (inclusive)
#'         and end date (exclusive) if the correspoding source type
#'         for the backtest is computed, and NA otherwise.
#'   \item holdoutStatuses. list: Contains statuses for holdout.
#'   \itemize{
#'     \item training. character: Status, one of \code{DatetimeTrendPlotsStatuses}
#'     \item validation. character: Status, one of \code{DatetimeTrendPlotsStatuses}
#'   }
#'   \item holdoutMetadata. list. Contains metadata for holdout.
#'   \itemize{
#'     \item training. list. Contains start and end date for holdout training.
#'     \item validation. list. Contains start and end date for holdout validation.
#'       \itemize{
#'         \item startDate. POSIXct or NA:
#'               The datetime of the start of the holdout training/validation (inclusive).
#'               NA if the data is not computed.
#'         \item endDate. POSIXct or NA:
#'               The datetime of the end of the holdout training/validation (exclusive).
#'               NA if the data is not computed.
#'         }
#'   }
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' GetAccuracyOverTimePlotsMetadata(model)
#' }
GetAccuracyOverTimePlotsMetadata <- function(model, forecastDistance = NULL) {
    validModel <- ValidateModel(model)
    projectId <- validModel$projectId
    modelId <- validModel$modelId
    routeString <- UrlJoin(
        "projects", projectId, "datetimeModels",
        modelId, "accuracyOverTimePlots", "metadata"
    )
    query <- list(
        forecastDistance = forecastDistance
    )
    query <- Filter(Negate(is.null), query)
    response <- DataRobotGET(routeString, query = query, simplifyDataFrame = TRUE)
    return(as.accuracyOverTimePlotsMetadata(response))
}

parseTimestampConditionally <- function(timestamp) {
    if (!is.null(timestamp)) {
        return(parseRFC3339Timestamp(timestamp))
    }
    return(NA)
}

as.datetimeTrendPlotsBacktestHoldoutMetadata <- function(inList) {
    outList <- inList
    outList$backtestMetadata$training[] <- lapply(
        outList$backtestMetadata$training, parseTimestampConditionally
    )
    outList$backtestMetadata$validation[] <- lapply(
        outList$backtestMetadata$validation, parseTimestampConditionally
    )
    outList$holdoutMetadata$training <- lapply(
        outList$holdoutMetadata$training, parseTimestampConditionally
    )
    outList$holdoutMetadata$validation <- lapply(
        outList$holdoutMetadata$validation, parseTimestampConditionally
    )
    return(outList)
}

as.accuracyOverTimePlotsMetadata <- function(inList) {
    outList <- inList
    outList <- as.datetimeTrendPlotsBacktestHoldoutMetadata(outList)
    outList$forecastDistance <- unlist(outList$forecastDistance)
    return(outList)
}
