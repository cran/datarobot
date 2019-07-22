#' Retrieve time series properties for a potential multiseries datetime partition column
#'
#' Multiseries time series projects use multiseries id columns to model multiple distinct
#' series within a single project. This function returns the time series properties
#' (time step and time unit) of this column if it were used as a datetime partition column
#' with the specified multiseries id columns, running multiseries detection automatically if
#' it had not previously been successfully ran.
#'
#' @inheritParams DeleteProject
#' @param dateColumn character. The name of the column containing the date that defines the
#' time series.
#' @param multiseriesIdColumns list. The name(s) of the multiseries id columns to use with this
#' datetime partition column. Currently only one multiseries id column is supported.
#' @param maxWait integer. if a multiseries detection task is run, the maximum amount of time to
#' wait for it to complete before giving up.
#' @return A named list which contains:
#' \itemize{
#'   \item time_series_eligible logical. Whether or not the series is eligible to be used for
#'     time series.
#'   \item timeUnit character. For time series eligible features, the time unit covered by a
#'     single time step, e.g. "HOUR", or NULL for features that are not time series eligible.
#'   \item timeStep integer Expected difference in time units between rows in the data.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetMultiSeriesProperties(projectId,
#'                            dateColumn = "myFeature",
#'                            multiseriesIdColumns = "Store")
#' }
#' @export
GetMultiSeriesProperties <- function(project, dateColumn, multiseriesIdColumns, maxWait = 600) {
  projectId <- ValidateProject(project)
  featureForUrl <- if (is.character(dateColumn)) {
                      URLencode(enc2utf8(dateColumn))
                   } else { dateColumn }
  routeString <- UrlJoin("projects", projectId, "features", featureForUrl, "multiseriesProperties")
  detected <- DataRobotGET(routeString, simplifyDataFrame = TRUE)
  if (is.list(multiseriesIdColumns)) {
    if (length(multiseriesIdColumns) > 1) {
      stop("Currently only one multiseries id column is supported.")
    }
    multiseriesIdColumns <- multiseriesIdColumns[[1]]
  }
  if (!is.null(detected$detectedMultiseriesIdColumns)) {
    detectedSubset <- detected$detectedMultiseriesIdColumns[
                        detected$detectedMultiseriesIdColumns$multiseriesIdColumns ==
                          multiseriesIdColumns, ]
    timeSeriesEligible <- TRUE
    timeUnit <- detectedSubset$timeUnit
    timeStep <- detectedSubset$timeStep
  }
  as.dataRobotFeatureInfo(list("timeSeriesEligible" = timeSeriesEligible,
                               "timeUnit" = timeUnit,
                               "timeStep" = timeStep))
}


#' Format a multiseries.
#'
#' Call this function to request the project be formatted as a multiseries project, with the
#' \code{dateColumn} specifying the time series.
#'
#' Note that as of v2.13 this function no longer needs to be called directly, but is called
#' indirectly as a part of \code{SetTarget} (which itself is called indirectly as part of
#' \code{StartProject}) when you pass a multiseries partition using
#' \code{CreateDatetimePartitionSpecification}.
#'
#' @inheritParams DeleteProject
#' @param dateColumn character. The name of the column containing the date that defines the
#' time series.
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to complete.
#' @return A named list which contains:
#' \itemize{
#'   \item datetimePartitionColumn character. The name of the datetime partition column.
#'   \item detectedMultiSeriesIdColumns list. Details of the detected multiseries columns:
#'    \itemize{
#'      \item multiseriesColumns character. The name of the potential multiseries ID column.
#'      \item timeUnit character. For time series eligible features, the time unit covered by a
#'         single time step, e.g. "HOUR", or NULL for features that are not time series eligible.
#'       \item timeStep integer. Expected difference in time units between rows in the data.
#'    }
#' }
#' @export
RequestMultiSeriesDetection <- function(project, dateColumn, maxWait = 600) {
  payload <- list("datetimePartitionColumn" = dateColumn)
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "multiseriesProperties")
  response <- DataRobotPOST(routeString, returnRawResponse = TRUE, body = payload)
  message(paste("Multiseries for feature", dateColumn, "submitted"))
  WaitForAsyncReturn(httr::headers(response)$location,
                     addUrl = FALSE,
                     maxWait = maxWait,
                     failureStatuses = "ERROR")
}
