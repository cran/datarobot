#' Create a group-based S3 object of class partition for the SetTarget function
#'
#' Group partitioning constructs data partitions such that all records with each
#' level in the column or columns specified by the parameter partitionKeyCols occurs
#' together in the same partition.
#'
#' This function is one of several convenience functions provided to simplify the task
#' of starting modeling projects with custom partitioning options. The other
#' functions are CreateRandomPartition, CreateStratifiedPartition, and CreateUserPartition.
#'
#' @param validationType Character string specifying the type of partition
#' generated, either 'TVH' or 'CV'.
#' @param holdoutPct Integer, giving the percentage of data to be used
#' as the holdout subset.
#' @param partitionKeyCols List containing character string specifying the name of
#' the variable used in defining the group partition.
#' @param reps Integer, specifying the number of cross-validation folds to
#' generate; only applicable when validationType = 'CV'.
#' @param validationPct Integer, giving the percentage of data to be used
#' as the validation subset.
#' @return An S3 object of class 'partition' including the parameters required
#' by the SetTarget function to generate a group-based partitioning of
#' the modeling dataset.
#' @examples
#' CreateGroupPartition(validationType = 'CV',
#'                      holdoutPct = 20,
#'                      partitionKeyCols = list("groupId"),
#'                      reps = 5)
#' @export
CreateGroupPartition <- function(validationType, holdoutPct, partitionKeyCols,
                                 reps = NULL, validationPct = NULL) {
  #
  ##############################################################################
  #
  #  Returns a partition object with components required when cvMethod = "group"
  #
  ##############################################################################
  #
  if (!is.list(partitionKeyCols)) {
    stop("Please specify partition column name as a list containing character string")
  }

  partition <- list(cvMethod = cvMethods$GROUP, validationType = validationType,
                    holdoutPct = holdoutPct,
                    partitionKeyCols = partitionKeyCols)
  if (validationType == "CV") {
    if (is.null(reps)) {
      stop(strwrap("Parameter reps must be specified for group partition with
              validationType = 'CV'"))
    } else {
      partition$reps <- reps
    }
  } else if (validationType == "TVH") {
    if (is.null(validationPct)) {
      stop(strwrap("Parameter validationPct must be specified for group
                partition with validationType = 'TVH'"))
    } else {
      partition$validationPct <- validationPct
    }
  } else {
    stop(strwrap(paste("validationType", validationType,
                       "not valid for group partitions")))
  }
  class(partition) <- "partition"
  return(partition)
}


#' Create a random sampling-based S3 object of class partition for the SetTarget function
#'
#' Random partitioning is supported for either Training/Validation/Holdout
#' ('TVH') or cross-validation ('CV') splits. In either case, the holdout
#' percentage (holdoutPct) must be specified; for the 'CV' method, the
#' number of cross-validation folds (reps) must also be specified, while
#' for the 'TVH' method, the validation subset percentage (validationPct)
#' must be specified.
#'
#' This function is one of several convenience functions provided to simplify
#' the task of starting modeling projects with custom partitioning options.
#' The other five functions are CreateGroupPartition,
#' CreateStratifiedPartition, and CreateUserPartition.
#'
#' @inheritParams CreateGroupPartition
#' @return An S3 object of class partition including the parameters
#'   required by SetTarget to generate a random partitioning of
#'   the modeling dataset.
#' @examples
#' CreateRandomPartition(validationType = 'CV', holdoutPct = 20, reps = 5)
#' @export
CreateRandomPartition <- function(validationType, holdoutPct, reps = NULL,
                                  validationPct = NULL) {
  #
  ##############################################################################
  #
  #  Returns a partition object with components required for cvMethod = "random"
  #
  ##############################################################################
  #
  partition <- list(cvMethod = cvMethods$RANDOM, validationType = validationType,
                    holdoutPct = holdoutPct)
  if (validationType == "CV") {
    if (is.null(reps)) {
      stop(strwrap("Parameter reps must be specified for random partition
              with validationType = 'CV'"))
    } else {
      partition$reps <- reps
    }
  } else if (validationType == "TVH") {
    if (is.null(validationPct)) {
      stop(strwrap("Parameter validationPct must be specified for random
                partition with validationType = 'TVH'"))
    } else {
      partition$validationPct <- validationPct
    }
  } else {
    stop(strwrap(paste("validationType", validationType,
                       "not valid for random partitions")))
  }
  class(partition) <- "partition"
  return(partition)
}


#' Create a stratified sampling-based S3 object of class partition for the SetTarget function
#'
#' Stratified partitioning is supported for binary classification problems and
#' it randomly partitions the modeling data, keeping the percentage of positive
#' class observations in each partition the same as in the original dataset.
#' Stratified partitioning is supported for either Training/Validation/Holdout
#' ('TVH') or cross-validation ('CV') splits. In either case, the holdout
#' percentage (holdoutPct) must be specified; for the 'CV' method, the number
#' of cross-validation folds (reps) must also be specified, while for the 'TVH'
#' method, the validation subset percentage (validationPct) must be specified.
#'
#' This function is one of several convenience functions provided to simplify the
#' task of starting modeling projects with custom partitioning options. The
#' other functions are CreateGroupPartition,
#' CreateRandomPartition, and CreateUserPartition.
#'
#' @inheritParams CreateGroupPartition
#' @return An S3 object of class 'partition' including the parameters required
#'   by the SetTarget function to generate a stratified partitioning of the
#'   modeling dataset.
#' @examples
#' CreateStratifiedPartition(validationType = 'CV', holdoutPct = 20, reps = 5)
#' @export
CreateStratifiedPartition <- function(validationType, holdoutPct, reps = NULL,
                                      validationPct = NULL) {
  #
  #############################################################################
  #
  #  Function returns a partition object with components required when
  #        cvMethod is "stratified"
  #
  #############################################################################
  #
  partition <- list(cvMethod = cvMethods$STRATIFIED, validationType = validationType,
                    holdoutPct = holdoutPct)
  if (validationType == "CV") {
    if (is.null(reps)) {
      stop(strwrap("Parameter reps must be specified for random partition
              with validationType = 'CV'"))
    } else {
      partition$reps <- reps
    }
  } else if (validationType == "TVH") {
    if (is.null(validationPct)) {
      stop(strwrap("Parameter validationPct must be specified for random
                partition with validationType = 'TVH'"))
    } else {
      partition$validationPct <- validationPct
    }
  } else {
    stop(strwrap(paste("validationType", validationType,
                       "not valid for random partitions")))
  }
  class(partition) <- "partition"
  return(partition)
}

#' Create a user-defined S3 object of class partition for the SetTarget function
#'
#' Creates a list object used by the SetTarget function to specify either
#' Training/Validation/Holdout (validationType = 'TVH') or cross-validation
#' (validationType = 'CV') partitions of the modeling dataset based on the values
#' included in a column from the dataset. In either case, the name of this data
#' column must be specified (as userPartitionCol).
#'
#' For the 'TVH' option of cvMethod, no cross-validation is used. Users must specify
#' the trainingLevel and validationLevel; use of a holdoutLevel is always recommended
#' but not required. If no holdoutLevel is used, then the column must contain exactly
#' 2 unique values. If a holdoutLevel is used, the column must contain exactly 3 unique
#' values.
#'
#' For the 'CV' option, each value in the column will be used to separate rows into
#' cross-validation folds. Use of a holdoutLevel is optional; if not specified, then
#' no holdout is used.
#'
#' This function is one of several convenience functions provided to simplify the
#' task of starting modeling projects with custom partitioning options. The other
#' functions are CreateGroupPartition, CreateRandomPartition, and CreateStratifiedPartition.
#'
#' @inheritParams CreateGroupPartition
#' @param userPartitionCol character. String naming the data column from the
#' modeling dataset containing the subset designations.
#' @param cvHoldoutLevel character. Data value from userPartitionCol that identifies the
#' holdout subset under the 'CV' option.
#' @param trainingLevel character. Data value from userPartitionCol that identifies the
#' training subset under the 'TVH' option.
#' @param holdoutLevel character. Data value from userPartitionCol that identifies the
#' holdout subset under both 'TVH' and 'CV' options. To specify that the project should
#' not use a holdout you can omit this parameter or pass NA directly.
#' @param validationLevel character. Data value from userPartitionCol that identifies the
#' validation subset under the 'TVH' option.
#' @return An S3 object of class 'partition' including the parameters required
#' by the SetTarget function to generate a user-specified of the modeling
#' dataset.
#' @examples
#' CreateUserPartition(validationType = 'CV', userPartitionCol = "TVHflag", cvHoldoutLevel = NA)
#' @export
CreateUserPartition <- function(validationType, userPartitionCol,
                                cvHoldoutLevel = NULL, trainingLevel = NULL,
                                holdoutLevel = NULL, validationLevel = NULL) {
  #
  #############################################################################
  #
  #  Returns a partition object with components required when cvMethod = "user"
  #
  #############################################################################
  #
  if (!class(userPartitionCol) == "character") {
    stop("Please specify partition column name as a character string")
  }

  partition <- list(cvMethod = cvMethods$USER, validationType = validationType,
                    userPartitionCol = userPartitionCol)
  if (validationType == "CV") {
    if (is.null(cvHoldoutLevel)) {
      partition$cvHoldoutLevel <- NA
    } else {
      partition$cvHoldoutLevel <- cvHoldoutLevel
    }
  } else if (validationType == "TVH") {
    if (is.null(trainingLevel)) {
      stop(strwrap("Parameter trainingLevel must be specified for user
                partition with validationType = 'TVH'"))
    } else {
      partition$trainingLevel <- trainingLevel
      partition$holdoutLevel <- holdoutLevel
    }
    if (is.null(validationLevel)) {
      stop(strwrap("Parameter validationLevel must be specified for user
                partition with validationType = 'TVH'"))
    } else {
      partition$validationLevel <- validationLevel
    }
  } else {
    stop(strwrap(paste("validationType", validationType,
                       "not valid for user partitions")))
  }
  class(partition) <- "partition"
  return(partition)
}

#' Create a list describing backtest parameters
#'
#' Uniquely defines a Backtest used in a DatetimePartitioning
#'
#' Includes only the attributes of a backtest directly controllable by users.  The other attributes
#' are assigned by the DataRobot application based on the project dataset and the user-controlled
#' settings.
#' All durations should be specified with a duration string such as those returned
#' by the ConstructDurationString helper function.
#'
#' @param index integer. The index of the backtest
#' @param gapDuration character. The desired duration of the gap
#'   between training and validation data for the backtest in duration format (ISO8601).
#' @param validationStartDate character. The desired start date of the validation data
#'   for this backtest (RFC 3339 format).
#' @param validationDuration character. The desired end date
#'   of the validation data for this backtest in duration format (ISO8601).
#' @return list with backtest parameters
#' @examples
#' zeroDayDuration <- ConstructDurationString()
#' hundredDayDuration <- ConstructDurationString(days = 100)
#' CreateBacktestSpecification(index = 0,
#'                             gapDuration = zeroDayDuration,
#'                             validationStartDate = "1989-12-01",
#'                             validationDuration = hundredDayDuration)
#' @export
CreateBacktestSpecification <- function(index, gapDuration, validationStartDate,
                                        validationDuration) {
  backtestSpec <- list(index = index, gapDuration = gapDuration,
                       validationStartDate = validationStartDate,
                       validationDuration = validationDuration)
  return(backtestSpec)
}

as.dataRobotBacktestSpecification <- function(inList) {
  elements <- c("index",
                "gapDuration",
                "validationStartDate",
                "validationDuration")
  outList <- ApplySchema(inList, elements)
  return(outList)
}



#' Construct a valid string representing a duration in accordance with ISO8601
#'
#' A duration of six months, 3 days, and 12 hours could be represented as P6M3DT12H.
#'
#' @param years integer. The number of years in the duration.
#' @param months integer. The number of months in the duration.
#' @param days integer. The number of days in the duration.
#' @param hours integer. The number of hours in the duration.
#' @param minutes integer. The number of minutes in the duration.
#' @param seconds integer. The number of seconds in the duration.
#' @return The duration string, specified compatibly with ISO8601.
#' @examples
#' ConstructDurationString()
#' ConstructDurationString(days = 100)
#' ConstructDurationString(years = 10, months = 2, days = 5, seconds = 12)
#' @export
ConstructDurationString <- function(years = 0, months = 0, days = 0,
                                    hours = 0, minutes = 0, seconds = 0) {
  return(paste("P", years, "Y",
               months, "M",
               days, "DT",
               hours, "H",
               minutes, "M",
               seconds, "S", sep = ""))
}

#' Create a list describing datetime partition parameters
#'
#' Uniquely defines a DatetimePartitioning for some project
#'
#' Includes only the attributes of DatetimePartitioning that are directly controllable by users,
#' not those determined by the DataRobot application based on the project dataset and the
#' user-controlled settings.
#' This is the specification that should be passed to SetTarget via the
#' partition parameter.  To see the full partitioning based on the project dataset,
#' GenerateDatetimePartition.
#' All durations should be specified with a duration string such as those returned
#' by the ConstructDurationString helper function.
#'
#' @param datetimePartitionColumn character. The name of the column whose values as dates
#'   are used to assign a row to a particular partition
#' @param autopilotDataSelectionMethod character. Optional. Whether models created
#'   by the autopilot should use "rowCount" or "duration" as their dataSelectionMethod
#' @param validationDuration character. Optional. The default validationDuration for the
#'   backtests
#' @param holdoutStartDate character. The start date of holdout scoring data
#'   (RFC 3339 format). If holdoutStartDate is specified, holdoutDuration must also be specified.
#' @param holdoutDuration character. Optional. The duration of the holdout scoring data.
#'   If holdoutDuration is specified, holdoutStartDate must also be specified.
#' @param gapDuration character. Optional. The duration of the gap between training and
#'   holdout scoring data.
#' @param numberOfBacktests integer. The number of backtests to use.
#' @param backtests list. List of BacktestSpecification the exact specification of backtests to use.
#'   The indexes of the specified backtests should range from 0 to numberOfBacktests - 1.
#'   If any backtest is left unspecified, a default configuration will be chosen.
#' @return An S3 object of class 'partition' including the parameters required by the
#'   SetTarget function to generate a datetime partitioning of the modeling dataset.
#' @examples
#' CreateDatetimePartitionSpecification("date_col")
#' @export
CreateDatetimePartitionSpecification <- function(datetimePartitionColumn,
                                                 autopilotDataSelectionMethod=NULL,
                                                 validationDuration=NULL,
                                                 holdoutStartDate=NULL, holdoutDuration=NULL,
                                                 gapDuration=NULL, numberOfBacktests=NULL,
                                                 backtests=NULL) {
  partition <- list(cvMethod = cvMethods$DATETIME)
  partition$datetimePartitionColumn <- datetimePartitionColumn
  partition$autopilotDataSelectionMethod <- autopilotDataSelectionMethod
  partition$validationDuration <- validationDuration
  partition$holdoutStartDate <- holdoutStartDate
  partition$holdoutDuration <- holdoutDuration
  partition$gapDuration <- gapDuration
  partition$numberOfBacktests <- numberOfBacktests
  partition$backtests <- backtests

  class(partition) <- "partition"
  return(partition)
}

as.dataRobotDatetimePartitionSpecification <- function(inList) {
  elements <- c("cvMethod",
                "datetimePartitionColumn",
                "autopilotDataSelectionMethod",
                "validationDuration",
                "holdoutStartDate",
                "holdoutDuration",
                "gapDuration",
                "numberOfBacktests",
                "backtests")
  outList <- ApplySchema(inList, elements)
  if (!is.null(outList$backtests)) {
    if (class(outList$backtests) == "list") {
    outList$backtests <- lapply(outList$backtests, as.dataRobotBacktestSpecification)
    } else if (is.data.frame(outList$backtests)) {
      outList$backtests <- as.dataRobotBacktestSpecification(outList$backtests)
    }
  }
  return(outList)
}


#' Preview the full partitioning determined by a DatetimePartitioningSpecification
#'
#' Based on the project dataset and the partitioning specification, inspect the full
#' partitioning that would be used if the same specification were passed into SetTarget
#'
#' @inheritParams DeleteProject
#' @param spec list. Datetime partition specification returned by
#'   \code{CreateDatetimePartitionSpecification}
#' @return list describing datetime partition with following components
#' \itemize{
#'   \item projectId. Character string the id of the project this partitioning applies to.
#'   \item datetimePartitionColumn. Character string the name of the column whose values
#'     as dates are used to assign a row to a particular partition.
#'   \item dateFormat. Character string the format (e.g. "%Y-%m-%d %H:%M:%S") by which the
#'     partition column was interpreted (compatible with strftime
#'     [https://docs.python.org/2/library/time.html#time.strftime]).
#'   \item autopilotDataSelectionMethod. Character string Whether models created
#'     by the autopilot use "rowCount" or "duration" as their dataSelectionMethod.
#'   \item validationDuration. Character string the validation duration specified when
#'     initializing the partitioning - not directly significant if the backtests have been
#'     modified, but used as the default validationDuration for the backtests.
#'   \item availableTrainingStartDate. Character string The start date of the available training
#'     data for scoring the holdout.
#'   \item availableTrainingDuration. Character string The duration of the available training data
#'     for scoring the holdout.
#'   \item availableTrainingRowCount. integer The number of rows in the available training data for
#'     scoring the holdout. Only available when retrieving the partitioning after setting the
#'     target.
#'   \item availableTrainingEndDate. Character string The end date of the available training data
#'     for scoring the holdout.
#'   \item primaryTrainingStartDate. Character string The start date of primary training data for
#'     scoring the holdout.
#'   \item primaryTrainingDuration. Character string The duration of the primary training data for
#'     scoring the holdout.
#'   \item primaryTrainingRowCount. integer The number of rows in the primary training data for
#'     scoring the holdout. Only available when retrieving the partitioning after setting the
#'     target.
#'   \item primaryTrainingEndDate. Character string The end date of the primary training data for
#'     scoring the holdout.
#'   \item gapStartDate. Character string The start date of the gap between training and holdout
#'     scoring data.
#'   \item gapDuration. Character string The duration of the gap between training and holdout
#'     scoring data.
#'   \item gapRowCount. integer The number of rows in the gap between training and holdout scoring
#'     data.
#'   Only available when retrieving the partitioning after setting the target.
#'   \item gapEndDate. Character string The end date of the gap between training and holdout scoring
#'     data.
#'   \item holdoutStartDate. Character string The start date of holdout scoring data.
#'   \item holdoutDuration. Character string The duration of the holdout scoring data.
#'   \item holdoutRowCount. integer The number of rows in the holdout scoring data.
#'     Only available when retrieving the partitioning after setting the target.
#'   \item holdoutEndDate. Character string The end date of the holdout scoring data.
#'   \item numberOfBacktests. integer the number of backtests used.
#'   \item backtests. data.frame of partition backtest. Each elemnet represent one backtest and has
#'     following components:
#'     index, availableTrainingStartDate, availableTrainingDuration, availableTrainingRowCount,
#'     availableTrainingEndDate, primaryTrainingStartDate, primaryTrainingDuration,
#'     primaryTrainingRowCount, primaryTrainingEndDate, gapStartDate,  gapDuration, gapRowCount,
#'     gapEndDate, validationStartDate, validationDuration, validationRowCount,
#'     validationEndDate, totalRowCount.
#'   \item totalRowCount. integer the number of rows in the project dataset.
#'     Only available when retrieving the partitioning after setting the target. Thus it will be
#'     null for GenerateDatetimePartition and populated for GetDatetimePartition.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   partitionSpec <- CreateDatetimePartitionSpecification("date_col")
#'   GenerateDatetimePartition(projectId, partitionSpec)
#' }
#' @export
GenerateDatetimePartition <- function(project, spec) {
  projectId <- ValidateProject(project)
  spec$cvMethod <- NULL
  routeString <- UrlJoin("projects", projectId, "datetimePartitioning")
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = spec, encode = "json")
  rawReturn$cvMethod <- cvMethods$DATETIME
  return(as.dataRobotDatetimePartition(rawReturn))
}

#' Retrieve the DatetimePartitioning from a project
#'
#' Only available if the project has already set the target as a datetime project.
#'
#' @inheritParams DeleteProject
#' @return list describing datetime partition. See GeneratetDatetimePartition
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetDatetimePartition(projectId)
#' }
#' @export
GetDatetimePartition <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "datetimePartitioning")
  part <- DataRobotGET(routeString, addUrl = TRUE)
  part$cvMethod <- cvMethods$DATETIME
  return(as.dataRobotDatetimePartition(part))
}


as.dataRobotDatetimePartition <- function(inList) {
  elements <- c("cvMethod",
                "projectId",
                "datetimePartitionColumn",
                "dateFormat",
                "autopilotDataSelectionMethod",
                "validationDuration",
                "availableTrainingStartDate",
                "availableTrainingDuration",
                "availableTrainingRowCount",
                "availableTrainingEndDate",
                "primaryTrainingStartDate",
                "primaryTrainingDuration",
                "primaryTrainingRowCount",
                "primaryTrainingEndDate",
                "gapStartDate",
                "gapDuration",
                "gapRowCount",
                "gapEndDate",
                "holdoutStartDate",
                "holdoutDuration",
                "holdoutRowCount",
                "holdoutEndDate",
                "numberOfBacktests",
                "backtests",
                "totalRowCount")
  outList <- ApplySchema(inList, elements)
  return(outList)
}
