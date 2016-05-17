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
#' @inheritParams CreateGroupPartition
#' @inheritParams CreateGroupPartition
#' @inheritParams CreateGroupPartition
#' @return An S3 object of class partition including the parameters
#' required by SetTarget to generate a random partitioning of
#' the modeling dataset.
#' @export
#'
CreateRandomPartition <- function(validationType, holdoutPct, reps = NULL,
                                  validationPct = NULL) {
  #
  ##############################################################################
  #
  #  Returns a partition object with components required for cvMethod = "random"
  #
  ##############################################################################
  #
  partition <- list(cvMethod = "random", validationType = validationType,
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
