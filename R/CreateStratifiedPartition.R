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
#' @inheritParams CreateGroupPartition
#' @inheritParams CreateGroupPartition
#' @inheritParams CreateGroupPartition
#' @return An S3 object of class 'partition' including the parameters required
#' by the SetTarget function to generate a stratified partitioning of the
#' modeling dataset.
#' @export
#'
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
  partition <- list(cvMethod = "stratified", validationType = validationType,
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
