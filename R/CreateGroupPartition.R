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
#' @param partitionKeyCols Character string specifying the name or names of
#' the variables used in defining the group partition.
#' @param reps Integer, specifying the number of cross-validation folds to
#' generate; only applicable when validationType = 'CV'.
#' @return An S3 object of class 'partition' including the parameters required
#' by the SetTarget function to generate a group-based partitioning of
#' the modeling dataset.
#' @param validationPct Integer, giving the percentage of data to be used
#' as the validation subset.
#' @export
#'
CreateGroupPartition <- function(validationType, holdoutPct, partitionKeyCols,
                                 reps = NULL, validationPct = NULL) {
  #
  ##############################################################################
  #
  #  Returns a partition object with components required when cvMethod = "group"
  #
  ##############################################################################
  #
  partition <- list(cvMethod = "group", validationType = validationType,
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
