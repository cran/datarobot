#' Create a user-defined S3 object of class partition for the SetTarget function
#'
#' Creates a list object used by the SetTarget function to specify either
#' Training/Validation/Holdout (validationType = 'TVH') or cross-validation
#' (validationType = 'CV') partitions of the modeling dataset based on the values
#' included in a column from the dataset. In either case, the name of this data
#' column must be specified (as userPartitionCol). For the 'TVH' option, the
#' column must have either exactly 3 values (in which case the values
#' used to specify each level must be given) or exactly 2 values (in which
#' case training and validation levels should be specified, but), while for the 'CV' option, only the
#' level that specifies the holdout subset must be given.
#'
#' This function is one of several convenience functions provided to simplify the
#' task of starting modeling projects with custom partitioning options. The other
#' functions are CreateGroupPartition, CreateRandomPartition, and CreateStratifiedPartition.
#'
#' @inheritParams CreateGroupPartition
#' @param userPartitionCol Character string naming the data column from the
#' modeling dataset containing the subset designations.
#' @param cvHoldoutLevel Data value from userPartitionCol that identifies the
#' holdout subset under the 'CV' option.
#' @param trainingLevel Data value from userPartitionCol that identifies the
#' training subset under the 'TVH' option.
#' @param holdoutLevel Data value from userPartitionCol that identifies the
#' holdout subset under the 'TVH' option.
#' @param validationLevel Data value from userPartitionCol that identifies the
#' validation subset under the 'TVH' option.
#' @return An S3 object of class 'partition' including the parameters required
#' by the SetTarget function to generate a user-specified of the modeling
#' dataset.
#' @export
#'
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
  partition <- list(cvMethod = "user", validationType = validationType,
                    userPartitionCol = userPartitionCol)
  if (validationType == "CV") {
    if (is.null(cvHoldoutLevel)) {
      stop(strwrap("Parameter cvHoldoutLevel must be specified for user
                   partition with validationType = 'CV'"))
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
