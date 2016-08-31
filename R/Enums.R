#' Autopilot modes
#'
#' This is a list that contains the valid values for autopilot mode. If you wish, you can
#' specify autopilot modes using the list values, e.g. AutopilotMode$FullAuto instead of typing
#' the string 'auto'. This way you can benefit from autocomplete and not have to remember the valid
#' optons.
#'
#' @export
#'
AutopilotMode <- list(
  FullAuto = 'auto',
  SemiAuto = 'semi',
  Manual = 'manual')


PredictJobStatus <- list(
  Queue = 'queue',
  InProgress = 'inprogress',
  Error = 'error',
  Aborted = 'ABORTED')


#' Job statuses
#'
#' This is a list that contains the valid values for job status when querying the list of jobs mode.
#' If you wish, you can specify autopilot modes using the list values, e.g. JobStatus$InProgress
#' instead of typing the string 'inprogress'. This way you can benefit from autocomplete and not have to
#' remember the valid optons.
#'
#'
#' @export
#'
JobStatus <- list(
  Queue = 'queue',
  InProgress = 'inprogress',
  Error = 'error',
  Aborted = 'ABORTED',
  Completed = 'COMPLETED')

JobFailureStatuses <- c(JobStatus$Error, JobStatus$Aborted)


JobType <- list(
  FeatureImpact = 'featureImpact',
  Predict = 'predict',
  Model = 'model'
)
