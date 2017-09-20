#' Autopilot modes
#'
#' This is a list that contains the valid values for autopilot mode. If you wish, you can
#' specify autopilot modes using the list values, e.g. AutopilotMode$FullAuto instead of typing
#' the string 'auto'. This way you can benefit from autocomplete and not have to remember the valid
#' optons.
#' @export
AutopilotMode <- list(
  FullAuto = 'auto',
  Manual = 'manual',
  Quick = 'quick')

#' Job statuses
#'
#' This is a list that contains the valid values for job status when querying the list of jobs mode.
#' If you wish, you can specify job status modes using the list values, e.g. JobStatus$InProgress
#' instead of typing the string 'inprogress'. This way you can benefit from autocomplete and not
#' have to remember the valid optons.
#' @export
JobStatus <- list(
  Queue = 'queue',
  InProgress = 'inprogress',
  Error = 'error',
  Aborted = 'ABORTED',
  Completed = 'COMPLETED')

JobFailureStatuses <- c(JobStatus$Error, JobStatus$Aborted)

#' Job type
#'
#' This is a list that contains the valid values for job type when querying the list of jobs.
#' @export
JobType <- list(
  FeatureImpact = 'featureImpact',
  Predict = 'predict',
  Model = 'model',
  PrimeRulesets = 'primeRulesets',
  PrimeDownloadValidation = 'primeDownloadValidation',
  PrimeModel = 'primeModel',
  ModelExport = 'modelExport',
  ReasonCodesInitialization = 'reasonCodesInitialization',
  ReasonCodes = 'reasonCodes'
)

#' Prime Language
#'
#' This is a list that contains the valid values for downloadable code programming languages.
#' @export
PrimeLanguage <- list(
  Python = 'Python',
  Java = 'Java')

#' PostgreSQL drivers
#'
#' This is a list that contains the valid values for PostgreSQL drivers.
#' @export
PostgreSQLdrivers <- list(
  Unicode = 'PostgreSQL Unicode',
  ANSI = 'PostgreSQL ANSI')

#' Blend methods
#'
#' This is a list that contains the valid values for Blend methods
#' @export
BlendMethods <- list(
  PLS = 'PLS',
  GLM = 'GLM',
  ENET = 'ENET',
  MED = 'MED',
  AVERAGE = 'AVG',
  MAE = 'MAE',
  MAEL1 = 'MAEL1'
 )

#' CV methods
#'
#' This is a list that contains the valid values for CV methods
#' @export
cvMethods <- list(
  RANDOM = 'random',
  STRATIFIED = 'stratified',
  USER = 'user',
  GROUP = 'group',
  DATETIME = 'datetime'
 )

#' Data Partition methods
#'
#' This is a list that contains the valid values for data partitions
#' @export
DataPartition <- list(
  VALIDATION = 'validation',
  CROSSVALIDATION = 'crossValidation',
  HOLDOUT = 'holdout'
 )
