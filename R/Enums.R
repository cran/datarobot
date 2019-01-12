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

#' Scaleout modeling modes
#'
#' This is a list that contains the valid values for the \code{scaleoutModelingMode} parameter
#' found in \code{SetTarget}. If you wish, you can specify \code{scaleoutModelingMode} using the
#' list values here, e.g. ScaleoutModelingMode$Autopilot instead of "Autopilot".
#' @export
ScaleoutModelingMode <- list(
  Disabled = "disabled",              # No scaleout models will run in autopilot or be available
                                      # in blueprints.
  RepositoryOnly = "repositoryOnly",  # Scaleout models will be available in blueprints and can
                                      # be run manually, but will not run in autopilot.
  Autopilot = "Autopilot")            # Scaleout models will run during autopilot and also be
                                      # available in blueprints.

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
  MAEL1 = 'MAEL1',
  RANDOM_FOREST = 'RF',
  LIGHT_GBM = 'LGBM',
  TENSORFLOW = 'TF'
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

#' Target Type modes
#'
#' This is a list that contains the valid values for the Target Types
#' @export
TargetType  <- list(
    Binary = "Binary",
    Multiclass = "Multiclass",
    Regression = "Regression"
)

#' Data subset for training predictions
#'
#' This is a list that contains the valid values for the \code{dataSubset} parameter
#' found in \code{RequestTrainingPredictions}. If you wish, you can specify
#' \code{dataSubset} using the list values here.
#' @export
DataSubset <- list(
  All = "all",                                    # All available data is used.
  ValidationAndHoldout = "validationAndHoldout",  # Only data outside the training set is used.
  Holdout = "holdout")                            # Only holdout data is used.


#' Treat as exponential
#' @export
TreatAsExponential <- list(
    Always = "always",
    Never = "never",
    Auto = "auto"
)

#' Differencing method
#' @export
DifferencingMethod <- list(
    Auto = "auto",
    Simple = "simple",
    None = "none",
    Seasonal = "seasonal"
)

#' Periodicity time units
#' @export
PeriodicityTimeUnits <- list(
    Second = "SECOND",
    Minute = "MINUTE",
    Hour = "HOUR",
    Day = "DAY",
    Week = "WEEK",
    Month = "MONTH",
    Quarter = "QUARTER",
    Year = "YEAR")

#' Target leakage report values
#' @export
TargetLeakageType <- list(
    SkippedDetection = "SKIPPED_DETECTION",
    False = "FALSE",
    ModerateRisk = "MODERATE_RISK",
    HighRisk = "HIGH_RISK"
)

#' Recommended model type values
#' @export
RecommendedModelType <- list(
  MostAccurate = "Most Accurate",
  FastAccurate = "Fast & Accurate",
  Recommended = "Recommended"
)

#' Periodicity max time step
#' @export
PeriodicityMaxTimeStep <- 9223372036854775807
