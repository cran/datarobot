#' Autopilot modes
#'
#' This is a list that contains the valid values for autopilot mode. If you wish, you can
#' specify autopilot modes using the list values, e.g. AutopilotMode$FullAuto instead of typing
#' the string "auto". This way you can benefit from autocomplete and not have to remember the valid
#' options.
#'
#' \code{FullAuto} represents running the entire autopilot. \code{Quick} runs a quicker, abridged
#' version of the autopilot that focuses on the most important models. \code{Manual} does not run
#' the autopilot and instead leaves it to the user to select the algorithms to be run.
#' @export
AutopilotMode <- list(
  FullAuto = "auto",
  Manual = "manual",
  Quick = "quick")


#' Scaleout modeling modes
#'
#' This is a list that contains the valid values for the \code{scaleoutModelingMode} parameter
#' found in \code{SetTarget}. If you wish, you can specify \code{scaleoutModelingMode} using the
#' list values here, e.g. ScaleoutModelingMode$Autopilot instead of "Autopilot".
#'
#' For \code{Disabled}, no scaleout models will run in autopilot or be available in blueprints.
#'
#' For \code{RepositoryOnly}, scaleout models will be available in blueprints and can be run
#' manually, but will not run in autopilot.
#'
#' For \code{Auopilot}, scaleout models will run during autopilot and also be available in
#' blueprints.
#' @export
ScaleoutModelingMode <- list(
  Disabled = "disabled",
  RepositoryOnly = "repositoryOnly",
  Autopilot = "Autopilot")


#' Job statuses
#'
#' This is a list that contains the valid values for job status when querying the list of jobs mode.
#' If you wish, you can specify job status modes using the list values, e.g. JobStatus$InProgress
#' instead of typing the string "inprogress". This way you can benefit from autocomplete and not
#' have to remember the valid options.
#' @export
JobStatus <- list(
  Queue = "queue",
  InProgress = "inprogress",
  Error = "error",
  Aborted = "ABORTED",
  Completed = "COMPLETED")

JobFailureStatuses <- c(JobStatus$Error, JobStatus$Aborted)


#' Job type
#'
#' This is a list that contains the valid values for job type when querying the list of jobs.
#' @export
JobType <- list(
  FeatureImpact = "featureImpact",
  Predict = "predict",
  Model = "model",
  PrimeRulesets = "primeRulesets",
  PrimeDownloadValidation = "primeDownloadValidation",
  PrimeModel = "primeModel",
  ModelExport = "modelExport",
  PredictionExplanationsInitialization = "predictionExplanationsInitialization",
  PredictionExplanations = "predictionExplanations"
)


#' Prime Language
#'
#' This is a list that contains the valid values for downloadable code programming languages.
#' @export
PrimeLanguage <- list(
  Python = "Python",
  Java = "Java")


#' PostgreSQL drivers
#'
#' This is a list that contains the valid values for PostgreSQL drivers.
#' @export
PostgreSQLdrivers <- list(
  Unicode = "PostgreSQL Unicode",
  ANSI = "PostgreSQL ANSI")


#' Blend methods
#'
#' This is a list that contains the valid values for Blend methods
#' @export
BlendMethods <- list(
  PLS = "PLS",
  GLM = "GLM",
  ENET = "ENET",
  MED = "MED",
  AVERAGE = "AVG",
  MAE = "MAE",
  MAEL1 = "MAEL1",
  RANDOM_FOREST = "RF",
  LIGHT_GBM = "LGBM",
  TENSORFLOW = "TF",
  FORECAST_DISTANCE = "FORECAST_DISTANCE"
 )


#' CV methods
#'
#' This is a list that contains the valid values for CV methods
#' @export
cvMethods <- list(
  RANDOM = "random",
  STRATIFIED = "stratified",
  USER = "user",
  GROUP = "group",
  DATETIME = "datetime"
 )


#' Data Partition methods
#'
#' This is a list that contains the valid values for data partitions
#' @export
DataPartition <- list(
  VALIDATION = "validation",
  CROSSVALIDATION = "crossValidation",
  HOLDOUT = "holdout"
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
#'
#' For \code{All}, all available data is used.
#'
#' For \code{ValidationAndHoldout}, only data outside the training set is used.
#'
#' For \code{Holdout}, only holdout data is used.
#'
#' For \code{AllBacktests}, data is used from all backtest validation folds. This requires
#' the model to have successfully scored all backtests. Backtests are available on datetime
#' partitioned projects only.
#' @export
DataSubset <- list(
  All = "all",
  ValidationAndHoldout = "validationAndHoldout",
  Holdout = "holdout",
  AllBacktests = "allBacktests")


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


#' Time units
#' @export
TimeUnits <- list(
    Second = "SECOND",
    Minute = "MINUTE",
    Hour = "HOUR",
    Day = "DAY",
    Week = "WEEK",
    Month = "MONTH",
    Quarter = "QUARTER",
    Year = "YEAR")


#' Periodicity time units
#'
#' Same as time units, but kept for backwards compatibility.
#' @export
PeriodicityTimeUnits <- TimeUnits


#' Periodicity max time step
#' @export
PeriodicityMaxTimeStep <- 9223372036854775807


#' Target leakage report values
#' @export
TargetLeakageType <- list(
    SkippedDetection = "SKIPPED_DETECTION",
    False = "FALSE",
    ModerateRisk = "MODERATE_RISK",
    HighRisk = "HIGH_RISK"
)


#' Recommended model type values
#'
#' \code{MostAccurate} retrieves the most accurate model based on validation or
#' cross-validation results. In most cases, this will be a blender model.
#'
#' \code{FastAccurate} retrieves the most accurate individual model (not blender) that passes
#' set guidelines for prediction speed. If no models meet the prediction speed guideline, this
#' will not retrieve anything.
#'
#' \code{RecommendedForDeployment} retrieves the most accurate individual model. This model
#' will have undergone specific pre-preparations to be deployment ready. See
#' \code{GetModelRecommendation} for details.
#'
#' \code{Recommended} is deprecated as of v2.14 and will be removed in v2.16.
#' @export
RecommendedModelType <- list(
  MostAccurate = "Most Accurate",
  FastAccurate = "Fast & Accurate",
  Recommended = "Recommended",
  RecommendedForDeployment = "Recommended for Deployment"
)


#' Project stage
#' @export
ProjectStage <- list(
  AIM = "aim",
  EDA = "eda",
  EMPTY = "empty",
  MODELING = "modeling"
)


#' Sharing role
#'
#' This is a list that contains the valid values for granting access to other users (see
#' \code{Share}). If you wish, you can specify access roles using the list values, e.g.,
#' \code{SharingRole$ReadWrite} instead of typing the string "READ_WRITE". This way you can
#' benefit from autocomplete and not have to remember the valid options.
#'
#' \code{Owner} allows any action including deletion.
#'
#' \code{ReadWrite} or \code{Editor} allows modifications to the state, e.g., renaming
#' and creating data sources from a data store, but *not* deleting the entity.
#'
#' \code{ReadOnly} or \code{Consumer} - for data sources, enables creating projects and predictions;
#' for data stores, allows viewing them only.
#' @export
SharingRole <- list(
  Owner = "OWNER",
  ReadWrite = "READ_WRITE",
  User = "USER",
  Editor = "EDITOR",
  ReadOnly = "READ_ONLY",
  Consumer = "CONSUMER"
)


#' Series aggregation type
#'
#' For details, see "Calculating features across series" in the Time Series section of the
#' DataRobot user guide.
#' @export
SeriesAggregationType <- list(
  Average = "average",
  Total = "total"
)
