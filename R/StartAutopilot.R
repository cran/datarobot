1#' Set the target variable (and by default, start the DataRobot Autopilot)
#'
#' This function sets the target variable for the project defined by
#' project, starting the process of building models to predict the response
#' variable target.  Both of these parameters - project and target - are
#' required and they are sufficient to start a modeling project with
#' DataRobot default specifications for the other 10 optional parameters.
#'
#' @inheritParams DeleteProject
#' @inheritParams GetValidMetrics
#' @param metric character. Optional. String specifying the model fitting metric
#'   to be optimized; a list of valid options for this parameter, which depends on
#'   both project and target, may be obtained with the function GetValidMetrics.
#' @param weights character. Optional. String specifying the name of the column
#'   from the modeling dataset to be used as weights in model fitting.
#' @param partition partition. Optional. S3 object of class 'partition' whose elements specify
#'   a valid partitioning scheme.  See help for functions CreateGroupPartition,
#'   CreateRandomPartition, CreateStratifiedPartition, CreateUserPartition
#'   and CreateDatetimePartitionSpecification.
#' @param targetType character. Optional. Used to specify the targetType to use for a project.
#'   Valid options are "Binary", "Multiclass", "Regression". Set to "Multiclass" to enable
#'   multiclass modeling. Otherwise, it can help to disambiguate, i.e. telling DataRobot how to
#'   handle a numeric target with a few unique values that could be used for either multiclass
#'   or regression. See \code{TargetType} for an easier way to keep track of the options.
#' @param mode character. Optional. Specifies the autopilot mode used to start the
#'   modeling project; valid options are "auto" (fully automatic,
#'   the current DataRobot default, obtained when mode = NULL), "manual" and "quick"
#' @param seed integer. Optional. Seed for the random number generator used in
#'   creating random partitions for model fitting.
#' @param positiveClass character. Optional. Target variable value corresponding to a positive
#'   response in binary classification problems.
#' @param blueprintThreshold integer. Optional. The maximum time
#'   (in hours) that any modeling blueprint is allowed to run before being
#'   excluded from subsequent autopilot stages.
#' @param responseCap numeric. Optional. Floating point value, between 0.5 and 1.0,
#'   specifying a capping limit for the response variable. The default value
#'   NULL corresponds to an uncapped response, equivalent to responseCap = 1.0.
#' @param featurelistId numeric. Specifies which feature list to use. If NULL (default),
#'   a default featurelist is used.
#' @param smartDownsampled logical. Optional. Whether to use smart downsampling to throw
#'   away excess rows of the majority class. Only applicable to classification and zero-boosted
#'   regression projects.
#' @param majorityDownsamplingRate numeric. Optional. Floating point value, between 0.0 and 100.0.
#'   The percentage of the majority rows that should be kept.  Specify only if using smart
#'   downsampling. May not cause the majority class to become smaller than the minority class.
#' @param scaleoutModelingMode character. Optional. Specifies the behavior of Scaleout models
#'   for the project. Possible options are in \code{ScaleoutModelingMode}.
#'   \itemize{
#'   \item \code{ScaleoutModelingMode$Disabled} will prevent scaleout models from running during
#'     autopilot and will prevent Scaleout models from showing up in blueprints.
#'   \item \code{ScaleoutModelingMode$RepositoryOnly} will prevent scaleout models from running
#'     during autopilot, but will make them available in blueprints to run manually.
#'   \item \code{ScaleoutModelingMode$Autopilot} will run scaleout models during autopilot and
#'     will make them available in blueprints.
#'   }
#'   Note that scaleout models are only supported in the Hadoop environment with the correct
#'   corresponding user permission set.
#' @param accuracyOptimizedBlueprints logical. Optional. When enabled, accuracy optimized
#'  blueprints will run in autopilot for the project. These are longer-running model blueprints
#'  that provide increased accuracy over normal blueprints that run during autopilot.
#' @param offset character. Optional. Vector of the names of the columns containing the offset of
#'   each row.
#' @param exposure character. Optional. The name of a column containing the exposure of each row.
#' @param eventsCount character. Optional. The name of a column specifying the events count.
#' @param maxWait integer. Specifies how many seconds to wait for the server to finish
#'   analyzing the target and begin the modeling process. If the process takes
#'   longer than this parameter specifies, execution will stop (but the server
#'   will continue to process the request).
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   SetTarget(projectId, "targetFeature")
#'   SetTarget(projectId, "targetFeature", metric = "LogLoss")
#'   SetTarget(projectId, "targetFeature", mode = AutopilotMode$Manual)
#'   SetTarget(projectId, "targetFeature", targetType = TargetType$Multiclass)
#' }
#' @export
SetTarget <- function(project, target, metric = NULL, weights = NULL,
                      partition = NULL, mode = NULL, seed = NULL, targetType = NULL,
                      positiveClass = NULL, blueprintThreshold = NULL,
                      responseCap = NULL, featurelistId = NULL,
                      smartDownsampled = NULL, majorityDownsamplingRate = NULL,
                      scaleoutModelingMode = NULL, accuracyOptimizedBlueprints = NULL,
                      offset = NULL, exposure = NULL, eventsCount = NULL, maxWait = 600) {
  if (is.null(target)) {
    stop("No target variable specified - cannot start Autopilot")
  }

  if (!is.null(mode) && mode == AutopilotMode$Quick) {
    mode <- AutopilotMode$FullAuto
  }

  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "aim")
  pStat <- GetProjectStatus(projectId)
  stage <- as.character(pStat[which(names(pStat) == "stage")])
  if (stage != "aim") {
    errorMsg <- paste("Autopilot stage is", stage,
                      "but it must be 'aim' to set the target and start a new project")
    stop(strwrap(errorMsg))
  }

  bodyList <- list(target = target)
  bodyList$metric <- metric
  bodyList$weights <- weights
  if (is.numeric(mode)) {
    Deprecated("Numeric modes (use e.g. AutopilotMode$FullAuto instead)", "2.1", "2.10")
  }
  bodyList$mode <- mode
  bodyList$seed <- seed
  bodyList$positiveClass <- positiveClass
  bodyList$blueprintThreshold <- blueprintThreshold
  bodyList$responseCap <- responseCap
  bodyList$featurelistId <- featurelistId
  bodyList$smartDownsampled <- smartDownsampled
  bodyList$majorityDownsamplingRate <- majorityDownsamplingRate
  bodyList$scaleoutModelingMode <- scaleoutModelingMode
  bodyList$accuracyOptimizedMb <- accuracyOptimizedBlueprints
  bodyList$offset <- offset
  bodyList$exposure <- exposure
  bodyList$eventsCount <- eventsCount
  ValidateParameterIn(targetType, TargetType)
  bodyList$targetType <- targetType

  if (!is.null(partition) && !is(partition, "partition")) {
    partitioningMethods <- c("CreateRandomPartition", "CreateStratifiedPartition",
                             "CreateGroupPartition", "CreateUserPartition",
                             "CreateDatetimePartitionSpecification")
    stop("You must use a valid partition object to specify your partitioning.",
         " See docs for ", paste0(lapply(partitioningMethods, sQuote), collapse = ", "),
         " for examples.")
  }
  if (!is.null(partition)) {
    if (partition$cvMethod == cvMethods$DATETIME) {
      partition <- as.dataRobotDatetimePartitionSpecification(partition)
    }
    bodyList <- append(bodyList, partition)
  }

  Unbox <- function(x) {
    if (length(x) == 1 && !is.list(x)) { jsonlite::unbox(x) }
    else { x }
  }
  response <- DataRobotPATCH(routeString,
                             addUrl = TRUE,
                             body = lapply(bodyList, Unbox),
                             returnRawResponse = TRUE,
                             encode = "json")
  WaitForAsyncReturn(httr::headers(response)$location,
                     addUrl = FALSE,
                     maxWait = maxWait,
                     failureStatuses = "ERROR")
  message("Autopilot started")
}

#' Starts autopilot on provided featurelist.
#
#' Only one autopilot can be running at the time.
#' That's why any ongoing autopilot on different featurelist will
#' be halted - modelling jobs in queue would not
#' be affected but new jobs would not be added to queue by
#' halted autopilot.
#'
#' There is an error if autopilot is currently running on or has already
#' finished running on the provided featurelist and also if project's target was not selected
#' (via SetTarget).
#'
#' @inheritParams DeleteProject
#' @param featurelistId numeric. Specifies which feature list to use.
#' @param mode character. The desired autopilot mode. Currently only AutopilotMode$FullAuto
#'   is supported.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   featureList <- CreateFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#'   featurelistId <- featureList$featurelistId
#'   StartNewAutoPilot(projectId, featurelistId)
#' }
#' @export
StartNewAutoPilot <- function(project, featurelistId, mode = AutopilotMode$FullAuto) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "autopilots")
  payload <- list(featurelistId = featurelistId, mode = mode)
  return(invisible(DataRobotPOST(routeString, addUrl = TRUE, body = payload)))
}
