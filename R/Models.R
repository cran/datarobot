#' Retrieve the details of a specified model
#'
#' This function returns a DataRobot S3 object of class
#' dataRobotModel for the model defined by project and modelId.
#'
#' The S3 object returned by this function is required by the
#' functions DeleteModel, ListModelFeatures, and RequestSampleSizeUpdate.
#'
#' @inheritParams DeleteProject
#' @param modelId character. Unique alphanumeric identifier for the model of interest.
#' @return An S3 object of class `dataRobotModel', which is a list
#' with the following components:
#' \itemize{
#'   \item featurelistId. Character string: unique alphanumeric identifier for the featurelist on
#'     which the model is based.
#'   \item processes. Character vector with components describing preprocessing; may include
#'     modelType.
#'   \item featurelistName. Character string giving the name of the featurelist on which the model
#'     is based.
#'   \item projectId. Character string giving the unique alphanumeric identifier for the project.
#'   \item samplePct. Numeric or NULL. The percentage of the project dataset used in training the
#'     model. If the project uses datetime partitioning, the \code{samplePct} will be NULL.
#'     See \code{trainingRowCount}, \code{trainingDuration}, and \code{trainingStartDate}
#'     and \code{trainingEndDate} instead.
#'   \item trainingRowCount. Integer. The number of rows of the project dataset used in training
#'     the model. In a datetime partitioned project, if specified, defines the number of
#'     rows used to train the model and evaluate backtest scores; if unspecified, either
#'     \code{trainingDuration} or \code{trainingStartDate} and \code{trainingEndDate} was used to
#'     determine that instead.
#'   \item isFrozen. Logical : is model created with frozen tuning parameters.
#'   \item modelType. Character string describing the model type.
#'   \item metrics. List with one element for each valid metric associated with the model. Each
#'     element is a list with elements for each possible evaluation type (holdout, validation,
#'     and crossValidation).
#'   \item modelCategory. Character string giving model category (e.g., blend, model).
#'   \item blueprintId. Character string giving the unique DataRobot blueprint identifier on which
#'     the model is based.
#'   \item modelId. Character string giving the unique alphanumeric model identifier.
#'   \item projectName. Character string: optional description of project defined by projectId.
#'   \item projectTarget. Character string defining the target variable predicted by all models in
#'     the project.
#'   \item projectMetric. Character string defining the fitting metric optimized by all project
#'     models.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetModel(projectId, modelId)
#' }
#' @export
GetModel <- function(project, modelId) {
  #  Fail if modelId is an empty string
  if (modelId == "") {
    stop("modelId must not be blank")
  } else {
    projectId <- ValidateProject(project)
    fullProject <- GetProject(projectId)
    routeString <- UrlJoin("projects", projectId, "models", modelId)
    modelDetails <- DataRobotGET(routeString, addUrl = TRUE)
    #
    #  Request successful - extract data from $content element of
    #  Reformat results: (1) change name "id" to "modelId";
    #  (2) reformat $metrics list component to replace NULL
    #  representation of missing values with NA
    #
    #  Also, add projectName, projectTarget, and projectMetric
    #
    #  NOTE: if the $processes list is empty, it is represented
    #        as an empty list rather than an empty character vector,
    #        while ListModels returns an empty character vector
    #        for this case; for compatability, check
    #        for this case and reformat if detected
    #

    names(modelDetails)[names(modelDetails) == "id"] <- "modelId"
    modelDetails$metrics <- ReformatMetrics(modelDetails$metrics)
    modelDetails$projectName <- fullProject$projectName
    modelDetails$projectTarget <- fullProject$target
    modelDetails$projectMetric <- fullProject$metric
    if (length(modelDetails$processes) == 0) {
      modelDetails$processes <- character(0)
    }
    as.dataRobotModelObject(modelDetails)
  }
}


#' Retrieve the details of a specified frozen model
#'
#' This function returns a DataRobot S3 object of class
#' dataRobotFrozenModel for the model defined by project and modelId.
#' GetModel also can be used to retrieve some information about
#' frozen model, however then some frozen specific information (parentModelId)
#' will not be returned
#'
#' The S3 object returned by this function is required by the
#' functions DeleteModel, ListModelFeatures, and RequestSampleSizeUpdate.
#'
#' @inheritParams DeleteProject
#' @param modelId Unique alphanumeric identifier for the model of interest.
#' @inherit GetModel return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetFrozenModel(projectId, modelId)
#' }
#' @export
GetFrozenModel <- function(project, modelId) {
  #  Fail if modelId is an empty string
  if (modelId == "") {
    stop("modelId must not be blank")
  } else {
    projectId <- ValidateProject(project)
    fullProject <- GetProject(projectId)
    projectName <- fullProject$projectName
    projectTarget <- fullProject$target
    projectMetric <- fullProject$metric
    routeString <- UrlJoin("projects", projectId, "frozenModels", modelId)
    modelDetails <- DataRobotGET(routeString, addUrl = TRUE)
    #
    #  Request successful - extract data from $content element of
    #  Reformat results: (1) change name "id" to "modelId";
    #  (2) reformat $metrics list component to replace NULL
    #  representation of missing values with NA
    #
    #  Also, add projectName, projectTarget, and projectMetric
    #
    #  NOTE: if the $processes list is empty, it is represented
    #        as an empty list rather than an empty character vector,
    #        while ListModels returns an empty character vector
    #        for this case; for compatability, check
    #        for this case and reformat if detected
    #
    listNames <- names(modelDetails)
    idIndex <- which(listNames == "id")
    names(modelDetails)[idIndex] <- "modelId"
    modelDetails$metrics <- ReformatMetrics(modelDetails$metrics)
    modelDetails$projectName <- projectName
    modelDetails$projectTarget <- projectTarget
    modelDetails$projectMetric <- projectMetric
    if (length(modelDetails$processes) == 0) {
      modelDetails$processes <- character(0)
    }
    as.dataRobotFrozenModelObject(modelDetails)
  }
}


#' Retrieve all available model information for a DataRobot project
#'
#' This function requests the model information for the DataRobot
#' project specified by the project argument, described under Arguments.
#' This parameter may be obtained in several ways, including: (1), from
#' the projectId element of the list returned by ListProjects; (2), as
#' the object returned by the GetProject function; or (3), as the list
#' returned by the SetupProject function. The function returns an S3
#' object of class 'listOfModels'.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class listOfModels, which may be characterized
#' using R's generic summary function or converted to a dataframe with
#' the as.data.frame method.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListModels(projectId)
#' }
#' @export
ListModels <- function(project) {
  projectId <- ValidateProject(project)
  fullProject <- GetProject(projectId)
  routeString <- UrlJoin("projects", projectId, "models")
  modelInfo <- DataRobotGET(routeString, addUrl = TRUE, simplify = FALSE)
  if (length(modelInfo) == 0) {
    message("No models have been built yet in this project.")
    returnList <- list()
  } else {
    modelInfo <- lapply(modelInfo, function(model) {
                          model$projectName <- fullProject$projectName
                          model$projectTarget <- fullProject$target
                          model$projectMetric <- fullProject$metric
                          model$metrics <- ReformatMetrics(model$metrics)
                          model
                        })
    returnList <- lapply(modelInfo, as.dataRobotModelObject)
  }
  currentModelJobs <- ListModelJobs(projectId)
  if (NROW(currentModelJobs) > 0) {
    message("Some models are still in progress")
  }
  class(returnList) <- c("listOfModels", "listSubclass")
  returnList
}


#' Retrieve a new or updated model defined by modelJobId
#'
#' The functions RequestNewModel and RequestSampleSizeUpdate
#' initiate the creation of new models in a DataRobot project.
#' Both functions submit requests to the DataRobot modeling
#' engine and return an integer-valued modelJobId.  The
#' GetModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete.  Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' @inheritParams DeleteProject
#' @param modelJobId The integer returned by either RequestNewModel
#' or RequestSampleSizeUpdate.
#' @param maxWait Integer, The maximum time (in seconds) to wait for the model job to complete
#' @return An S3 object of class 'dataRobotModel' summarizing all
#' available information about the model.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- ListModelJobs(project)
#'   job <- initialJobs[[1]]
#'   modelJobId <- job$modelJobId
#'   GetModelJobFromJobId(projectId, modelJobId)
#' }
#' @export
GetModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                     failureStatuses = JobFailureStatuses)
  modelId <- modelDetails$id
  returnModel <- GetModel(projectId, modelId)
  message("Model ", modelId, " retrieved")
  class(returnModel) <- "dataRobotModel"
  returnModel
}

#' Retrieve a frozen model defined by modelJobId
#'
#' The function RequestFrozenModel
#' initiate the creation of frozen models in a DataRobot project.
#' RequestFrozenModel function submit requests to the DataRobot modeling
#' engine and return an integer-valued modelJobId.  The
#' GetFrozenModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotFrozenModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete.  Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' GetModelFromJobId also can be used to retrieve some information about
#' frozen model, however then some frozen specific information (parentModelId)
#' will not be returned.
#'
#' @inheritParams DeleteProject
#' @param modelJobId integer. The integer returned by either \code{RequestNewModel}
#'   or \code{RequestSampleSizeUpdate}.
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to
#'   complete.
#' @return An S3 object of class 'dataRobotFrozenModel' summarizing all
#' available information about the model.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- ListModelJobs(project)
#'   job <- initialJobs[[1]]
#'   modelJobId <- job$modelJobId
#'   GetModelJobFromJobId(projectId, modelJobId)
#' }
#' @export
GetFrozenModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                     failureStatuses = JobFailureStatuses)
  modelId <- modelDetails$id
  returnModel <- GetFrozenModel(projectId, modelId)
  message("Model ", modelId, " retrieved")
  class(returnModel) <- "dataRobotFrozenModel"
  returnModel
}


#' Adds a new model of type specified by blueprint to a DataRobot project
#'
#' This function requests the creation of a new model in the DataRobot
#' modeling project defined by the project parameter.  The function also
#' allows the user to specify alternatives to the project default for
#' featurelist, samplePct, and scoringType.  This function returns an
#' integer modelJobId value, which can be used by the GetModelFromJobId
#' function to return the full model object.
#'
#' Motivation for this function is the fact that some models - e.g., very
#' complex machine learning models fit to large datasets - may take a long
#' time to complete.  Splitting the model creation request from model
#' retrieval in these cases allows the user to perform other interactive R
#' session tasks between the time the model creation/update request is made
#' and the time the final model is available.
#'
#' Either `sample_pct` or `training_row_count` can be used to specify the amount of data to
#' use, but not both. If neither are specified, a default of the maximum amount of data that
#' can safely be used to train any blueprint without going into the validation data will be
#' selected.

#' In smart-sampled projects, `samplePct` and `trainingRowCount` are assumed to be in terms of rows
#' of the minority class.
#'
#' Note : For datetime partitioned projects, use \code{RequestNewDatetimeModel} instead
#'
#' @inheritParams DeleteProject
#' @param blueprint list. A list with at least the following two elements:
#'   blueprintId and projectId.  Note that the individual elements of the
#'   list returned by ListBlueprints are admissible values for this parameter.
#' @param featurelist list. A list that contains the element featurelistId that
#'   specifies the featurelist to be used in building the model; if not
#'   specified (i.e., for the default value NULL), the project default
#'   (Informative Features) is used.
#' @param samplePct numeric. The percentage of the training
#'   dataset to be used in building the new model; if not specified
#'   (i.e., for the default value NULL), the maxTrainPct value for the
#'   project is used. Value should be between 0 and 100.
#' @param trainingRowCount integer. The number of rows to use to train
#'   the requested model.
#' @param scoringType character. String specifying the scoring type;
#'   default is validation set scoring, but cross-validation averaging
#'   is also possible.
#' @param monotonicIncreasingFeaturelistId character. Optional. The id of the featurelist
#'   that defines the set of features with a monotonically increasing relationship to the
#'   target. If \code{NULL} (default), the default for the project will be used (if any).
#'   Note that currently there is no way to create a model without monotonic constraints
#'   if there was a project-level default set. If desired, the featurelist itself can
#'   also be passed as this parameter.
#' @param monotonicDecreasingFeaturelistId character. Optional. The id of the featurelist
#'   that defines the set of features with a monotonically decreasing relationship to the
#'   target. If \code{NULL}, the default for the project will be used (if any). If empty
#'   (i.e., \code{""}), no such constraints are enforced. Also, if desired, the featurelist
#'   itself can be passed as this parameter.
#' @return An integer value that can be used as the modelJobId parameter
#'   in subsequent calls to the GetModelFromJobId function.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   blueprints <- ListBlueprints(projectId)
#'   blueprint <- blueprints[[1]]
#'   RequestNewModel(projectId, blueprint)
#' }
#' @export
RequestNewModel <- function(project, blueprint, featurelist = NULL,
                            samplePct = NULL, trainingRowCount = NULL, scoringType = NULL,
                            monotonicIncreasingFeaturelistId = NULL,
                            monotonicDecreasingFeaturelistId = NULL) {
  #
  #########################################################################
  #
  #  Sets up the creation of a new model in project, based on blueprint,
  #  both required parameters; additional optional parameters are
  #  featurelist, samplePct, and scoringType.  Note that blueprint is a
  #  list, which must contain the elements blueprintId and projectId.
  #  This function returns the integer-valued modelJobId, which can be
  #  used with the function GetModelFromJobId to retrieve the model once
  #  it has been built.
  #
  #########################################################################
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "models")
  #
  #  Construct the body for the POST command
  #
  #  NOTE: if more than one parameter is to be passed in the body of the POST
  #        command, they are passed as an unboxed dataframe with
  #        encode = "json"; if only the required parameter blueprintID is
  #        to be passed, it is passed as a list without encode = "json"
  #
  #  Check whether blueprint$projectId differs from projectId.
  #  If so, need to specify sourceProjectId as value
  #  from blueprint list; since this is not usually the case,
  #  pre-specify secondProject = FALSE # nolint
  #

  blueprintId <- blueprint$blueprintId
  bodyFrame <- list(blueprintId = blueprintId)
  if (!identical(blueprint$projectId, projectId)) {
    bodyFrame$sourceProjectId <- blueprint$projectId
  }
  if (!is.null(featurelist)) {
    bodyFrame$featurelistId <- featurelist$featurelistId
  }
  if (!is.null(samplePct)) {
    bodyFrame$samplePct <- samplePct
  }
  if (!is.null(trainingRowCount)) {
    bodyFrame$trainingRowCount <- trainingRowCount
  }
  if (!is.null(scoringType)) {
    bodyFrame$scoringType <- scoringType
  }
  if (is.list(monotonicIncreasingFeaturelistId) &&
      "featurelistId" %in% names(monotonicIncreasingFeaturelistId)) {
    monotonicIncreasingFeaturelistId <- monotonicIncreasingFeaturelistId$featurelistId
  }
  if (!is.null(monotonicIncreasingFeaturelistId)) {
    bodyFrame$monotonicIncreasingFeaturelistId <- monotonicIncreasingFeaturelistId
  }
  if (is.list(monotonicDecreasingFeaturelistId) &&
      "featurelistId" %in% names(monotonicDecreasingFeaturelistId)) {
    monotonicDecreasingFeaturelistId <- monotonicDecreasingFeaturelistId$featurelistId
  }
  if (!is.null(monotonicDecreasingFeaturelistId)) {
    bodyFrame$monotonicDecreasingFeaturelistId <- monotonicDecreasingFeaturelistId
  }
  # The only way to make a NULL exist in a list in R is to invoke the list constructor
  # so we have to go to this trouble.
  if (identical(monotonicDecreasingFeaturelistId, "") &&
      identical(monotonicIncreasingFeaturelistId, "")) {
    bodyFrame <- append(list(monotonicDecreasingFeaturelistId = NULL,
                             monotonicIncreasingFeaturelistId = NULL),
                        bodyFrame[setdiff(names(bodyFrame), c("monotonicDecreasingFeaturelistId",
                                                              "monotonicIncreasingFeaturelistId"))])
  }
  else if (identical(monotonicIncreasingFeaturelistId, "")) {
    bodyFrame <- append(list(monotonicIncreasingFeaturelistId = NULL),
                        bodyFrame[setdiff(names(bodyFrame), "monotonicIncreasingFeaturelistId")])
  }
  else if (identical(monotonicDecreasingFeaturelistId, "")) {
    bodyFrame <- append(list(monotonicDecreasingFeaturelistId = NULL),
                        bodyFrame[setdiff(names(bodyFrame), "monotonicDecreasingFeaturelistId")])
  }
  body <- if (length(bodyFrame) > 1) { lapply(bodyFrame, jsonlite::unbox) }
          else { list(blueprintId = bodyFrame$blueprintId) }
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                             returnRawResponse = TRUE, encode = "json")
  message("New model request received")
  JobIdFromResponse(rawReturn)
}

#' Train a new frozen model with parameters from specified model
#'
#' Frozen models use the same tuning parameters as their parent model
#' instead of independently optimizing them to allow efficiently
#' retraining models on larger amounts of the training data.
#'
#' Either `sample_pct` or `training_row_count` can be used to specify the amount of data to
#' use, but not both. If neither are specified, a default of the maximum amount of data that
#' can safely be used to train any blueprint without going into the validation data will be
#' selected.

#' In smart-sampled projects, `samplePct` and `trainingRowCount` are assumed to be in terms of rows
#' of the minority class.
#'
#' Note : For datetime partitioned projects, use ``RequestFrozenDatetimeModel` instead
#'
#' @inheritParams DeleteModel
#' @param samplePct Numeric, specifying the percentage of the training
#' dataset to be used in building the new model
#' @return An integer value that can be used as the modelJobId parameter
#' in subsequent calls to the GetModelFromJobId function.
#' @param trainingRowCount integer. The number of rows to use to train
#'   the requested model.
#' @return An integer value that can be used as the modelJobId parameter
#'   in subsequent calls to the GetModelFromJobId function.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   RequestFrozenModel(model, samplePct = 10)
#' }
#' @export
RequestFrozenModel <- function(model, samplePct = NULL, trainingRowCount = NULL) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "frozenModels")
  body <- list(modelId = modelId)
  if (!is.null(samplePct)) {
    body$samplePct <- samplePct
  }
  if (!is.null(trainingRowCount)) {
    body$trainingRowCount <- trainingRowCount
  }
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                               returnRawResponse = TRUE)
  message("Frozen model request received")
  JobIdFromResponse(rawReturn)
}


#' Delete a specified DataRobot model
#'
#' This function removes the model specified by the parameter model from its
#' associated project.
#'
#' @param model An S3 object of class dataRobotModel like that returned by
#'   the function GetModel, or each element of the list returned by
#'   the function ListModels.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   DeleteModel(model)
#' }
#' @export
DeleteModel <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId)
  DataRobotDELETE(routeString, addUrl = TRUE)
  modelName <- validModel$modelType
  message(paste("Model", modelName,
                "(modelId = ", modelId, ") deleted from project", projectId))
  invisible(NULL)
}

#' Validate that model belongs to class 'dataRobotModel' and includes
#' projectId and modelId.
#'
#' @param model An S3 object of class dataRobotModel like that returned by
#'   the function GetModel, or each element of the list returned by
#'   the function ListModels.
ValidateModel <- function(model) {
  errorMessage <- "Invalid model specification"
  if (!(is(model, "dataRobotModel") | is(model, "dataRobotFrozenModel") |
        is(model, "dataRobotDatetimeModel") | is(model, "dataRobotPrimeModel"))) {
    stop(errorMessage)
  } else {
    projectId <- model$projectId
    modelId <- model$modelId
    if (!is.null(projectId) & !is.null(modelId)) {
      model
    } else {
      stop(errorMessage, call. = FALSE)
    }
  }
}

as.dataRobotModelObject <- function(inList) {
  if ("id" %in% names(inList) && !("modelId" %in% names(inList))) {
    inList$modelId <- inList$id
  }
  elements <- c("featurelistId",
                "processes",
                "featurelistName",
                "projectId",
                "samplePct",
                "trainingRowCount",
                "isFrozen",
                "modelType",
                "metrics",
                "modelCategory",
                "blueprintId",
                "modelId",
                "projectName",
                "projectTarget",
                "projectMetric",
                "supportsMonotonicConstraints",
                "monotonicIncreasingFeaturelistId",
                "monotonicDecreasingFeaturelistId")
  outList <- ApplySchema(inList, elements)
  class(outList) <- "dataRobotModel"
  outList
}

as.dataRobotFrozenModelObject <- function(inList) {
  elements <- c("featurelistId",
                "processes",
                "featurelistName",
                "projectId",
                "samplePct",
                "trainingRowCount",
                "isFrozen",
                "parentModelId",
                "modelType",
                "metrics",
                "modelCategory",
                "blueprintId",
                "modelId",
                "projectName",
                "projectTarget",
                "projectMetric",
                "supportsMonotonicConstraints",
                "monotonicIncreasingFeaturelistId",
                "monotonicDecreasingFeaturelistId")
  outList <- ApplySchema(inList, elements)
  class(outList) <- "dataRobotFrozenModel"
  outList
}


#' Run cross validation on a model.
#'
#' Note that this runs cross validation on a model as-is. If you would like to run cross-validation
#' on a model with new parameters, use \code{RequestNewModel} instead.
#'
#' Note that this is not implemented for prime models or datetime models.
#'
#' @inheritParams DeleteModel
#' @return Job ID of the cross validation job.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   CrossValidateModel(model)
#' }
#' @export
CrossValidateModel <- function(model) {
  validModel <- ValidateModel(model)
  if (inherits(validModel, "dataRobotPrimeModel")) {
    stop("CrossValidateModel is not implemented for prime models.")
  }
  if (inherits(validModel, "dataRobotDatetimeModel")) {
    stop("CrossValidateModel is not implemented for datetime models.")
  }
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "crossValidation")
  response <- DataRobotPOST(routeString, addUrl = TRUE, returnRawResponse = TRUE)
  message("Cross validation request received")
  JobIdFromResponse(response)
}


#' Retrieve model parameters
#'
#' @inheritParams GetModel
#' @return List with the following components:
#' \itemize{
#'   \item parameters. List of model parameters that are related to the whole model with following
#'     components: name, value.
#'   \item derivedFeatures. List containing preprocessing information about derived features with
#'     following components: originalFeature, derivedFeature, type, coefficient, transformations
#'     and stageCoefficients. `transformations` is a list itself with components: name and value.
#'     `stageCoefficients` is also a list with components: stage and coefficient. It contains
#'     coefficients for each stage of multistage models and is empty list for single stage models.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetModelParameters(projectId, modelId)
#' }
#' @export
GetModelParameters <- function(project, modelId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "models", modelId, "parameters")
  params <- DataRobotGET(routeString, addUrl = TRUE,
                            simplifyDataFrame = FALSE)
  as.dataRobotModelParameters(params)
}

as.dataRobotModelParameters <- function(inList) {
  elements <- c("parameters",
                "derivedFeatures")
  outList <- ApplySchema(inList, elements)
  outList$derivedFeatures <- lapply(outList$derivedFeatures,
                                    as.dataRobotModelParametersDerivedFeatures)
  outList$parameters <- lapply(outList$parameters, as.dataRobotNameValueSchema)
  outList
}

as.dataRobotModelParametersDerivedFeatures <- function(inList) {
  elements <- c("coefficient",
                "type",
                "stageCoefficients",
                "derivedFeature",
                "originalFeature",
                "transformations")
  outList <- ApplySchema(inList, elements)
  if (is.null(outList$stageCoefficients)) {
    outList$stageCoefficients <- list()
  }
  outList$transformations <- lapply(outList$transformations, as.dataRobotNameValueSchema)
  outList$stageCoefficients <- lapply(outList$stageCoefficients, as.dataRobotStageCoefficient)
  outList
}

as.dataRobotStageCoefficient <- function(inList) {
  elements <- c("stage",
                "coefficient")
  ApplySchema(inList, elements)
}


as.dataRobotNameValueSchema <- function(inList) {
  elements <- c("name",
                "value")
  ApplySchema(inList, elements)
}


as.dataRobotDatetimeModelObject <- function(inList) {
  elements <- c("modelId",
                "projectId",
                "processes",
                "featurelistId",
                "featurelistName",
                "samplePct",
                "trainingRowCount",
                "isFrozen",
                "modelType",
                "metrics",
                "modelCategory",
                "blueprintId",
                "projectName",
                "projectTarget",
                "projectMetric",
                "trainingRowCount",
                "trainingDuration",
                "trainingStartDate",
                "trainingEndDate",
                "backtests",
                "dataSelectionMethod",
                "trainingInfo",
                "holdoutScore",
                "holdoutStatus")
  outList <- ApplySchema(inList, elements)
  class(outList) <- "dataRobotDatetimeModel"
  outList
}

#' Retrieve the details of a specified datetime model.
#'
#' This function returns a DataRobot S3 object of class
#' dataRobotDatetimeModel for the model defined by project and modelId.
#'
#' If the project does not use datetime partitioning an error will occur.
#'
#' @inheritParams DeleteProject
#' @param modelId character. Unique alphanumeric identifier for the model of interest.
#' @return An S3 object of class `dataRobotDatetimeModel', which is a list
#' with the following components:
#' \itemize{
#'   \item featurelistId. Character string: unique alphanumeric identifier for the featurelist on
#'     which the model is based.
#'   \item processes. Character vector with components describing preprocessing; may include
#'     modelType.
#'   \item featurelistName. Character string giving the name of the featurelist on which the model
#'     is based.
#'   \item projectId. Character string giving the unique alphanumeric identifier for the project.
#'   \item samplePct. Numeric: percentage of the dataset used to form the training dataset for
#'     model fitting.
#'   \item isFrozen. Logical : is model created with frozen tuning parameters.
#'   \item modelType. Character string describing the model type.
#'   \item metrics. List with one element for each valid metric associated with the model.
#'     Each element is a list with elements for each possible evaluation type (holdout, validation,
#'     and crossValidation).
#'   \item modelCategory. Character string giving model category (e.g., blend, model).
#'   \item blueprintId. Character string giving the unique DataRobot blueprint identifier on which
#'     the model is based.
#'   \item modelId. Character string giving the unique alphanumeric model identifier.
#'   \item projectName. Character string: optional description of project defined by projectId.
#'   \item projectTarget. Character string defining the target variable predicted by all models in
#'     the project.
#'   \item projectMetric. Character string defining the fitting metric optimized by all project
#'     models.
#'   \item trainingRowCount. Integer. The number of rows of the project dataset used in training
#'     the model. In a datetime partitioned project, if specified, defines the number of
#'      rows used to train the model and evaluate backtest scores; if unspecified, either
#'      \code{trainingDuration} or \code{trainingStartDate} and \code{trainingEndDate} was used to
#'      determine that instead.
#'   \item trainingDuration. Character string or none only present for models in datetime
#'     partitioned projects. If specified, a duration string specifying the duration spanned by the
#'     data used to train the model and evaluate backtest scores.
#'   \item trainingStartDate. Charcter string or none only present for frozen models in datetime
#'     partitioned projects. If specified, the start date of the data used to train the model.
#'   \item trainingEndDate. Charcter string or none only present for frozen models in datetime
#'     partitioned projects. If specified, the end date of the data used to train the model.
#'   \item backtests. list describes what data was used to fit each backtest, the score for the
#'     project metric, and why the backtest score is unavailable if it is not provided.
#'   \item dataSelectionMethod. Character string which of trainingRowCount, trainingDuration,
#'     or trainingStartDate and trainingEndDate were used to determine the data used to fit the
#'     model. One of 'rowCount', 'duration', or 'selectedDateRange'.
#'   \item trainingInfo. list describes which data was used to train on when scoring the holdout and
#'     making predictions.
#'   trainingInfo will have the following keys: `holdoutTrainingStartDate`,
#'     `holdoutTrainingDuration`, `holdoutTrainingRowCount`, `holdoutTrainingEndDate`,
#'     `predictionTrainingStartDate`, `predictionTrainingDuration`,
#'     `predictionTrainingRowCount`, `predictionTrainingEndDate`.  Start and end dates will be
#'     datetime string, durations will be duration strings, and rows will be integers.
#'   \item holdoutScore. numeric or none the score against the holdout, if available and the holdout
#'     is unlocked, according to the project metric.
#'   \item holdoutStatus. Character string the status of the holdout score, e.g. "COMPLETED",
#'     "HOLDOUT_BOUNDARIES_EXCEEDED".
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetDatetimeModelObject(projectId, modelId)
#' }
#' @export
GetDatetimeModelObject <- function(project, modelId) {
  #  Fail if modelId is an empty string
  if (modelId == "") {
    stop("modelId must not be blank")
  } else {
    projectId <- ValidateProject(project)
    fullProject <- GetProject(projectId)
    routeString <- UrlJoin("projects", projectId, "datetimeModels", modelId)
    modelDetails <- DataRobotGET(routeString, addUrl = TRUE)
    #
    #  Request successful - extract data from $content element of
    #  Reformat results: (1) change name "id" to "modelId";
    #  (2) reformat $metrics list component to replace NULL
    #  representation of missing values with NA
    #
    #  Also, add projectName, projectTarget, and projectMetric
    #
    #  NOTE: if the $processes list is empty, it is represented
    #        as an empty list rather than an empty character vector,
    #        while ListModels returns an empty character vector
    #        for this case; for compatability, check
    #        for this case and reformat if detected
    #
    names(modelDetails)[names(modelDetails) == "id"] <- "modelId"
    modelDetails$metrics <- ReformatMetrics(modelDetails$metrics)
    modelDetails$projectName <- fullProject$projectName
    modelDetails$projectTarget <- fullProject$target
    modelDetails$projectMetric <- fullProject$metric
    if (length(modelDetails$processes) == 0) {
      modelDetails$processes <- character(0)
    }
    modelDetails <- as.dataRobotDatetimeModelObject(modelDetails)
    class(modelDetails) <- "dataRobotDatetimeModel"
    modelDetails
  }
}

#' Retrieve a new or updated datetime model defined by modelJobId
#'
#' The functions RequestNewDatatimeModel and RequestFrozenDatetimeModel
#' initiate the creation of new models in a DataRobot project.
#' Both functions submit requests to the DataRobot modeling
#' engine and return an integer-valued modelJobId.  The
#' GetDatetimeModelFromJobId function polls the modeling engine until
#' the model has been built or a specified time limit is exceeded,
#' returning an S3 object of class 'dataRobotDatetimeModel' when the model
#' is available.
#'
#' Motivation for this function is the fact that some models -
#' e.g., very complex machine learning models fit to large datasets -
#' may take a long time to complete.  Splitting the model creation
#' request from model retrieval in these cases allows the user to
#' perform other interactive R session tasks between the time the
#' model creation/update request is made and the time the final
#' model is available.
#'
#' @inheritParams DeleteProject
#' @param modelJobId The integer returned by either RequestNewDatetimeModel
#' @param maxWait Integer, The maximum time (in seconds) to wait for the model job to complete
#' @return An S3 object of class 'dataRobotDatetimeModel' summarizing all
#' available information about the model. See GetDatetimeModelObject
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   initialJobs <- ListModelJobs(project)
#'   job <- initialJobs[[1]]
#'   modelJobId <- job$modelJobId
#'   GetDatetimeModelFromJobId(projectId, modelJobId)
#' }
#' @export
GetDatetimeModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                     failureStatuses = JobFailureStatuses)
  modelId <- modelDetails$id
  returnModel <- GetDatetimeModelObject(projectId, modelId)
  message("Model ", modelId, " retrieved")
  class(returnModel) <- "dataRobotDatetimeModel"
  returnModel
}

#' Adds a new datetime model of the type specified by the blueprint to a DataRobot project
#'
#' This function requests the creation of a new datetime model in the DataRobot
#' modeling project defined by the project parameter.  The function also
#' allows the user to specify alternatives to the project default for
#' featurelist, samplePct, and scoringType.  This function returns an
#' integer modelJobId value, which can be used by the GetDatetimeModelFromJobId
#' function to return the full model object.
#'
#' Motivation for this function is the fact that some models - e.g., very
#' complex machine learning models fit to large datasets - may take a long
#' time to complete.  Splitting the model creation request from model
#' retrieval in these cases allows the user to perform other interactive R
#' session tasks between the time the model creation/update request is made
#' and the time the final model is available.
#'
#' @inheritParams DeleteProject
#' @param blueprint list. A list with at least the following two elements:
#'   blueprintId and projectId.  Note that the individual elements of the
#'   list returned by ListBlueprints are admissible values for this parameter.
#' @param featurelist list. A list that contains the element featurelistId that
#'   specifies the featurelist to be used in building the model; if not
#'   specified (i.e., for the default value NULL), the project default
#'   (Informative Features) is used.
#' @param trainingRowCount integer. Optional, the number of rows of data
#'   that should be used to train the model. If specified, trainingDuration may not be specified.
#' @param trainingDuration character. String (optional) a duration string specifying what
#'   time range the data used to train the model should span.
#'   If specified, trainingRowCount may not be specified.
#' @param timeWindowSamplePct integer. Optional. May only be specified when the requested model
#'   is a time window (e.g. duration or start and end dates).
#'   An integer between 1 and 99 indicating the percentage to sample by within the window.
#'   The points kept are determined by a random uniform sample.
#' @return An integer value that can be used as the modelJobId parameter
#'   in subsequent calls to the GetDatetimeModelFromJobId function.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   blueprints <- ListBlueprints(projectId)
#'   blueprint <- blueprints[[1]]
#'   RequestNewDatetimeModel(projectId, blueprint)
#' }
#' @export
RequestNewDatetimeModel <- function(project, blueprint, featurelist = NULL,
                                    trainingRowCount = NULL, trainingDuration = NULL,
                                    timeWindowSamplePct = NULL) {
  #
  #########################################################################
  #
  #  Sets up the creation of a new model in project, based on blueprint,
  #  both required parameters; additional optional parameters are
  #  featurelist, samplePct, and scoringType.  Note that blueprint is a
  #  list, which must contain the elements blueprintId and projectId.
  #  This function returns the integer-valued modelJobId, which can be
  #  used with the function GetModelFromJobId to retrieve the model once
  #  it has been built.
  #
  #########################################################################
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "datetimeModels")
  #
  #  Construct the body for the POST command
  #
  #  NOTE: if more than one parameter is to be passed in the body of the POST
  #        command, they are passed as an unboxed dataframe with
  #        encode = "json"; if only the required parameter blueprintID is
  #        to be passed, it is passed as a list without encode = "json"
  #
  #  Check whether blueprint$projectId differs from projectId.
  #  If so, need to specify sourceProjectId as value
  #  from blueprint list; since this is not usually the case,
  #  pre-specify secondProject = FALSE # nolint
  #
  blueprintId <- blueprint$blueprintId
  bodyFrame <- list(blueprintId = blueprintId)
  if (!identical(blueprint$projectId, projectId)) {
    bodyFrame$sourceProjectId <- blueprint$projectId
  }
  if (!is.null(featurelist)) {
    bodyFrame$featurelistId <- featurelist$featurelistId
  }
  if (!is.null(trainingRowCount)) {
    bodyFrame$trainingRowCount  <- trainingRowCount
  }
  if (!is.null(trainingDuration)) {
    bodyFrame$trainingDuration <- trainingDuration
  }
  if (!is.null(timeWindowSamplePct)) {
    bodyFrame$timeWindowSamplePct <- timeWindowSamplePct
  }
  if (length(bodyFrame) > 1) {
    body <- jsonlite::unbox(as.data.frame(bodyFrame))
    rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                               returnRawResponse = TRUE, encode = "json")
  } else {
    body <- list(blueprintId = bodyFrame$blueprintId)
    rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                               returnRawResponse = TRUE)
  }
  message("New datetime model request received")
  JobIdFromResponse(rawReturn)
}

#' Train a new frozen datetime model with parameters from the specified model
#'
#'  Requires that this model belongs to a datetime partitioned project.
#'  If it does not, an error will occur when submitting the job
#'
#' Frozen models use the same tuning parameters as their parent model
#' instead of independently optimizing them to allow efficiently
#' retraining models on larger amounts of the training data.
#'
#' In addition to trainingRowCount and trainingDuration, frozen datetime models
#' may be trained on an exact date range.  Only one of trainingRowCount,
#' trainingDuration, or trainingStartDate and trainingEndDate should be specified.
#' Models specified using trainingStartDate and trainingEndDate are the only ones
#' that can be trained into the holdout data (once the holdout is unlocked).
#'
#' @inheritParams DeleteModel
#' @param trainingRowCount integer. (optional) the number of rows of data that
#'   should be used to train the model.
#' @param trainingDuration character. string (optional) a duration string specifying what
#'   time range the data used to train the model should span.
#' @param trainingStartDate character. string(optional) the start date of the data to train to model
#'   on ("%Y-%m-%dT%H:%M:%SZ" RFC 3339 format). Only rows occurring at or after this datetime will
#'   be used.
#' @param trainingEndDate character. string(optional) the end date of the data to train the model on
#'   ("%Y-%m-%dT%H:%M:%SZ" RFC 3339 format). Only rows occurring strictly before this datetime
#'   will be used.
#' @param timeWindowSamplePct integer. (optional) May only be specified when the requested model
#'   is a time window (e.g. duration or start and end dates). An integer between 1 and 99
#'   indicating the percentage to sample by within the window. The points kept are determined by
#"   a random uniform sample.
#' @return An integer value that can be used as the modelJobId parameter
#'   in subsequent calls to the GetDatetimeModelFromJobId function.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetDatetimeModelObject(modelId)
#'   RequestFrozenDatetimeModel(model)
#' }
#' @export
RequestFrozenDatetimeModel <- function(model, trainingRowCount = NULL,
                                       trainingDuration = NULL, trainingStartDate = NULL,
                                       trainingEndDate = NULL, timeWindowSamplePct = NULL) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "frozenDatetimeModels")
  body <- list(modelId = modelId, trainingRowCount = trainingRowCount,
               trainingDuration = trainingDuration,
               trainingStartDate = trainingStartDate,
               trainingEndDate = trainingEndDate,
               timeWindowSamplePct = timeWindowSamplePct)
  body <- Filter(Negate(is.null), body) # Drop NULL parameters from request
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                             returnRawResponse = TRUE)
  message("Frozen datetime model request received")
  JobIdFromResponse(rawReturn)
}



#' Compute the scores for all available backtests.
#'
#' Some backtests may be unavailable if the model is trained into their validation data.
#'
#' @inheritParams DeleteModel
#' @return Integer a job tracking the backtest computation.
#' When it is complete, all available backtests will have scores computed.
#' Function WaitForJobToComplete can be used to wait for the job completion
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   ScoreBacktests(model)
#' }
#' @export
ScoreBacktests <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "datetimeModels", modelId, "backtests")
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, returnRawResponse = TRUE)
  message("Backtest score request received")
  JobIdFromResponse(rawReturn)
}

#' Retrieve word cloud data for a model.
#'
#' @inheritParams DeleteProject
#' @param modelId Character string: unique alphanumeric identifier for the model of interest.
#' @param excludeStopWords Logical (optional) : Set to True if you want stopwords filtered out the
#'   response.
#' @return data.frame with the following components:
#' \describe{
#'   \item{ngram}{Character string: word or ngram value}
#'   \item{coefficient}{Numerical:  value from [-1.0, 1.0] range, describes effect of this ngram on
#'   the target. A large negative value means a strong effect toward the negative class in
#'   classification projects and a smaller predicted target value in regression projects.
#'   A large positive value means a strong effect toward the positive class and a larger
#'   predicted target value respectively}
#'   \item{count}{Integer: number of rows in the training sample where this ngram appears}
#'   \item{frequency}{Numerical: value from (0.0, 1.0] range, frequency of this ngram
#'   relative to the most frequent ngram}
#'   \item{isStopword}{Logical: true for ngrams that DataRobot evaluates as stopwords}
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetWordCloud(projectId, modelId)
#' }
#' @export
GetWordCloud <- function(project, modelId, excludeStopWords = FALSE) {
    projectId <- ValidateProject(project)
    routeString <- UrlJoin("projects", projectId, "models", modelId, "wordCloud")
    responseData <- DataRobotGET(routeString, addUrl = TRUE,
                                 query = list("excludeStopWords" =
                                                tolower(as.character(excludeStopWords))))
    as.dataRobotWordCloud(responseData$ngrams)
}

as.dataRobotWordCloud <- function(inList) {
  elements <- c("ngram",
                "coefficient",
                "count",
                "frequency",
                "isStopword")
  ApplySchema(inList, elements)
}

#' Download scoring code JAR
#'
#' @inheritParams DeleteProject
#' @param modelId Character string: unique alphanumeric identifier for the model of interest.
#' @param fileName Character string: File path where scoring code will be saved.
#' @param sourceCode Logical (optional) : Set to True to download source code archive.
#' It will not be executable.
#' @return NULL
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   file <- file.path(tempdir(), "scoringCode.jar")
#'   DownloadScoringCode(projectId, modelId, file)
#' }
#' @export
DownloadScoringCode <- function(project, modelId, fileName, sourceCode = FALSE) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "models", modelId, "scoringCode")
  response <- DataRobotGET(routeString, addUrl = TRUE, as = "raw",
                           query = list("sourceCode" = tolower(as.character(sourceCode))))
  writeBin(response, fileName)
  invisible(NULL)
}
