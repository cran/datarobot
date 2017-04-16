#' Retrieve the details of a specified model
#'
#' This function returns a DataRobot S3 object of class
#' dataRobotModel for the model defined by project and modelId.
#'
#' The S3 object returned by this function is required by the
#' functions DeleteModel, ListModelFeatures, and RequestSampleSizeUpdate.
#'
#' @inheritParams DeleteProject
#' @param modelId Unique alphanumeric identifier for the model of interest.
#' @return An S3 object of class `dataRobotModel', which is a list
#' with the following components:
#' \describe{
#'   \item{featurelistId}{Character string: unique alphanumeric identifier for the featurelist on which the model is based}
#'   \item{processes}{Character vector with components describing preprocessing; may include modelType}
#'   \item{featurelistName}{Character string giving the name of the featurelist on which the model is based}
#'   \item{projectId}{Character string giving the unique alphanumeric identifier for the project}
#'   \item{samplePct}{Numeric: percentage of the dataset used to form the training dataset for model fitting}
#'   \item{isFrozen}{Logical : is model created with frozen tuning parameters}
#'   \item{modelType}{Character string describing the model type}
#'   \item{metrics}{List with one element for each valid metric associated with the model. Each element is a list with elements for each possible evaluation type (holdout, validation, and crossValidation)}
#'   \item{modelCategory}{Character string giving model category (e.g., blend, model)}
#'   \item{blueprintId}{Character string giving the unique DataRobot blueprint identifier on which the model is based}
#'   \item{modelId}{Character string giving the unique alphanumeric model identifier}
#'   \item{projectName}{Character string: optional description of project defined by projectId}
#'   \item{projectTarget}{Character string defining the target variable predicted by all models in the project}
#'   \item{projectMetric}{Character string defining the fitting metric optimized by all project models}
#' }
#' @export
#'
GetModelObject <- function(project, modelId) {
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
    #        while GetAllModels returns an empty character vector
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
    modelDetails <- as.dataRobotModelObject(modelDetails)
    return(modelDetails)
  }
}

#' Retrieve the details of a specified frozen model
#'
#' This function returns a DataRobot S3 object of class
#' dataRobotFrozenModel for the model defined by project and modelId.
#' GetModelObject also can be used to retrieve some information about 
#' frozen model, however then some frozen specific information (parentModelId)
#' will not be returned
#'
#' The S3 object returned by this function is required by the
#' functions DeleteModel, ListModelFeatures, and RequestSampleSizeUpdate.
#'
#' @inheritParams DeleteProject
#' @param modelId Unique alphanumeric identifier for the model of interest.
#' @return An S3 object of class `dataRobotModel', which is a list
#' with the following 14 components:
#' \describe{
#'   \item{featurelistId}{Character string: unique alphanumeric identifier for the featurelist on which the model is based}
#'   \item{processes}{Character vector with components describing preprocessing; may include modelType}
#'   \item{featurelistName}{Character string giving the name of the featurelist on which the model is based}
#'   \item{projectId}{Character string giving the unique alphanumeric identifier for the project}
#'   \item{samplePct}{Numeric: percentage of the dataset used to form the training dataset for model fitting}
#'   \item{isFrozen}{Logical : is model created with frozen tuning parameters}
#'   \item{parentModelId}{Character string giving the unique alphanumeric parent model identifier}
#'   \item{modelType}{Character string describing the model type}
#'   \item{metrics}{List with one element for each valid metric associated with the model. Each element is a list with elements for each possible evaluation type (holdout, validation, and crossValidation)}
#'   \item{modelCategory}{Character string giving model category (e.g., blend, model)}
#'   \item{blueprintId}{Character string giving the unique DataRobot blueprint identifier on which the model is based}
#'   \item{modelId}{Character string giving the unique alphanumeric model identifier}
#'   \item{projectName}{Character string: optional description of project defined by projectId}
#'   \item{projectTarget}{Character string defining the target variable predicted by all models in the project}
#'   \item{projectMetric}{Character string defining the fitting metric optimized by all project models}
#' }
#' @export
#'
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
    #        while GetAllModels returns an empty character vector
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
    modelDetails <- as.dataRobotFrozenModelObject(modelDetails)
    return(modelDetails)
  }
}


#' Retrieve all available model information for a DataRobot project
#'
#' This function requests the model information for the DataRobot
#' project specified by the project argument, described under Arguments.
#' This parameter may be obtained in several ways, including: (1), from
#' the projectId element of the list returned by GetProjectList; (2), as
#' the object returned by the GetProject function; or (3), as the list
#' returned by the SetupProject function. The function returns an S3
#' object of class 'listOfModels'.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class listOfModels, which may be characterized
#' using R's generic summary function or converted to a dataframe with
#' the as.data.frame method.
#' @export
#'
GetAllModels <- function(project) {
  projectId <- ValidateProject(project)
  fullProject <- GetProject(projectId)
  projectDetails <- list(projectName = fullProject$projectName,
                         projectTarget = fullProject$target,
                         projectMetric = fullProject$metric)
  routeString <- UrlJoin("projects", projectId, "models")
  modelInfo <- DataRobotGET(routeString, addUrl = TRUE)
  if (length(modelInfo) == 0) {
    message("No model information available for this project. \n
             \nThis usually means the Autopilot has not yet started building
             models.")
    returnList <- list()
  } else {
    returnList <- ReformatListOfModels(modelInfo, projectDetails)
  }
  returnList <- lapply(returnList, as.dataRobotModelObject)
  currentModelJobs <- GetModelJobs(projectId)
  if (nrow(currentModelJobs) > 0){
    message("Some models are still in progress")
  }
  class(returnList) <- c('listOfModels', 'listSubclass')
  return(returnList)
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
#' @export
#'
GetModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                     failureStatuses = JobFailureStatuses)
  modelId <- modelDetails$id
  returnModel <- GetModelObject(projectId, modelId)
  message("Model ", modelId, " retrieved")
  class(returnModel) <- 'dataRobotModel'
  return(returnModel)
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
#' @param modelJobId The integer returned by either RequestNewModel
#' or RequestSampleSizeUpdate.
#' @param maxWait Integer, The maximum time (in seconds) to wait for the model job to complete
#' @return An S3 object of class 'dataRobotFrozenModel' summarizing all
#' available information about the model.
#' @export
#'
GetFrozenModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                     failureStatuses = JobFailureStatuses)
  modelId <- modelDetails$id
  returnModel <- GetFrozenModel(projectId, modelId)
  message("Model ", modelId, " retrieved")
  class(returnModel) <- 'dataRobotFrozenModel'
  return(returnModel)
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
#' Note : For datetime partitioned projects, use ``RequestNewDatetimeModel` instead
#'
#' @inheritParams DeleteProject
#' @param blueprint A list with at least the following two elements:
#' blueprintId and projectId.  Note that the individual elements of the
#' list returned by GetRecommendedBlueprints are admissible values for
#' this parameter.
#' @param featurelist A list that contains the element featurelistId that
#' specifies the featurelist to be used in building the model; if not
#' specified (i.e., for the default value NULL), the project default
#' (Informative Features) is used.
#' @param samplePct Numeric, specifying the percentage of the training
#' dataset to be used in building the new model; if not specified
#' (i.e., for the default value NULL), the maxTrainPct value for the
#' project is used.
#' @param scoringType Character string specifying the scoring type;
#' default is validation set scoring, but cross-validation averaging
#' is also possible.
#' @return An integer value that can be used as the modelJobId parameter
#' in subsequent calls to the GetModelFromJobId function.
#' @export
#'
RequestNewModel <- function(project, blueprint, featurelist = NULL,
                            samplePct = NULL, scoringType = NULL) {
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

  bodyFrame <- data.frame(blueprintId = blueprintId)
  if (blueprint$projectId != projectId) {
    bodyFrame$sourceProjectId <- blueprint$projectId
  }
  if (!is.null(featurelist)) {
    bodyFrame$featurelistId <- featurelist$featurelistId
  }
  if (!is.null(samplePct)) {
    bodyFrame$samplePct <- samplePct
  }
  if (!is.null(scoringType)) {
    bodyFrame$scoringType <- scoringType
  }
  if (ncol(bodyFrame) > 1) {
    body <- jsonlite::unbox(bodyFrame)
    rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                               returnRawResponse = TRUE, encode = "json")
  } else {
    body <- list(blueprintId = bodyFrame$blueprintId)
    rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                               returnRawResponse = TRUE)
  }
  message("New model request received")
  return(JobIdFromResponse(rawReturn))
}

#' Train a new frozen model with parameters from specified model
#'
#' Frozen models use the same tuning parameters as their parent model 
#' instead of independently optimizing them to allow efficiently 
#' retraining models on larger amounts of the training data.
#' 
#' Note : For datetime partitioned projects, use ``RequestFrozenDatetimeModel` instead
#'
#' @inheritParams DeleteModel
#' @param samplePct Numeric, specifying the percentage of the training
#' dataset to be used in building the new model
#' @return An integer value that can be used as the modelJobId parameter
#' in subsequent calls to the GetModelFromJobId function.
#' @export
#'
RequestFrozenModel <- function(model, samplePct) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "frozenModels")
  body <- list(modelId = modelId, samplePct = samplePct)
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                               returnRawResponse = TRUE)
  message("Frozen model request received")
  return(JobIdFromResponse(rawReturn))
}


#' Delete a specified DataRobot model
#'
#' This function removes the model specified by the parameter model from its
#' associated project.
#'
#' @param model An S3 object of class dataRobotModel like that returned by
#' the function GetModelObject, or each element of the list returned by
#' the function GetAllModels.
#' @export
#'
DeleteModel <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId)
  response <- DataRobotDELETE(routeString, addUrl = TRUE)
  modelName <- validModel$modelType
  message(paste("Model", modelName,
                "(modelId = ", modelId, ") deleted from project", projectId))
}

#
#  ValidateModel.R - function to validate that model belongs to class 'dataRobotModel' and includes projectId and modelId
#

ValidateModel <- function(model) {
  errorMessage <- "Invalid model specification"
  if (!(is(model, 'dataRobotModel') | is(model, 'dataRobotFrozenModel') |
        is(model, 'dataRobotDatetimeModel'))) {
    stop(errorMessage)
  } else {
    projectId <- model$projectId
    modelId <- model$modelId
    if (!is.null(projectId) & !is.null(modelId)) {
      return(model)
    } else {
      stop(errorMessage, call. = FALSE)
    }
  }
}


#
#  ReformatListOfModels.R - function to convert the list-of-lists output
#  from the server to a list-of-modelObjects format
#
  
ReformatListOfModels <- function(listOfLists, projectDetails) {
  #
  ###########################################################################
  #
  #  Reformat the input list-of-lists object from the DataRobot Public API
  #  server summarizing all project models into a list-of-S3-objects format,
  #  where each element is an object of class 'dataRobotModel'
  #
  ###########################################################################
  #
  #  Note: the format of the listOfLists returned by the Public API server in
  #        response to the GetAllModels request is complicated.  As of
  #        8/24/2015, this list has 10 elements: 8 of these 10 elements are
  #        simple vectors of numbers or characters; the element $processes is
  #        a list of character vectors, and the element $metrics is a
  #        dataframe with one row for each model and a variable number of
  #        columns
  #
  #  Also, the name of the "modelId" element of the list is returned as "id"
  #  - correct this for the S3 objects to be returned by this function
  #
  allNames <- names(listOfLists)
  idIndex <- which(allNames == "id")
  newNames <- allNames
  newNames[idIndex] <- "modelId"
  #
  #  Note that a simple nested loop structure works for all elements of
  #  listOfLists EXCEPT $metrics.  This element can be extracted as a
  #  row of a dataframe; thus, detect $metric and treat it specially
  #
  nModels <- length(listOfLists[[1]])
  outObject <- vector("list", nModels)
  mElements <- length(newNames)
  metricIndex <- which(newNames == "metrics")
  for (i in 1:nModels) {
    element <- vector("list", mElements)
    for (j in 1:mElements) {
      if (j != metricIndex) {
        element[[j]] <- listOfLists[[j]][[i]]
      } else {
        metricsList <- as.list(listOfLists[[j]][i, ])
        for (k in 1:length(metricsList)) {
          xFrame <- metricsList[[k]]
          rownames(xFrame) <- NULL
          for (m in 1:ncol(xFrame)) {
            xFrame[, m] <- as.numeric(xFrame[, m])
          }
          metricsList[[k]] <- xFrame
        }
        element[[j]] <- metricsList
      }
    }
    names(element) <- newNames
    element <- append(element, projectDetails)
    class(element) <- 'dataRobotModel'
    outObject[[i]] <- element
  }
  class(outObject) <- c('listOfModels', 'listSubclass')
  return(outObject)
}


as.dataRobotModelObject <- function(inList){
  elements <- c("featurelistId",
                "processes",
                "featurelistName",
                "projectId",
                "samplePct",
                "isFrozen",
                "modelType",
                "metrics",
                "modelCategory",
                "blueprintId",
                "modelId",
                "projectName",
                "projectTarget",
                "projectMetric")
  outList <- ApplySchema(inList, elements)
  class(outList) <- 'dataRobotModel'
  return(outList)
}

as.dataRobotFrozenModelObject <- function(inList){
  elements <- c("featurelistId",
                "processes",
                "featurelistName",
                "projectId",
                "samplePct",
                "isFrozen",
                "parentModelId",
                "modelType",
                "metrics",
                "modelCategory",
                "blueprintId",
                "modelId",
                "projectName",
                "projectTarget",
                "projectMetric")
  outList <- ApplySchema(inList, elements)
  class(outList) <- 'dataRobotFrozenModel'
  return(outList)
}


#' Retrieve model parameters
#'
#' @inheritParams GetModelObject
#' @return List with the following components:
#' \describe{
#'   \item{parameters}{List of model parameters that are related to the whole model with following components: name, value}
#'   \item{derivedFeatures}{List containing preprocessing information about derived features with following components: 
#'   originalFeature, derivedFeature, type, coefficient and list of applied transformations}
#' }
#' @export
#'
GetModelParameters <- function(project, modelId){
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "models", modelId, "parameters")
  params <- DataRobotGET(routeString, addUrl = TRUE,
                            simplifyDataFrame = FALSE)
  return(params)
}

as.dataRobotModelParameters <- function(inList){
  elements <- c("parameters",
                "derivedFeatures")
  outList <- ApplySchema(inList, elements)
  outList$derivedFeatures <- as.dataRobotModelParametersDerivedFeatures(outList$derivedFeatures)
  outList$parameters <- as.dataRobotModelParametersParameters(outList$parameters)
  return(outList)
}

as.dataRobotModelParametersDerivedFeatures <- function(inList){
  elements <- c("coefficient",
                "type",
                "derivedFeature",
                "originalFeature",
                "transformations")
  return(ApplySchema(inList, elements))
}

as.dataRobotModelParametersParameters <- function(inList){
  elements <- c("name",
                "value")
  return(ApplySchema(inList, elements))
}

#######################################################################################################################################

as.dataRobotDatetimeModelObject <- function(inList){
  elements <- c("modelId",
                "projectId",
                "processes",
                "featurelistId",
                "featurelistName",
                "samplePct",
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
  class(outList) <- 'dataRobotDatetimeModel'
  return(outList)
}

#' Retrieve the details of a specified datetime model
#'
#' This function returns a DataRobot S3 object of class
#' dataRobotDatetimeModel for the model defined by project and modelId.
#'
#' If the project does not use datetime partitioning an error will occur
#'
#' @inheritParams DeleteProject
#' @param modelId Unique alphanumeric identifier for the model of interest.
#' @return An S3 object of class `dataRobotDatetimeModel', which is a list
#' with the following components:
#' \describe{
#'   \item{featurelistId}{Character string: unique alphanumeric identifier for the featurelist on which the model is based}
#'   \item{processes}{Character vector with components describing preprocessing; may include modelType}
#'   \item{featurelistName}{Character string giving the name of the featurelist on which the model is based}
#'   \item{projectId}{Character string giving the unique alphanumeric identifier for the project}
#'   \item{samplePct}{Numeric: percentage of the dataset used to form the training dataset for model fitting}
#'   \item{isFrozen}{Logical : is model created with frozen tuning parameters}
#'   \item{modelType}{Character string describing the model type}
#'   \item{metrics}{List with one element for each valid metric associated with the model. 
#'   Each element is a list with elements for each possible evaluation type (holdout, validation, and crossValidation)}
#'   \item{modelCategory}{Character string giving model category (e.g., blend, model)}
#'   \item{blueprintId}{Character string giving the unique DataRobot blueprint identifier on which the model is based}
#'   \item{modelId}{Character string giving the unique alphanumeric model identifier}
#'   \item{projectName}{Character string: optional description of project defined by projectId}
#'   \item{projectTarget}{Character string defining the target variable predicted by all models in the project}
#'   \item{projectMetric}{Character string defining the fitting metric optimized by all project models}
#'   \item{trainingRowCount}{Integer or none only present for models in datetime partitioned projects.  
#'   If specified, defines the number of rows used to train the model and evaluate backtest scores}
#'   \item{trainingDuration}{Character string or none only present for models in datetime partitioned projects.  
#'   If specified, a duration string specifying the duration spanned by the data used to train the model and 
#'   evaluate backtest scores}
#'   \item{trainingStartDate}{Charcter string or none only present for frozen models in datetime partitioned projects.  
#'   If specified, the start date of the data used to train the model}
#'   \item{trainingEndDate}{Charcter string or none only present for frozen models in datetime partitioned projects.  
#'   If specified, the end date of the data used to train the model}
#'   \item{backtests}{list describes what data was used to fit each backtest, the score for the project metric, 
#'   and why the backtest score is unavailable if it is not provided}
#'   \item{dataSelectionMethod}{Character string which of trainingRowCount, trainingDuration, 
#'   or trainingStartDate and trainingEndDate were used to determine the data used to fit the model.  
#'   One of 'rowCount', 'duration', or 'selectedDateRange'}
#'   \item{trainingInfo}{list describes which data was used to train on when scoring the holdout and making predictions.
#'   trainingInfo will have the following keys: `holdoutTrainingStartDate`, `holdoutTrainingDuration`, 
#'   `holdoutTrainingRowCount`, `holdoutTrainingEndDate`, `predictionTrainingStartDate`, `predictionTrainingDuration`,
#'   `predictionTrainingRowCount`, `predictionTrainingEndDate`.  Start and end dates will be datetime string, durations will be 
#'   duration strings, and rows will be integers}
#'   \item{holdoutScore}{numeric or none the score against the holdout, 
#'   if available and the holdout is unlocked, according to the project metric}
#'   \item{holdoutStatus}{Character string the status of the holdout score, e.g. "COMPLETED", "HOLDOUT_BOUNDARIES_EXCEEDED"}
#' }
#' @export
#'
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
    #        while GetAllModels returns an empty character vector
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
    class(modelDetails) <- 'dataRobotDatetimeModel'
    return(modelDetails)
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
#' @export
#'
GetDatetimeModelFromJobId <- function(project, modelJobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelJobs", modelJobId)
  message("Model request issued: awaiting response")
  modelDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                     failureStatuses = JobFailureStatuses)
  modelId <- modelDetails$id
  returnModel <- GetDatetimeModelObject(projectId, modelId)
  message("Model ", modelId, " retrieved")
  class(returnModel) <- 'dataRobotDatetimeModel'
  return(returnModel)
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
#' @param blueprint A list with at least the following two elements:
#' blueprintId and projectId.  Note that the individual elements of the
#' list returned by GetRecommendedBlueprints are admissible values for
#' this parameter.
#' @param featurelist A list that contains the element featurelistId that
#' specifies the featurelist to be used in building the model; if not
#' specified (i.e., for the default value NULL), the project default
#' (Informative Features) is used.
#' @param trainingRowCount  Integer (optional) the number of rows of data 
#' that should be used to train the model. If specified, trainingDuration may not be specified.
#' @param trainingDuration Character string (optional) a duration string specifying what 
#' time range the data used to train the model should span.
#' If specified, trainingRowCount may not be specified.
#' @return An integer value that can be used as the modelJobId parameter
#' in subsequent calls to the GetDatetimeModelFromJobId function.
#' @export
#'
RequestNewDatetimeModel <- function(project, blueprint, featurelist = NULL,
                                    trainingRowCount = NULL, trainingDuration = NULL) {
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
  bodyFrame <- data.frame(blueprintId = blueprintId)
  if (blueprint$projectId != projectId) {
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
  if (ncol(bodyFrame) > 1) {
    body <- jsonlite::unbox(bodyFrame)
    rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                               returnRawResponse = TRUE, encode = "json")
  } else {
    body <- list(blueprintId = bodyFrame$blueprintId)
    rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                               returnRawResponse = TRUE)
  }
  message("New datetime model request received")
  return(JobIdFromResponse(rawReturn))
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
#' @param trainingRowCount Integer (optional) the number of rows of data that
#' should be used to train the model.
#' @param trainingDuration Character string (optional) a duration string specifying what
#' time range the data used to train the model should span.
#' @param trainingStartDate Character string(optional) the start date of the data to train to model on 
#' ("%Y-%m-%dT%H:%M:%SZ" RFC 3339 format).
#' Only rows occurring at or after this datetime will be used.
#' @param trainingEndDate Character string(optional) the end date of the data to train the model on 
#' ("%Y-%m-%dT%H:%M:%SZ" RFC 3339 format).
#' Only rows occurring strictly before this datetime will be used.
#' @return An integer value that can be used as the modelJobId parameter
#' in subsequent calls to the GetDatetimeModelFromJobId function.
#' @export
#'
RequestFrozenDatetimeModel <- function(model, trainingRowCount=NULL,
                                       trainingDuration=NULL, trainingStartDate=NULL,
                                       trainingEndDate=NULL) {
  if (is.null(trainingRowCount) &
      is.null(trainingDuration) &
      is.null(trainingStartDate) &
      is.null(trainingEndDate)){
    err <- strwrap('one of trainingRowCount, trainingDuration,
                   or trainingStartDate and trainingEndDate should be specified')
    stop(err)
  }

  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "frozenDatetimeModels")
  body <- list(modelId = modelId, trainingRowCount = trainingRowCount,
               trainingDuration = trainingDuration,
               trainingStartDate = trainingStartDate,
               trainingEndDate = trainingEndDate)
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                             returnRawResponse = TRUE)
  message("Frozen datetime model request received")
  return(JobIdFromResponse(rawReturn))
}



#' Compute the scores for all available backtests
#' 
#' Some backtests may be unavailable if the model is trained into their validation data.
#'
#' @inheritParams DeleteModel
#' @return Integer a job tracking the backtest computation.  
#' When it is complete, all available backtests will have scores computed.
#' Function WaitForJobToComplete can be used to wait for the job completion
#' @export
#'
ScoreBacktests <- function(model){
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "datetimeModels", modelId, "backtests")
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = list(),
                             returnRawResponse = TRUE)
  message("Backtest score request received")
  return(JobIdFromResponse(rawReturn))
}
