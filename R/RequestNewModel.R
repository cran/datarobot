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
  secondProject <- FALSE
  blueprintId <- blueprint$blueprintId
  blueProjectId <- blueprint$projectId
  if (blueProjectId != projectId) {
    secondProject <- TRUE
  }
  bodyFrame <- data.frame(blueprintId = blueprintId)

  if (secondProject) {
    bodyFrame$sourceProjectId <- blueProjectId
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
  #
  #  Extract modelJobId from the RawReturn$headers$location string
  #
  message("New model request received")
  rawHeaders <- httr::headers(rawReturn)
  modelJobPath <- rawHeaders$location
  pathSplit <- unlist(strsplit(modelJobPath, "modelJobs/"))
  modelJobId <- gsub("/", "", pathSplit[2])
  return(modelJobId)
}
