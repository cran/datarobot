#' Retrieve a list of all DataRobot projects
#'
#' This function returns an S3 object of class projectSummaryList
#' that describes all DataRobot modeling projects available to the user.
#' This list may be converted into a dataframe with the as.data.frame
#' method for this class of S3 objects.
#'
#' @return An S3 object of class 'projectSummaryList', consisting
#' of the following 15 elements:
#' \describe{
#'   \item{projectId}{List of character strings giving the unique DataRobot identifier for each project}
#'   \item{projectName}{List of character strings giving the user-supplied project names}
#'   \item{fileName}{List of character strings giving the name of the modeling dataset for each project}
#'   \item{stage}{List of character strings specifying each project's Autopilot stage (e.g., 'aim' is necessary to set target)}
#'   \item{autopilotMode}{List of integers specifying the Autopilot mode (0 = fully automatic, 1 = semi-automatic, 2 = manual)}
#'   \item{created}{List of character strings giving the project creation time and date}
#'   \item{target}{List of character strings giving the name of the target variable for each project}
#'   \item{metric}{List of character strings identifying the fitting metric optimized for each project}
#'   \item{partition}{Dataframe with one row for each project and 12 columns specifying partitioning details}
#'   \item{recommender}{Dataframe with one row for each project and 3 columns characterizing recommender projects}
#'   \item{advancedOptions}{Dataframe with one row for each project and 4 columns specifying values for advanced option parameters}
#'   \item{positiveClass}{Character string identifying the positive target class for binary classification projects}
#'   \item{maxTrainPct}{List of integers specifying the maximum training set percentage possible for each project}
#'   \item{holdoutUnlocked}{Logical flag indicating whether holdout subset results have been computed}
#'   \item{targetType}{Character string giving the type of modeling project (e.g., regression or binary classification)}
#' }
#' @export
#'
GetProjectList <- function() {
  routeString <- "projects/"
  returnValue <- DataRobotGET(routeString, addUrl = TRUE)
  return(projectSummaryList(returnValue))
}


projectSummaryList <- function(projectSummaryData) {
  emptyProjectSummaryList <- structure(list(projectId = character(0), projectName = character(0),
                 fileName = character(0), stage = character(0), autopilotMode = logical(0),
                 created = character(0), target = logical(0), metric = logical(0),
                 partition = data.frame(datetimeCol = logical(0), cvMethod = logical(0),
                                        validationPct = logical(0), reps = logical(0),
                                        cvHoldoutLevel = logical(0),  holdoutLevel = logical(0),
                                        userPartitionCol = logical(0),  validationType = logical(0),
                                        trainingLevel = logical(0), partitionKeyCols = logical(0),
                                        holdoutPct = logical(0), validationLevel = logical(0)),
                 recommender = data.frame(recommenderItemId = logical(0),
                                          isRecommender = logical(0),
                                          recommenderUserId = logical(0)),
                 advancedOptions = data.frame(blueprintThreshold = logical(0),
                                              responseCap = logical(0), seed = logical(0),
                                              weights = logical(0)),
                 positiveClass = logical(0), maxTrainPct = logical(0), holdoutUnlocked = logical(0),
                 targetType = logical(0)),
            class = "projectSummaryList")

  if (length(projectSummaryData) == 0) {
    return(emptyProjectSummaryList)
  } else {
    idIndex <- which(names(projectSummaryData) == "id")
    names(projectSummaryData)[[idIndex]] <- "projectId"
    projectSummaryData <- as.dataRobotProject(projectSummaryData)
    class(projectSummaryData) <- "projectSummaryList"
    return(projectSummaryData)
  }
}
