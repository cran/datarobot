#' Retrieve dtails about a specified DataRobot modeling project
#'
#' Returns a list of details about the DataRobot modeling project
#' specified by project.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class 'dataRobotProject', consisting of the following 15 elements:
#' \itemize{
#'   \item projectId. Character string giving the unique project identifier.
#'   \item projectName. Character string giving the name assigned to the project.
#'   \item fileName. Character string giving the name of the modeling dataset for the project.
#'   \item stage. Character string describing the stage of the DataRobot Autopilot.
#'   \item autopilotMode. Numeric: 0 for fully automatic mode; 1 for semi-automatic mode; 2 for
#'     manual mode.
#'   \item created. Character string representation of the project creation time and date.
#'   \item target. Name of the target variable from fileName.
#'   \item metric. Character string specifying the metric optimized by all project models.
#'   \item partition. A 7-element list describing the data partitioning for model fitting
#'     and cross validation.
#'   \item recommender. A 3-element list with information specific to recommender models.
#'   \item advancedOptions. A 4-element list with advanced option specifications.
#'   \item positiveClass. Character string: name of positive class for binary response models.
#'   \item maxTrainPct. Maximum training subset percentage for models in this project.
#'   \item holdoutUnlocked. A logical flag indicating whether the holdout dataset has been used for
#'     model evaluation.
#'   \item targetType. Character string specifying the type of modeling problem (e.g., regression or
#'     binary classification).
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetProject(projectId)
#' }
#' @export
GetProject <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId)
  projectDetails <- DataRobotGET(routeString, addUrl = TRUE)
  idIndex <- which(names(projectDetails) == "id")
  names(projectDetails)[idIndex] <- "projectId"
  return(as.dataRobotProject(projectDetails))
}

as.dataRobotProject <- function(inProject) {
  elements <- c("projectId",
                "projectName",
                "fileName",
                "stage",
                "autopilotMode",
                "created",
                "target",
                "metric",
                "partition",
                "recommender",
                "advancedOptions",
                "positiveClass",
                "maxTrainPct",
                "holdoutUnlocked",
                "targetType")
  outProject <- inProject[elements]
  class(outProject) <- 'dataRobotProject'
  return(outProject)
}
