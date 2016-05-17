#' Retrieve the list of recommended blueprints for a project
#'
#' This function returns the list of recommended blueprints
#' for a specified modeling project, as an S3 object of class
#' listOfBlueprints; see Value.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class 'listOfBlueprints', a list
#' with one element for each recommended blueprint in the
#' associated project.  Each element of this list is itself a
#' list with the following four components:
#' \describe{
#'   \item{projectId}{Character string giving the unique DataRobot project identifier}
#'   \item{processes}{List of character strings, identifying any preprocessing steps included in the blueprint}
#'   \item{blueprintId}{Character string giving the unique DataRobot blueprint identifier}
#'   \item{modelType}{Character string, specifying the type of model the blueprint builds}
#' }
#' @export
#'
GetRecommendedBlueprints <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "blueprints")
  blueprints <- DataRobotGET(routeString, addUrl = TRUE)
  idIndex <- which(names(blueprints) == "id")
  names(blueprints)[idIndex] <- "blueprintId"
  n <- length(blueprints$blueprintId)
  blueNames <- names(blueprints)
  m <- length(blueNames)
  blueprintList <- vector("list", n)
  element <- vector("list", m)
  for (i in 1:n) {
    for (j in 1:m) {
      element[[j]] <- blueprints[[j]][[i]]
    }
    names(element) <- blueNames
    blueprintList[[i]] <- element
  }
  class(blueprintList) <- c('listOfBlueprints', 'listSubclass')
  return(blueprintList)
}
