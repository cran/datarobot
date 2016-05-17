#' Retrieve the valid fitting metrics for a specified project and target
#'
#' For the response variable defined by the character string target
#' and the project defined by the parameter project, return the vector
#' of metric names that can be specified for fitting models in this project.
#' This function is intended for use after SetupProject has been run but
#' before SetTarget, allowing the user to specify valid non-default
#' values for the metric parameter.
#'
#' @inheritParams DeleteProject
#' @param target Character string giving the name of the response variable
#' to be predicted by all project models.
#' @return Character vector containing the names of the metric values
#' that are valid for a subsequent call to the SetTarget function.
#' @export
#'
GetValidMetrics <- function(project, target) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "features", "metrics")
  response <- DataRobotGET(routeString, addUrl = TRUE, query = list(featureName = target))
  return(response$availableMetrics)
}
