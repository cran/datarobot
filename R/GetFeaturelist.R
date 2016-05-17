#' Retrieve a specific featurelist from a DataRobot project
#'
#' This function returns information about and the contents
#' of a specified featurelist from a specified project.
#'
#' DataRobot featurelists define the variables from the modeling
#' dataset used in fitting each project model. In most cases,
#' the same featurelist is used in fitting all project models,
#' but models can be fit using alternative featurelists using the
#' RequestNewModel function. To do this, featurelistId is required,
#' and this is one of the elements returned by the GetFeaturelist
#' function.
#'
#' DataRobot featurelists define the variables from the modeling
#' dataset used in fitting each project model. In most cases, the
#' same featurelist is used in fitting all project models, but models
#' can be fit using alternative featurelists using the RequestNewModel
#' function. To do this, featurelistId is required, and this is one of
#' the elements returned by the GetFeaturelist function.
#'
#' @inheritParams DeleteProject
#' @param featurelistId Unique alphanumeric identifier for the featurelist
#' to be retrieved.
#' @return A list with the following four elements describing the
#' requested featurelist:
#' \describe{
#'   \item{featurelistId}{Character string giving the unique alphanumeric identifier for the featurelist}
#'   \item{projectId}{Character string identifying the project to which the featurelist belongs}
#'   \item{features}{Character vector with the names of the variables included in the featurelist}
#'   \item{name}{Character string giving the name of the featurelist}
#' }
#' @export
#'
GetFeaturelist <- function(project, featurelistId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "featurelists", featurelistId)
  featurelist <- DataRobotGET(routeString, addUrl = TRUE)
  idIndex <- which(names(featurelist) == "id")
  names(featurelist)[idIndex] <- 'featurelistId'
  return(featurelist)
}
