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
#' \itemize{
#'   \item featurelistId. Character string giving the unique alphanumeric identifier for the
#'     featurelist.
#'   \item projectId. Character string identifying the project to which the featurelist belongs.
#'   \item features. Character vector with the names of the variables included in the featurelist.
#'   \item name. Character string giving the name of the featurelist.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   featureList <- CreateFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#'   featurelistId <- featureList$featurelistId
#'   GetFeaturelist(projectId, featurelistId)
#' }
#' @export
GetFeaturelist <- function(project, featurelistId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "featurelists", featurelistId)
  featurelist <- DataRobotGET(routeString, addUrl = TRUE)
  idIndex <- which(names(featurelist) == "id")
  names(featurelist)[idIndex] <- "featurelistId"
  as.dataRobotFeaturelist(featurelist)
}


as.dataRobotFeaturelist <- function(inList) {
  elements <- c("featurelistId",
                "projectId",
                "features",
                "name")
  ApplySchema(inList, elements)
}


#' Retrieve a specific modeling featurelist from a DataRobot project
#'
#' In time series projects, a new set of modeling features is created after setting the
#' partitioning options. These features are automatically derived from those in the project's
#' dataset and are the features used for modeling. Modeling features are only accessible once
#' the target and partitioning options have been set. In projects that don't use time series
#' modeling, once the target has been set, ModelingFeaturelists and Featurelists will behave
#' the same.
#'
#' @inheritParams GetFeaturelist
#' @inherit GetFeaturelist return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   featureList <- CreateModelingFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#'   featurelistId <- featureList$featurelistId
#'   GetModelingFeaturelist(projectId, featurelistId)
#' }
#' @export
GetModelingFeaturelist <- function(project, featurelistId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelingFeaturelists", featurelistId)
  featurelist <- DataRobotGET(routeString, addUrl = TRUE)
  idIndex <- which(names(featurelist) == "id")
  names(featurelist)[idIndex] <- "featurelistId"
  as.dataRobotFeaturelist(featurelist)
}


as.dataRobotFeaturelist <- function(inList) {
  elements <- c("featurelistId",
                "projectId",
                "features",
                "name")
  ApplySchema(inList, elements)
}
