#' Create a new featurelist in a DataRobot project
#'
#' This function allows the user to create a new featurelist
#' in a project by specifying its name and a list of variables
#' to be included
#'
#' DataRobot featurelists define the variables from the modeling
#' dataset used in fitting each project model. Some functions
#' (SetTarget, StartNewAutopilot) optionally accept a featurelist
#' (and use a default featurelist if none is specified).
#'
#' @inheritParams DeleteProject
#' @param listName character. String identifying the new featurelist
#' to be created.
#' @param featureNames character. Vector listing the names of the
#' variables to be included in the featurelist.
#' @return A list with the following four elements describing
#' the featurelist created:
#' \describe{
#'   \item{featurelistId}{Character string giving the unique
#'   alphanumeric identifier for the new featurelist.}
#'   \item{projectId}{Character string giving the projectId
#'   identifying the project to which the featurelist was added.}
#'   \item{features}{Character vector with the names of the
#'   variables included in the new featurelist.}
#'   \item{name}{Character string giving the name of the new
#'   featurelist.}
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   CreateFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2", "otherFeature"))
#' }
#' @export
CreateFeaturelist <- function(project, listName, featureNames) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "featurelists")
  # I(featureNames) tells httr/jsonlite not to unbox length-1 vectors to scalars
  body <- list(name = listName, features = I(featureNames))
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE,
                             body = body,
                             returnRawResponse = TRUE,
                             encode = "json")
  rawHeaders <- httr::headers(rawReturn)
  featurelistInfo <- DataRobotGET(rawHeaders$location, addUrl = FALSE)
  idIndex <- which(names(featurelistInfo) == "id")
  names(featurelistInfo)[idIndex] <- "featurelistId"
  message(paste("Featurelist", listName, "created"))
  as.dataRobotFeaturelist(featurelistInfo)
}

#' This function allows the user to create a new featurelist
#' in a project by specifying its name and a list of variables
#' to be included
#'
#' In time series projects, a new set of modeling features is created after setting the
#' partitioning options. These features are automatically derived from those in the project's
#' dataset and are the features used for modeling. Modeling features are only accessible once
#' the target and partitioning options have been set. In projects that don't use time series
#' modeling, once the target has been set, ModelingFeaturelists and Featurelists will behave
#' the same.
#'
#' @inheritParams CreateFeaturelist
#' @inherit CreateModelingFeaturelist return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   CreateModelingFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#' }
#' @export
CreateModelingFeaturelist <- function(project, listName, featureNames) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelingFeaturelists")
  # I(featureNames) tells httr/jsonlite not to unbox length-1 vectors to scalars
  body <- list(name = listName, features = I(featureNames))
  featurelistInfo <- DataRobotPOST(routeString, addUrl = TRUE,
                                   body = body,
                                   returnRawResponse = FALSE,
                                   encode = "json")
  featurelistInfo$featurelistId <- featurelistInfo$id
  featurelistInfo$id <- NULL
  message(paste("Featurelist", listName, "created"))
  as.dataRobotFeaturelist(featurelistInfo)
}
