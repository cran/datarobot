#' Retrieve all featurelists associated with a project
#'
#' This function returns an S3 object of class listOfFeaturelists that
#' describes all featurelists (i.e., lists of modeling variables)
#' available for the project specified by the project parameter.
#' This list may be converted to a dataframe with the as.data.frame
#' method for objects of class listOfFeaturelists.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class 'listOfFeaturelists', which is a
#' list of dataframes: each element of the list corresponds to one
#' featurelist associated with the project, and each dataframe has
#' one row and the following four columns:
#' \itemize{
#'   \item featurelistId. Unique alphanumeric identifier for the featurelist.
#'   \item projectId. Unique alphanumeric project identifier.
#'   \item features. Comma-separated character string listing the variables included in the
#'     featurelist.
#'   \item name. Character string giving the name of the featurelist.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListFeaturelists(projectId)
#' }
#' @export
ListFeaturelists <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "featurelists")
  response <- DataRobotGET(routeString, addUrl = TRUE)
  featurelists <- list()
  for (i in seq(nrow(response))) {
    flist <- as.list(response[i, ])
    flist$features <- flist$features[[1]]
    flist$featurelistId <- flist$id
    flist$id <- NULL
    flist <- as.dataRobotFeaturelist(flist)
    featurelists <- append(featurelists, list(flist))
  }
  class(featurelists) <- c("listOfFeaturelists", "listSubclass")
  featurelists
}


#' Retrieve all modeling featurelists associated with a project
#'
#' In time series projects, a new set of modeling features is created after setting the
#' partitioning options. These features are automatically derived from those in the project's
#' dataset and are the features used for modeling. Modeling features are only accessible once
#' the target and partitioning options have been set. In projects that don't use time series
#' modeling, once the target has been set, ModelingFeaturelists and Featurelists will behave
#' the same.
#'
#' @inheritParams ListFeaturelists
#' @inherit ListFeaturelists return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListModelingFeaturelists(projectId)
#' }
#' @export
ListModelingFeaturelists <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "modelingFeaturelists")
  response <- DataRobotGET(routeString, addUrl = TRUE)
  featurelists <- list()
  # TODO: These come back paginated
  for (i in seq(nrow(response$data))) {
    flist <- as.list(response$data[i, ])
    flist$features <- flist$features[[1]]
    flist$featurelistId <- flist$id
    flist$id <- NULL
    flist <- as.dataRobotFeaturelist(flist)
    featurelists <- append(featurelists, list(flist))
  }
  class(featurelists) <- c("listOfModelingFeaturelists", "listSubclass")
  featurelists
}
