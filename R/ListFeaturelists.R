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
#' \describe{
#'   \item{featurelistId}{Unique alphanumeric identifier for the featurelist}
#'   \item{projectId}{Unique alphanumeric project identifier}
#'   \item{features}{Comma-separated character string listing the variables included in the featurelist}
#'   \item{name}{Character string giving the name of the featurelist}
#' }
#' @export
#'
ListFeaturelists <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "featurelists")
  featurelist <- DataRobotGET(routeString, addUrl = TRUE)
  oldNames <- names(featurelist)
  selectIndex <- which(oldNames == "id")
  names(featurelist)[selectIndex] <- "featurelistId"
  returnFrame <- featurelist
  nList <- nrow(returnFrame)
  returnList <- vector("list", nList)
  for (i in 1:nList) {
    returnList[[i]] <- returnFrame[i, ]
  }
  class(returnList) <- c('listOfFeaturelists', 'listSubclass')
  return(returnList)
}
