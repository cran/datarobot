#' Retrieve all available model information for a DataRobot project
#'
#' This function requests the model information for the DataRobot
#' project specified by the project argument, described under Arguments.
#' This parameter may be obtained in several ways, including: (1), from
#' the projectId element of the list returned by GetProjectList; (2), as
#' the object returned by the GetProject function; or (3), as the list
#' returned by the SetupProject function. The function returns an S3
#' object of class 'listOfModels'.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class listOfModels, which may be characterized
#' using R's generic summary function or converted to a dataframe with
#' the as.data.frame method.
#' @export
#'
GetAllModels <- function(project) {
  projectId <- ValidateProject(project)

  fullProject <- GetProject(projectId)
  projectDetails <- list(projectName = fullProject$projectName,
                         projectTarget = fullProject$target,
                         projectMetric = fullProject$metric)

  routeString <- UrlJoin("projects", projectId, "models")
  modelInfo <- DataRobotGET(routeString, addUrl = TRUE)
  if (length(modelInfo) == 0) {
    message("No model information available for this project. \n
             \nThis usually means the Autopilot has not yet started building
             models.")
    returnList <- list()
  } else {
    returnList <- ReformatListOfModels(modelInfo, projectDetails)
  }
  returnList <- lapply(returnList, as.dataRobotModelObject)
  currentModelJobs <- GetModelJobs(projectId)
  if (nrow(currentModelJobs) > 0){
    message("Some models are still in progress")
  }

  class(returnList) <- c('listOfModels', 'listSubclass')
  return(returnList)
}
