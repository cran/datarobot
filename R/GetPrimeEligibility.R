#' Check if model can be approximated with DataRobot Prime
#'
#' This function returns list with two members: 
#' canMakePrime : logical value. TRUE if model can be approximated using DataRobot Prime, FALSE if model can not be approximated
#' message : character. Provides information why model may not be approximated with DataRobot Prime
#'
#' @inheritParams DeleteProject
#' @param modelId Unique alphanumeric identifier for the model of interest.
#' @return list with two members: 
#' canMakePrime : logical value. TRUE if model can be approximated using DataRobot Prime, FALSE if model can not be approximated
#' message : character. Provides information why model may not be approximated with DataRobot Prime
#' @export
#'
GetPrimeEligibility <- function(project, modelId) {

    projectId <- ValidateProject(project)
    routeString <- UrlJoin("projects", projectId, "models", modelId, "primeInfo")
    modelPrimeInfo <- DataRobotGET(routeString, addUrl = TRUE)
    modelPrimeInfo$messageId <- NULL
    return(as.dataRobotPrimeEligibility(modelPrimeInfo))

}

as.dataRobotPrimeEligibility <- function(inList){
  elements <- c("canMakePrime",
                "message")
  return(ApplySchema(inList, elements))
}
