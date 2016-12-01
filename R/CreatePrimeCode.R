#' Create and validate the downloadable code for the ruleset associated with this model
#'
#' @inheritParams DeleteProject
#' @param primeModelId String. Id returned by GetPrimeModel(s) functions
#' @param language Programming language to use for downloadable code (see PrimeLanguage)
#' @return job Id
#' @export
#'
CreatePrimeCode <- function(project, primeModelId, language) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeFiles")
  body <- list(modelId = primeModelId, language = language)
  rawResponse <- DataRobotPOST(routeString, addUrl = TRUE, body =  body, returnRawResponse = TRUE)
  routeString <- UrlJoin("projects", projectId, "jobs", JobIdFromResponse(rawResponse))
  jobsResponse <- DataRobotGET(routeString, addUrl = TRUE, query = NULL, simplifyDataFrame = FALSE)
  return(jobsResponse$id)
}
