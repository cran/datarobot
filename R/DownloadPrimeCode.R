#' Download the code of DataRobot Prime model and save it to a file
#'
#' Training a model using a ruleset is a necessary prerequisite for being able to download the code for a ruleset
#'
#' @inheritParams DeleteProject
#' @param primeFileId Prime file Id (can be aquired using ListPrimeFiles function)
#' @param filepath String. The location to save the file to
#' @return NULL
#' @export
#'
DownloadPrimeCode <- function(project, primeFileId, filepath) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeFiles", primeFileId, "download")

  response <- DataRobotGET(routeString, addUrl = TRUE, query = NULL,
                           simplifyDataFrame = FALSE, returnRawResponse = TRUE)
  textContent <- httr::content(response, as = "text", encoding = "UTF-8")
  cat(textContent, file = filepath)
  return(invisible(NULL))
}
