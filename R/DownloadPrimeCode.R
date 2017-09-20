#' Download the code of DataRobot Prime model and save it to a file.
#'
#' Training a model using a ruleset is a necessary prerequisite for being able to download the
#'   code for a ruleset.
#'
#' @inheritParams DeleteProject
#' @param primeFileId numeric. Prime file Id (can be aquired using ListPrimeFiles function)
#' @param filepath character. The location to save the file to.
#' @return NULL
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   primeFiles <- ListPrimeFiles(projectId)
#'   primeFile <- primeFiles[[1]]
#'   primeFileId <- primeFile$id
#'   DownloadPrimeCode(projectId, primeFileId, "prime_code.py")
#' }
#' @export
DownloadPrimeCode <- function(project, primeFileId, filepath) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeFiles", primeFileId, "download")
  response <- DataRobotGET(routeString, addUrl = TRUE, query = NULL, as = "text")
  cat(response, file = filepath)
  return(invisible(NULL))
}
