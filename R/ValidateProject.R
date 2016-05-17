#
#  ValidateProject.R - accept either list with projectId element or projectId value
#

ValidateProject <- function(project) {
  if (is.list(project)) {
    projectId <- project$projectId
  } else {
    projectId <- project
  }
  if (is.null(projectId)) {
    rawMsg <- paste("Project specification does not contain a valid project
                    in call to ", sys.calls()[[1]][[1]])
    stop(strwrap(rawMsg), call. = FALSE)
  } else {
    return(projectId)
  }
}
