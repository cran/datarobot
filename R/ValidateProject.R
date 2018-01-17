#' Get a projectId from a project object.
#'
#' @param project object. Either list with projectId element or projectId value
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

#' Ensure a parameter is valid
#'
#' A valid parameter \code{paramValue} is either NULL or in the space
#' of \code{paramPossibilites}.
#'
#' @param paramValue object. The parameter value to check.
#' @param paramPossibilities vector. A vector of possible values for the parameter.
#' @param allowNULL logical. Whether or not to allow NULL as a possibility.
#' @return TRUE if \code{paramValue} is valid, otherwise it raises an error.
#' @examples
#' \dontrun{
#'   ValidateParameterIn("all", DataSubset)
#' }
ValidateParameterIn <- function(paramValue, paramPossibilities, allowNULL = TRUE) {
  isUnallowedNull <- is.null(paramValue) && !allowNULL
  if (isUnallowedNull ||
      length(paramValue) > 1 ||
      !(is.null(paramValue) ||
      any(paramValue %in% paramPossibilities))) {
        stop("Invalid ", sQuote(substitute(paramPossibilities)), ". Must be in ",
             paste(vapply(paramPossibilities, sQuote, character(1)), collapse = ", "),
             " but got ",
             sQuote(gsub("\"", "", capture.output(dput(paramValue)))), " instead.")
  }
  TRUE
}
