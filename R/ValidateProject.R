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


#' Check if a parameter is in a list of possibilities.
#'
#' @param paramValue object. The parameter value to check.
#' @param paramPossibilities vector. A vector of possible values for the parameter.
#' @param allowNULL logical. Whether or not to allow NULL as a possibility.
#' @param paramName character. The name of the parameter to check.
#' @return TRUE if \code{paramValue} is valid, otherwise returns an error message.
#' @examples
#' \dontrun{
#'   IsParameterIn("all", DataSubset)
#' }
IsParameterIn <- function(paramValue, paramPossibilities, allowNULL = TRUE, paramName = NULL) {
  if (is.null(paramName)) {
    paramName <- substitute(paramPossibilities)
    if (length(paramName) > 1) { paramName <- "value" }
  }
  isUnallowedNull <- is.null(paramValue) && !allowNULL
  if (isUnallowedNull ||
      length(paramValue) > 1 ||
      !(is.null(paramValue) ||
      any(paramValue %in% paramPossibilities))) {
        paste0("Invalid ", sQuote(paramName), ". Must be in ",
               paste(vapply(paramPossibilities, sQuote, character(1)), collapse = ", "),
               " but got ",
               sQuote(gsub("\"", "", capture.output(dput(paramValue)))), " instead.")
  } else { TRUE }
}

#' Ensure a parameter is valid
#'
#' A valid parameter \code{paramValue} is either NULL or in the space
#' of \code{paramPossibilites}.
#'
#' @inheritParams IsParameterIn
#' @return TRUE if \code{paramValue} is valid, otherwise it raises an error.
#' @examples
#' \dontrun{
#'   ValidateParameterIn("all", DataSubset)
#' }
ValidateParameterIn <- function(paramValue, paramPossibilities, allowNULL = TRUE) {
  paramName <- substitute(paramPossibilities)
  if (length(paramName) > 1) { paramName <- "value" }
  error <- IsParameterIn(paramValue,
                         paramPossibilities,
                         allowNULL = allowNULL,
                         paramName = paramName)
  if (isTRUE(error)) { TRUE } else { stop(error) }
}
