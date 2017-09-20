#' Format mixed JSON required for group-based partitioning
#' @param mixedList list. The list to format JSON for.
#' @param specialCase character. A vector of names of columns where JSON
#'   unboxing should apply.
FormatMixedList <- function(mixedList, specialCase) {
  listNames <- names(mixedList)
  specialIndex <- which(listNames == specialCase)
  n <- length(mixedList)
  outList <- list(length = n)
  for (i in 1:n) {
    if (i != specialIndex) {
      outList[[i]] <- jsonlite::unbox(mixedList[[i]])
    } else {
      outList[[i]] <- mixedList[[i]]
    }
  }
  names(outList) <- listNames
  return(outList)
}
