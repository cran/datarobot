#
#  FormatMixedList.R - function to format mixed JSON required for group-based partitioning
#

FormatMixedList <- function(mixedList, specialCase) {
  #
  #  First, split out and handle the special case
  #
  listNames <- names(mixedList)
  specialIndex <- which(listNames == specialCase)
  #
  #  Merge the boxed/unboxed list elements as required
  #
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
