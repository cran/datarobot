#
#  Stringify.R - function to convert a function into a single string for DataRobot
#

Stringify <- function(functionToConvert, dputFile = tempfile()) {
  #
  dput(functionToConvert, file = dputFile)
  charVector <- readLines(dputFile)
  outString <- paste(charVector, collapse = "\n")
  return(outString)
}
