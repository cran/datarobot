# Unexpored helper functions we use in the tests

fileToChar <- function(x) {
  readChar(x, file.info(x)$size)
}
