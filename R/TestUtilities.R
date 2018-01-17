# Unexpored helper functions we use in the tests

fileToChar <- function(x) {
  readChar(x, file.info(x)$size)
}

#' Make sure that the object has all of the keys specified. Also tests that there
#' are not additional keys if \code{allowAdditional} is FALSE (default).
#'
#' @param obj object. A list, vector, or data.frame to check names.
#' @param keys character. A vector of names of keys to check.
#' @param allowAdditional logical. Should we allow there to be more keys than specified?
ExpectHasKeys <- function(obj, keys, allowAdditional = FALSE) {
  missingKeys <- setdiff(keys, names(obj))
  testthat::expect_equal(length(missingKeys), 0,
                         info = paste(paste0(missingKeys, collapse = ", "),
                                      " was not found."))
  if (identical(allowAdditional, FALSE)) {
    extraKeys <- setdiff(names(obj), keys)
    testthat::expect_equal(length(extraKeys), 0,
                           info = paste(paste0(extraKeys, collapse = ", "),
                                        " extra keys found."))
  }
}
