#' @export
#'
'[.listSubclass' <- function(x, i, ...) {
  structure(NextMethod("["), class = class(x))
}
