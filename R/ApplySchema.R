#
#  ApplySchema.R - function to apply schema to DataRobot objects (lists, frames)
#

ApplySchema <- function(inList, schema, mask = NULL){
  if (!is.null(mask)){
    schema <- c(schema, names(inList)[grep(mask, names(inList))])
  }
  elements <- names(inList)[names(inList) %in% schema]
  if (is.data.frame(inList)){
    if (nrow(inList) > 0){
      outList <- inList[, elements]
    } else {
      outList <- data.frame(matrix(ncol = length(elements), nrow = 0))
      names(outList) <- elements
    }
    return(outList)
  }
  outList <- inList[elements]
  return(outList)
}
