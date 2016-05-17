#
#  ReformatListOfModels.R - function to convert the list-of-lists output
#  from the server to a list-of-modelObjects format
#

ReformatListOfModels <- function(listOfLists, projectDetails) {
  #
  ###########################################################################
  #
  #  Reformat the input list-of-lists object from the DataRobot Public API
  #  server summarizing all project models into a list-of-S3-objects format,
  #  where each element is an object of class 'dataRobotModel'
  #
  ###########################################################################
  #
  #  Note: the format of the listOfLists returned by the Public API server in
  #        response to the GetAllModels request is complicated.  As of
  #        8/24/2015, this list has 10 elements: 8 of these 10 elements are
  #        simple vectors of numbers or characters; the element $processes is
  #        a list of character vectors, and the element $metrics is a
  #        dataframe with one row for each model and a variable number of
  #        columns
  #
  #  Also, the name of the "modelId" element of the list is returned as "id"
  #  - correct this for the S3 objects to be returned by this function
  #
  allNames <- names(listOfLists)
  idIndex <- which(allNames == "id")
  newNames <- allNames
  newNames[idIndex] <- "modelId"
  #
  #  Note that a simple nested loop structure works for all elements of
  #  listOfLists EXCEPT $metrics.  This element can be extracted as a
  #  row of a dataframe; thus, detect $metric and treat it specially
  #
  nModels <- length(listOfLists[[1]])
  outObject <- vector("list", nModels)
  mElements <- length(newNames)
  metricIndex <- which(newNames == "metrics")
  for (i in 1:nModels) {
    element <- vector("list", mElements)
    for (j in 1:mElements) {
      if (j != metricIndex) {
        element[[j]] <- listOfLists[[j]][[i]]
      } else {
        metricsList <- as.list(listOfLists[[j]][i, ])
        for (k in 1:length(metricsList)) {
          xFrame <- metricsList[[k]]
          rownames(xFrame) <- NULL
          for (m in 1:ncol(xFrame)) {
            xFrame[, m] <- as.numeric(xFrame[, m])
          }
          metricsList[[k]] <- xFrame
        }
        element[[j]] <- metricsList
      }
    }
    names(element) <- newNames
    element <- append(element, projectDetails)
    class(element) <- 'dataRobotModel'
    outObject[[i]] <- element
  }

  class(outObject) <- c('listOfModels', 'listSubclass')
  return(outObject)
}
