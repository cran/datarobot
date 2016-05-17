#
#  CreateAndUpload.R - function to create a project, uploading the
#  modeling dataset as part of the process
#

CreateAndUpload <- function(dataset, projectName, maxWait = 60) {
  routeString <- "projects/"
  dataList <- list(projectName = projectName, file = httr::upload_file(dataset))
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList,
                             returnRawResponse = TRUE)
  message(paste("Project", projectName,
                      "creation requested, awaiting creation"))
  projectInfo <- WaitForAsyncReturn(httr::headers(rawReturn)$location,
                                    addUrl = FALSE,
                                    maxWait = maxWait,
                                    failureStatuses = "ERROR")
  message(paste("Project", projectName, "created"))
  return(projectInfo)
}
