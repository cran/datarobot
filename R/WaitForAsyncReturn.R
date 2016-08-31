#
#  WaitForAsyncReturn.R - support for async HTTP calls
#


WaitForAsyncReturn <- function(routeString, maxWait = 60, addUrl = TRUE, failureStatuses = c()) {
  #
  #########################################################################
  #
  #  Wait for async 303 return, then call DataRobotGET for results
  #
  #########################################################################
  #
  rawStatusInfo <- httr::with_config(httr::config(followlocation = FALSE),
                                     WaitFor303(routeString, maxWait, addUrl, failureStatuses))
  asyncHeaders <- httr::headers(rawStatusInfo)
  asyncLocation <- asyncHeaders$location
  return(DataRobotGET(asyncLocation, addUrl = FALSE))
}


WaitFor303 <- function(routeString, maxWait, addUrl, failureStatuses) {
  GetWaitStatus <- StartRetryWaiter(timeout = maxWait, maxdelay = 1)
  while (GetWaitStatus()$stillTrying) {
    rawReturn <- DataRobotGET(routeString, addUrl = addUrl, returnRawResponse = TRUE)
    parsedResponse <- ParseReturnResponse(rawReturn)
    statusCode <- httr::status_code(rawReturn)
    StopIfResponseIsError(rawReturn)
    if (statusCode == 303) {
      return(rawReturn)
    } else if (statusCode == 200) {
      if (parsedResponse$status %in% failureStatuses) {
        Raise(Exceptions$PendingJobFailed(paste("\n", "status:", parsedResponse$status,
                                                "\n", "message:", parsedResponse$message)))
      }
    } else {
      stop(sprintf("Unexpected status code %d.", statusCode))
    }
  }

  # Extra details in a message instead of in the error itself so that you still get the data
  # when calling functions catch the error and display a different message:
  message(sprintf("Async URL: %s\nLatest status: %s", routeString, parsedResponse$status))
  Raise(Exceptions$AsyncTimeout(message = "Async service timed out"))
}
