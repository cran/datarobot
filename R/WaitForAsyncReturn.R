WaitForAsyncReturn <- function(routeString, maxWait = 600, addUrl = TRUE, failureStatuses = c()) {
  rawStatusInfo <- httr::with_config(httr::config(followlocation = FALSE),
                                     WaitFor303(routeString, maxWait, addUrl, failureStatuses))
  asyncHeaders <- httr::headers(rawStatusInfo)
  asyncLocation <- asyncHeaders$location
  tryCatch(DataRobotGET(asyncLocation, addUrl = FALSE, timeout = maxWait),
           error = function(e) {
             if (grepl("Expected JSON, received", e)) {  # Allow JSON parse errors
               NULL                                      # (happens when awaiting download jobs)
             } else { stop(e) }
           })
}


WaitFor303 <- function(routeString, maxWait, addUrl = TRUE, failureStatuses) {
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
