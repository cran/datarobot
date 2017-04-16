DefaultHTTPTimeout <- 60

MakeDataRobotRequest <- function(requestMethod, routeString, addUrl, returnRawResponse,
                                 simplifyDataFrame = TRUE,
                                 body = NULL,
                                 timeout = DefaultHTTPTimeout, ...) {
  # Makes authenticated DataRobot requests. Most uses require the $content element from the return
  # list, but some require the raw response, which is returned if returnRawResponse is TRUE
  path <- BuildPath(routeString, addUrl)
  rawReturn <- requestMethod(path$fullPath,
                             DataRobotAddHeaders(Authorization = path$authHead),
                             body = body,
                             httr::timeout(timeout), ...)
  StopIfResponseIsError(rawReturn)
  if (returnRawResponse) {
    return(rawReturn)
  } else {
    return(ParseReturnResponse(rawReturn, simplifyDataFrame = simplifyDataFrame))
  }
}

DataRobotGET <- function(routeString, addUrl, returnRawResponse = FALSE, ...) {
  return(MakeDataRobotRequest(httr::GET, routeString, addUrl, returnRawResponse, ...))
}

DataRobotDELETE <- function(routeString, addUrl, returnRawResponse = FALSE, ...) {
  return(MakeDataRobotRequest(httr::DELETE, routeString, addUrl, returnRawResponse, ...))
}

DataRobotPATCH <- function(routeString, addUrl, body, returnRawResponse = FALSE, ...) {
  return(MakeDataRobotRequest(httr::PATCH, routeString, addUrl, returnRawResponse,
                              body = body, ...))
}

DataRobotPOST <- function(routeString, addUrl, body, returnRawResponse = FALSE, ...) {
  return(MakeDataRobotRequest(httr::POST, routeString, addUrl, returnRawResponse,
                              simplifyDataFrame = TRUE,
                              body = body, ...))
}

DataRobotAddHeaders <- function(...) {
  platform <- as.list(Sys.info())
  platformStr <- paste(platform$sysname, platform$release, platform$machine)
  userAgent <- sprintf("DataRobotRClient/%s (%s)",
                       packageVersion(packageName()),
                       platformStr)
  headers <- httr::add_headers("User-Agent" = userAgent, ...)
  return(headers)
}

StopIfResponseIsError <- function(rawReturn) {
  returnStatus <- httr::status_code(rawReturn)
  if (returnStatus >= 400) {
    parsedReturn <- ParseReturnResponse(rawReturn)
    errors <- parsedReturn$errors
    errorSummary <- parsedReturn$message
    errorMessages <- Map(function(keyName) paste0("  ", keyName, ": ", errors[[keyName]]),
                         names(errors))
    errorMessagesCombined <- paste(errorMessages, collapse = "\n")
    statusString <- sprintf("(%s)", returnStatus)
    stop(paste(statusString, errorSummary, errorMessagesCombined, sep = "\n"), call. = FALSE)
  }
}

Endpoint <- function() {
  return(Sys.getenv("DataRobot_URL"))
}

Token <- function() {
  return(Sys.getenv("DataRobot_Token"))
}

BuildPath <- function(routeString, addUrl = TRUE) {
  endpoint <- Endpoint()
  token <- Token()
  if ( (endpoint == "") | (token == "") ) {
    rawMsg <- paste("User authentication required. See ConnectToDataRobot documentation")
    stop(strwrap(rawMsg), call. = FALSE)
  } else if (addUrl) {
    fullPath <- UrlJoin(endpoint, routeString)
  } else {
    fullPath <- routeString
  }

  CheckUrl(fullPath)
  authHead <- paste("Token", token, sep = " ")
  pathList <- list(fullPath = fullPath, authHead = authHead)
  return(pathList)
}

CheckUrl <- function(url) {
  # Maybe sure the path is a reasonable URL:
  if (grepl("//$", url) || grepl("\\s", url) ) {
    stop(paste("Internal error, URL invalid:", url))
  }
}

UrlJoin <- function(...) {
  for (urlComponent in list(...)) {
    if (grepl("\\?", urlComponent)) {
      stop("Arguments to UrlJoin should not contain query parameters.")
    }
  }
  components <- list(...)
  # Remove trailing /'s to avoid double slashing when pasting
  components <- Map(function(x) sub("\\/+$", "", x), components)
  components <- c(components, "") # for trailing "/"
  return(paste(components, collapse = "/"))
}

ParseReturnResponse <- function(rawReturn, ...) {
  textContent <- httr::content(rawReturn, as = "text", encoding = "UTF-8")
  if (is.na(textContent)){
    return(NA)
  }
  OnError <- function(error) {
    stop(paste("Expected JSON, received:\n", textContent),
         call. = FALSE)
  }
  if (textContent == "") {
    return("")
  } else {
    return(tryCatch(jsonlite::fromJSON(textContent, ...), error = OnError))
  }
}

ResponseIsRedirection <- function(rawResponse) {
  responseCategory <- httr::http_status(rawResponse)$category
  return(tolower(responseCategory) == 'redirection')
}
