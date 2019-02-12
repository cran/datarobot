DefaultHTTPTimeout <- 60

#' Make a HTTP request
#'
#' @param requestMethod function. A function from httr (e.g., `httr::GET`, `httr::POST`) to use.
#' @param routeString character. The path to make the request on.
#' @param addUrl logical. Should the endpoint be prepended to the routeString? (Default TRUE).
#' @param returnRawResponse logical. Whether to return the raw httr respnose object (as opposed
#'   to postprocessing and returning the content of that object, which is the default.)
#' @param as character. What should the resulting data be interpreted as? (default "json").
#' @param simplifyDataFrame logical. Whether to invoke \code{jsonlite::simplifyDataFrame}.
#' @param body list. The body of the request.
#' @param timeout numeric. How many seconds before the request times out?
#' @param encode character. What should the body be encoded as for the JSON request?
#' @param ... list. Extra arguments to pass to the requestMethod
MakeDataRobotRequest <- function(requestMethod, routeString,
                                 addUrl = TRUE,
                                 returnRawResponse = TRUE,
                                 as = "json",
                                 simplifyDataFrame = TRUE,
                                 body = NULL,
                                 timeout = DefaultHTTPTimeout,
                                 encode = NULL,
                                 ...) {
  # Makes authenticated DataRobot requests. Most uses require the $content element from the return
  # list, but some require the raw response, which is returned if returnRawResponse is TRUE
  path <- BuildPath(routeString, addUrl)
  SetSSLVerification()

  # ...Okay, deep breath. If we want to pass NULL values into the API we're going to need to do
  # some heavy work, as httr will drop NULL values by default.
  if (any(unlist(lapply(body, is.null)))) { # detect if any NULL values are present and then
                                            # switch to alternate handling...
    # To properly pass null values as JSON, we need to get the parameter value to be `null`.
    # Interpreting to JSON gives `{}` instead of `null`, so we hack this with find-and-replace.
    body <- structure(gsub("{}", "null",
                           as.character(jsonlite::toJSON(body)), fixed = TRUE),
                      class = "json")
    # To pass raw JSON into the request, we have to encode with "raw" instead of "json"
    encode <- "raw"
    # We also have to manually set the header to JSON
    headers <- DataRobotAddHeaders(Authorization = path$authHead,
                                   "Content-Type" = "application/json")
  } else {
    headers <- DataRobotAddHeaders(Authorization = path$authHead)
  }

  if (isTRUE(getOption("dataRobotCurlDebugMode"))) {
    # If a debug mode is activated, use httr::verbose to print the internals of the
    # API call for debugging purposes.
    rawReturn <- requestMethod(path$fullPath, headers, body = body, httr::timeout(timeout),
                               encode = encode, httr::verbose(), ...)
  } else {
    rawReturn <- requestMethod(path$fullPath, headers, body = body, httr::timeout(timeout),
                               encode = encode, ...)
  }
  StopIfResponseIsError(rawReturn)
  if (isTRUE(returnRawResponse)) {
    rawReturn
  } else if (identical(as, "json")) {
    ParseReturnResponse(rawReturn, simplifyDataFrame = simplifyDataFrame)
  } else {
    httr::content(rawReturn, as = as, encoding = "UTF-8")
  }
}

DataRobotGET <- function(routeString, addUrl, returnRawResponse = FALSE, ...) {
  MakeDataRobotRequest(httr::GET, routeString, addUrl, returnRawResponse, ...)
}

DataRobotDELETE <- function(routeString, addUrl, returnRawResponse = FALSE, ...) {
  MakeDataRobotRequest(httr::DELETE, routeString, addUrl, returnRawResponse, ...)
}

DataRobotPATCH <- function(routeString, addUrl, body = NULL, returnRawResponse = FALSE, ...) {
  MakeDataRobotRequest(httr::PATCH, routeString, addUrl, returnRawResponse, body = body, ...)
}

DataRobotPOST <- function(routeString, addUrl, body = NULL, returnRawResponse = FALSE, ...) {
  MakeDataRobotRequest(httr::POST, routeString, addUrl, returnRawResponse,
                       simplifyDataFrame = TRUE,
                       body = body, ...)
}

DataRobotAddHeaders <- function(...) {
  platform <- as.list(Sys.info())
  platformStr <- paste(platform$sysname, platform$release, platform$machine)
  userAgent <- sprintf("DataRobotRClient/%s (%s)",
                       packageVersion(packageName()),
                       platformStr)
  suffix <- UserAgentSuffix()
  if (nzchar(suffix)) {
    userAgent <- paste(userAgent, suffix)
  }
  httr::add_headers("User-Agent" = userAgent, ...)
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
  Sys.getenv("DATAROBOT_API_ENDPOINT")
}

Token <- function() {
  Sys.getenv("DATAROBOT_API_TOKEN")
}

UserAgentSuffix <- function() {
  Sys.getenv("DataRobot_User_Agent_Suffix")
}

SSLVerify <- function() {
  Sys.getenv("DataRobot_SSL_Verify")
}

BuildPath <- function(routeString, addUrl = TRUE) {
  endpoint <- Endpoint()
  token <- Token()
  if (endpoint == "" | token == "") {
    rawMsg <- paste("User authentication required. See ConnectToDataRobot documentation")
    stop(strwrap(rawMsg), call. = FALSE)
  } else if (addUrl) {
    fullPath <- UrlJoin(endpoint, routeString)
  } else {
    fullPath <- routeString
  }
  CheckUrl(fullPath)
  authHead <- paste("Token", token, sep = " ")
  list(fullPath = fullPath, authHead = authHead)
}

CheckUrl <- function(url) {
  # Maybe sure the path is a reasonable URL:
  if (grepl("//$", url) || grepl("\\s", url)) {
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
  paste(components, collapse = "/")
}

ParseReturnResponse <- function(rawReturn, ...) {
  textContent <- httr::content(rawReturn, as = "text", encoding = "UTF-8")
  if (is.na(textContent)) {
    return(NA)
  }
  OnError <- function(error) {
    stop(paste("Expected JSON, received:\n", textContent),
         call. = FALSE)
  }
  if (textContent == "") {
    ""
  } else {
    tryCatch(jsonlite::fromJSON(textContent, ...), error = OnError)
  }
}

ResponseIsRedirection <- function(rawResponse) {
  responseCategory <- httr::http_status(rawResponse)$category
  tolower(responseCategory) == 'redirection'
}
