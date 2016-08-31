#' Establish a connection to the DataRobot modeling engine
#'
#' This function initializes a DataRobot session. If a (YAML) config file (with keys for endpoint
#' and token) is placed at $HOME/.config/datarobot/drconfig.yaml, then we
#' attempt to establish a connection to DataRobot when the package loads, so
#' (if successful) this function does not need to be called.
#'
#' The function creates the environment variables "DataRobot_URL" and "DataRobot_Token" used by
#' other functions to access the DataRobot modeling engine.
#' 
#'
#' @param endpoint URL specifying the DataRobot server to be used. 
#' It depends on DataRobot modeling engine implementation (cloud-based, on-prem...) you are using. 
#' Contact your DataRobot admin for endpoint to use and to turn on API access to your account. 
#' The endpoint for DataRobot cloud accounts is https://app.datarobot.com/api/v2
#' @param token DataRobot API access token. It is unique for each DataRobot modeling engine account and can be accessed using DataRobot webapp in Account profile section.
#' @param configPath Path to YAML config file specifying configuration
#' (token and endpoint)
#' @param username (no longer supported)
#' @param password (no longer supported)
#' @export
#'
ConnectToDataRobot <- function(endpoint = NULL, token = NULL,
                               username=NULL, password=NULL,
                               configPath = NULL
) {
  #  If the user provides a token, save it to the environment
  #  variable DataRobot_Token and call GetProjectList to verify it

  haveToken <- !is.null(token)
  haveUsernamePassword <- (!is.null(username)) || (!is.null(password))
  haveConfigPath <- !is.null(configPath)
  numAuthMethodsProvided <- haveToken + haveConfigPath + haveUsernamePassword
  if (numAuthMethodsProvided > 1) {
    stop("Please provide only one of: config file or token.")
  } else if (haveToken) {
    return(ConnectWithToken(endpoint, token))
  } else if (haveUsernamePassword) {
    return(ConnectWithUsernamePassword(endpoint, username, password))
  } else if (haveConfigPath) {
    ConnectWithConfigFile(configPath)
  } else {
    errorMsg <- "No authentication method provided."
    stop(strwrap(errorMsg), call. = FALSE)
  }
}

GetDefaultConfigPath <- function() {
  return(file.path(Sys.getenv("HOME"), ".config", "datarobot", "drconfig.yaml"))
}

ConnectWithConfigFile <- function(configPath) {
  config <- yaml::yaml.load_file(configPath)
  if ("configPath" %in% names(config)) {
    stop("Please do not specify the config path in the config file itself.")
  }
  do.call(ConnectToDataRobot, config)
}

ConnectWithToken <- function(endpoint, token) {
  #
  authHead <- paste("Token", token, sep = " ")
  #
  #  This statement gives an absolute_paths_linter false positive:
  #
  fullURL <- paste(endpoint, "/projects/", sep = "")  # nolint
  rawReturn <- httr::GET(fullURL, DataRobotAddHeaders(Authorization = authHead))
  StopIfDenied(rawReturn)
  SaveConnectionEnvironmentVars(endpoint, token)
}

ConnectWithUsernamePassword <- function(endpoint, username, password) {
  stop("Using your username/password to authenticate with the DataRobot API is no longer supported.
       Please supply your API token instead. You can find your API token in your account profile in
       the DataRobot web app.")
}

SaveConnectionEnvironmentVars <- function(endpoint, token) {
  message("Authentication token saved")
  Sys.setenv(DataRobot_URL = endpoint)
  Sys.setenv(DataRobot_Token = token)
}

StopIfDenied <- function(rawReturn) {
  returnStatus <- httr::status_code(rawReturn)
  if (returnStatus >= 400) {
    response <- unlist(ParseReturnResponse(rawReturn))
    errorMsg <- paste("Authorization request denied: ", response)
    stop(strwrap(errorMsg), call. = FALSE)
  }
}
