#' Function to set up a new DataRobot project
#'
#' This function uploads a modeling dataset defined by the dataSource parameter
#' and allows specification of the optional project name projectName. The
#' dataSource parameter can be either the name of a CSV file or a dataframe;
#' in the latter case, it is saved as a CSV file whose name is described in
#' the Details section. This function returns the projectName specified in the
#' calling sequence, the unique alphanumeric identifier projectId for the new
#' project, the name of the modeling dataset uploaded to create this project,
#' and the project creation time and date.
#'
#' The DataRobot modeling engine requires a CSV file containing the data to be
#' used in fitting models, and this has been implemented here in two ways.
#' The first and simpler is to specify dataSource as the name of this CSV file,
#' but for the convenience of those who wish to work with dataframes, this
#' function also provides the option of specifying a dataframe, which is then
#' written to a CSV file and uploaded to the DataRobot server. In this case, the
#' file name is either specified directly by the user through the saveFile
#' parameter, or indirectly from the name of the dataSource dataframe if
#' saveFile = NULL (the default).  In this second case, the file name consists
#' of the name of the dataSource dataframe with the string csvExtension appended.
#'
#' @param dataSource Either (a) the name of a CSV file or (b) a dataframe
#' (c) url to publicly available file;
#' in each case, this parameter identifies the source of the data from which
#' all project models will be built.  See Details.
#' @param projectName Optional character string specifying a project name.
#' @param maxWait The maximum time to wait for each of two steps: (1) The initial project creation
#' request, and (2) data processing that occurs after receiving the response to this initial
#' request.
#' @return This function returns a list with the following four components:
#' \describe{
#'   \item{projectName}{The name assigned to the DataRobot project}
#'   \item{projectId}{The unique alphanumeric project identifier for this DataRobot project}
#'   \item{fileName}{The name of the CSV modeling file uploaded for this project}
#'   \item{created}{Character string containing the time and date of project creation}
#' }
#' @export
#'
SetupProject <- function(dataSource, projectName = NULL,
                         maxWait = 60 * 60) {
  if (isURL(dataSource)){
    dataList <- list(projectName = projectName, url = dataSource)
  }else{
    dataPath <- DataPathFromDataArg(dataSource)
    dataList <- list(projectName = projectName, file = httr::upload_file(dataPath))
  }
  routeString <- "projects/"
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList, returnRawResponse = TRUE,
                             httr::timeout(maxWait))
  message(paste("Project", projectName,
                "creation requested, awaiting creation"))
  project <- ProjectFromAsyncUrl(httr::headers(rawReturn)$location, maxWait = maxWait)
  message(sprintf("Project %s (%s) created", project$projectId, project$projectName))
  return(as.dataRobotProjectShort(project))
}

#' Function to set up a new DataRobot project using data from MySQL table
#'
#' This function returns the projectName specified in the
#' calling sequence, the unique alphanumeric identifier projectId for the new
#' project, the name of the modeling dataset uploaded to create this project,
#' and the project creation time and date.
#'
#' @param server Character string. The address of the MySQL server
#' @param database Character string. The name of the database to use
#' @param table Character string. The name of the table to fetch
#' @param user Character string. The username to use to access the database
#' @param port Optional integer. The port to reach the MySQL server. 
#' If not specified, will use the default specified by DataRobot (3306).
#' @param prefetch Optional integer. If specified, specifies the number of rows 
#' to stream at a time from the database. If not specified, fetches all results at once. 
#' This is an optimization for reading from the database
#' @param projectName Optional character string specifying a project name.
#' @param password Optional character string. The plaintext password to be used to access MySQL database.
#' Will be first encrypted with DataRobot. Only use this or `encryptedPassword`, not both.
#' @param encryptedPassword Optional character string. The encrypted password to be used to access MySQL database. 
#' Only use this or `password`, not both.
#' @param maxWait The maximum time to wait for each of two steps: (1) The initial project creation
#' request, and (2) data processing that occurs after receiving the response to this initial
#' request.
#' @return This function returns a list with the following four components:
#' \describe{
#'   \item{projectName}{The name assigned to the DataRobot project}
#'   \item{projectId}{The unique alphanumeric project identifier for this DataRobot project}
#'   \item{fileName}{The name of the CSV modeling file uploaded for this project}
#'   \item{created}{Character string containing the time and date of project creation}
#' }
#' @export
#'
SetupProjectFromMySQL <- function(server, database, table, user, port = NULL,
                                  prefetch = NULL, projectName = NULL,
                                  password = NULL, encryptedPassword = NULL,
                                  maxWait = 60 * 60) {
  if (!is.null(password) & !is.null(encryptedPassword)){
    stop('Both password and crypted password defined, please use just one')
  }
  if (!is.null(password)){
    encryptedPassword <- encryptedString(password)
  }
  routeString <- "mysqlProjects/"
  dataList <- list(projectName = projectName,
                   server = server,
                   database = database,
                   table = table,
                   user = user,
                   encryptedPassword = encryptedPassword,
                   port = port,
                   prefetch = prefetch
  )
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList, returnRawResponse = TRUE,
                             httr::timeout(maxWait))
  message(paste("Project", projectName,
                "creation requested, awaiting creation"))
  project <- ProjectFromAsyncUrl(httr::headers(rawReturn)$location, maxWait = maxWait)
  message(sprintf("Project %s (%s) created", project$projectId, project$projectName))
  return(as.dataRobotProjectShort(project))
}

#' Function to set up a new DataRobot project using data from Oracle table
#'
#' This function returns the projectName specified in the
#' calling sequence, the unique alphanumeric identifier projectId for the new
#' project, the name of the modeling dataset uploaded to create this project,
#' and the project creation time and date.
#'
#' @param dbq Character string. tnsnames.ora entry in host:port/sid format
#' @param table Character character string. The name of the table to fetch
#' @param username Character character string. The username to use to access the database
#' @param fetchBufferSize Optional integer. If specified, specifies the size of buffer 
#' that will be used to stream data from the database. Otherwise will use DataRobot default value.
#' @param projectName Optional character string specifying a project name.
#' @param password Optional character string. The plaintext password to be used to access MySQL database.
#' Will be first encrypted with DataRobot. Only use this or `encryptedPassword`, not both.
#' @param encryptedPassword Optional character string. The encrypted password to be used to access MySQL database. 
#' Only use this or `password`, not both.
#' @param maxWait The maximum time to wait for each of two steps: (1) The initial project creation
#' request, and (2) data processing that occurs after receiving the response to this initial
#' request.
#' @return This function returns a list with the following four components:
#' \describe{
#'   \item{projectName}{The name assigned to the DataRobot project}
#'   \item{projectId}{The unique alphanumeric project identifier for this DataRobot project}
#'   \item{fileName}{The name of the CSV modeling file uploaded for this project}
#'   \item{created}{Character string containing the time and date of project creation}
#' }
#' @export
#'
SetupProjectFromOracle <- function(dbq, table, username,
                                  fetchBufferSize = NULL, projectName = NULL,
                                  password = NULL, encryptedPassword = NULL,
                                  maxWait = 60 * 60) {
  if (!is.null(password) & !is.null(encryptedPassword)){
    stop('Both password and crypted password defined, please use just one')
  }
  if (!is.null(password)){
    encryptedPassword <- encryptedString(password)
  }
  routeString <- "oracleProjects/"
  dataList <- list(projectName = projectName,
                   dbq = dbq,
                   table = table,
                   username = username,
                   encryptedPassword = encryptedPassword,
                   fetchBufferSize = fetchBufferSize
  )
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList, returnRawResponse = TRUE,
                             httr::timeout(maxWait))
  message(paste("Project", projectName,
                "creation requested, awaiting creation"))
  project <- ProjectFromAsyncUrl(httr::headers(rawReturn)$location, maxWait = maxWait)
  message(sprintf("Project %s (%s) created", project$projectId, project$projectName))
  return(as.dataRobotProjectShort(project))
}


#' Function to set up a new DataRobot project using data from PostgreSQL table
#'
#' This function returns the projectName specified in the
#' calling sequence, the unique alphanumeric identifier projectId for the new
#' project, the name of the modeling dataset uploaded to create this project,
#' and the project creation time and date.
#'
#' @param server Character string. The address of the MySQL server
#' @param database Character string. The name of the database to use
#' @param table Character string. The name of the table to fetch
#' @param username Character string. The username to use to access the database
#' @param port Optional integer. The port to reach the PostgreSQL server. 
#' If not specified, will use the default specified by DataRobot (5432).
#' @param driver Optional character string. Specify ODBC driver to use. If not specified - use DataRobot default.
#' See the values within datarobot.enums.POSTGRESQL_DRIVER
#' @param fetch Optional integer. If specified, specifies the number of rows 
#' to stream at a time from the database. If not specified, fetches all results at once. 
#' This is an optimization for reading from the database
#' @param useDeclareFetch Optional bool. On True, server will fetch result as available using DB cursor.
#' On False it will try to retrieve entire result set - not recommended for big tables.
#' If not specified - use the default specified by DataRobot.
#' @param projectName Optional character string specifying a project name.
#' @param password Optional character string. The plaintext password to be used to access MySQL database.
#' Will be first encrypted with DataRobot. Only use this or `encryptedPassword`, not both.
#' @param encryptedPassword Optional character string. The encrypted password to be used to access MySQL database. 
#' Only use this or `password`, not both.
#' @param maxWait The maximum time to wait for each of two steps: (1) The initial project creation
#' request, and (2) data processing that occurs after receiving the response to this initial
#' request.
#' @return This function returns a list with the following four components:
#' \describe{
#'   \item{projectName}{The name assigned to the DataRobot project}
#'   \item{projectId}{The unique alphanumeric project identifier for this DataRobot project}
#'   \item{fileName}{The name of the CSV modeling file uploaded for this project}
#'   \item{created}{Character string containing the time and date of project creation}
#' }
#' @export
#'
SetupProjectFromPostgreSQL <- function(server, database, table, username, port = NULL,
                                  driver = NULL, fetch = NULL, useDeclareFetch = NULL,
                                  projectName = NULL, password = NULL,
                                  encryptedPassword = NULL, maxWait = 60 * 60) {
  if (!is.null(password) & !is.null(encryptedPassword)){
    stop('Both password and crypted password defined, please use just one')
  }
  if (!is.null(password)){
    encryptedPassword <- encryptedString(password)
  }
  routeString <- "postgresqlProjects/"
  dataList <- list(projectName = projectName,
                   server = server,
                   database = database,
                   table = table,
                   username = username,
                   encryptedPassword = encryptedPassword,
                   port = port,
                   fetch = fetch,
                   driver = driver,
                   useDeclareFetch = useDeclareFetch
  )
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList, returnRawResponse = TRUE,
                             httr::timeout(maxWait))
  message(paste("Project", projectName,
                "creation requested, awaiting creation"))
  project <- ProjectFromAsyncUrl(httr::headers(rawReturn)$location, maxWait = maxWait)
  message(sprintf("Project %s (%s) created", project$projectId, project$projectName))
  return(as.dataRobotProjectShort(project))
}

#' Function to set up a new DataRobot project using datasource on a WebHDFS server
#'
#' This function returns the projectName specified in the
#' calling sequence, the unique alphanumeric identifier projectId for the new
#' project, the name of the modeling dataset uploaded to create this project,
#' and the project creation time and date.
#'
#' @param url Character string. The location of the WebHDFS file, 
#' both server and full path. Per the DataRobot specification, must begin with `hdfs://`
#' @param port Optional int. The port to use. If not specified, will default to the server default (50070)
#' @param projectName Optional character string specifying a project name.
#' @param maxWait The maximum time to wait for each of two steps: (1) The initial project creation
#' request, and (2) data processing that occurs after receiving the response to this initial
#' request.
#' @return This function returns a list with the following four components:
#' \describe{
#'   \item{projectName}{The name assigned to the DataRobot project}
#'   \item{projectId}{The unique alphanumeric project identifier for this DataRobot project}
#'   \item{fileName}{The name of the CSV modeling file uploaded for this project}
#'   \item{created}{Character string containing the time and date of project creation}
#' }
#' @export
#'
SetupProjectFromHDFS <- function(url, port = NULL, projectName = NULL, maxWait = 60 * 60) {
  routeString <- "hdfsProjects/"
  dataList <- list(projectName = projectName,
                   port = port,
                   url = url
  )
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList, returnRawResponse = TRUE,
                             httr::timeout(maxWait))
  message(paste("Project", projectName,
                "creation requested, awaiting creation"))
  project <- ProjectFromAsyncUrl(httr::headers(rawReturn)$location, maxWait = maxWait)
  message(sprintf("Project %s (%s) created", project$projectId, project$projectName))
  return(as.dataRobotProjectShort(project))
}


#' Retrieve a project from the project-creation URL
#'
#' If project creation times out, the error message includes a URL corresponding to the project
#' creation task. That URL can be passed to this function (which will return the completed project
#' details when finished) to resume waiting for project creation.
#'
#' @param asyncUrl The temporary status URL
#' @param maxWait The maximum time to wait (in seconds) for project creation before aborting.
#' @export
#'
ProjectFromAsyncUrl <- function(asyncUrl, maxWait = 60) {
  timeoutMessage <-
    paste(sprintf("Project creation did not complete before timeout (%ss).", maxWait),
          "To query its status and (if complete) retrieve the completed project, use:\n  ",
          sprintf("%s('%s')", "ProjectFromAsyncUrl", asyncUrl))
  projectInfo <- tryCatch(WaitForAsyncReturn(asyncUrl,
                                               addUrl = FALSE,
                                               maxWait = maxWait,
                                               failureStatuses = "ERROR"),
                            AsyncTimeout = function(e) stop(timeoutMessage))
  return(list(projectName = projectInfo$projectName,
              projectId = projectInfo$id,
              fileName = projectInfo$fileName,
              created = projectInfo$created))
}

DataPathFromDataArg <- function(dataSource, saveFile = NULL, csvExtension = NULL) {
  # Can remove last two arguments after 2.3

  #  Verify that newdata is either an existing datafile or a dataframe
  #    If a dataframe, save as a CSV file
  #    If neither an existing datafile nor a dataframe, halt with error
  #
  if (is(dataSource, "character")) {
    if (file.exists(dataSource)) {
      dataPath <- dataSource
    } else {
      errorMsg <- paste("No file named", dataSource,
                        "exists in the working directory", getwd())
      stop(strwrap(errorMsg))
    }
  } else {
    if (is.data.frame(dataSource)) {
      #
      #  If dataSource is a dataframe, save as a CSV file
      #
      if (is.null(saveFile)) {
        dataPath <- tempfile(fileext = "_autoSavedDF.csv")
      } else {
        dataPath <- saveFile
      }
      write.csv(dataSource, dataPath, row.names = FALSE)
    } else {
      errorMsg <- paste(deparse(substitute(dataSource)),
                        "is not a valid data file name or dataframe")
      stop(strwrap(errorMsg))
    }
  }
  return(dataPath)
}

encryptedString <- function(plainText, maxWait = 60 * 10){
  routeString <- "stringEncryptions/"
  dataList <- list(plainText = plainText)
  ret <- DataRobotPOST(routeString, addUrl = TRUE, body = dataList,
                             returnRawResponse = FALSE, httr::timeout(maxWait))
  return(ret$cipherText)
}

isURL <- function(dataSource) {
  if (class(dataSource) == 'character' && substr(dataSource, 1, 4) == 'http'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


as.dataRobotProjectShort <- function(inProject){
  elements <- c("projectName",
                "projectId",
                "fileName",
                "created")
  outProject <- inProject[elements]
  return(outProject)
}
