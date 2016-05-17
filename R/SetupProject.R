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
#' @param dataSource Either (a) the name of a CSV file or (b) a dataframe;
#' in either case, this parameter identifies the source of the data from which
#' all project models will be built.  See Details.
#' @param projectName Optional character string specifying a project name;
#' default is "None".
#' @param saveFile If dataSource is a dataframe, it is written to a CSV file
#' that can be uploaded to the DataRobot server, and this character string
#' specifies the name of this file.  If saveFile = NULL (the default value),
#' the required CSV file name is constructed by appending the string
#' csvExtension to the name of the dataSource dataframe.  This parameter is
#' ignored if dataSource specifies a CSV file name. (This argument is deprecated.)
#' @param csvExtension If dataSource is a dataframe and saveFile = NULL
#' (the default value), the contents of the dataframe are written to a CSV
#' file whose name is constructed by appending this string to the name of
#' the dataSource dataframe. This parameter is ignored if dataSource
#' specifies a CSV file name. (This argument is deprecated.)
#' @param maxWait This process creates a project on the DataRobot server
#' asynchronously, and this function waits for that process to complete
#' before continuing. You can set the maximum wait time (in seconds) before
#' this function aborts.
#' @return This function returns a list with the following four components:
#' \describe{
#'   \item{projectName}{The name assigned to the DataRobot project}
#'   \item{projectId}{The unique alphanumeric project identifier for this DataRobot project}
#'   \item{fileName}{The name of the CSV modeling file uploaded for this project}
#'   \item{created}{Character string containing the time and date of project creation}
#' }
#' @export
#'
SetupProject <- function(dataSource, projectName = "None",
                         saveFile = NULL, csvExtension = "_autoSavedDF.csv",
                         maxWait = 60) {
  if ("cvsExtension" %in% names(match.call())) {
    Deprecated("csvExtension argument", "2.1", "2.3")
  }
  if ("saveFile" %in% names(match.call())) {
    Deprecated("saveFile argument", "2.1", "2.3")
  }
  dataPath <- DataPathFromDataArg(dataSource, saveFile, csvExtension)
  projectInfo <- CreateAndUpload(dataPath, projectName, maxWait = maxWait)

  return(list(projectName = projectInfo$projectName,
              projectId = projectInfo$id,
              fileName = projectInfo$fileName,
              created = projectInfo$created))
}

DataPathFromDataArg <- function(dataSource, saveFile, csvExtension) {
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
