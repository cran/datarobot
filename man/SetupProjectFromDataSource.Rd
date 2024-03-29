% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SetupProject.R
\name{SetupProjectFromDataSource}
\alias{SetupProjectFromDataSource}
\title{Create a project from a data source.}
\usage{
SetupProjectFromDataSource(
  dataSourceId,
  username,
  password,
  projectName = NULL,
  maxWait = 60 * 60
)
}
\arguments{
\item{dataSourceId}{character. The ID of the data source to create a project from.}

\item{username}{character. The username to use for authentication to the database.}

\item{password}{character. The password to use for authentication to the database.}

\item{projectName}{character. Optional. String specifying a project name.
The password is encrypted at server side and never saved or stored.}

\item{maxWait}{integer. The maximum time to wait for each of two steps: (1) The initial
project creation request, and (2) data processing that occurs after receiving the response
to this initial request.}
}
\value{
A named list that contains:
\describe{
  \item{projectName}{character. The name assigned to the DataRobot project}
  \item{projectId}{character. The unique alphanumeric project identifier for this DataRobot
    project}
  \item{fileName}{character. The name of the CSV modeling file uploaded for this project}
  \item{created}{character. The time and date of project creation}
}
}
\description{
Create a project from a data source.
}
\examples{
\dontrun{
 dataSourceId <- "5c1303269300d900016b41a7"
 SetupProjectFromDataSource(dataSourceId, username = "username", password = "hunter1",
                            projectName = "My Project")
}
}
