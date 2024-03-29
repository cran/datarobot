% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Projects.R
\name{GetProject}
\alias{GetProject}
\title{Retrieve details about a specified DataRobot modeling project}
\usage{
GetProject(project)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}
}
\value{
An S3 object of class 'dataRobotProject', consisting of the following elements:
\itemize{
  \item projectId. Character string giving the unique project identifier.
  \item projectName. Character string giving the name assigned to the project.
  \item fileName. Character string giving the name of the modeling dataset for the project.
  \item stage. Character string describing the stage of the DataRobot Autopilot.
  \item autopilotMode. Numeric: 0 for fully automatic mode; 1 for semi-automatic mode; 2 for
    manual mode.
  \item created. Character string representation of the project creation time and date.
  \item target. Name of the target variable from fileName.
  \item metric. Character string specifying the metric optimized by all project models.
  \item partition. A 7-element list describing the data partitioning for model fitting
    and cross validation.
  \item advancedOptions. A 4-element list with advanced option specifications.
  \item positiveClass. Character string: name of positive class for binary response models.
  \item maxTrainPct. The maximum percentage of the project dataset that can be used without going
    into the validation data or being too large to submit any blueprint for training a project.
  \item maxTrainRows. The maximum number of rows that can be trained on without going into the
    validation data or being too large to submit any blueprint for training.
  \item holdoutUnlocked. A logical flag indicating whether the holdout dataset has been used for
    model evaluation.
  \item targetType. Character string specifying the type of modeling problem (e.g., regression or
    binary classification).
}
}
\description{
Returns a list of details about the DataRobot modeling project
specified by project.
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  GetProject(projectId)
}
}
