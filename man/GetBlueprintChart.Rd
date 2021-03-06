% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Blueprints.R
\name{GetBlueprintChart}
\alias{GetBlueprintChart}
\title{Retrieve a blueprint chart}
\usage{
GetBlueprintChart(project, blueprintId)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{blueprintId}{character. Id of blueprint to retrieve.}
}
\value{
List with the following two components:
\itemize{
  \item nodes. list each element contains information about one node
     of a blueprint : id and label.
  \item edges. Two column matrix, identifying blueprint nodes connections.
}
}
\description{
A Blueprint chart can be used to understand data flow in blueprint.
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  modelId <- "5996f820af07fc605e81ead4"
  model <- GetModel(projectId, modelId)
  blueprintId <- model$blueprintId
  GetBlueprintChart(projectId, blueprintId)
}
}
