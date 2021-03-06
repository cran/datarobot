% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Blueprints.R
\name{GetBlueprintDocumentation}
\alias{GetBlueprintDocumentation}
\title{Get documentation for tasks used in the blueprint}
\usage{
GetBlueprintDocumentation(project, blueprintId)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{blueprintId}{character. Id of blueprint to retrieve.}
}
\value{
list with following components
\describe{
  \item{task}{Character string name of the task described in document}
  \item{description}{Character string task description}
  \item{title}{Character string title of document}
  \item{parameters}{List of parameters that task can received in human-readable
  format with following components: name, type, description}
  \item{links}{List of external lines used in document with following components: name, url}
  \item{references}{List of references used in document with following components: name, url}
}
}
\description{
Get documentation for tasks used in the blueprint
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  modelId <- "5996f820af07fc605e81ead4"
  model <- GetModel(projectId, modelId)
  blueprintId <- model$blueprintId
  GetBlueprintDocumentation(projectId, blueprintId)
}
}
