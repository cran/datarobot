% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RequestApproximation.R
\name{RequestApproximation}
\alias{RequestApproximation}
\title{Request an approximation of a model using DataRobot Prime}
\usage{
RequestApproximation(project, modelId)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{modelId}{character. Unique alphanumeric identifier for the model of interest.}
}
\value{
job Id
}
\description{
This function will create several rulesets that approximate the specified model.
The code used in the approximation can be downloaded to be run locally.
Currently only Python and Java downloadable code is available
}
\details{
General workflow of creating and downloading Prime code may look like following:
RequestApproximation - create several rulesets that approximate the specified model
GetRulesets - list all rulesets created for the parent model
RequestPrimeModel - create Prime model for specified ruleset (use one of rulesets return by
  GetRulesets)
GetPrimeModelFromJobId - get PrimeModelId using JobId returned by RequestPrimeModel
CreatePrimeCode - create code for one of available Prime models
GetPrimeFileFromJobId - get PrimeFileId using JobId returned by CreatePrimeCode
DownloadPrimeCode - download specified Prime code file
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  modelId <- "5996f820af07fc605e81ead4"
  RequestApproximation(projectId, modelId)
}
}
