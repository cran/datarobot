% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RequestPrimeModel.R
\name{RequestPrimeModel}
\alias{RequestPrimeModel}
\title{Request training for a DataRobot Prime model using a specified ruleset}
\usage{
RequestPrimeModel(project, ruleset)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{ruleset}{list. A list specifying ruleset parameters (see GetRulesets)}
}
\value{
job Id
}
\description{
Training a model using a ruleset is a necessary prerequisite for being able to download the code
for a ruleset.
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  modelId <- "5996f820af07fc605e81ead4"
  rulesets <- GetRulesets(projectId, modelId)
  ruleset <- rulesets[[1]]
  RequestPrimeModel(projectId, ruleset)
}
}
