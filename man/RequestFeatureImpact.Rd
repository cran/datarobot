% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FeatureImpact.R
\name{RequestFeatureImpact}
\alias{RequestFeatureImpact}
\title{Request Feature Impact to be computed.}
\usage{
RequestFeatureImpact(model, rowCount = NULL)
}
\arguments{
\item{model}{character. The model for which you want to compute Feature Impact, e.g.
from the list of models returned by \code{ListModels(project)}.}

\item{rowCount}{numeric. The sample size to use for Feature Impact computation. It is
possible to re-compute Feature Impact with a different row count.}
}
\value{
A job ID (character)
}
\description{
This adds a Feature Impact job to the project queue.
}
\examples{
\dontrun{
  model <- ListModels(project)[[1]]
  featureImpactJobId <- RequestFeatureImpact(model)
  featureImpact <- GetFeatureImpactForJobId(project, featureImpactJobId)
}
}
