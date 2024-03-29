% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FeatureImpact.R
\name{GetFeatureImpactForModel}
\alias{GetFeatureImpactForModel}
\title{Retrieve completed Feature Impact results given a model}
\usage{
GetFeatureImpactForModel(model)
}
\arguments{
\item{model}{character. The model for which you want to retrieve Feature Impact.}
}
\value{
A data frame with the following columns:
\itemize{
  \item featureName character. The name of the feature.
  \item impactNormalized numeric. The normalized impact score (largest value is 1).
  \item impactUnnormalized numeric. The unnormalized impact score.
  \item redundantWith character. A feature that makes this feature redundant, or \code{NA}
    if the feature is not redundant.
  }
}
\description{
This will only succeed if the Feature Impact computation has completed.
}
\details{
Feature Impact is computed for each column by creating new data with that column randomly
permuted (but the others left unchanged), and seeing how the error metric score for the
predictions is affected. The 'impactUnnormalized' is how much worse the error metric score is
when making predictions on this modified data. The 'impactNormalized' is normalized so that the
largest value is 1. In both cases, larger values indicate more important features. Elsewhere this
technique is sometimes called 'Permutation Importance'.

Feature impact also runs redundancy detection, which detects if some features are redundant with
higher importance features. Note that some types of projects, like multiclass, do not run
redundancy detection. This function will generate a warning if redundancy detection was not run.
}
\examples{
\dontrun{
  model <- ListModels(project)[[1]]
  featureImpactJobId <- RequestFeatureImpact(model)
  # Note: This will only work after the feature impact job has completed. Use
  #       GetFeatureImpactFromJobId to automatically wait for the job.\
  featureImpact <- GetFeatureImpactForModel(model)
}
}
