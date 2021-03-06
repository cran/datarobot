% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Features.R
\name{GetFeatureHistogram}
\alias{GetFeatureHistogram}
\title{Retrieve histogram plot data for a specific feature}
\usage{
GetFeatureHistogram(project, featureName, binLimit = NULL)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{featureName}{Name of the feature to retrieve. Note: DataRobot renames some features, so
the feature name may not be the one from your original data. You can use ListFeatureInfo to list
the features and check the name.}

\item{binLimit}{integer. Optional. Desired max number of histogram bins. The default is 60.}
}
\value{
list containing:
  \itemize{
    \item count numeric. The number of values in this bin's range. If a project is using weights,
      the value is equal to the sum of weights of all feature values in the bin's range.
    \item target numeric. Average of the target feature for values in this bin. It may be NULL
      if the feature is deemed uninformative, if the target has not yet been set
      (see \code{SetTarget}), or if the project is multiclass.
    \item label character. The value of the feature if categorical, otherwise the low end of the
      bin range such that the difference between two consecutive bin labels is the length of the
      bin.
  }
}
\description{
A histogram is a popular way of visual representation of a feature values
distribution in a series of bins. For categorical features every bin represents
exactly one of feature values plus the number of occurrences of that value.
For numeric features every bin represents a range of values (low end inclusive,
high end exclusive) plus the total number of occurrences of all values in this range.
In addition to that, with every bin for categorical and numeric features there is also
included a target feature average for values in that bin (though it can be missing
if the feature is deemed uninformative, if the project target has not been selected
yet using \code{SetTarget}, or if the project is a multiclass project).
}
