% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FeatureAssociations.R
\name{GetFeatureAssociationMatrix}
\alias{GetFeatureAssociationMatrix}
\title{Get pairwise feature association statistics for a project's informative features}
\usage{
GetFeatureAssociationMatrix(project, associationType, metric)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{associationType}{character. The type of association, must be either "association"
or "correlation".}

\item{metric}{character. The specified association metric, must be one of "mutualInfo",
"cramersV", "spearman", "pearson", or "tau".}
}
\value{
A list with two items:
\itemize{
  \item features data.frame. A data.frame containing the following info for each feature:
    \itemize{
      \item alphabeticSortIndex integer. A number representing the alphabetical order of this
        feature compared to the other features in this dataset.
      \item feature character. The name of the feature.
      \item importanceSortIndex integer. A number ranking the importance of this feature compared
        to the other features in this dataset.
      \item strengthSortIndex integer. A number ranking the strength of this feature compared to
        the other features in this dataset.
    }
  \item strengths data.frame. A data.frame of pairwise strength data, with the following info:
    \itemize{
      \item feature1 character. The name of the first feature.
      \item feature2 character. The name of the second feature.
      \item statistic numeric. Feature association statistics for `feature1` and `feature2`.
    }
}
}
\description{
Get pairwise feature association statistics for a project's informative features
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  GetFeatureAssociationMatrix(projectId)
}
}
