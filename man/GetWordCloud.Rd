% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Models.R
\name{GetWordCloud}
\alias{GetWordCloud}
\title{Retrieve word cloud data for a model.}
\usage{
GetWordCloud(project, modelId, excludeStopWords = FALSE)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{modelId}{character. Unique alphanumeric identifier for the model of interest.}

\item{excludeStopWords}{logical. Optional. Set to TRUE if you want stopwords filtered out the
response.}
}
\value{
data.frame with the following components:
\describe{
  \item{ngram}{Character string: word or ngram value}
  \item{coefficient}{Numerical:  value from [-1.0, 1.0] range, describes effect of this ngram on
  the target. A large negative value means a strong effect toward the negative class in
  classification projects and a smaller predicted target value in regression projects.
  A large positive value means a strong effect toward the positive class and a larger
  predicted target value respectively}
  \item{count}{Integer: number of rows in the training sample where this ngram appears}
  \item{frequency}{Numerical: value from (0.0, 1.0] range, frequency of this ngram
  relative to the most frequent ngram}
  \item{isStopword}{Logical: true for ngrams that DataRobot evaluates as stopwords}
}
}
\description{
Retrieve word cloud data for a model.
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  modelId <- "5996f820af07fc605e81ead4"
  GetWordCloud(projectId, modelId)
}
}
