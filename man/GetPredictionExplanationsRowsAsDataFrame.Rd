% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PredictionExplanations.R
\name{GetPredictionExplanationsRowsAsDataFrame}
\alias{GetPredictionExplanationsRowsAsDataFrame}
\title{Retrieve all prediction explanations rows and return them as a data frame}
\usage{
GetPredictionExplanationsRowsAsDataFrame(
  project,
  predictionExplanationId,
  excludeAdjustedPredictions = TRUE,
  batchSize = NULL
)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{predictionExplanationId}{character. Id of the prediction explanations.}

\item{excludeAdjustedPredictions}{logical. Optional. Set to FALSE to include adjusted
predictions, which are predictions adjusted by an exposure column. This is only relevant for
projects that use an exposure column.}

\item{batchSize}{integer. Optional. Maximum number of prediction explanations rows to
retrieve per request}
}
\value{
data frame with following columns:
\itemize{
  \item rowId integer. Row id from prediction dataset.
  \item prediction numeric. The output of the model for this row (numeric prediction for
    regression problem, predicted class for classification problem).
  \item class1Label character. Label of class 0. Available only for classification
    problem.
  \item class1Probability numeric. Predicted probability of class 0. Available only for
    classification problem.
  \item class2Label character. Label of class 1. Available only for classification
    problem.
  \item class2Probability numeric. Predicted probability of class 1. Available only for
    classification problem.
  \item explanation1FeatureName character. The name of the feature contributing to the
    prediction.
  \item explanation1FeatureValue character. the value the feature took on for this row.
  \item explanation1QualitativeStrength numeric. How strongly the feature affected the
    prediction.
  \item explanation1Strength character. A human-readable description of how strongly the
    feature affected the prediction (e.g. '+++', '--', '+').
  \item explanation1Label character. Describes what output was driven by this prediction
    explanation.
  For regression projects, it is the name of the target feature.  For classification projects,
  it is the class whose probability increasing would correspond to a positive strength of this.
  \item explanationNFeatureName character. The name of the feature contributing to the
    prediction.
  \item explanationNFeatureValue character. The value the feature took on for this row.
  \item explanationNQualitativeStrength numeric. How strongly the feature affected the
    prediction.
  \item explanationNStrength character. A human-readable description of how strongly the
    feature affected the prediction (e.g. '+++', '--', '+').
  \item explanationNLabel character. Describes what output was driven by this prediction
    explanation.
  For regression projects, it is the name of the target feature.  For classification projects,
  it is the class whose probability increasing would correspond to a positive strength of this.
  \item explanationNFeatureName. Character string the name of the feature contributing to the
    prediction.
  }
}
\description{
There are some groups of columns whose appearance depends on the exact
contents of the project dataset. For classification projects,
columns "classNLabel", 'classNProbability", "classNLabel", "classNProbability"
will appear corresponding to each class within the target;
these columns will not appear for regression projects.
Columns like "explanationNLabel" will appear corresponding to each included prediction
explanation in the row. In both cases, the value of N will start at 1 and count up.
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  modelId <- "5996f820af07fc605e81ead4"
  datasets <- ListPredictionDatasets(projectId)
  dataset <- datasets[[1]]
  datasetId <- dataset$id
  model <- GetModel(projectId, modelId)
  jobId <- RequestPredictionExplanations(model, datasetId)
  predictionExplanationId <- GetPredictionExplanationsMetadataFromJobId(projectId, jobId)$id
  GetPredictionExplanationsRowsAsDataFrame(projectId, predictionExplanationId)
}
}
