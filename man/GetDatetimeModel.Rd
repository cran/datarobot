% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Models.R
\name{GetDatetimeModel}
\alias{GetDatetimeModel}
\title{Retrieve the details of a specified datetime model.}
\usage{
GetDatetimeModel(project, modelId)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{modelId}{character. Unique alphanumeric identifier for the model of interest.}
}
\value{
An S3 object of class `dataRobotDatetimeModel`, which is a list
with the following components:
\itemize{
  \item featurelistId character. Unique alphanumeric identifier for the featurelist on
    which the model is based.
  \item processes character. Vector with components describing preprocessing; may include
    `modelType`.
  \item featurelistName character. The name of the featurelist on which the model is based.
  \item projectId character. The unique alphanumeric identifier for the project.
  \item samplePct numeric. Percentage of the dataset used to form the training dataset for
    model fitting.
  \item isFrozen logical. Is model created with frozen tuning parameters?
  \item modelType character. A description of the model.
  \item metrics list. List with one element for each valid metric associated with the model.
    Each element is a list with elements for each possible evaluation type (holdout, validation,
    and crossValidation).
  \item modelCategory character. The model category (e.g., blend, model).
  \item blueprintId character. The unique DataRobot blueprint identifier on which
    the model is based.
  \item modelId character. The unique alphanumeric model identifier.
  \item modelNumber. integer. The assigned model number.
  \item projectName character. Optional description of project defined by projectId.
  \item projectTarget character. The target variable predicted by all models in the project.
  \item projectMetric character. The fitting metric optimized by all project models.
  \item trainingRowCount integer. The number of rows of the project dataset used in training
    the model. In a datetime partitioned project, if specified, defines the number of
     rows used to train the model and evaluate backtest scores; if unspecified, either
     \code{trainingDuration} or \code{trainingStartDate} and \code{trainingEndDate} was used to
     determine that instead.
  \item trainingDuration character. Only present for models in datetime partitioned projects.
     If specified, a duration string specifying the duration spanned by the data used to train
     the model and evaluate backtest scores.
  \item trainingStartDate character. Only present for frozen models in datetime partitioned
     projects. If specified, the start date of the data used to train the model.
  \item trainingEndDate character. Only present for frozen models in datetime partitioned
     projects. If specified, the end date of the data used to train the model.
  \item backtests list. What data was used to fit each backtest, the score for the
    project metric, and why the backtest score is unavailable if it is not provided.
  \item dataSelectionMethod character. Which of trainingRowCount, trainingDuration,
    or trainingStartDate and trainingEndDate were used to determine the data used to fit the
    model. One of "rowCount", "duration", or "selectedDateRange".
  \item trainingInfo list. Which data was used to train on when scoring the holdout and
    making predictions. trainingInfo will have the following keys: `holdoutTrainingStartDate`,
    `holdoutTrainingDuration`, `holdoutTrainingRowCount`, `holdoutTrainingEndDate`,
    `predictionTrainingStartDate`, `predictionTrainingDuration`,
    `predictionTrainingRowCount`, `predictionTrainingEndDate`.  Start and end dates will be
    datetime string, durations will be duration strings, and rows will be integers.
  \item holdoutScore numeric. The score against the holdout, if available and the holdout
    is unlocked, according to the project metric.
  \item holdoutStatus character. The status of the holdout score, e.g. "COMPLETED",
    "HOLDOUT_BOUNDARIES_EXCEEDED".
  \item effectiveFeatureDerivationWindowStart integer. Only available for time series projects.
    How many timeUnits into the past relative to the forecast point the user needs to provide
    history for at prediction time. This can differ from the `featureDerivationWindowStart` set
    on the project due to the differencing method and period selected, or if the model is a time
    series native model such as ARIMA. Will be a negative integer in time series projects and
    `NULL` otherwise.
  \item effectiveFeatureDerivationWindowEnd integer. Only available for time series projects.
    How many timeUnits into the past relative to the forecast point the feature derivation window
    should end. Will be a non-positive integer in time series projects and `NULL` otherwise.
  \item forecastWindowStart integer. Only available for time series projects. How many timeUnits
    into the future relative to the forecast point the forecast window should start. Note that
    this field will be the same as what is shown in the project settings. Will be a non-negative
    integer in time series projects and `NULL` otherwise.
  \item forecastWindowEnd integer. Only available for time series projects. How many timeUnits
    into the future relative to the forecast point the forecast window should end. Note that this
    field will be the same as what is shown in the project settings. Will be a non-negative
    integer in time series projects and `NULL` otherwise.
  \item windowsBasisUnit character. Only available for time series projects. Indicates which unit
    is the basis for the feature derivation window and the  forecast window. Note that this field
    will be the same as what is shown in the project settings. In time series projects, will be
    either the detected time unit or "ROW", and `NULL` otherwise.
}
}
\description{
This function returns a DataRobot S3 object of class
dataRobotDatetimeModel for the model defined by project and modelId.
}
\details{
If the project does not use datetime partitioning an error will occur.
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  modelId <- "5996f820af07fc605e81ead4"
  GetDatetimeModel(projectId, modelId)
}
}
