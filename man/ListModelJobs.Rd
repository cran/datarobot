% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ListModelJobs.R
\name{ListModelJobs}
\alias{ListModelJobs}
\title{Retrieve status of Autopilot modeling jobs that are not complete}
\usage{
ListModelJobs(project, status = NULL)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{status}{character. The status of the desired jobs: one of JobStatus$Queue,
JobStatus$InProgress, orJobStatus$Error. If NULL (default), queued and inprogress jobs
are returned.}
}
\value{
A list of lists with one element for each modeling task
in the group being queried; if there are no tasks in the class
being queried, an empty list is returned. If the group is not empty,
a list is returned with the following nine elements:
\itemize{
  \item status. Prediction job status; an element of JobStatus, e.g. JobStatus$Queue.
  \item processes. List of character vectors describing any preprocessing applied.
  \item projectId. Character string giving the unique identifier for the project.
  \item modelId character. The unique identifier for the related model.
  \item samplePct. Numeric: the percentage of the dataset used for model building.
  \item modelType. Character string specifying the model type.
  \item modelCategory. Character string: what kind of model this is - 'prime' for DataRobot
    Prime models, 'blend' for blender models, and 'model' for other models.
  \item featurelistId. Character string: id of the featurelist used in fitting the model.
  \item blueprintId. Character string: id of the DataRobot blueprint on which the model is based.
  \item modelJobId. Character: id of the job.
  \item isBlocked logical. If TRUE, the job is blocked (cannot be executed) until its
    dependencies are resolved.
}
}
\description{
This function requests information on DataRobot Autopilot modeling
tasks that are not complete, for one of three reasons: the task is
running and has not yet completed; the task is queued and has not
yet been started; or, the task has terminated due to an error.
}
\details{
The jobStatus variable specifies which of the three groups of
modeling tasks is of interest. Specifically, if jobStatus has the
value 'inprogress', the request returns information about modeling
tasks that are running but not yet complete; if jobStatus has the
value 'queue', the request returns information about modeling tasks
that are scheduled to run but have not yet started; if jobStatus
has the value 'error', the request returns information about modeling
tasks that have terminated due to an error. By default, jobStatus is
NULL, which means jobs with status "inprogress" or "queue" are returned,
but not those with status "error".
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  ListModelJobs(projectId)
}
}
