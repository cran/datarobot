#' Request Feature Impact to be computed.
#'
#' This adds a Feature Impact job to the project queue.

#' @param model The model for which you want to compute Feature Impact, e.g. from the list of models
#' returned by GetAllModels(project)
#'
#' @return
#' A job ID (character)
#'
#'
#' @examples
#' \dontrun{
#' model <- GetAllModels(project)[[1]]
#' featureImpactJobId <- RequestFeatureImpact(model)
#' featureImpact <- GetFeatureImpactForJobId(project, featureImpactJobId)
#' }
#'
#' @export
RequestFeatureImpact <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "featureImpact")
  rawReturn <- DataRobotPOST(routeString, addUrl = TRUE, body = list(),
                             returnRawResponse = TRUE)
  return(JobIdFromResponse(rawReturn))
}

FeatureImpactFromResponseList <- function(response) {
  featureImpactDF <- response$featureImpacts
  expectedKeys <- c('impactNormalized', 'impactUnnormalized', 'featureName')
  missingKeys <- setdiff(expectedKeys, names(featureImpactDF))
  if (length(missingKeys) > 0) {
    stop(sprintf("Expected keys were missing from Feature Impact data received: %s",
         missingKeys))
  }
  return(as.dataRobotFeatureImpact(featureImpactDF))
}

#' Retrieve completed Feature Impact results given a model
#'
#' This will only succeed if the Feature Impact computation has completed.
#'
#' Feature Impact is computed for each column by creating new data with that column randomly
#' permuted (but the others left unchanged), and seeing how the error metric score for the
#' predictions is affected. The 'impactUnnormalized' is how much worse the error metric score is
#' when making predictions on this modified data. The 'impactNormalized' is normalized so that the
#' largest value is 1. In both cases, larger values indicate more important features. Elsewhere this
#' technique is sometimes called 'Permutation Importance'.
#'
#' @param model The model for which you want to retrieve Feature Impact
#'
#' @return
#' A data frame with the following columns:
#' \describe{
#'   \item{featureName}{The name of the feature}
#'   \item{impactNormalized}{The normalized impact score (largest value is 1)}
#'   \item{impactUnnormalized}{The unnormalized impact score}
#'   }
#'
#' @examples
#' \dontrun{
#' model <- GetAllModels(project)[[1]]
#' featureImpactJobId <- RequestFeatureImpact(model)
#' # Note: This will only work after the feature impact job has completed. Use
#' #       GetFeatureImpactFromIobId to automatically wait for the job.\
#' featureImpact <- GetFeatureImpactForModel(model)
#' }
#' @export

GetFeatureImpactForModel <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "featureImpact")
  response <- DataRobotGET(routeString, addUrl = TRUE)
  return(FeatureImpactFromResponseList(response))

}

#' Retrieve completed Feature Impact results given a job ID
#'
#' This will wait for the Feature Impact job to be completed (giving an error if the job is not a
#' Feature Impact job and an error if the job errors).
#'
#' @param project The project the Feature Impact is part of.
#' @param jobId The ID of the job (e.g. as returned from RequestFeatureImpact)
#' @param maxWait Integer, The maximum time (in seconds) to wait for the model job to complete
#' @return
#' A data frame with the following columns:
#' \describe{
#'   \item{featureName}{The name of the feature}
#'   \item{impactNormalized}{The normalized impact score (largest value is 1)}
#'   \item{impactUnnormalized}{The unnormalized impact score}
#'   }
#'
#' @examples
#' \dontrun{
#' model <- GetAllModels(project)[[1]]
#' featureImpactJobId <- RequestFeatureImpact(model)
#' featureImpact <- GetFeatureImpactForJobId(project, featureImpactJobId)
#' }
#'
#' @export
GetFeatureImpactForJobId <- function(project, jobId, maxWait = 60) {
  # Gets generic job, including link to completed resource (as completedUrl) if available.
  # For now, this is just an internal function used to support GetFeatureImpactResults
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  job <- DataRobotGET(routeString, addUrl = TRUE, config = httr::config(followlocation = 0))
  if (job$jobType != JobType$FeatureImpact) {
    stop(sprintf("Job %s is of type: %s. Can only get Feature Impact for jobs of type: %s.",
                 jobId, job$jobType, JobType$FeatureImpact))
  }
  featureImpactResponse <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                              failureStatuses = JobFailureStatuses)
  return(FeatureImpactFromResponseList(featureImpactResponse))
}


as.dataRobotFeatureImpact <- function(inList){
  elements <- c("featureName",
                "name",
                "impactNormalized",
                "impactUnnormalized")
  return(ApplySchema(inList, elements))
}
