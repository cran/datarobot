#' Request reason codes initialization for specified model (deprecated)
#'
#' Use \link{RequestPredictionExplanationsInitialization} instead.
#'
#' @inheritParams DeleteModel
#' @return job Id
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   RequestReasonCodesInitialization(model)
#' }
#' @export
RequestReasonCodesInitialization <- function(model) {
  Deprecated(paste("RequestReasonCodesInitialization (use",
                   "RequestPredictionExplanationsInitialization instead)"), "2.13", "2.15")
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  modelName <- validModel$modelType
  routeString <- UrlJoin("projects", projectId, "models", modelId, "reasonCodesInitialization")
  rawResponse <- DataRobotPOST(routeString, returnRawResponse = TRUE)
  message(paste("Reason codes initialization requested for model", modelName,
                "(modelId = ", modelId, ")"))
  JobIdFromResponse(rawResponse)
}

#' Retrieve the reason codes initialization for a model (deprecated).
#'
#' Use \link{GetPredictionExplanationsInitialization} instead.
#'
#' @inheritParams DeleteModel
#' @return A named list which contains:
#' \itemize{
#'   \item projectId. Character id of the project the feature belonges to.
#'   \item modelId. Character string giving the unique alphanumeric model identifier.
#'   \item reasonCodesSample. list which contains sample of reason codes.
#'     Each element of the list is information about reason codes for one data row. For more
#'     information see GetReasonCodesRows.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   GetReasonCodesInitialization(model)
#' }
#' @export
GetReasonCodesInitialization <- function(model) {
  Deprecated(paste("GetReasonCodesInitialization (use",
                   "GetPredictionExplanationsInitialization instead)"), "2.13", "2.15")
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "reasonCodesInitialization")
  as.dataRobotReasonCodesInitialization(DataRobotGET(routeString, simplifyDataFrame = FALSE))
}


as.dataRobotReasonCodesInitialization <- function(inList) {
  elements <- c("projectId",
                "modelId",
                "reasonCodesSample"
               )
  ApplySchema(inList, elements)
}


#' Retrieve the reason codes initialization for a model using jobId (deprecated)
#'
#' Use \link{GetPredictionExplanationsInitializationFromJobId} instead.
#'
#' @inheritParams DeleteProject
#' @param jobId integer. Unique integer identifier pointing to the reason codes job (returned
#' for example by \code{RequestReasonCodesInitialization}.)
#' @param maxWait Integer, The maximum time (in seconds) to wait for the model job to complete
#' @return A named list which contains:
#' \itemize{
#'   \item projectId. Character id of the project the feature belonges to.
#'   \item modelId. Character string giving the unique alphanumeric model identifier.
#'   \item reasonCodesSample. list which contains sample of reason codes.
#'     Each element of the list is information about reason codes for one data row. For more
#'     information see GetReasonCodesRows.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   jobId <- RequestReasonCodesInitialization(model)
#'   GetReasonCodesInitializationFromJobId(projectId, jobId)
#' }
#' @export
GetReasonCodesInitializationFromJobId <- function(project, jobId, maxWait = 600) {
  Deprecated(paste("GetReasonCodesInitializationFromJobId (use",
                   "GetPredictionExplanationsInitializationFromJobId instead)"), "2.13", "2.15")
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  message("Waiting for reason codes initialization to complete")
  reasonCodeDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                     failureStatuses = JobFailureStatuses)
  pseudoModel <- list(modelId = reasonCodeDetails$modelId, projectId = reasonCodeDetails$projectId)
  class(pseudoModel) <- 'dataRobotModel'
  return(GetReasonCodesInitialization(pseudoModel))
}

#' Delete the reason codes initialization for a model (deprecated).
#'
#' Use \link{DeletePredictionExplanationsInitialization} instead.
#'
#' @inheritParams DeleteModel
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModel(projectId, modelId)
#'   DeleteReasonCodesInitialization(model)
#' }
#' @export
DeleteReasonCodesInitialization <- function(model) {
  Deprecated(paste("DeleteReasonCodesInitialization (use",
                   "DeletePredictionExplanationsInitialization instead)"), "2.13", "2.15")
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "reasonCodesInitialization")
  response <- DataRobotDELETE(routeString)
  modelName <- validModel$modelType
  message(paste("Reason code initialization for model", modelName,
                "(modelId = ", modelId, ") deleted from project", projectId))
}


#' Request reason codes computation for a specified model and dataset (deprecated).
#'
#' Use \link{RequestPredictionExplanations} instead.
#'
#' @inheritParams DeleteModel
#' @param datasetId Character string. Id of the prediction dataset for which reason codes are
#'   requested
#' @param maxCodes integer (optional) The maximum number of reason codes to supply per row of the
#'   dataset, default: 3.
#' @param thresholdLow numeric (optional) The lower threshold, below which a prediction must score
#'   in order for reason codes to be computed for a row in the dataset. If neither
#'   \code{threshold_high} nor \code{threshold_low} is specified, reason codes will be computed
#'   for all rows.
#' @param thresholdHigh numeric (optional) The high threshold, above which a prediction must score
#'   in order for reason codes to be computed. If neither \code{threshold_high} nor
#'   \code{threshold_low} is specified, reason codes will be computed for all rows.
#' @return job Id
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(model, datasetId)
#'   RequestReasonCodes(model, datasetId)
#' }
#' @export
RequestReasonCodes <- function(model, datasetId, maxCodes = NULL, thresholdLow = NULL,
                               thresholdHigh = NULL) {
  Deprecated(paste("RequestReasonCodes (use",
                   "RequestPredictionExplanations instead)"), "2.13", "2.15")
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  modelName <- validModel$modelType
  body <- list(modelId = modelId, datasetId = datasetId)
  if (!is.null(maxCodes)) {
    body$maxCodes <- maxCodes
  }
  if (!is.null(thresholdLow)) {
    body$thresholdLow <- thresholdLow
  }
  if (!is.null(thresholdHigh)) {
    body$thresholdHigh <- thresholdHigh
  }
  routeString <- UrlJoin("projects", projectId, "reasonCodes")
  rawResponse <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  message(paste("Reason codes requested for model", modelName,
                "(modelId = ", modelId, ")"))
  return(JobIdFromResponse(rawResponse))
}

#' Retrieve the reason codes metadata for a model using jobId (deprecated)
#'
#' Use \link{GetPredictionExplanationsMetadataFromJobId} instead.
#'
#' @inheritParams DeleteProject
#' @param jobId Unique integer identifier (return for example by \code{RequestReasonCodes}).
#' @param maxWait Integer, The maximum time (in seconds) to wait for the model job to complete.
#' @return A named list which contains reason code metadata. For more information see
#'   \code{GetReasonCodesMetadata}.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   GetReasonCodesMetadataFromJobId(projectId, jobId)
#' }
#' @export
GetReasonCodesMetadataFromJobId <- function(project, jobId, maxWait = 600) {
  Deprecated(paste("GetReasonCodesMetadataFromJobId (use",
                   "GetPredictionExplanationsMetadataFromJobId instead)"), "2.13", "2.15")
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  message("Reason codes request issued: awaiting response")
  reasonCodeDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                          failureStatuses = JobFailureStatuses)
  return(GetReasonCodesMetadata(projectId, reasonCodeDetails$id))
}


#' Retrieve metadata for specified reason codes (deprecated)
#'
#' Use \link{GetPredictionExplanationsMetadata} instead.
#'
#' @inheritParams DeleteProject
#' @param reasonCodeId character. id of the reason codes.
#' @return A named list which contains reason code metadata:
#' \itemize{
#'   \item id. Character string id of the record and reason codes computation result.
#'   \item projectId. Character string id of the project the model belongs to.
#'   \item modelId. Character string id of the model reason codes initialization is for.
#'   \item datasetId. Character string id of the prediction dataset reason codes were computed for.
#'   \item maxCodes. Integer maximum number of reason codes to supply per row of the dataset.
#'   \item thresholdLow. Numeric the low threshold, below which a prediction must score in order
#'   for reason codes to be computed for a row in the dataset.
#'   \item thresholdHigh. Numeric the high threshold, above which a prediction must score in order
#'   for reason codes to be computed for a row in the dataset.
#'   \item numColumns. Integer the number of columns reason codes were computed for.
#'   \item finishTime. Numeric timestamp referencing when computation for these reason codes
#'     finished.
#'   \item reasonCodesLocation. Character string  where to retrieve the reason codes.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)$id
#'   GetReasonCodesMetadata(projectId, reasonCodeId)
#' }
#' @export
GetReasonCodesMetadata <- function(project, reasonCodeId) {
  Deprecated(paste("GetReasonCodesMetadata (use",
                   "GetPredictionExplanationsMetadata instead)"), "2.13", "2.15")
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "reasonCodesRecords", reasonCodeId)
  as.dataRobotReasonCodesMetadata(DataRobotGET(routeString, simplifyDataFrame = FALSE))
}


as.dataRobotReasonCodesMetadata <- function(inList) {
  elements <- c("id",
                "projectId",
                "modelId",
                "datasetId",
                "maxCodes",
                "thresholdLow",
                "thresholdHigh",
                "numColumns",
                "finishTime",
                "reasonCodesLocation")
  ApplySchema(inList, elements)
}



#' Retrieve metadata for reason codes in specified project (deprecated)
#'
#' Use \link{ListPredictionExplanationsMetadata} instead.
#'
#' @inheritParams DeleteProject
#' @param modelId character. Optional. If specified, only reason codes computed for this
#"   model will be returned
#' @param limit integer. Optional. At most this many results are returned, default: no limit
#' @param offset integer. This many results will be skipped, default: 0
#' @return List of metadata for all reason codes in the project.
#'   Each element of list is metadata for one reason codes
#'   (for format see \code{GetReasonCodesMetadata}).
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListReasonCodesMetadata(projectId)
#' }
#' @export
ListReasonCodesMetadata <- function(project, modelId = NULL, limit = NULL, offset = NULL) {
  Deprecated(paste("ListReasonCodesMetadata (use",
                   "ListPredictionExplanationsMetadata instead)"), "2.13", "2.15")
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "reasonCodesRecords")
  lapply(DataRobotGET(routeString, simplifyDataFrame = FALSE,
                      body = list(modelId = modelId, limit = limit, offset = offset))$data,
         as.dataRobotReasonCodesMetadata)
}


GetReasonCodesPage <- function(project, reasonCodeId, limit = NULL, offset = 0,
                               excludeAdjustedPredictions = TRUE) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "reasonCodes", reasonCodeId)
  excludeAdjustedPredictions <- tolower(as.character(identical(excludeAdjustedPredictions, TRUE)))
  params <- list(offset = offset,
                 limit = limit,
                 excludeAdjustedPredictions = excludeAdjustedPredictions)
  serverData <- DataRobotGET(routeString,
                             simplifyDataFrame = FALSE,
                             query = params)
  serverData$nextPage <- serverData$`next`
  serverData$previousPage <- serverData$previous
  serverData$`next` <- NULL
  serverData$previous <- NULL
  serverData
}

#' Retrieve all reason codes rows (deprecated)
#'
#' Use \link{GetPredictionExplanationsRows} instead.
#'
#' @inheritParams GetAllReasonCodesRowsAsDataFrame
#' @param batchSize integer. Optional. Maximum number of reason codes rows to retrieve per request
#' @return list of raw reason codes, each element corresponds to a row of the prediction dataset
#"  and has following components.
#'    \itemize{
#'      \item rowId. Character string row Id.
#'      \item prediction. prediction for the row.
#'      \item predictionValues. list containing
#'        \itemize{
#'          \item label. describes what this model output corresponds to. For regression projects,
#'             it is the name of the target feature. For classification projects, it is a level
#'             from the  target feature.
#'          \item value. the output of the prediction.  For regression projects, it is the predicted
#'             value of the target. For classification projects, it is the predicted probability the
#'             row belongs to the class identified by the label.
#'        }
#'      \item adjustedPrediction. adjusted predictions, if they are not excluded.
#'      \item adjustedPredictionValues. Similar to predictionValues, but for adjusted predictions,
#'        if they are not excluded.
#'      \item reasonCodes. list contaning
#'        \itemize{
#'          \item label. described what output was driven by this reason code. For regression
#'            projects, it is the name of the target feature. For classification projects, it is
#"            the class whose probability increasing would correspond to a positive strength of this
#"            reason code.
#'          \item feature. the name of the feature contributing to the prediction.
#'          \item featureValue. the value the feature took on for this row
#'          \item strength. the amount this feature's value affected the prediction
#'          \item qualitativateStrength. a human-readable description of how strongly the feature
#'            affected the prediction (e.g. '+++', '--', '+').
#'        }
#'    }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)$id
#'   GetReasonCodesRows(projectId, reasonCodeId)
#' }
#' @export
GetReasonCodesRows <- function(project, reasonCodeId, batchSize = NULL,
                               excludeAdjustedPredictions = TRUE) {
  Deprecated(paste("GetReasonCodesRows (use",
                   "GetPredictionExplanationRows instead)"), "2.13", "2.15")
  page <- GetReasonCodesPage(project, reasonCodeId, limit = batchSize, offset = 0,
                             excludeAdjustedPredictions = excludeAdjustedPredictions)
  GetServerDataInRows(page, batchSize = batchSize)
}

#' Retrieve all reason codes rows and return them as a data frame (deprecated)
#'
#' Use \link{GetPredictionExplanationsRowsAsDataFrame} instead.
#'
#' @inheritParams GetReasonCodesMetadata
#' @param excludeAdjustedPredictions logical. Optional. Set to FALSE to include adjusted
#'   predictions, which are predictions adjusted by an exposure column. This is only relevant for
#'   projects that use an exposure column.
#' @return data frame with following colums:
#' \itemize{
#'   \item rowId. Integer row id from prediction dataset.
#'   \item prediction. Numeric the output of the model for this row (numric prediction for
#'     regression problem, predicted class for classification problem).
#'   \item class1Label. Character string Label of class 0. Available only for classification
#'     problem.
#'   \item class1Probability. Numeric Predicted probability of class 0. Available only for
#'     classification problem.
#'   \item class2Label. Character string Label of class 1. Available only for classification
#"     problem.
#'   \item class2Probability. Numeric Predicted probability of class 1. Available only for
#'     classification problem.
#'   \item reason1FeatureName. Character string the name of the feature contributing to the
#'     prediction.
#'   \item reason1FeatureValue. the value the feature took on for this row.
#'   \item reason1QualitativeStrength. Numeric how strongly the feature affected the prediction.
#'   \item reason1Strength. Character string  a human-readable description of how strongly the
#'     feature affected the prediction (e.g. '+++', '--', '+').
#'   \item reason1Label. Character string describes what output was driven by this reason code.
#'   For regression projects, it is the name of the target feature.  For classification projects,
#'   it is theclass whose probability increasing would correspond to a positive strength of this.
#'   \item reasonNFeatureName. Character string the name of the feature contributing to the
#'     prediction.
#'   \item reasonNFeatureValue. the value the feature took on for this row.
#'   \item reasonNQualitativeStrength. Numeric how strongly the feature affected the prediction.
#'   \item reasonNStrength. Character string  a human-readable description of how strongly the
#'     feature affected the prediction (e.g. '+++', '--', '+').
#'   \item reasonNLabel. Character string describes what output was driven by this reason code.
#'   For regression projects, it is the name of the target feature.  For classification projects,
#'   it is theclass whose probability increasing would correspond to a positive strength of this.
#'   \item reasonNFeatureName. Character string the name of the feature contributing to the
#'     prediction.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)$id
#'   GetReasonCodesRowsAsDataFrame(projectId, reasonCodeId)
#' }
#' @export
GetAllReasonCodesRowsAsDataFrame <- function(project, reasonCodeId,
                                             excludeAdjustedPredictions = TRUE) {
  Deprecated(paste("GetAllReasonCodesRowsAsDataFrame (use",
                   "GetPredictionExplanationsRowsAsDataFrame instead)"), "2.13", "2.15")
  reasonCodesList <- GetReasonCodesRows(project,
                                        reasonCodeId,
                                        excludeAdjustedPredictions = excludeAdjustedPredictions)
  nList <- length(reasonCodesList)
  message("Reason codes are available for ", nList, " records", sep = "")
  if (nList == 0) {
    emptyFrame <- data.frame(rowId = integer(),
                          prediction = numeric(),
                          reason1FeatureName = character(),
                          reason1FeatureValue = character(),
                          reason1QualitativeStrength = character(),
                          reason1Strength = numeric(),
                          reason1Label = character(),
                          stringsAsFactors = FALSE)
    return(emptyFrame)
  }
  maxCodes <- max(sapply(reasonCodesList, function(x) length(x$reasonCodes)))

  outFrame <- NULL
  for (i in 1:nList) {
    element <- reasonCodesList[[i]]
    oneRowFrame <- data.frame(rowId = element$rowId,
                             prediction = element$prediction,
                             stringsAsFactors = FALSE)
    if (!is.null(element$adjustedPrediction)) {
      oneRowFrame$adjustedPrediction <- element$adjustedPrediction
      oneRowFrame$adjustedPredictionValues <- element$adjustedPredictionValues
    }
    if (length(element$predictionValues) > 1) {
      for (m in 1:length(element$predictionValues)) {
        oneRowFrame[1, paste("class", m, "Label", sep = "")] <-
          element$predictionValues[[m]]$label
        oneRowFrame[1, paste("class", m, "Probability", sep = "")] <-
          element$predictionValues[[m]]$value
      }
    }
    if (length(element$reasonCodes) > 0) {
      for (n in 1:maxCodes) {
        oneRowFrame[1, paste("reason", n, "FeatureName", sep = "")] <-
          element$reasonCodes[[n]]$feature
        oneRowFrame[1, paste("reason", n, "FeatureValue", sep = "")] <-
          element$reasonCodes[[n]]$featureValue
        oneRowFrame[1, paste("reason", n, "QualitativeStrength", sep = "")] <-
          element$reasonCodes[[n]]$qualitativeStrength
        oneRowFrame[1, paste("reason", n, "Strength", sep = "")] <-
          element$reasonCodes[[n]]$strength
        oneRowFrame[1, paste("reason", n, "Label", sep = "")] <-
          element$reasonCodes[[n]]$label
      }
    } else {
      for (n in 1:maxCodes) {
        oneRowFrame[1, paste("reason", n, "FeatureName", sep = "")] <- NA
        oneRowFrame[1, paste("reason", n, "FeatureValue", sep = "")] <- NA
        oneRowFrame[1, paste("reason", n, "QualitativeStrength", sep = "")] <- NA
        oneRowFrame[1, paste("reason", n, "Strength", sep = "")] <- NA
        oneRowFrame[1, paste("reason", n, "Label", sep = "")] <- NA
      }
    }
    outFrame <- rbind.data.frame(outFrame, oneRowFrame)
  }
    return(outFrame)
}

#' Function to download and save reason codes rows as csv file (deprecated)
#'
#' Use \link{DownloadPredictionExplanations} instead
#'
#' @inheritParams GetAllReasonCodesRowsAsDataFrame
#' @param filename character. Fileneme of file to save reason codes rows
#' @param encoding character. Optional. Character string A string representing the encoding
#'   to use in the output file, defaults to 'UTF-8'.
#' @return Logical TRUE and displays a message to the user if the delete
#'   request was successful; otherwise an error message is displayed.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)$id
#'   file <- file.path(tempdir(), "testReasonCode.csv")
#'   DownloadReasonCodes(projectId, reasonCodeId, file)
#' }
#' @export
DownloadReasonCodes <- function(project, reasonCodeId, filename, encoding = "UTF-8",
                                excludeAdjustedPredictions = TRUE) {
  Deprecated(paste("DownloadReasonCodes (use",
                   "DownloadPredictionExplanations instead)"), "2.13", "2.15")
  reasonCodesFrame <- GetAllReasonCodesRowsAsDataFrame(
                                           project,
                                           reasonCodeId,
                                           excludeAdjustedPredictions = excludeAdjustedPredictions)
  write.csv(reasonCodesFrame, file = filename, row.names = FALSE, fileEncoding = encoding)
}

#' Function to delete reason codes (deprecated)
#'
#' This function deletes reason codes specified by project and reasonCodeId
#'
#' Use \link{DeletePredictionExplanations} instead.
#'
#' @inheritParams GetReasonCodesMetadata
#' @return Logical TRUE and displays a message to the user if the delete
#' request was successful; otherwise an error message is displayed.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   datasets <- ListPredictionDatasets(projectId)
#'   dataset <- datasets[[1]]
#'   datasetId <- dataset$id
#'   model <- GetModel(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)$id
#'   DeleteReasonCodes(projectId, reasonCodeId)
#' }
#' @export
DeleteReasonCodes <- function(project, reasonCodeId) {
  Deprecated(paste("DeleteReasonCodes (use",
                   "DeletePredictionExplanations instead)"), "2.13", "2.15")
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "reasonCodesRecords",  reasonCodeId)
  response <- DataRobotDELETE(routeString)
  message(paste("Reason code ", reasonCodeId, "deleted from project", projectId))
}
