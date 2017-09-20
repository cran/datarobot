#' Request reason codes initialization for specified model
#'
#' Reason codes initializations are a prerequisite for computing reason codes, and include
#' a sample what the computed reason codes for a prediction dataset would look like.
#'
#' @inheritParams DeleteModel
#' @return job Id
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModelObject(projectId, modelId)
#'   RequestReasonCodesInitialization(model)
#' }
#' @export
RequestReasonCodesInitialization <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  modelName <- validModel$modelType
  routeString <- UrlJoin("projects", projectId, "models", modelId, "reasonCodesInitialization")
  rawResponse <- DataRobotPOST(routeString, addUrl = TRUE, returnRawResponse = TRUE)
  message(paste("Reason codes initialization requested for model", modelName,
                "(modelId = ", modelId, ")"))
  return(JobIdFromResponse(rawResponse))
}

#' Retrieve the reason codes initialization for a model.
#'
#' Reason codes initializations are a prerequisite for computing reason codes, and include
#' a sample what the computed reason codes for a prediction dataset would look like.
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
#'   model <- GetModelObject(projectId, modelId)
#'   GetReasonCodesInitialization(model)
#' }
#' @export
GetReasonCodesInitialization <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "reasonCodesInitialization")
  return(as.dataRobotReasonCodesInitialization(DataRobotGET(routeString, addUrl = TRUE,
                                                            simplifyDataFrame = FALSE)))
}


as.dataRobotReasonCodesInitialization <- function(inList) {
  elements <- c("projectId",
                "modelId",
                "reasonCodesSample"
               )
  outList <- ApplySchema(inList, elements)
  return(outList)
}


#' Retrieve the reason codes initialization for a model using jobId
#'
#' Reason codes initializations are a prerequisite for computing reason codes, and include
#' a sample what the computed reason codes for a prediction dataset would look like.
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
#'   model <- GetModelObject(projectId, modelId)
#'   jobId <- RequestReasonCodesInitialization(model)
#'   GetReasonCodesInitializationFromJobId(projectId, jobId)
#' }
#' @export
GetReasonCodesInitializationFromJobId <- function(project, jobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  message("Waiting for reason codes initialization to complete")
  reasonCodeDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                     failureStatuses = JobFailureStatuses)
  pseudoModel <- list(modelId = reasonCodeDetails$modelId, projectId = reasonCodeDetails$projectId)
  class(pseudoModel) <- 'dataRobotModel'
  return(GetReasonCodesInitialization(pseudoModel))
}

#' Delete the reason codes initialization for a model.
#'
#' @inheritParams DeleteModel
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   model <- GetModelObject(projectId, modelId)
#'   DeleteReasonCodesInitialization(model)
#' }
#' @export
DeleteReasonCodesInitialization <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "reasonCodesInitialization")
  response <- DataRobotDELETE(routeString, addUrl = TRUE)
  modelName <- validModel$modelType
  message(paste("Reason code initialization for model", modelName,
                "(modelId = ", modelId, ") deleted from project", projectId))
}


#' Request reason codes computation for a specified model and dataset.
#'
#' In order to create ReasonCodes for a particular model and dataset, you must first:
#' Compute feature impact for the model via \code{RequestFeatureImpact()}
#' Compute a ReasonCodesInitialization for the model via
#' \code{RequestReasonCodesInitialization()}
#' Compute predictions for the model and dataset via
#'\code{RequestPredictionsForDataset()}
#' After reason codes are requested information about them can be accessed using
#' the functions \code{GetReasonCodesMetadataFromJobId} and \code{GetReasonCodesMetadata}
#' And reason codes themselves can be accessed using the functions
#' \code{GetReasonCodesRows}, \code{GetAllReasonCodesRowsAsDataFrame}, \code{DownloadReasonCodes}
#'
#' \code{threshold_high} and \code{threshold_low} are optional filters applied to speed up
#' computation.  When at least one is specified, only the selected outlier rows will have
#' reason codes computed. Rows are considered to be outliers if their predicted
#' value (in case of regression projects) or probability of being the positive
#' class (in case of classification projects) is less than \code{threshold_low} or greater than
#' \code{thresholdHigh}.  If neither is specified, reason codes will be computed for all rows.
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
#'   model <- GetModelObject(model, datasetId)
#'   RequestReasonCodes(model, datasetId)
#' }
#' @export
RequestReasonCodes <- function(model, datasetId, maxCodes = NULL, thresholdLow = NULL,
                               thresholdHigh = NULL) {
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
  rawResponse <- DataRobotPOST(routeString, addUrl = TRUE, body = body, returnRawResponse = TRUE)
  message(paste("Reason codes requested for model", modelName,
                "(modelId = ", modelId, ")"))
  return(JobIdFromResponse(rawResponse))
}

#' Retrieve the reason codes metadata for a model using jobId
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
#'   model <- GetModelObject(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   GetReasonCodesMetadataFromJobId(projectId, jobId)
#' }
#' @export
GetReasonCodesMetadataFromJobId <- function(project, jobId, maxWait = 600) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  message("Reason codes request issued: awaiting response")
  reasonCodeDetails <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                          failureStatuses = JobFailureStatuses)
  return(GetReasonCodesMetadata(project, reasonCodeDetails$id))
}


#' Retrieve metadata for specified reason codes
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
#'   model <- GetModelObject(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)
#'   GetReasonCodesMetadata(projectId, reasonCodeId)
#' }
#' @export
GetReasonCodesMetadata <- function(project, reasonCodeId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "reasonCodesRecords", reasonCodeId)
  return(as.dataRobotReasonCodesMetadata(DataRobotGET(routeString, addUrl = TRUE,
                                                      simplifyDataFrame = FALSE)))
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
                "reasonCodesLocation"
 )
  outList <- ApplySchema(inList, elements)
  return(outList)
}



#' Retrieve metadata for reason codes in specified project
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
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "reasonCodesRecords")
  return(lapply(DataRobotGET(routeString, addUrl = TRUE, simplifyDataFrame = FALSE,
                      body = list(modelId = modelId, limit = limit, offset = offset))$data,
                as.dataRobotReasonCodesMetadata))
}


GetReasonCodesPage <- function(project, reasonCodeId, limit = NULL, offset = 0) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "reasonCodes", reasonCodeId)
  params <- list(offset = offset, limit = limit)
  serverData <- DataRobotGET(routeString, addUrl = TRUE, simplifyDataFrame = FALSE, query = params)
  serverData$nextPage <- serverData$`next`
  serverData$previousPage <- serverData$previous
  serverData$`next` <- NULL
  serverData$previous <- NULL
  return(serverData)
}

#' Retrieve all reason codes rows
#'
#' @inheritParams GetReasonCodesMetadata
#' @param batchSize (optional) Integer maximum number of reason codes rows to retrieve per request
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
#'   model <- GetModelObject(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)
#'   GetReasonCodesRows(projectId, reasonCodeId)
#' }
#' @export
GetReasonCodesRows <- function(project, reasonCodeId, batchSize = NULL) {
  page <- GetReasonCodesPage(project, reasonCodeId, limit = batchSize, offset = 0)
  rows <- page$data
  n <- 0
  while (!is.null(page$nextPage)) {
    page <- DataRobotGET(page$nextPage, addUrl = FALSE, simplifyDataFrame = FALSE)
    page$nextPage <- page$`next`
    rows <- append(rows, page$data)
  }
  return(rows)
}

#' Retrieve all reason codes rows and return them as a data frame
#'
#' There are some groups of columns whose appearance depends on the exact
#' contents of the project dataset. For classification projects,
#' columns "classNLabel", 'classNProbability", "classNLabel", "classNProbability"
#' will appear corresponding to each class within the target;
#' these columns will not appear for regression projects.
#' Columns like "reasonNLabel" will appear corresponding to each included reason code
#' in the row. In both cases, the value of N will start at 1 and count up.
#'
#' @inheritParams GetReasonCodesMetadata
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
#'   model <- GetModelObject(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)
#'   GetReasonCodesRowsAsDataFrame(projectId, reasonCodeId)
#' }
#' @export
GetAllReasonCodesRowsAsDataFrame <- function(project, reasonCodeId) {
  reasonCodesList <- GetReasonCodesRows(project, reasonCodeId)
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

#' Function to download and save reason codes rows as csv file
#'
#' @inheritParams GetReasonCodesMetadata
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
#'   model <- GetModelObject(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)
#'   DownloadReasonCodes(projectId, reasonCodeId, "testReasonCode.csv")
#' }
#' @export
DownloadReasonCodes <- function(project, reasonCodeId, filename, encoding = "UTF-8") {
  reasonCodesFrame <- GetAllReasonCodesRowsAsDataFrame(project, reasonCodeId)
  write.csv(reasonCodesFrame, file = filename, row.names = F, fileEncoding = encoding)
}

#' Function to delete reason codes
#'
#' This function deletes reason codes specified by project and reasonCodeId
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
#'   model <- GetModelObject(model, datasetId)
#'   jobId <- RequestReasonCodes(model, datasetId)
#'   reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)
#'   DeleteReasonCodes(projectId, reasonCodeId)
#' }
#' @export
DeleteReasonCodes <- function(project, reasonCodeId) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "reasonCodesRecords",  reasonCodeId)
  response <- DataRobotDELETE(routeString, addUrl = TRUE)
  message(paste("Reason code ", reasonCodeId, "deleted from project", projectId))
}
