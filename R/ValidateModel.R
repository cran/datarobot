#
#  ValidateModel.R - function to validate that model belongs to class 'dataRobotModel' and includes projectId and modelId
#

ValidateModel <- function(model) {
  errorMessage <- "Invalid model specification"
  if (!is(model, 'dataRobotModel')) {
    stop(errorMessage)
  } else {
    projectId <- model$projectId
    modelId <- model$modelId
    if (!is.null(projectId) & !is.null(modelId)) {
      return(model)
    } else {
      stop(errorMessage, call. = FALSE)
    }
  }
}
