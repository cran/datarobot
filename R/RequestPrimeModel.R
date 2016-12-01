#' Request training for a DataRobot Prime model using a specified ruleset
#'
#' Training a model using a ruleset is a necessary prerequisite for being able to download the code for a ruleset
#'
#' @inheritParams DeleteProject
#' @param ruleset A list specifying rulest parameters (see GetRulesets)
#' @return job Id
#' @export
#'
RequestPrimeModel <- function(project, ruleset) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeModels")
  parentModelId <- ruleset$parentModelId
  rulesetId <- ruleset$rulesetId
  bodyFrame <- data.frame(parentModelId = parentModelId, rulesetId = rulesetId)
  body <- jsonlite::unbox(bodyFrame)
  rawResponse <- DataRobotPOST(routeString, addUrl = TRUE, body = body,
                               returnRawResponse = TRUE, encode = "json")
  routeString <- UrlJoin("projects", projectId, "jobs", JobIdFromResponse(rawResponse))
  jobsResponse <- DataRobotGET(routeString, addUrl = TRUE, query = NULL, simplifyDataFrame = FALSE)
  return(jobsResponse$id)
}
