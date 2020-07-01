context("Test Deployments")
library(stubthat)
library(testthat)


deploymentCols <- c("defaultPredictionServer", "description", "modelHealth",
                  "predictionUsage", "capabilities", "label", "id", "model",
                  "accuracyHealth", "serviceHealth", "permissions")
checks <- c("targetType", "target", "notCurrentModel", "permission", "supported",
            "modelCanBeDeployed", "seriesType", "modelStatus", "featureDataTypes",
            "features")


test_that("it can list deployments", {
  getStub <- stub(httr::GET)
  listDeploymentUrl <- "deployments"
  listDeploymentJson <- fileToChar("responses/listDeployments.json")
  deploymentResponse <- httr:::response(url = listDeploymentUrl,
                                        status_code = 200L,
                                        content = charToRaw(listDeploymentJson))
  getStub$onCall(1)$returns(deploymentResponse)
  deployments <- with_mock(`httr::GET` = getStub$f,
                           `datarobot:::Endpoint` = function() fakeEndpoint,
                           `datarobot:::Token` = function() fakeToken,
                           ListDeployments())
  expect_equal(getStub$calledTimes(), 1)
  expect_is(deployments, "listOfDeployments")
  deployment <- deployments[[1]]
  expect_is(deployment, "dataRobotDeployment")
  ExpectHasKeys(deployment, deploymentCols)
  expect_is(deployment$model, "dataRobotModel")
  expect_is(deployment$permissions, "character")
})


test_that("it can get a deployment", {
  getStub <- stub(httr::GET)
  getDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId)
  getDeploymentJson <- fileToChar("responses/getDeployment.json")
  deploymentResponse <- httr:::response(url = getDeploymentUrl,
                                        status_code = 200L,
                                        content = charToRaw(getDeploymentJson))
  getStub$onCall(1)$returns(deploymentResponse)
  deployment <- with_mock(`httr::GET` = getStub$f,
                          `datarobot:::Endpoint` = function() fakeEndpoint,
                          `datarobot:::Token` = function() fakeToken,
                          GetDeployment(fakeDeploymentId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(deployment, "dataRobotDeployment")
  ExpectHasKeys(deployment, deploymentCols)
  expect_is(deployment$model, "dataRobotModel")
  expect_is(deployment$permissions, "character")
})


describe("CreateDeployment", {
  test_that("it can create a new deployment", {
    postStub <- stub(httr::POST)
    createDeploymentUrl <- UrlJoin("deployments")
    createDeploymentResponse <- httr:::response(url = createDeploymentUrl,
                                                status_code = 202L,
                                                content = charToRaw(sprintf('{"id": "%s"}',
                                                                            fakeDeploymentId)))
    postStub$onCall(1)$returns(createDeploymentResponse)
    getStub <- stub(httr::GET)
    getDeploymentUrl <- UrlJoin("deployments", "fromLearningModel")
    getDeploymentJson <- fileToChar("responses/getDeployment.json")
    deploymentResponse <- httr:::response(url = getDeploymentUrl,
                                          status_code = 200L,
                                          content = charToRaw(getDeploymentJson))
    getStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(`httr::POST` = postStub$f,
                          `httr::GET` = getStub$f,
                          `datarobot:::DataRobotPOST` = function(routeString,
                                                                 addUrl = TRUE,
                                                                 body = NULL,
                                                                 returnRawResponse = FALSE, ...) {
                            bodyForInspect <<- body
                            datarobot:::MakeDataRobotRequest(httr::POST, routeString,
                                                             addUrl = addUrl,
                                                             returnRawResponse = returnRawResponse,
                                                             body = body, ...)
                          },
                          `datarobot:::Endpoint` = function() fakeEndpoint,
                          `datarobot:::Token` = function() fakeToken,
                          CreateDeployment(fakeModel,
                                           label = "label"))
    expect_equal(postStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_is(response, "dataRobotDeployment")
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$label, "label")
    expect_equal(bodyForInspect$description, "")
    expect_null(bodyForInspect$defaultPredictionServerId)
  })

  test_that("it can pass a description and defaultPredictionServerId", {
    postStub <- stub(httr::POST)
    createDeploymentUrl <- UrlJoin("deployments")
    createDeploymentResponse <- httr:::response(url = createDeploymentUrl,
                                                status_code = 202L,
                                                content = charToRaw(sprintf('{"id": "%s"}',
                                                                            fakeDeploymentId)))
    postStub$onCall(1)$returns(createDeploymentResponse)
    getStub <- stub(httr::GET)
    getDeploymentUrl <- UrlJoin("deployments", "fromLearningModel")
    getDeploymentJson <- fileToChar("responses/getDeployment.json")
    deploymentResponse <- httr:::response(url = getDeploymentUrl,
                                          status_code = 200L,
                                          content = charToRaw(getDeploymentJson))
    getStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(`httr::POST` = postStub$f,
                          `httr::GET` = getStub$f,
                          `datarobot:::DataRobotPOST` = function(routeString,
                                                                 addUrl = TRUE,
                                                                 body = NULL,
                                                                 returnRawResponse = FALSE, ...) {
                            bodyForInspect <<- body
                            datarobot:::MakeDataRobotRequest(httr::POST, routeString,
                                                             addUrl = addUrl,
                                                             returnRawResponse = returnRawResponse,
                                                             body = body, ...)
                          },
                          `datarobot:::Endpoint` = function() fakeEndpoint,
                          `datarobot:::Token` = function() fakeToken,
                          CreateDeployment(fakeModel,
                                           label = "label",
                                           description = "description",
                                           defaultPredictionServerId = fakePredictionServerId))
    expect_equal(postStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_is(response, "dataRobotDeployment")
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$label, "label")
    expect_equal(bodyForInspect$description, "description")
    expect_equal(bodyForInspect$defaultPredictionServerId, fakePredictionServerId)
  })

  test_that("it can pass a prediction server as the defaultPredictionServerId", {
    postStub <- stub(httr::POST)
    createDeploymentUrl <- UrlJoin("deployments")
    createDeploymentResponse <- httr:::response(url = createDeploymentUrl,
                                                status_code = 202L,
                                                content = charToRaw(sprintf('{"id": "%s"}',
                                                                            fakeDeploymentId)))
    postStub$onCall(1)$returns(createDeploymentResponse)
    getStub <- stub(httr::GET)
    getDeploymentUrl <- UrlJoin("deployments", "fromLearningModel")
    getDeploymentJson <- fileToChar("responses/getDeployment.json")
    deploymentResponse <- httr:::response(url = getDeploymentUrl,
                                          status_code = 200L,
                                          content = charToRaw(getDeploymentJson))
    getStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(`httr::POST` = postStub$f,
                          `httr::GET` = getStub$f,
                          `datarobot:::DataRobotPOST` = function(routeString,
                                                                 addUrl = TRUE,
                                                                 body = NULL,
                                                                 returnRawResponse = FALSE, ...) {
                            bodyForInspect <<- body
                            datarobot:::MakeDataRobotRequest(httr::POST, routeString,
                                                             addUrl = addUrl,
                                                             returnRawResponse = returnRawResponse,
                                                             body = body, ...)
                          },
                          `datarobot:::Endpoint` = function() fakeEndpoint,
                          `datarobot:::Token` = function() fakeToken,
                          CreateDeployment(fakeModel,
                                           label = "label",
                                           description = "description",
                                           defaultPredictionServerId = fakePredictionServer))
    expect_equal(postStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_is(response, "dataRobotDeployment")
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$label, "label")
    expect_equal(bodyForInspect$description, "description")
    expect_equal(bodyForInspect$defaultPredictionServerId, fakePredictionServerId)
  })
})


describe("DeleteDeployment", {
  test_that("it can delete a deployment", {
    deleteStub <- stub(httr::DELETE)
    deleteDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId)
    deploymentResponse <- httr:::response(url = deleteDeploymentUrl,
                                          status_code = 204L,
                                          content = raw(0))
    deleteStub$onCall(1)$returns(deploymentResponse)
    deployment <- with_mock(`httr::DELETE` = deleteStub$f,
                            `datarobot:::Endpoint` = function() fakeEndpoint,
                            `datarobot:::Token` = function() fakeToken,
                            DeleteDeployment(fakeDeploymentId))
    expect_null(deployment)
  })

  test_that("it can delete a deployment with an object", {
    deleteStub <- stub(httr::DELETE)
    deleteDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId)
    deploymentResponse <- httr:::response(url = deleteDeploymentUrl,
                                          status_code = 204L,
                                          content = raw(0))
    deleteStub$onCall(1)$returns(deploymentResponse)
    deployment <- with_mock(`httr::DELETE` = deleteStub$f,
                            `datarobot:::Endpoint` = function() fakeEndpoint,
                            `datarobot:::Token` = function() fakeToken,
                            DeleteDeployment(fakeDeployment))
    expect_null(deployment)
  })
})


describe("ValidateReplaceDeployedModel", {
  test_that("it can validate a deployment replacement", {
    postStub <- stub(httr::POST)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "model", "validation")
    replaceDeploymentJson <- fileToChar("responses/validateDeployment.json")
    deploymentResponse <- httr:::response(url = replaceDeploymentUrl,
                                          status_code = 202L,
                                          content = charToRaw(replaceDeploymentJson))
    postStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(`httr::POST` = postStub$f,
                          `httr::GET` = function(...) stop("Should not be called!"),
                          `datarobot:::Endpoint` = function() fakeEndpoint,
                          `datarobot:::Token` = function() fakeToken,
                          ValidateReplaceDeployedModel(fakeDeployment, fakeModel))
    expect_equal(postStub$calledTimes(), 1)
    expect_is(response, "list")
    ExpectHasKeys(response, c("status", "message", "checks"))
    expect_equal(response$status, "passing")
    ExpectHasKeys(response$checks, checks)
    ExpectHasKeys(response$checks[[1]], c("status", "message"))
    expect_equal(response$checks[[1]]$status, "passing")
  })

  test_that("it can validate a deployment replacement that has errors", {
    postStub <- stub(httr::POST)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "model", "validation")
    replaceDeploymentJson <- fileToChar("responses/validateDeploymentWithErrors.json")
    deploymentResponse <- httr:::response(url = replaceDeploymentUrl,
                                          status_code = 202L,
                                          content = charToRaw(replaceDeploymentJson))
    postStub$onCall(1)$returns(deploymentResponse)
    response <- with_mock(`httr::POST` = postStub$f,
                          `httr::GET` = function(...) stop("Should not be called!"),
                          `datarobot:::Endpoint` = function() fakeEndpoint,
                          `datarobot:::Token` = function() fakeToken,
                          ValidateReplaceDeployedModel(fakeDeployment, fakeModel))
    expect_equal(postStub$calledTimes(), 1)
    expect_is(response, "list")
    ExpectHasKeys(response, c("status", "message", "checks"))
    expect_equal(response$status, "failing")
    ExpectHasKeys(response$checks, checks)
    ExpectHasKeys(response$checks[[1]], c("status", "message"))
    expect_equal(response$checks[[1]]$status, "passing")
    expect_equal(response$checks[[3]]$status, "failing")
  })
})


describe("ReplaceDeployedModel", {
  test_that("it can replace a deployment", {
    patchStub <- stub(httr::PATCH)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "model")
    replaceDeploymentJson <- fileToChar("responses/replaceDeployment.json")
    replaceDeploymentResponse <- httr:::response(url = replaceDeploymentUrl,
                                                 status_code = 202L,
                                                 content = charToRaw(replaceDeploymentJson))
    patchStub$onCall(1)$returns(replaceDeploymentResponse)
    deploymentReplaceJson <- fileToChar("responses/getDeployment.json")
    deploymentReplaceUrl <- UrlJoin("deployments", fakeDeploymentId, "model")
    deploymentReplaceRequestResponse <- httr:::response(url = deploymentReplaceUrl,
                                                        status_code = 202L,
                                                        content = charToRaw(deploymentReplaceJson))
    response <- with_mock(`httr::PATCH` = patchStub$f,
                          `httr::POST` = function(...) stop("Should not be called!"),
                          `datarobot:::DataRobotPATCH` = function(routeString,
                                                                  addUrl = TRUE,
                                                                  body = NULL,
                                                                  returnRawResponse = FALSE, ...) {
                            bodyForInspect <<- body
                            datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
                                                             addUrl = addUrl,
                                                             returnRawResponse = returnRawResponse,
                                                             body = body, ...)
                          },
                          `datarobot::WaitForAsyncReturn` = function(...) {
                             ParseReturnResponse(deploymentReplaceRequestResponse)
                          },
                          `datarobot:::Endpoint` = function() fakeEndpoint,
                          `datarobot:::Token` = function() fakeToken,
                          ReplaceDeployedModel(fakeDeployment,
                                               fakeModel,
                                               ModelReplacementReason$Other))
    expect_equal(patchStub$calledTimes(), 1)
    expect_is(response, "dataRobotDeployment")
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$reason, ModelReplacementReason$Other)
  })

  test_that("it can replace a deployment", {
    patchStub <- stub(httr::PATCH)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "model")
    replaceDeploymentJson <- fileToChar("responses/replaceDeploymentWithErrors.json")
    replaceDeploymentResponse <- httr:::response(url = replaceDeploymentUrl,
                                                 status_code = 409L,
                                                 content = charToRaw(replaceDeploymentJson))
    patchStub$onCall(1)$returns(replaceDeploymentResponse)
    deploymentReplaceJson <- fileToChar("responses/getDeployment.json")
    deploymentReplaceUrl <- UrlJoin("deployments", fakeDeploymentId, "model")
    deploymentReplaceRequestResponse <- httr:::response(url = deploymentReplaceUrl,
                                                        status_code = 202L,
                                                        content = charToRaw(deploymentReplaceJson))
    expect_error(with_mock(`httr::PATCH` = patchStub$f,
                           `httr::POST` = function(...) stop("Should not be called!"),
                           `datarobot:::DataRobotPATCH` = function(routeString,
                                                                   addUrl = TRUE,
                                                                   body = NULL,
                                                                   returnRawResponse = FALSE, ...) {
                             bodyForInspect <<- body
                             datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
                                                              addUrl = addUrl,
                                                              returnRawResponse = returnRawResponse,
                                                              body = body, ...)
                           },
                           `datarobot::WaitForAsyncReturn` = function(...) {
                              ParseReturnResponse(deploymentReplaceRequestResponse)
                           },
                           `datarobot:::Endpoint` = function() fakeEndpoint,
                           `datarobot:::Token` = function() fakeToken,
                           ReplaceDeployedModel(fakeDeployment,
                                                fakeModel,
                                                ModelReplacementReason$Other)),
                 paste("Model deployment failure - The following model deployment checks",
                       "failed: Model is already used as current model of the deployment."))
    expect_equal(patchStub$calledTimes(), 1)
    expect_equal(bodyForInspect$modelId, fakeModelId)
    expect_equal(bodyForInspect$reason, ModelReplacementReason$Other)
  })
})


test_that("it can list prediction servers", {
  getStub <- stub(httr::GET)
  listPredictionServerUrl <- "predictionServers"
  listPredictionServerJson <- fileToChar("responses/listPredictionServers.json")
  predictionServerResponse <- httr:::response(url = listPredictionServerUrl,
                                              status_code = 200L,
                                              content = charToRaw(listPredictionServerJson))
  getStub$onCall(1)$returns(predictionServerResponse)
  predictionServers <- with_mock(`httr::GET` = getStub$f,
                                 `datarobot:::Endpoint` = function() fakeEndpoint,
                                 `datarobot:::Token` = function() fakeToken,
                                 ListPredictionServers())
  expect_equal(getStub$calledTimes(), 1)
  expect_is(predictionServers, "listOfPredictionServers")
  predictionServer <- predictionServers[[1]]
  expect_is(predictionServer, "dataRobotPredictionServer")
  ExpectHasKeys(predictionServer, c("id", "url", "dataRobotKey"))
})


describe("Deployment drift tracking settings", {
  test_that("it can get deployment drift tracking settings", {
    getStub <- stub(httr::GET)
    getDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "settings")
    getDeploymentJson <- fileToChar("responses/getDeploymentDriftTrackingSettings.json")
    deploymentResponse <- httr:::response(url = getDeploymentUrl,
                                          status_code = 200L,
                                          content = charToRaw(getDeploymentJson))
    getStub$onCall(1)$returns(deploymentResponse)
    settings <- with_mock(`httr::GET` = getStub$f,
                          `datarobot:::Endpoint` = function() fakeEndpoint,
                          `datarobot:::Token` = function() fakeToken,
                          GetDeploymentDriftTrackingSettings(fakeDeploymentId))
    expect_equal(getStub$calledTimes(), 1)
    expect_is(settings, "list")
    ExpectHasKeys(settings, c("predictionIntervals", "targetDrift",
                              "associationId", "featureDrift"))
    expect_is(settings$predictionIntervals, "list")
    ExpectHasKeys(settings$predictionIntervals, c("percentiles", "enabled"))
    expect_is(settings$featureDrift, "list")
    ExpectHasKeys(settings$featureDrift, "enabled")
    expect_is(settings$associationId, "list")
    ExpectHasKeys(settings$associationId, c("columnNames", "requiredInPredictionRequests"))
  })

  test_that("it can update deployment drift tracking settings", {
    patchStub <- stub(httr::PATCH)
    replaceDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "settings")
    replaceDeploymentJson <- fileToChar("responses/replaceDeploymentDriftSettings.json")
    replaceDeploymentResponse <- httr:::response(url = replaceDeploymentUrl,
                                                 status_code = 202L,
                                                 content = charToRaw(replaceDeploymentJson))
    patchStub$onCall(1)$returns(replaceDeploymentResponse)
    getStub <- stub(httr::GET)
    getDeploymentUrl <- UrlJoin("deployments", fakeDeploymentId, "settings")
    getDeploymentJson <- fileToChar("responses/getDeploymentDriftTrackingSettings.json")
    deploymentResponse <- httr:::response(url = getDeploymentUrl,
                                          status_code = 200L,
                                          content = charToRaw(getDeploymentJson))
    getStub$onCall(1)$returns(deploymentResponse)
    settings <- with_mock(`httr::PATCH` = patchStub$f,
                          `httr::GET` = getStub$f,
                          `httr::POST` = function(...) stop("Should not be called!"),
                          `datarobot:::DataRobotPATCH` = function(routeString,
                                                                  addUrl = TRUE,
                                                                  body = NULL,
                                                                  returnRawResponse = FALSE, ...) {
                            bodyForInspect <<- body
                            datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
                                                             addUrl = addUrl,
                                                             returnRawResponse = returnRawResponse,
                                                             body = body, ...)
                          },
                          `datarobot::WaitForAsyncReturn` = function(...) {
                             ParseReturnResponse(deploymentResponse)
                          },
                          `datarobot:::Endpoint` = function() fakeEndpoint,
                          `datarobot:::Token` = function() fakeToken,
                          UpdateDeploymentDriftTrackingSettings(fakeDeployment,
                                                                targetDriftEnabled = TRUE,
                                                                featureDriftEnabled = FALSE))
    expect_equal(patchStub$calledTimes(), 1)
    expect_false(bodyForInspect$featureDrift$enabled)
    expect_true(bodyForInspect$targetDrift$enabled)
    expect_is(settings, "list")
    ExpectHasKeys(settings, c("predictionIntervals", "targetDrift",
                              "associationId", "featureDrift"))
    expect_is(settings$predictionIntervals, "list")
    ExpectHasKeys(settings$predictionIntervals, c("percentiles", "enabled"))
    expect_is(settings$featureDrift, "list")
    ExpectHasKeys(settings$featureDrift, "enabled")
    expect_is(settings$associationId, "list")
    ExpectHasKeys(settings$associationId, c("columnNames", "requiredInPredictionRequests"))
  })
})
