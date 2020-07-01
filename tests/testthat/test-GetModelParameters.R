context("Test GetModelParameters")

parametersUrl <- UrlJoin(projectUrl, "models", fakeModelId, "parameters")

parametersJson <- fileToChar("responses/modelParameters.json")
parametersResponse <- httr:::response(url = parametersUrl,
                                      status_code = 200L,
                                      content = charToRaw(parametersJson))
expectedCols <- c("coefficient", "type", "derivedFeature", "originalFeature",
                 "transformations", "stageCoefficients")

test_that("GetModelParameters succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(parametersResponse)
  testReturn <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetModelParameters(fakeProject, fakeModelId))
  expect_is(testReturn, "list")
  ExpectHasKeys(testReturn, c("parameters", "derivedFeatures"))
  expect_is(testReturn$parameters, "list")
  ExpectHasKeys(testReturn$derivedFeatures[[1]], expectedCols)
  expect_is(testReturn$derivedFeatures, "list")
  ExpectHasKeys(testReturn$parameters[[1]], c("name", "value"))
})

parametersJson <- fileToChar("responses/modelParametersTwoStage.json")
parametersResponse <- httr:::response(url = parametersUrl,
                                      status_code = 200L,
                                      content = charToRaw(parametersJson))

test_that("GetModelParameters succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(parametersResponse)
  testReturn <- with_mock("httr::GET" = getStub$f,
                          "datarobot:::Endpoint" = function() fakeEndpoint,
                          "datarobot:::Token" = function() fakeToken,
                          GetModelParameters(fakeProject, fakeModelId))
  expect_is(testReturn, "list")
  ExpectHasKeys(testReturn, c("parameters", "derivedFeatures"))
  expect_is(testReturn$parameters, "list")
  ExpectHasKeys(testReturn$derivedFeatures[[1]], expectedCols)
  expect_is(testReturn$derivedFeatures, "list")
  ExpectHasKeys(testReturn$parameters[[1]], c("name", "value"))
  ExpectHasKeys(testReturn$derivedFeatures[[1]]$stageCoefficients[[1]], c("coefficient", "stage"))
})
