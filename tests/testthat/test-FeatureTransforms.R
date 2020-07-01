context("Feature Transforms")
library(testthat)
library(stubthat)

derivedFeaturesUrl <- datarobot:::UrlJoin(projectUrl, "typeTransformFeatures")
newFeatureName <- "new-feature"
newFeatureUrl <- datarobot:::UrlJoin(projectUrl, "features", newFeatureName)
statusUrl <- datarobot:::UrlJoin(fakeEndpoint, "status", "some-status")


test_that("derived feature functions work", {
  functionsToTest <- c(CreateDerivedFeatureAsNumeric, CreateDerivedFeatureAsCategorical,
                       CreateDerivedFeatureAsText, CreateDerivedFeatureIntAsCategorical)
  for (functionToTest in functionsToTest) {
    postStub <- stub(httr::POST)
    getStub <- stub(httr::GET)

    postStub$onCall(1)$expects(url = derivedFeaturesUrl)
    postStub$onCall(1)$returns(httr:::response(url = derivedFeaturesUrl,
                                               status_code = 202L,
                                               headers = list(location = statusUrl),
                                               content = raw(0)))

    getStub$onCall(1)$expects(url = statusUrl)
    getStub$onCall(1)$returns(httr:::response(url = statusUrl,
                                              status_code = 200L,
                                              content = charToRaw('{"status": "RUNNING"}')))

    getStub$onCall(2)$expects(url = statusUrl)
    getStub$onCall(2)$returns(httr:::response(url = statusUrl,
                                              status_code = 303L,
                                              headers = list(location = newFeatureUrl),
                                              content = raw(0)))

    getStub$onCall(3)$expects(url = newFeatureUrl)
    getStub$onCall(3)$returns(httr:::response(url = newFeatureUrl,
                                              status_code = 200L,
                                              content = raw(0))) # put feature data here if needed

    with_mock("httr::POST" = postStub$f,
              "httr::GET" = getStub$f,
              "datarobot:::Endpoint" = function() fakeEndpoint,
              "datarobot:::Token" = function() fakeToken, {
                output <- functionToTest(fakeProject, "old-feature", "new-feature")
                expect_equal(output, character(0))
              })
  }})
