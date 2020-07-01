context("GetModelCapabilities")
library(stubthat)
library(testthat)

capabilitiesUrl <- UrlJoin(projectUrl, "models", fakeModelId, "supportedCapabilities")
capabilities <- list(supportsMonotonicConstraints = FALSE, supportsBlending = TRUE,
                     hasParameters = FALSE, eligibleForPrime = TRUE, hasWordCloud = TRUE)
capabilitiesResponse <- httr:::response(url = capabilitiesUrl,
                                        status_code = 200L,
                                        content = charToRaw(jsonlite::toJSON(capabilities)))

test_that("GetModelCapabilities succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(capabilitiesResponse)
  capabilitiesReturn <- with_mock("httr::GET" = getStub$f,
                                  "datarobot:::Endpoint" = function() fakeEndpoint,
                                  "datarobot:::Token" = function() fakeToken,
                                  GetModelCapabilities(fakeModel))
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(capabilitiesReturn, capabilities)
})
