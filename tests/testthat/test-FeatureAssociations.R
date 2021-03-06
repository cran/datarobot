context("Feature associations")
library(stubthat)
library(testthat)


test_that("it can get a feature association matrix", {
  getStub <- stub(httr::GET)
  getFAMUrl <- UrlJoin(projectUrl, "featureAssociationMatrix")
  getFAMJson <- fileToChar("responses/featureAssociationMatrix.json")
  FAMResponse <- httr:::response(url = getFAMUrl,
                                 status_code = 200L,
                                 content = charToRaw(getFAMJson))
  getStub$onCall(1)$returns(FAMResponse)
  fam <- with_mock("httr::GET" = getStub$f,
                   "datarobot:::Endpoint" = function() fakeEndpoint,
                   "datarobot:::Token" = function() fakeToken,
                   GetFeatureAssociationMatrix(fakeProjectId, "association", "mutualInfo"))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(fam, "list")
  ExpectHasKeys(fam, c("strengths", "features"))
  expect_is(fam$strengths, "data.frame")
  ExpectHasKeys(fam$strengths, c("feature1", "feature2", "statistic"))
  expect_is(fam$features, "data.frame")
  ExpectHasKeys(fam$features, c("feature", "alphabeticSortIndex",
                                "importanceSortIndex", "strengthSortIndex"))
})

test_that("it can get feature association matrix details", {
  getStub <- stub(httr::GET)
  getFAMDetailsUrl <- UrlJoin(projectUrl, "featureAssociationMatrixDetails")
  getFAMDetailsJson <- fileToChar("responses/featureAssociationMatrixDetails.json")
  FAMDetailsResponse <- httr:::response(url = getFAMDetailsUrl,
                                        status_code = 200L,
                                        content = charToRaw(getFAMDetailsJson))
  getStub$onCall(1)$returns(FAMDetailsResponse)
  famDeets <- with_mock("httr::GET" = getStub$f,
                        "datarobot:::Endpoint" = function() fakeEndpoint,
                        "datarobot:::Token" = function() fakeToken,
                        GetFeatureAssociationMatrixDetails(fakeProjectId, "feature1", "feature2"))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(famDeets, "list")
  ExpectHasKeys(famDeets, c("values", "features", "types"))
  expect_is(famDeets$values, "data.frame")
  ExpectHasKeys(famDeets$values, c("feature1", "feature2", "relativeFreq"))
})
