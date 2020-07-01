context("Test GetRulesets")
library(stubthat)
library(testthat)

rulesetsUrl <- UrlJoin(projectUrl, "models", fakeModelId, "primeRulesets")
rulesetsJson <- fileToChar("responses/primeRulesets.json")
completedRulesetsResponse <- httr:::response(url = rulesetsUrl,
                                             status_code = 200L,
                                             content = charToRaw(rulesetsJson))

test_that("GetRulesets succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedRulesetsResponse)
  primeRulesets <- with_mock("httr::GET" = getStub$f,
                             "datarobot:::Endpoint" = function() fakeEndpoint,
                             "datarobot:::Token" = function() fakeToken,
                             GetRulesets(fakeProject, fakeModelId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(primeRulesets, "list")
  expect_is(primeRulesets[[1]]$projectId, "character")
  expect_is(primeRulesets[[1]]$ruleCount, "integer")
  expect_is(primeRulesets[[1]]$parentModelId, "character")
  expect_is(primeRulesets[[1]]$projectId, "character")
  expect_is(primeRulesets[[1]]$score, "numeric")
})
