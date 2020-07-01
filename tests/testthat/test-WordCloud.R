context("Test GetWordCloud")
library(stubthat)
library(testthat)

wordCloudUrl <- UrlJoin(projectUrl, "models", fakeModelId, "wordCloud")
wordCloudJson <- fileToChar("responses/wordCloud.json")
completedWordCloudResponse <- httr:::response(url = wordCloudUrl,
                                              status_code = 200L,
                                              content = charToRaw(wordCloudJson))

test_that("GetWordCloud succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedWordCloudResponse)
  wordCloud <- with_mock("httr::GET" = getStub$f,
                             "datarobot:::Endpoint" = function() fakeEndpoint,
                             "datarobot:::Token" = function() fakeToken,
                             GetWordCloud(fakeProject, fakeModelId))
  expect_equal(getStub$calledTimes(), 1)
  expect_is(wordCloud, "data.frame")
  expect_is(wordCloud$coefficient, "numeric")
  expect_is(wordCloud$count, "integer")
  expect_is(wordCloud$ngram, "character")
  expect_is(wordCloud$frequency, "numeric")
  expect_is(wordCloud$isStopword, "logical")
})
