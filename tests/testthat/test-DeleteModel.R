context("Test DeleteModel")
library(stubthat)
library(testthat)

test_that("Required parameters are present", {
  expect_error(DeleteModel())
  expect_error(DeleteModel(modelToDelete$modelId))
  expect_error(DeleteModel(modelToDelete$projectId))
})

completedModelResponse <- httr:::response(url = modelUrl,
                                          status_code = 200L,
                                          content = raw(0))
test_that("DeleteModel succeeds", {
  deleteStub <- stub(httr::DELETE)
  deleteStub$onCall(1)$returns(completedModelResponse)
  expect_message(with_mock("httr::DELETE" = deleteStub$f,
                           "datarobot:::Endpoint" = function() fakeEndpoint,
                           "datarobot:::Token" = function() fakeToken,
                           DeleteModel(fakeModel)), "deleted")
})
