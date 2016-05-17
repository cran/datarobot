#
#  CreateRandomPartitionTest.R - testthat-based unit test for CreateRandomParition
#

context("Test CreateRandomPartition")

test_that("Required parameters are present", {
  expect_error(CreateRandomPartition())
  expect_error(CreateRandomPartition(validationType = 'CV'))
})

test_that("validationType = 'CV' option", {
  expect_error(CreateRandomPartition(validationType = 'CV', holdoutPct = 20),
               "reps must be specified")
  ValidCase <- CreateRandomPartition(validationType = 'CV', holdoutPct = 20,
                                     reps = 5)
  expect_equal(length(ValidCase), 4)
  expect_equal(ValidCase$cvMethod, "random")
  expect_equal(ValidCase$validationType, "CV")
  expect_equal(ValidCase$holdoutPct, 20)
  expect_equal(ValidCase$reps, 5)
})

test_that("validationType = 'TVH' option", {
  expect_error(CreateRandomPartition(validationType = 'TVH', holdoutPct = 20),
               "validationPct must be specified")
  ValidCase <- CreateRandomPartition(validationType = 'TVH', holdoutPct = 20,
                                     validationPct = 16)
  expect_equal(length(ValidCase), 4)
  expect_equal(ValidCase$cvMethod, "random")
  expect_equal(ValidCase$validationType, "TVH")
  expect_equal(ValidCase$holdoutPct, 20)
  expect_equal(ValidCase$validationPct, 16)
})


test_that("Invalid validationType returns message", {
  expect_error(CreateRandomPartition(validationType = 'XYZ', holdoutPct = 20,
                                     validationPct = 16),
                                     "not valid for random partitions")
})
