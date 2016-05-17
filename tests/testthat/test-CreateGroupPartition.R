#
#  CreateGroupPartitionTest.R - testthat-based unit test for CreateGroupParition
#

context("Test CreateGroupPartition")

test_that("Required parameters are present", {
  expect_error(CreateGroupPartition())
  expect_error(CreateGroupPartition(validationType = 'CV'))
  expect_error(CreateGroupPartition(validationType = 'CV', holdoutPct = 20))
})

test_that("validationType = 'CV' option", {
  expect_error(CreateGroupPartition(validationType = 'CV', holdoutPct = 20,
                                    partitionKeyCols = c("tax", "crim")),
                "reps must be specified")
  ValidCase <- CreateGroupPartition(validationType = 'CV', holdoutPct = 20,
                                    partitionKeyCols = c("tax", "crim"),
                                     reps = 5)
  expect_equal(length(ValidCase), 5)
  expect_equal(ValidCase$cvMethod, "group")
  expect_equal(ValidCase$validationType, "CV")
  expect_equal(ValidCase$holdoutPct, 20)
  expect_equal(ValidCase$partitionKeyCols, c("tax", "crim"))
  expect_equal(ValidCase$reps, 5)
})

test_that("validationType = 'TVH' option", {
  expect_error(CreateGroupPartition(validationType = 'TVH', holdoutPct = 20,
                                      partitionKeyCols = c("tax", "crim")),
                                      "validationPct must be specified")
  ValidCase <- CreateGroupPartition(validationType = 'TVH', holdoutPct = 20,
                                    partitionKeyCols = c("tax", "crim"),
                                    validationPct = 16)
  expect_equal(length(ValidCase), 5)
  expect_equal(ValidCase$cvMethod, "group")
  expect_equal(ValidCase$validationType, "TVH")
  expect_equal(ValidCase$holdoutPct, 20)
  expect_equal(ValidCase$partitionKeyCols, c("tax", "crim"))
  expect_equal(ValidCase$validationPct, 16)
})


test_that("Invalid validationType returns message", {
  expect_error(CreateGroupPartition(validationType = 'XYZ', holdoutPct = 20,
                                    partitionKeyCols = c("tax", "crim"),
                                     validationPct = 16),
                                    "not valid for group partitions")
})
