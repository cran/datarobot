#
#  CreateUserPartitionTest.R - testthat-based unit test for CreateUserParition
#

context("Test CreateUserPartition")

test_that("Required parameters are present", {
  expect_error(CreateUserPartition())
  expect_error(CreateUserPartition(validationType = 'CV'))
})

test_that("validationType = 'CV' option", {
  expect_error(CreateUserPartition(validationType = 'CV',
                                   userPartitionCol = "TVHflag"),
               "cvHoldoutLevel must be specified")
  ValidCase <- CreateUserPartition(validationType = 'CV',
                                   userPartitionCol = "TVHflag",
                                   cvHoldoutLevel = "H")
  expect_equal(length(ValidCase), 4)
  expect_equal(ValidCase$cvMethod, "user")
  expect_equal(ValidCase$validationType, "CV")
  expect_equal(ValidCase$userPartitionCol, "TVHflag")
  expect_equal(ValidCase$cvHoldoutLevel, "H")
})

test_that("validationType = 'TVH' option", {
  expect_error(CreateUserPartition(validationType = 'TVH',
                                   userPartitionCol = "TVHflag"),
               "trainingLevel must be specified")
  expect_error(CreateUserPartition(validationType = 'TVH',
                                   userPartitionCol = "TVHflag",
                                   trainingLevel = "T",
                holdoutLevel = "H"), "validationLevel must be specified")
  ValidCase <- CreateUserPartition(validationType = 'TVH',
                                   userPartitionCol = "TVHflag",
                                   trainingLevel = "T",
                                   holdoutLevel = "H", validationLevel = "V")
  expect_equal(length(ValidCase), 6)
  expect_equal(ValidCase$cvMethod, "user")
  expect_equal(ValidCase$validationType, "TVH")
  expect_equal(ValidCase$userPartitionCol, "TVHflag")
  expect_equal(ValidCase$trainingLevel, "T")
  expect_equal(ValidCase$holdoutLevel, "H")
  expect_equal(ValidCase$validationLevel, "V")
  CreateUserPartition(validationType = 'TVH',
                      userPartitionCol = "TVHflag",
                      trainingLevel = "T",
                      validationLevel = "V")
})

test_that("Invalid validationType returns message", {
  expect_error(CreateUserPartition(validationType = 'XYZ',
                                   userPartitionCol = "TVHflag"),
               "not valid for user partitions")
})
