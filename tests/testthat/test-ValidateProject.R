context("ValidateProject")
library(testthat)
library(stubthat)

allowedPizzaToppings <- c("cheese", "mushroom", "pineapple", "spinach")

describe("ValidateProject", {
  test_that("it returns a projectId from a project", {
    expect_equal(ValidateProject(fakeProject), fakeProjectId)
  })

  test_that("it returns a projectId from a projectId", {
    expect_equal(ValidateProject(fakeProjectId), fakeProjectId)
  })

  test_that("it errors if nothing is found", {
    expect_error(ValidateProject(list(nothing = "to see here")), "does not contain a valid project")
  })
})


describe("IsParameterIn", {
  test_that("It is TRUE if the parameter is in the set", {
    expect_true(IsParameterIn("cheese", allowedPizzaToppings))
    expect_true(IsParameterIn("mushroom", allowedPizzaToppings))
  })

  test_that("It is an error if the parameter is not in the set", {
    expect_equal(IsParameterIn("bicycle", allowedPizzaToppings),
                 paste0("Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
                        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
                        ", ", sQuote("spinach"), " but got ", sQuote("bicycle"), " instead."))
    expect_equal(IsParameterIn("France", allowedPizzaToppings),
                 paste0("Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
                        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
                        ", ", sQuote("spinach"), " but got ", sQuote("France"), " instead."))
  })

  test_that("A length > 1 vector is an error", {
    expect_equal(IsParameterIn(c("cheese", "mushroom"), allowedPizzaToppings),
                 paste0("Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
                        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
                        ", ", sQuote("spinach"), " but got ",
                        sQuote("c(cheese, mushroom)"), " instead."))
  })

  test_that("NULL is allowed if allowNULL = TRUE", {
    expect_true(IsParameterIn(NULL, allowedPizzaToppings))
    expect_true(IsParameterIn(NULL, allowedPizzaToppings, allowNULL = TRUE))
  })

  test_that("NULL is not allowed if allowNULL = FALSE", {
    expect_equal(IsParameterIn(NULL, allowedPizzaToppings, allowNULL = FALSE),
                 paste0("Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
                        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
                        ", ", sQuote("spinach"), " but got ", sQuote("NULL"), " instead."))
  })

  test_that("It accepts `paramName`", {
    expect_equal(IsParameterIn("bicycle", allowedPizzaToppings, paramName = "foo"),
                 paste0("Invalid ", sQuote("foo"), ". Must be in ",
                        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
                        ", ", sQuote("spinach"), " but got ", sQuote("bicycle"), " instead."))
  })

  test_that("It has a sensical error message for unnamed `paramPossibilities`", {
    expect_equal(IsParameterIn("bicycle", c("cheese", "milk")),
                 paste0("Invalid ", sQuote("value"), ". Must be in ",
                        sQuote("cheese"), ", ", sQuote("milk"),
                        " but got ", sQuote("bicycle"), " instead."))
  })
})


describe("ValidateParameterIn", {
  test_that("It is TRUE if the parameter is in the set", {
    expect_true(ValidateParameterIn("cheese", allowedPizzaToppings))
    expect_true(ValidateParameterIn("mushroom", allowedPizzaToppings))
  })

  test_that("It raises an error if the parameter is not in the set", {
    expect_error(ValidateParameterIn("bicycle", allowedPizzaToppings),
                 paste0("Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
                        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
                        ", ", sQuote("spinach"), " but got ", sQuote("bicycle"), " instead."),
                 fixed = TRUE)
  })

  test_that("NULL is allowed if allowNULL = TRUE", {
    expect_true(ValidateParameterIn(NULL, allowedPizzaToppings))
    expect_true(ValidateParameterIn(NULL, allowedPizzaToppings, allowNULL = TRUE))
  })

  test_that("NULL is not allowed if allowNULL = FALSE", {
    expect_error(ValidateParameterIn(NULL, allowedPizzaToppings, allowNULL = FALSE),
                 paste0("Invalid ", sQuote("allowedPizzaToppings"), ". Must be in ",
                        sQuote("cheese"), ", ", sQuote("mushroom"), ", ", sQuote("pineapple"),
                        ", ", sQuote("spinach"), " but got ", sQuote("NULL"), " instead."))
  })

  test_that("It has a sensical error message for unnamed `paramPossibilities`", {
    expect_error(ValidateParameterIn("bicycle", c("cheese", "milk")),
                 paste0("Invalid ", sQuote("value"), ". Must be in ",
                        sQuote("cheese"), ", ", sQuote("milk"),
                        " but got ", sQuote("bicycle"), " instead."),
                 fixed = TRUE)
  })
})
