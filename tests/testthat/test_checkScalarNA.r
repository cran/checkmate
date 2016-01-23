context("checkScalarNA")

test_that("checkScalarNA", {
  expect_succ_all("ScalarNA", NA)
  expect_fail_all("ScalarNA", 1)
  expect_true(testScalarNA(NA_real_))
  expect_false(testScalarNA(1))
  expect_false(testScalarNA(rep(NA_character_, 2)))
  expect_true(is.expectation(expect_scalar_na(NA)))

  expect_error(assertScalarNA(integer(0)), "missing value")
})
