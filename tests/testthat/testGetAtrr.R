library(testthat)
library(regutools)

test_check("regutools")
context("errors")

expect_error(1 / "a", "non-numeric argument")
