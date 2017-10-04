context("ListAttributes")
# List an object

test_that("errors work",{
  # Wrong dataset
  expect_error(ListAttributes("gene"))
  # List of right dataset
  expect_error(ListAttributes(c("GENE","NETWORK","TF")))
  # Wrong data input class
  expect_error(ListAttributes(data.frame("GENE")))

})
