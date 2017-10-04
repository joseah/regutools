context("GetNetwork")

test_that("all types of network are retrieved",{
  # type = GENE-GENE
  # type = TF-GENE
  # type = TF-TF
})

test_that("errors work",{
  expect_error(GetNetwork("wrong_type"))
})
