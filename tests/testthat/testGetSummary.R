# Un gen
# Lista de genes
# Output multirow
# Output onerow
# Output table
context("GetSummary")

test_that("errors work",{
  #Wrong gene
  expect_error(GetSummary("wrong_gene"))

  #Wrong list of genes
  expect_error(GetSummary(c("wrong_gene1","wrong_gene2")))

  #Wrong and right list of genes
  #expect_error(GetSummary(c("wrong_gene1","araC")))

  #Wrong data input class
  expect_error(GetSummary(data.frame("araC")))

  #Wrong data.frame
  expect_error(GetSummary(GetGeneRegulation("araC",format="table")))
  expect_error(GetSummary(GetGeneRegulation("araC",format="onerow")))

})
