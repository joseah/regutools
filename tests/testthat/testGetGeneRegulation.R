context("GetGeneRegulation.R")

test_that("one gene as input works in all cases", {

  # Solo un gen, format multirow, output.type TF
  expect_equivalent(GetGeneRegulation("araC"),data.frame("genes"=c("araC","araC","araC"),
                                                         "regulators"=c("CRP","AraC","XylR"),
                                                         "effect"=c("+","+/-","-"),
                                                         stringsAsFactors=FALSE))

  # Solo un gen format onerow, output.type TF
  #expect_equivalent(GetGeneRegulation("araC",format="onerow"),data.frame("genes"=c("araC","araC","araC"),
                                                        # "regulators"=c("CRP","AraC","XylR"),
                                                        # "effect"=c("+","+/-","-"),
                                                        # stringsAsFactors=FALSE))
  # Solo un gen format table, output.type TF

  # Solo un gen, format multirow, output.type GENE
  expect_equivalent(GetGeneRegulation("araC", output.type="GENE"),data.frame("genes"=c("araC","araC","araC"),
                                                         "regulators"=c("araC","crp","xylR"),
                                                         "effect"=c("+/-","+","-"),
                                                         stringsAsFactors=FALSE))

  # Solo un gen format onerow, output.type GENE
  # Solo un gen format table, output.type GENE

  # Un gen en formato ID format multirow, output.type TF

})

test_that("list of genes as input works in all cases",{
  # Lista con varios genes, format multirow, output.type TF
  # Lista con varios genes format onerow, output.type TF
  # Lista con varios genes format table, output.type TF
  # Lista con genes en formato ID

  # Lista con varios genes, format multirow, output.type GENE
  # Lista con varios genes format onerow, output.type GENE
  # Lista con varios genes format table, output.type GENE

  # Lista de genes en formato ID format multirow, output.type TF
  # Lista de genes en formato ID y name, format multirow, output.type TF
})

test_that("errors work",{
  # Un gen que no existe
  expect_error(GetGeneRegulation("wrong_gene"))
  # Un gen, format que no existe
  expect_error(GetGeneRegulation("araC",format="wrong_format"))
  # Un gen, on output.type que no existe
  expect_error(GetGeneRegulation("araC",output.type="wrong_type"))
  # Lista de genes con algunos que no existen
  #expect_error(GetGeneRegulation(c("araC","araB","wrong_gene")))

})

