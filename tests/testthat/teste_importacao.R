context("importacao")

antigo <- Sys.getlocale("LC_CTYPE")
on.exit(Sys.setlocale("LC_CTYPE", antigo))
Sys.setlocale("LC_CTYPE", "C")

arquivos <- dir(system.file("inst", "ext", package = "rais"), full.names = TRUE)
nomes <- c("cnae_cod", "cnae_desc", "tipo", "ano", "regiao",
           "micro_cod", "micro_desc", "quantidade")

test_that("le adequadamente", {
  expect_silent(
    planilha <- read_rais(arquivos[[1]])
  )
  expect_true(ncol(planilha) > 4)
  expect_identical(names(planilha)[[1]], "CNAE 2.0 Subclasse")
})

test_that("organiza a tabela", {
  expect_silent(
    tidy <- arquivos[[2]] %>%
      read_rais() %>%
      tidy_rais()
  )
  expect_identical(ncol(tidy), 8L)
  expect_identical(names(tidy), nomes)
})

