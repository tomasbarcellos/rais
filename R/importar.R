#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Ler tabelas extraidas do BI da RAIS
#'
#' @param caminho Caminho para arquivo
#'
#' @return um data.frame
#' @export
#'
read_rais <- function(caminho) {
  stopifnot(stringr::str_detect(caminho, "csv$"))

  info <- stringr::str_extract(caminho, "(?<=/)([\\w\\-]+_?){3}(?=\\.csv)") %>%
    stringr::str_split("_", 3) %>%
    `[[`(1)
  locale <- readr::locale(encoding = "latin1")

  suppressMessages(
    suppressWarnings(
      res <- readr::read_csv2(caminho, skip = 1, locale = locale) %>%
        dplyr::mutate(tipo = info[[1]], ano = info[[2]], regiao = info[[3]])
    )
  )

  linha_total <- which(stringr::str_to_lower(res[[1]]) == "total")
  col_total <- which(stringr::str_to_lower(names(res)) == "total")

  res[seq_len(linha_total-1), -col_total]
}

#' Arrumar dados da RAIS
#'
#' @param df O data.frame gerado por \link{read_rais}
#'
#' @return Um data.frame "tidy"
#' @export
#'
tidy_rais <- function(df) {
  df %>%
    tidyr::gather(micro, quantidade, -`CNAE 2.0 Subclasse`, -(tipo:regiao)) %>%
    tidyr::separate(`CNAE 2.0 Subclasse`, c("cnae_cod", "cnae_desc"), ":") %>%
    tidyr::separate(micro, c("micro_cod", "micro_desc"), ":")
}

