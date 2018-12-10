library(tidyverse)

arquivos <- dir("^trabalhadores_.+\\.csv$", full.names = TRUE)

read_rais <- function(caminho) {
  regiao <- str_extract(caminho, "(?<=_)([a-z]+_?){1,2}(?=\\.csv)")
  suppressMessages(
    suppressWarnings(
      res <- read_csv2(caminho, skip = 1, locale = locale(encoding = "latin1")) %>% 
        mutate(regiao = str_replace(regiao, "_", " "))
    )
  )
  
  linha_total <- which(str_to_lower(res[[1]]) == "total")
  col_total <- which(str_to_lower(names(res)) == "total")
  res[seq_len(linha_total-1), -col_total]
}

tabela <- map(arquivos, read_rais) %>% 
  map_df(gather, micro, quantidade, -`CNAE 2.0 Subclasse`, -regiao) %>% 
  separate(`CNAE 2.0 Subclasse`, c("cnae_cod", "cnae_desc"), ":") %>% 
  separate(micro, c("micro_cod", "micro_desc"), ":")
