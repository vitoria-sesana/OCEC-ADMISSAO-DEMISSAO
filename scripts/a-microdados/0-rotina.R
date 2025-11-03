rm(list = ls())

# pacotes ----------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(geobr)

# leitura base CAGED pré processada --------------------------------------------
base <- arrow::read_parquet("bases/cagedmov_ES_2024_tratado.parquet")

# leitura base RAIS: total de empregos ES 2024 ---------------------------------
base_total_empregos_muni_ES_2024 <- 
  read.csv("bases/RAIS/total_empregos_muni_ES_2024.csv") 

base_total_empregos_ES_2024 <- 
  read.csv("bases/RAIS/total_empregos_ES_2024.csv") |>
  select(Total) |> 
  as.numeric()
# fonte: vinculos rais 2024 (victor -> base de dados)

# leitura base RAIS: total de empregos ES 2023 ----------------------------
base_total_empregos_muni_ES_2023 <- 
  read.csv("bases/RAIS/total_empregos_muni_ES_2023.csv") 

base_total_empregos_ES_2023 <- 
  base_total_empregos_muni_ES_2023$total |> 
  sum() |> 
  as.integer()
# fonte qntd_vinculo do estabelecimento 2023 (base de dados)


# # CBO ---------------------------------------------------------------------
# base_cbo <- read.csv("bases/dicionario_CBO_CRIATIVO.csv")
# 
# # CNAE --------------------------------------------------------------------
# base_cnae <- read.csv("bases/dicionario_CNAE_CRIATIVO.csv") %>% 
#   mutate(
#     cnae_trat = as.integer(stringr::str_replace_all(CNAE, "[^0-9]", "")),
#   )

