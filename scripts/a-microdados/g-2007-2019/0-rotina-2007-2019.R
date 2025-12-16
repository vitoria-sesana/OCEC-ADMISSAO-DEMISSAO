rm(list = ls())

# pacotes ----------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(dplyr)

# leitura base CAGED pré processada --------------------------------------------
base <- arrow::read_parquet("bases/cagedmov_ES_2007-2019_tratado.parquet")

# CBO ---------------------------------------------------------------------
base_cbo_criativo <- read.csv("bases/dicionario_CBO_CRIATIVO.csv") |>
  janitor::clean_names()

# CNAE --------------------------------------------------------------------
base_cnae_criativo <- read.csv("bases/dicionario_CNAE_CRIATIVO.csv") %>%
  mutate(
    cnae_trat = as.integer(stringr::str_replace_all(CNAE, "[^0-9]", "")),
  ) |>
  janitor::clean_names()

# leitura base RAIS: 2006-2024 por cnae -----------------------------------
total_empregos_ES_anual_cnae_2006_2024 <- 
  read.csv("bases/RAIS/total_empregos_ES_anual_cnae_2006-2024.csv") |>
  rename(cnae_trat = cnae_2_subclasse) |>
  mutate(cnae_trat = as.integer(cnae_trat)) |>
  na.omit()

total_empregos_ES_anual_cnae_2006_2024_trat <- 
  left_join(
    total_empregos_ES_anual_cnae_2006_2024,
    base_cnae_criativo, 
    by = "cnae_trat"
  ) |>
  mutate(setor_criativo = ifelse(is.na(grande_setor),
                                 "Setor não criativo",
                                 "Setor criativo")) |>
  select(cnae_trat,ano,quantidade_total_vinculos_clt,setor_criativo)

base_total_empregos_ES_anual_cnae_2006_2024_final <-  
  total_empregos_ES_anual_cnae_2006_2024_trat |>
  group_by(setor_criativo,ano) |>
  summarise(quantidade_total_vinculos_clt = sum(quantidade_total_vinculos_clt))

rm(total_empregos_ES_anual_cnae_2006_2024_trat,
   total_empregos_ES_anual_cnae_2006_2024)

base <- base[ano != 2025,]
