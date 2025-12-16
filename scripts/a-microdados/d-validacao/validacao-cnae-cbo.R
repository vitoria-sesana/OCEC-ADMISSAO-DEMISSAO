# validação das classificações CNAE e CBO nas bases RAIS e CAGED 2024

# pacotes -----------------------------------------------------------------
library(dplyr)

# leitura -----------------------------------------------------------------

## CNAE ------------------------
caminho_cnae_classe <- "bases/CONCLA/CNAE20_EstruturaDetalhada.xls"
# caminho_cnae_subclasse <- "bases/CONCLA/CNAE20_Subclasses_EstruturaDetalhada.xls" # 2.0
caminho_cnae_subclasse <- "bases/CONCLA/CNAE_Subclasses_2_3_Estrutura_Detalhada.xlsx" # 2.3

cnae_classe <- 
  readxl::read_xls(caminho_cnae_classe) %>% 
  janitor::clean_names()

cnae_subclasse <- 
  readxl::read_xlsx(caminho_cnae_subclasse) %>% 
  janitor::clean_names() 

cnae_classe_tratado <- 
  cnae_classe %>% 
  select(x4, x5) %>% 
  rename(cnae = x4, descricao = x5) %>% 
  slice(-c(1:2)) %>% 
  na.omit() %>% 
  filter(cnae != "Classe") %>% 
  mutate(cnae_num = as.integer(stringr::str_replace_all(cnae, "[^0-9]", "")))

cnae_subclasse_tratado <- 
  cnae_subclasse %>% 
  select(x5, x6) %>% 
  rename(cnae = x5, descricao = x6) %>% 
  slice(-c(1:3)) %>% 
  na.omit() %>% 
  mutate(cnae_num = as.integer(stringr::str_replace_all(cnae, "[^0-9]", "")))

## CBO ------------------------------------------
caminho_cbo <- "bases/CONCLA/ESTRUTURA CBO/CBO2002 - Ocupacao.csv"

cbo <- 
  read.csv2(caminho_cbo) %>% 
  janitor::clean_names()


## CAGEDMOV ------------------------------------------
caminho_caged_2024 <- "bases/cagedmov_ES_2024.csv"

caged_2024 <- 
  data.table::fread(
    file = caminho_caged_2024,
    sep = ",", dec = ".",
    data.table = TRUE,
    showProgress = FALSE
  ) |>
  janitor::clean_names()

## RAIS ------------------------------------------
caminho_rais <- "bases/RAIS/query_rais_es_2023.csv"

rais_2023 <- 
  data.table::fread(
    file = caminho_rais,
    sep = ",", dec = ".",
    data.table = TRUE,
    showProgress = FALSE
  ) |>
  janitor::clean_names()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# VALIDACAO --------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## validacao caged x cnae ----------------------------------------------------------
vec_caged <- caged_2024$cnae_2_subclasse %>% unique() 
vec_cnae <- cnae_subclasse_tratado$cnae_num %>% unique()

setdiff(vec_caged, vec_cnae) # tem no caged, não tem na cnae
setdiff(vec_cnae, vec_caged) # tem no cnae, mas não tem no caged
uniao <- intersect(vec_caged, vec_cnae) 
length(uniao) # tem em ambos

## validacao rais x cnae ----------------------------------------------------------
vec_rais <- rais_2023$cnae_2_subclasse %>% unique() 
vec_cnae <- cnae_subclasse_tratado$cnae_num %>% unique()

setdiff(vec_rais, vec_cnae)
setdiff(vec_cnae, vec_rais)

uniao <- intersect(vec_rais, vec_cnae)
length(uniao)
length(vec_cnae)
length(vec_rais)


## validacao caged x cbo ----------------------------------------------------------
vec_caged <- caged_2024$cbo_2002 %>% unique() 
vec_cbo <- cbo$codigo %>% unique()

setdiff(vec_caged, vec_cbo)
setdiff(vec_cbo, vec_caged)

uniao <- intersect(vec_caged, vec_cbo)
length(uniao)
length(vec_cbo)
length(vec_caged)

