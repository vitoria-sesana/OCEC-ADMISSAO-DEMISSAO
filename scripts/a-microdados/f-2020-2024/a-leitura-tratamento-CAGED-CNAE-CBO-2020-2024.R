# análises caged movimentação 2024 ----------------------------------------
# vitória sesana
# outubro 2024

# pacotes -----------------------------------------------------------------
# install.packages("basedosdados")
library(basedosdados)
library(dplyr)
library(ggplot2)
library(data.table)

# leitura -----------------------------------------------------------------
caminho_CBO_CRIATIVOS <- "bases/dicionario_CBO_CRIATIVO.csv"
caminho_CNAE_CRIATIVOS <- "bases/dicionario_CNAE_CRIATIVO.csv"
caminho_caged_2024 <- "bases/cagedmov_ES_2020-2024.csv"

base <- 
  data.table::fread(
    file = caminho_caged_2024,
    sep = ",", dec = ".",
    data.table = TRUE,
    showProgress = FALSE
  ) |>
  janitor::clean_names()

cd_CBO_CRIATIVOS <- 
  read.csv(caminho_CBO_CRIATIVOS) |>
  janitor::clean_names() |>
  unlist() |>
  as.integer()

base_cnae <- read.csv(caminho_CNAE_CRIATIVOS) |> 
  mutate(
    cnae_2_subclasse = as.integer(stringr::str_replace_all(CNAE, "[^0-9]", "")),
  ) |> 
  janitor::clean_names() |> 
  select(cnae_2_subclasse, grande_setor, segmento, descricao_da_atividade)


# tratamento -----------------------------------------------------------------
base <- base[, mes := factor(mes, levels = c(1:12))]

# análises iniciais ------------------------------------------------------------
print("Tabela frequência dos tipos de movimentação.")
tab_mov <- base[,.(Total=.N),by = tipo_movimentacao] 
setorder(tab_mov, -Total)
tab_mov

# classificacao: admitidos e desligamentos --------------------------------
cd_tm_admissoes <- 
  c(10, 20, 25, 35, 70, 97)
cd_tm_desligamentos <- 
  c(31, 32, 33, 40, 43, 45, 50, 60, 80, 90, 98)
cd_tm_outros <-
  c(99)

base[, tipo_movimentacao_agrup :=
       fcase(
         tipo_movimentacao %in% cd_tm_admissoes, "Admissão",
         tipo_movimentacao %in% cd_tm_desligamentos, "Desligamento",
         tipo_movimentacao %in% cd_tm_outros, "Outros",
         default = NA
       )]

# classificação: ocupação criativos ----------------------------------------------------
base[, ocupacao_criativo := 
       fifelse(
         cbo_2002 %in% cd_CBO_CRIATIVOS, 
         "Ocupação Criativa",
         "Ocupação não Criativa"
       )]


base[, id_municipio_trat := as.numeric(
  stringr::str_sub(id_municipio, 1,6)    
)]


# CNAE --------------------------------------------------------------------
base_merge <- 
  left_join(base, base_cnae, by = c("cnae_2_subclasse")) |>
  as.data.table()

base_merge[,grande_setor := fifelse(is.na(grande_setor), "Setor não criativo", grande_setor)]
base_merge[,setor_criativo := fifelse(grande_setor == "Setor não criativo", "Setor não criativo", "Setor criativo")]
base_merge[,segmento := fifelse(is.na(segmento), "Segmento não criativo", grande_setor)]
base_merge[,descricao_da_atividade := fifelse(is.na(descricao_da_atividade), "Sem descrição", descricao_da_atividade)]

# saída -------------------------------------------------------------------

arrow::write_parquet(base_merge, "bases/cagedmov_ES_2020-2024_tratado.parquet")
