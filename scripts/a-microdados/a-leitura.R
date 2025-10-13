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
caminho_caged_2024 <- "bases/cagedmov_ES_2024.csv"

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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# análises base caged 2024 ------------------------------------------------
# variáveis de interesse: ocupacao criativa, admitidos e demitidos (com e sem agrupação)
print("Tabela frequência das ocupações criativas e não criativas.")
base[,.(Total=.N),by = ocupacao_criativo] 

print("Tabela frequência das ocupações criativas e não criativas, série histórica.")
tab_01 <- base[,.(Total=.N), by = c("ocupacao_criativo", "mes")]
setorder(tab_01, mes)
tab_01 

print("Tabela frequência dos tipos de movimentação.")
base[,.(Total=.N),by = tipo_movimentacao] 

print("Tabela frequência dos tipos de movimentação agrupados.")
base[,.(Total=.N),by = tipo_movimentacao_agrup] 

print("Tabela frequência dos tipos de movimentação agrupados e o saldo.")
base[,.(Total=.N, Sum=sum(saldo_movimentacao)),by = tipo_movimentacao_agrup] 

# outras variáveis 
print("Tabela frequência das fontes de movimentação.")
base[,.(Total=.N),by = origem_informacao] 

print("Tabela frequência indicador de aprendiz.")
base[,.(Total=.N),by = indicador_aprendiz] 


# análise cruzada ---------------------------------------------------------
base[,.(Total=.N), by = c(tipo_movimentacao_agrup, ocupacao_criativo)] 
