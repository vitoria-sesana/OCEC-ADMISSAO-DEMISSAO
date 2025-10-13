# classificar  movimentação (admissão e demissão)
# classificar em criativos e não criativos
# classificar o setor, segmento e atividade criativa??

# pacotes -----------------------------------------------------------------
library(data.table)

# leitura -----------------------------------------------------------------
caminho_CBO_CRIATIVOS <- "bases/dicionario_CBO_CRIATIVO.csv"
caminho_CAGED_ES_2025 <- "bases/CAGED_ES_202507.parquet"

cd_CBO_CRIATIVOS <- 
  read.csv(caminho_CBO_CRIATIVOS) |>
  janitor::clean_names() |>
  unlist() |>
  as.integer()

base <- 
  arrow::read_parquet(caminho_CAGED_ES_2025) 
setDT(base)

# classificação: movimentação ---------------------------------------------
print("Tabela frequência dos tipos de movimentação.")
base[,.(Total=.N),by = tipomovimentacao]

cd_tm_admissoes <- 
  c(10, 20, 25, 35, 70, 97)
cd_tm_desligamentos <- 
  c(31, 32, 40, 43, 45, 50, 60, 80, 90, 98)
cd_tm_outros <-
  c(33, 99)

base[, tipomovimentacao_agrup :=
       fcase(
         tipomovimentacao %in% cd_tm_admissoes, "Admissão",
         tipomovimentacao %in% cd_tm_desligamentos, "Desligamento",
         tipomovimentacao %in% cd_tm_outros, "Outros",
         default = NA
       )]

# classificação: ocupação criativos ----------------------------------------------------
base[, ocupacao_criativo := 
       fifelse(
         cbo2002ocupacao %in% cd_CBO_CRIATIVOS, 
         "Ocupação Criativa",
         "Ocupação não Criativa"
         )]


# análises ----------------------------------------------------------------
print("Tabela frequência das ocupações criativas.")
base[,.(Total=.N),by = ocupacao_criativo]

print("Tabela frequência dos tipos de movimentação.")
base[,.(Total=.N),by = tipomovimentacao]

print("Tabela frequência dos tipos de movimentação agrupado.")
base[,.(Total=.N),by = tipomovimentacao_agrup]

print("Tabela frequência dos tipos de movimentação por criativos e não criativos.")
base[, .(Total=.N),
     by = .(ocupacao_criativo,
            tipomovimentacao_agrup)]

print("Tabela frequência dos tipos de movimentação por criativos e não criativos - sem o código 97.")
base[tipomovimentacao != 97, .(Total=.N),
     by = .(ocupacao_criativo,
            tipomovimentacao_agrup)]

# há mais autonomos? informais?
# politicas publicas para a formalização?

print("Tabela frequência dos tipos de movimentação entre criativos.")
base[ocupacao_criativo != "Ocupação Criativa", .(Total=.N),
     by = .(ocupacao_criativo,
            tipomovimentacao_agrup)]

print("Tabela frequência dos tipos de movimentação entre criativos.")
base[ocupacao_criativo == "Ocupação Criativa", .(Total=.N),
     by = .(ocupacao_criativo,
            tipomovimentacao)]

# 40: por parte do empregado
# 97: admissões de tipo ignorado - o que significa?
# 97: admissão sem motivo especifícado

# a proporção de admitidos e demitidos entre as ocupações criativas é proporcional entre os criativos e não criativos??

