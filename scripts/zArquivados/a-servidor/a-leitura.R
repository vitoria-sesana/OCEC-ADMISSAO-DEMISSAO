# desenvolver rotina para obter a base anual
# tem 3 opções de bases para coletar no servidor do mte

# pacote ------------------------------------------------------------------
library(data.table)

# leitura -----------------------------------------------------------------
caminho <- "bases/arquivados/CAGEDMOV202507/CAGEDMOV202507.txt"

dado <- 
  fread(
    file = caminho , 
    sep = ";" , dec = "," , 
    select = 3L,
    header = T ,     
    # encoding = "Latin-1" , 
    data.table = TRUE ,
    showProgress = FALSE ) |> 
  janitor::clean_names()


# definindo ponto inicial e final -----------------------------------------
setorder(dado, uf)
mapa_caract <- dado[,.(Total=.N),by=uf]
mapa_caract[,Inicial:=cumsum(Total)-Total]
mapa_caract[,Final:=cumsum(Total)]

UF_escolhida <- "32"
inicio <- mapa_caract[uf==UF_escolhida,Inicial] 
final <- mapa_caract[uf==UF_escolhida,Final]


# filtrando informações ---------------------------------------------------
dado_completo <- fread(
  file = caminho,
  sep = ";", dec = ",",
  data.table = TRUE,
  showProgress = FALSE
) |>
  janitor::clean_names()

# Ordena pela terceira coluna
setorder(dado_completo, uf)

# Aplica o filtro de linhas equivalente a skip/nrows
dado <- dado_completo[(inicio + 1):(final)]

arrow::write_parquet(dado, "bases/CAGED_ES_202507.parquet")

