# desenvolver rotina para obter a base anual
# tem 3 opções de bases para coletar no servidor do mte

library(data.table)

caminho <- "bases/CAGEDMOV202507/CAGEDMOV202507.txt"

primeira_linha <-
  data.table::fread(
    file = caminho,
    sep = ";", dec = ",",
    # encoding = "Latin-1" , 
    data.table = TRUE ,
    nrows = 1
    )

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

setorder(dado, uf)
mapa_caract <- dado[,.(Total=.N),by=uf]
mapa_caract[,Inicial:=cumsum(Total)-Total]
mapa_caract[,Final:=cumsum(Total)]

UF_escolhida <- "32"
inicio <- mapa_caract[uf==UF_escolhida,Inicial] 
final <- mapa_caract[uf==UF_escolhida,Final]

# dado <-
#   data.table::fread(
#     file = caminho,
#     sep = ";", dec = ",",
#     # encoding = "Latin-1" , 
#     # header = T,
#     data.table = TRUE,
#     skip = inicio,
#     nrows = final-inicio,
#     showProgress = FALSE 
#     )

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

