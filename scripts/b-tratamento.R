# tratar o tipo de movimentação (admissão e demissão)
# classificar o setor, segmento e atividade criativa

base <- 
  arrow::read_parquet("bases/CAGED_ES_202507.parquet") 

x <- base[,.(Total=.N),by = tipomovimentacao]

x$Total |> sum()
