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
