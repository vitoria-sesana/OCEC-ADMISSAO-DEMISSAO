require(dplyr)

# leitura -----------------------------------------------------------------

caminho_pibtrim <- "bases/IJSN/PIB_Trimestral_2025_III.xlsx"

pibtrim <- 
  readxl::read_xlsx(caminho_pibtrim, sheet = "Valores") |>
  janitor::row_to_names(3) |>
  janitor::clean_names()
  
pibtrim_tratado <-
  pibtrim |> 
  select(ano_6, trimestre_6, espirito_santo_4) |>
  tidyr::fill(ano_6) |>
  rename(ano = ano_6, trimestre = trimestre_6, resultados = espirito_santo_4) |>
  filter(ano >= 2008 & ano < 2025)


# pibtrim_tratado <-
#   pibtrim |>
#   # estrutura da base
#   janitor::row_to_names(1) |>
#   janitor::clean_names() |>
#   dplyr::slice(-96) |>
#   mutate(ano_trim = na) |>
#   # novas colunas 
#   tidyr::separate(
#     col = na,   
#     into = c("ano", "trimestre"), 
#     sep = "\\."            
#   ) |>
#   # padronização da base 
#   filter(ano >= 2008 & ano < 2025)


# saida -------------------------------------------------------------------
write.csv2(pibtrim_tratado, "scripts/a-microdados/h-completo/saidas/tabela_pib.csv")

