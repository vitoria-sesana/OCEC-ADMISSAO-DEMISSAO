require(dplyr)

# leitura -----------------------------------------------------------------

caminho_pibtrim <- "bases/IJSN/PIB_Trimestral_2025_III.xlsx"


pibtrim <- 
  readxl::read_xlsx(caminho_pibtrim, sheet = "Planilha7") 


pibtrim_tratado <-
  pibtrim |>
  # estrutura da base
  janitor::row_to_names(1) |>
  janitor::clean_names() |>
  dplyr::slice(-96) |>
  mutate(ano_trim = na) |>
  # novas colunas 
  tidyr::separate(
    col = na,   
    into = c("ano", "trimestre"), 
    sep = "\\."            
  ) |>
  # padronização da base 
  filter(ano >= 2008 & ano < 2025)


# saida -------------------------------------------------------------------
write.csv2(pibtrim_tratado, "scripts/a-microdados/h-completo/tabela_pib.csv")
