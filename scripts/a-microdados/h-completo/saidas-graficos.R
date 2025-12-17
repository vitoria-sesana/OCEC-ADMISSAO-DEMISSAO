# analise descritiva das séries
# tabela das séries? ano, trimestre, setor criativo, setor não criativo, pib, geral

# analise comparativa entre criativos e não criativos
# comaparar com taxa de rotatividade geral 
# comparar com a variação do pib
# transformar as analises mensais da taxa de rotatividade em trimestrais: média?

# correlação cruzada 
# correlação entre as séries positivo (próciclico) e negativo 
# como verificar se a série é sucetivel a volatitilidade 
# taxa média da rotatividade entre criativos e não criativos

# perguntas: a volatidade cai ou decai?
# teste de grandewech; defasagem temporal

# leitura -----------------------------------------------------------------
# taxas de rotatividade calculadas
tabela_geral <- read.csv2("scripts/a-microdados/h-completo/tabela_geral.csv")
tabela_criativos <- read.csv2("scripts/a-microdados/h-completo/tabela_setores_criativos.csv")
tabela_pib <- read.csv2("scripts/a-microdados/h-completo/tabela_pib.csv")

tabela_criativos_trimestral <- 
  tabela_criativos |>
  mutate(
    trimestre = case_when(
      mes %in% c(1,2,3) ~ "I",
      mes %in% c(4,5,6) ~ "II",
      mes %in% c(7,8,9) ~ "III",
      mes %in% c(10,11,12) ~ "IV"
    )
  ) |>
  group_by(setor_criativo, ano, trimestre) |>
  summarise(
    resultados_taxa_rotatividade = mean(resultados_taxa_rotatividade)
  ) |> 
  mutate(classe = setor_criativo) |>
  select(classe, ano, trimestre, resultados_taxa_rotatividade)
  
tabela_geral_trimestral <- 
  tabela_geral |>
  mutate(
    trimestre = case_when(
      mes %in% c(1,2,3) ~ "I",
      mes %in% c(4,5,6) ~ "II",
      mes %in% c(7,8,9) ~ "III",
      mes %in% c(10,11,12) ~ "IV"
    )
  ) |> 
  group_by(classe, ano, trimestre) |>
  summarise(
    resultados_taxa_rotatividade = mean(resultados_taxa_rotatividade)
  )


# indicador de base 100 pib -----------------------------------------------

referencia_pib <- 
  tabela_pib |> 
  filter(trimestre == "IV" & ano == 2024) |>
  mutate(pib_nominal_ajustado_ao_benchmark_anual = as.numeric(pib_nominal_ajustado_ao_benchmark_anual))

tabela_pib_trimestral <-
  tabela_pib |>
  mutate(
    pib = as.numeric(pib_nominal_ajustado_ao_benchmark_anual),
    classe = "pib",
    e0_pib = as.numeric(referencia_pib$pib_nominal_ajustado_ao_benchmark_anual),
    resultados_pib = round((pib_nominal_ajustado_ao_benchmark_anual / e0_pib), 3)
    )

tabela_pib$
