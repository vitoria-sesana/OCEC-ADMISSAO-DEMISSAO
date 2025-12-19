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


# pacotes -----------------------------------------------------------------
require(dplyr)
require(ggplot2)
# leitura -----------------------------------------------------------------
# taxas de rotatividade calculadas
tabela_geral <- read.csv2("scripts/a-microdados/h-completo/saidas/tabela_geral.csv")
tabela_criativos <- read.csv2("scripts/a-microdados/h-completo/saidas/tabela_setores_criativos.csv")
tabela_pib <- read.csv2("scripts/a-microdados/h-completo/saidas/tabela_pib.csv")


# tratamento --------------------------------------------------------------

## base ccriativo mensal para trimestral ----
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
    resultados = mean(taxa_rotatividade)
  ) |> 
  mutate(classe = setor_criativo) |>
  select(classe, ano, trimestre, resultados)


referencia_criativos <- 
  tabela_criativos_trimestral |> ungroup() |>
  filter(ano == 2024 & trimestre == "IV") |>
  select(classe, resultados) |>
  rename(resultados_referencia = resultados)

tabela_criativos_trimestral <-
  left_join(tabela_criativos_trimestral, referencia_criativos, by = "classe") |>
  mutate(
    resultados_base100 = round((resultados/resultados_referencia), 3)
  )

## base geral mensal para trimestral ----
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
    resultados = mean(taxa_rotatividade)
  ) |> 
  select(classe, ano, trimestre, resultados)


referencia_geral <- 
  tabela_geral_trimestral |> ungroup() |>
  filter(ano == 2024 & trimestre == "IV") |>
  select(classe, resultados) |>
  rename(resultados_referencia = resultados)

tabela_geral_trimestral <-
  left_join(tabela_geral_trimestral, referencia_geral, by = "classe") |>
  mutate(
    resultados_base100 = round((resultados/resultados_referencia), 3)
  )

## indicador de base 100 pib -----------------------------------------------

referencia_pib <- 
  tabela_pib |> 
  filter(trimestre == "IV" & ano == 2024) |>
  mutate(resultados = as.numeric(resultados))

tabela_pib_trimestral <-
  tabela_pib |>
  mutate(
    classe = "Pib",
    pib = as.numeric(resultados),
    resultados_referencia = as.numeric(referencia_pib$resultados),
    resultados = pib,
    resultados_base100 = round((pib / resultados_referencia), 3)
    ) |>
  select(classe, ano, trimestre, resultados, resultados_referencia, resultados_base100) 


# todos os resultados -----------------------------------------------------

tabela_completa <-
  bind_rows(tabela_geral_trimestral, tabela_pib_trimestral) |>
  mutate(trimestre = factor(trimestre, levels = c("I", "II", "III", "IV"))) |>
  mutate(tempo = paste(ano, trimestre, sep = " - ")) |>
  arrange(ano, trimestre) %>%
  mutate(tempo = factor(tempo, levels = unique(tempo)))


# gráficos -----------------------------------------------------------------
# gráficos -----------------------------------------------------------------

cores <- c("Setor criativo" = "#1f77b4",
           "Setor não criativo" = "gray",
           "Geral" = "red", 
           "Pib" = "black")

breaks_x <- tabela_completa$tempo[
  grepl(" - I$", tabela_completa$tempo) &     # apenas 1º trimestre
    as.numeric(substr(tabela_completa$tempo, 1, 4)) %% 4 == 0
]

tabela_completa <- tabela_completa %>% 
  filter(classe != "Geral")

## 1 -----
ggplot(
  tabela_completa, 
  aes(
    x = tempo,
    y = resultados,
    color = classe,
    group = classe
  )
) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  scale_x_discrete(breaks = breaks_x) +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 61)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Taxa de rotatividade trimestral e evolução do PIB", subtitle = "Espírito Santo, 2008-2024") 

## 2 -----

ggplot(
  tabela_completa, 
  aes(
    x = tempo,
    y = resultados,
    color = classe,
    group = classe
  )
) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  scale_x_discrete(breaks = breaks_x) +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 7)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("Taxa de rotatividade trimestral", subtitle = "Espírito Santo, 2008-2024") +
  labs(color = "Legenda") +
  xlab("Tempo") +
  ylab("Resultados")


## 3 ----
ggplot(
  tabela_completa, 
  aes(
    x = tempo,
    y = resultados_base100,
    color = classe,
    group = classe
  )
) +
  geom_line(size = 1) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "green",
    linewidth = 0.8
  ) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  scale_x_discrete(breaks = breaks_x) +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 2)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    ) +
  ggtitle(
    "Índice de base 100 da taxa de rotatividade trimestral e PIB",
    subtitle = "Espírito Santo, 2008-2024"
    ) +
  labs(color = "Legenda") +
  xlab("Tempo") +
  ylab("Resultados")


# saidas ------------------------------------------------------------------

tabela_geral_trimestral
tabela_criativos_trimestral
tabela_pib_trimestral
tabela_completa

write.csv2(tabela_completa, "scripts/a-microdados/h-completo/saidas/tabela_trimestral.csv")
