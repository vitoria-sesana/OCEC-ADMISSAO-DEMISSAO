rm(list = ls())

# pacotes -----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)

# leitura -----------------------------------------------------------------
caminho_tabela_trimestral <- 
  "scripts/a-microdados/h-completo/saidas/tabela_trimestral.csv"

tabela_trimestral <- 
  read.csv2(caminho_tabela_trimestral) 



# gráficos ----------------------------------------------------------------
cores <- c("Setor criativo" = "#1f77b4",
           "Setor não criativo" = "gray",
           "Geral" = "red", 
           "Pib" = "black")

breaks_x <- tabela_trimestral$tempo[
  grepl(" - I$", tabela_trimestral$tempo) &     # apenas 1º trimestre
    as.numeric(substr(tabela_trimestral$tempo, 1, 4)) %% 4 == 0
]


## 1 -----
ggplot(
  tabela_trimestral, 
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
    limits = c(0, 175)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Taxa de rotatividade trimestral e evolução do PIB", subtitle = "Espírito Santo, 2008-2024") 

## 2 -----

ggplot(
  tabela_trimestral, 
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
  tabela_trimestral, 
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
