rm(list = ls())

# pacotes -----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(forecast)
library(tseries)
library(lubridate)
library(zoo)
library(psych)

# leitura -----------------------------------------------------------------
caminho_tabela_trimestral <- 
  "scripts/a-microdados/h-completo/saidas/tabela_trimestral.csv"

tabela_trimestral <- 
  read.csv2(caminho_tabela_trimestral) 


# bases temporais -----------------------------------------------------
base_ts <- tabela_trimestral %>%
  mutate(
    ano = as.numeric(substr(tempo, 1, 4)),
    trimestre = case_when(
      grepl(" - I$",  tempo) ~ 1,
      grepl(" - II$", tempo) ~ 2,
      grepl(" - III$", tempo) ~ 3,
      grepl(" - IV$", tempo) ~ 4
    )
  ) %>%
  arrange(classe, ano, trimestre)

cria_ts <- function(df, variavel) {
  ts(
    df[[variavel]],
    start = c(min(df$ano), min(df$trimestre)),
    frequency = 4
  )
}

## criativo -----
ts_criativo_resultados <- base_ts %>%
  filter(classe == "Setor criativo") %>%
  cria_ts("resultados")

ts_criativo_base100 <- base_ts %>%
  filter(classe == "Setor criativo") %>%
  cria_ts("resultados_base100")

## nao criativo ----- 
ts_nao_criativo_resultados <- base_ts %>%
  filter(classe == "Setor não criativo") %>%
  cria_ts("resultados")

ts_nao_criativo_base100 <- base_ts %>%
  filter(classe == "Setor não criativo") %>%
  cria_ts("resultados_base100")

## pib -----
ts_pib_resultados <- base_ts %>%
  filter(classe == "Pib") %>%
  cria_ts("resultados")

ts_pib_base100 <- base_ts %>%
  filter(classe == "Pib") %>%
  cria_ts("resultados_base100")
ts_pib_resultados

## removendo
rm(base_ts, cria_ts, caminho_tabela_trimestral)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# análises ---------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## correlação das séries -----
cor(
  subset(tabela_trimestral, classe == "Setor criativo")$resultados,
  subset(tabela_trimestral, classe == "Setor não criativo")$resultados
)

## deixando as séries estacionárias ------
ts_criativo_diff <- diff(ts_criativo_resultados, lag = 4)
plot(ts_criativo_diff)
tseries::adf.test(ts_criativo_diff)

ts_nao_criativo_diff <- diff(ts_nao_criativo_resultados, lag = 4)
plot(ts_nao_criativo_diff)
tseries::adf.test(ts_nao_criativo_diff)

tseries::adf.test(ts_pib_resultados)


# análises de comovimentos ----- 

## 1: extração do ciclo ------------
library(mFilter)

ciclo_pib <- hpfilter(ts_pib_resultados, freq = 1600)$cycle
ciclo_criativo <- hpfilter(ts_criativo_diff, freq = 1600)$cycle
ciclo_nao_criativo <- hpfilter(ts_nao_criativo_diff, freq = 1600)$cycle


## 2: CCF: função de correlação cruzada ------
analise_comovimento_c <- stats::ccf(ciclo_pib, ciclo_criativo, lag.max = 8, plot = TRUE)
analise_comovimento_nc <- stats::ccf(ciclo_pib, ciclo_nao_criativo, lag.max = 8, plot = TRUE)

summary(analise_comovimento_c)
stats::ccf(ciclo_pib, ciclo_criativo, lag.max = 8, plot = FALSE)


ccf(ciclo_pib, ciclo_criativo) # ciclo das séries
# pró ciclíca e coincidente 
ccf(ts_pib_resultados, ts_criativo_diff) # séries estacionárias
# prever o que vai acontecer na economia, no pib
# pró ciclíca e Antecedente  

## discutir a questão da série usando ela estacionária ou ajustada pelo HP

ccf(ciclo_pib, ciclo_nao_criativo) # ciclo das séries
# pró ciclíca e coincidente 
ccf(ts_pib_resultados, ts_nao_criativo_diff) # séries estacionárias
# prever o que vai acontecer na economia, no pib
# pró ciclíca e Antecedente  
