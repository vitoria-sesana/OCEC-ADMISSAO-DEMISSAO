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

tabela_trimestral <- 
  read.csv2(
    "scripts/a-microdados/h-completo/saidas/tabela_trimestral.csv"
    ) %>% 
  select(classe, resultados, resultados_base100, tempo)


# tratamento --------------------------------------------------------------


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

ts_criativo_resultados <- base_ts %>%
  filter(classe == "Setor criativo") %>%
  cria_ts("resultados")

ts_criativo_base100 <- base_ts %>%
  filter(classe == "Setor criativo") %>%
  cria_ts("resultados_base100")

ts_nao_criativo_resultados <- base_ts %>%
  filter(classe == "Setor não criativo") %>%
  cria_ts("resultados")

ts_nao_criativo_base100 <- base_ts %>%
  filter(classe == "Setor não criativo") %>%
  cria_ts("resultados_base100")

ts_pib_resultados <- base_ts %>%
  filter(classe == "Pib") %>%
  cria_ts("resultados")

ts_pib_base100 <- base_ts %>%
  filter(classe == "Pib") %>%
  cria_ts("resultados_base100")

# validacao
frequency(ts_pib_resultados)
start(ts_pib_resultados)
end(ts_pib_resultados)

# analises temporais ------------------------------------------------------
# tendencia, sazonalidade, ciclos e pontos atípicos
# checar estacionariedade (não faz sentido para o nosso problema, não queremos modelar)
# verificar autocorrelação (parcial e simples)
# modelo é aditivo ou multiplicativo

# criativo 
describe(ts_criativo_resultados)
plot(ts_criativo_resultados, main = "Série Temporal", ylab = "Valor", xlab = "Tempo")

decomposicao <- stl(ts_criativo_resultados, s.window = "periodic")
plot(decomposicao)

acf(ts_criativo_resultados, main = "ACF")
pacf(ts_criativo_resultados, main = "PACF")

tseries::adf.test(ts_criativo_resultados) # não estacionária

# nao criativo
describe(ts_nao_criativo_resultados)
plot(ts_nao_criativo_resultados, main = "Série Temporal", ylab = "Valor", xlab = "Tempo")

decomposicao <- stl(ts_nao_criativo_resultados, s.window = "periodic")
plot(decomposicao)

acf(ts_nao_criativo_resultados, main = "ACF")
pacf(ts_nao_criativo_resultados, main = "PACF")

tseries::adf.test(ts_nao_criativo_resultados) # não estacionária

# correlação cruzada ------------------------------------------------------

# autoplot(cbind(ts_criativo_resultados, ts_nao_criativo_resultados)) +
#   labs(title = "Comparação das Séries")

serie1_diff <- diff(ts_criativo_resultados)
serie2_diff <- diff(ts_nao_criativo_resultados)

serie1_diff <- diff(ts_criativo_resultados, lag = 12)
serie2_diff <- diff(ts_nao_criativo_resultados, lag = 12)

tseries::adf.test(serie1_diff)
tseries::adf.test(serie2_diff)

ccf(serie1_diff, serie2_diff, lag.max = 12, main = "Correlação Cruzada")
cor(serie1_diff, serie2_diff, use = "complete.obs") # sem defasagem

# correlação espúria 
serie1_std <- scale(serie1_diff)
serie2_std <- scale(serie2_diff)

ccf(serie1_std, serie2_std, lag.max = 12)



# correlação das series simples criativo e nao criativo -------------------
cor(
  subset(tabela_trimestral, classe == "Setor criativo")$resultados,
  subset(tabela_trimestral, classe == "Setor não criativo")$resultados
  )

# calcular o p-valor da correção 

# teste de granger: pib dessassonalizada ----------------------------------
# tambem realizar com a sassonalizada 
# pib e taxa de rotatividade
# acontece antes ou depois?

