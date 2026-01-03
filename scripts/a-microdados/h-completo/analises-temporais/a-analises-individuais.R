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

## decomposição das séries ---------
plot(decompose(ts_criativo_resultados))
plot(decompose(ts_nao_criativo_resultados))
plot(decompose(ts_pib_resultados))


## análises trimestrais ------------
# gráfico de subséries
# observando cada trimestre separadamente
# verificar se o comportamento é ciclo ou sazonal
forecast::ggsubseriesplot(ts_criativo_resultados)
forecast::ggsubseriesplot(ts_nao_criativo_resultados)
forecast::ggsubseriesplot(ts_pib_resultados)
# forecast::ggsubseriesplot(ts_criativo_base100)
# forecast::ggsubseriesplot(ts_nao_criativo_base100)
# forecast::ggsubseriesplot(ts_pib_base100)

## análise da autocorrelação -----------------------------------------------
forecast::Acf(ts_criativo_resultados, main="Gráfico de Autocorrelação", lag.max = 100)
forecast::Acf(ts_nao_criativo_resultados, main="Gráfico de Autocorrelação", lag.max = 100)
forecast::Acf(ts_pib_resultados, main="Gráfico de Autocorrelação", lag.max = 100)

## análise da autocorrelação parcial ---------------------------------------
forecast::Pacf(ts_criativo_resultados, main="Gráfico de Autocorrelação Parcial", lag.max = 20)
forecast::Pacf(ts_nao_criativo_resultados, main="Gráfico de Autocorrelação Parcial", lag.max = 20)
forecast::Pacf(ts_pib_resultados, main="Gráfico de Autocorrelação Parcial", lag.max = 20)

## encontrando frequencia --------------------------------------------------
forecast::findfrequency(ts_criativo_resultados)
forecast::findfrequency(ts_nao_criativo_resultados)
forecast::findfrequency(ts_pib_resultados)

# ciclo: Filtro Hodrick-Prescott -------------------------------------
## separar a série temporal em tendência e componente ciclico
# plot(ts_criativo_resultados)
# 
# criativo.hp <- hpfilter(ts_criativo_resultados)
# plot(criativo.hp)
# 
# criativo.hp1 <- hpfilter(ts_criativo_resultados, drift=TRUE)
# criativo.hp2 <- hpfilter(ts_criativo_resultados, freq=4, drift=TRUE)
# criativo.hp3 <- hpfilter(ts_criativo_resultados, freq=12,type="frequency",drift=TRUE)
# criativo.hp4 <- hpfilter(ts_criativo_resultados, freq=52,type="frequency",drift=TRUE)
# 
# par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
# plot(criativo.hp$x,  ylim=c(2,13),
#      main="Hodrick-Prescott filter of criativoloyment: Trend, drift=TRUE",
#      col=1, ylab="")
# lines(criativo.hp1$trend,col=2)
# lines(criativo.hp2$trend,col=3)
# lines(criativo.hp3$trend,col=4)
# lines(criativo.hp4$trend,col=5)
# legend("topleft",legend=c("series", "lambda=1600", "lambda=4",
#                           "freq=12", "freq=52"), col=1:5, lty=rep(1,5), ncol=1)
# 
# plot(criativo.hp1$cycle,
#      main="Hodrick-Prescott filter of criativoloyment: Cycle,drift=TRUE",
#      col=2, ylab="", ylim=range(criativo.hp4$cycle,na.rm=TRUE))
# lines(criativo.hp2$cycle,col=3)
# lines(criativo.hp3$cycle,col=4)
# lines(criativo.hp4$cycle,col=5)


# transformações para deixar a série estacionária -------------------------
# pq deixar a série estacionária? para evitar o risco de correlação espúria
# queremos verficar a correlação cruzada das séries

## teste Augmented Dickey-Fuller (ADF) -------
# verificando se possui (não estacionária) ou não possui raiz unitária (estacionária) 
# h0: raiz unitária/não é estacionária
# hA: sem raiz unitária/é estacionária

tseries::adf.test(ts_criativo_resultados) # não estacionária
tseries::adf.test(ts_criativo_base100) # não estacionária
tseries::adf.test(ts_nao_criativo_resultados) # não estacionária
tseries::adf.test(ts_nao_criativo_base100) # não estacionária
tseries::adf.test(ts_pib_resultados) # estacionária
tseries::adf.test(ts_pib_base100) # estacionária

## transformações -----------------------



# dessazonalização --------------------------------------------------------

# decomposição
ts_decomp <- stl(ts_criativo_base100, s.window = "periodic")

# dessazonalidade STL
ts_sa <- seasadj(ts_decomp)

# visualizar
plot(ts_criativo_base100, main = "Série Original")
plot(ts_sa, main = "Série Dessazonalizada (STL)")

ts_decomp_sa <- stl(ts_sa, s.window = "periodic")
plot(ts_decomp)
plot(ts_decomp_sa)

acf(ts_criativo_base100, lag.max = 100)
acf(ts_sa, lag.max = 100)
acf(ts_criativo_base100, lag.max = 100)
acf(ts_sa, lag.max = 100)

## 1 = X-13ARIMA-SEATS
modelo_ajuste <- seasonal::seas(ts_criativo_resultados)
serie_ajustada1 <- seasonal::final(modelo_ajuste)
tseries::adf.test(serie_ajustada1)

## 2 = STL (Seasonal-Trend decomposition using Loess)
decomposicao <- stl(ts_criativo_resultados, s.window = "periodic")
serie_ajustada2 <- ts_criativo_resultados - decomposicao$time.series[, "seasonal"]
plot(serie_ajustada2)
tseries::adf.test(serie_ajustada2)

## 3 = Decomposição Clássica 
dec <- decompose(ts_criativo_resultados, type = "additive")
serie_ajustada3 <- ts_criativo_resultados - dec$seasonal
plot(serie_ajustada3)
tseries::adf.test(serie_ajustada3)


## estabilizar a variância 
ts_log <- log(ts_criativo_resultados)
tseries::adf.test(ts_log)

## remover a tendência com o diff 
ts_diff <- diff(ts_criativo_resultados, lag = 4)
plot(ts_diff)
tseries::adf.test(ts_diff)

ts_diff <- diff(ts_nao_criativo_resultados, lag = 1)
plot(ts_diff)
tseries::adf.test(ts_diff)

ts_diff <- diff(ts_nao_criativo_resultados, lag = 4)
plot(ts_diff)
tseries::adf.test(ts_diff)


# série aditivo ou multiplicativo -----------------------------------------


# análise de intervenção --------------------------------------------------

