
# 1. Definições
set.seed(456)
anos <- 6
n <- anos * 4 # 24 trimestres

# 2. Criar a Sazonalidade Trimestral (Efeito fixo por trimestre)
# T1: +6 (Alta), T2: 0, T3: -5 (Baixa), T4: -1
padrao_sazonal <- c(6, 0, -5, -1)
componente_sazonal <- rep(padrao_sazonal, anos)

# 3. Gerar a série (Base 100 + Sazonalidade + Ruído aleatório)
ruido <- rnorm(n, mean = 0, sd = 1.5)
dados_trimestrais <- 100 + componente_sazonal + ruido

# 4. Converter para objeto de série temporal
rotatividade_ts <- ts(dados_trimestrais, start = c(2020, 1), frequency = 4)

# 5. Visualizar e Decompor
plot(rotatividade_ts, type="o", pch=16, col="darkblue", main="Série de Rotatividade com Sazonalidade Trimestral")
decomposicao <- decompose(rotatividade_ts)
plot(decomposicao)
rotatividade_ts
acf(rotatividade_ts)
plot(decompose(rotatividade_ts))


# ciclo -------------------------------------------------------------------

# 1. Configurações (10 anos de dados trimestrais)
set.seed(789)
anos <- 10
n <- anos * 4 
tempo <- 1:n

# 2. Criar o Componente Cíclico
# Queremos um ciclo que dure 3 anos (12 trimestres)
# A amplitude será de 7 pontos para cima e para baixo
ciclo <- 7 * sin(2 * pi * tempo / 12)

# 3. Gerar a série (Base 100 + Ciclo + Ruído)
ruido <- rnorm(n, mean = 0, sd = 1)
dados_ciclicos <- 100 + ciclo + ruido

# 4. Converter para ts
rotatividade_ciclo_ts <- ts(dados_ciclicos, start = c(2016, 1), frequency = 4)

# 5. Visualizar
plot(rotatividade_ciclo_ts, type="l", lwd=2, col="darkgreen", 
     main="Indicador com Comportamento Cíclico (Ciclo de 3 anos)",
     ylab="Índice (Base 100)", xlab="Tempo")
abline(h=100, lty=2, col="red") # Linha de base para referência

Acf(rotatividade_ciclo_ts,lag.max = 100)
plot(decompose(rotatividade_ciclo_ts))
