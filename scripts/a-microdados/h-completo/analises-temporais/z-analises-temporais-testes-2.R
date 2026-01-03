# 1  ----------------------------------------------------------------------

# 1. Gerar dados aleatórios (Ruído Branco)
# n = 60 meses (5 anos), média 100, desvio padrão 2
set.seed(123) # Para você obter os mesmos resultados
dados <- rnorm(60, mean = 100, sd = 2)

# 2. Converter para objeto de série temporal (ts)
# Definimos frequência 12 para simular dados mensais
serie_pura <- ts(dados, start = c(2020, 1), frequency = 12)

# 3. Realizar a Decomposição
# Usamos o método aditivo já que a variância é constante
decomposicao <- decompose(serie_pura, type = "additive")

# 4. Plotar os resultados
plot(decomposicao)



# 2 -----------------------------------------------------------------------

# 1. Configurações iniciais
set.seed(123)
n_anos <- 5
n_periodos <- n_anos * 4  # 20 trimestres

# 2. Criar o componente sazonal (Padrão que se repete a cada 4 trimestres)
# Ex: T1 baixo, T2 médio, T3 alto, T4 médio
padrao_sazonal <- c(-5, 2, 8, -5) 
sazonalidade <- rep(padrao_sazonal, n_anos)

# 3. Gerar ruído branco (aleatoriedade)
ruido <- rnorm(n_periodos, mean = 0, sd = 1.5)

# 4. Compor a série final (Base 100 + Sazonalidade + Ruído)
dados_sazonais <- 100 + sazonalidade + ruido

# 5. Converter para ts e decompor
serie_sazonal_ts <- ts(dados_sazonais, start = c(2020, 1), frequency = 4)
dec_sazonal <- decompose(serie_sazonal_ts, type = "additive")

# 6. Visualizar
plot(dec_sazonal)


# 2 -----------------------------------------------------------------------
set.seed(42)
tempo <- 1:40 # 10 anos de trimestres

# Tendência Linear: sobe 1.5 unidades por trimestre
tendencia_pura <- 100 + (1.5 * tempo) + rnorm(40, sd = 1.5)

ts_tendencia <- ts(tendencia_pura, start = c(2015, 1), frequency = 4)
plot(ts_tendencia, main="Série com Tendência (Base 100)", col="blue", lwd=2, ylab="Índice")
abline(reg=lm(ts_tendencia~time(ts_tendencia)), col="red", lty=2) # Linha de tendência

plot(decompose(ts_tendencia))

# 3 -----------------------------------------------------------------------
# Criando uma onda senoidal para simular um ciclo de ~3 anos (12 trimestres)
ciclo_puro <- 100 + 8 * sin(2 * pi * tempo / 12) + rnorm(40, sd = 1)

ts_ciclo <- ts(ciclo_puro, start = c(2015, 1), frequency = 4)
plot(ts_ciclo, main="Série com Ciclo (Base 100)", col="darkgreen", lwd=2, ylab="Índice")
abline(h=100, lty=2) # Linha de referência da base

plot(decompose(ts_ciclo))

# 4 -----------------------------------------------------------------------
# 1. Configurações (8 anos de dados trimestrais)
n_anos <- 8
n_periodos <- n_anos * 4 
tempo <- 1:n_periodos

# 2. Tendência (Crescimento de 0.5 por trimestre)
tendencia <- 0.5 * tempo

# 3. Componente Cíclico (Onda longa usando seno)
# Simula um ciclo que dura aproximadamente 3 anos (12 trimestres)
ciclo <- 5 * sin(2 * pi * tempo / 12)

# 4. Sazonalidade (O padrão de serra trimestral)
sazonalidade <- rep(c(-3, 1, 5, -3), n_anos)

# 5. Composição Final (Base 100 + Componentes + Ruído)
set.seed(42)
ruido <- rnorm(n_periodos, sd = 1)
dados_completos <- 100 + tendencia + ciclo + sazonalidade + ruido

# 6. Transformar em ts e Decompor
serie_completa_ts <- ts(dados_completos, start = c(2018, 1), frequency = 4)
dec_completa <- decompose(serie_completa_ts)

# Plotar
plot(dec_completa)
