
# rotina ------------------------------------------------------------------
source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# saldo municipios --------------------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
tab_muni <-
  base[, .(total = .N,
     Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
     Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(id_municipio)]

tab_muni[, Var_abs := Saldo_adm + Saldo_dem]
tab_muni

### gráfico boxplot ------------------------
ggplot(tab_muni, aes(x = Var_abs)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot do Ano", x = "Ano") +
  theme_minimal() 


### gráfico espacial ------------------------
municipios_es <- read_municipality(code_muni = "ES", year = 2020)
tab_muni_df <- as.data.frame(tab_muni)
municipios_es_joined <- municipios_es %>%
  left_join(tab_muni_df, by = c("code_muni" = "id_municipio"))

ggplot(municipios_es_joined) +
  geom_sf(aes(fill = Var_abs), color = "black") +  # Use Var_abs com sinal original
  scale_fill_gradient2(
    low = "red",         # Valores negativos em vermelho
    mid = "white",       # Valor zero em branco
    high = "green",      # Valores positivos em verde
    midpoint = 0,        # Zero no meio do gradiente
    na.value = "grey90", # Cor para NA
    name = "Saldo"
  ) +
  scale_x_continuous(limits = c(-42, -39)) +
  labs(title = "Saldo por Município no ES (Ocupação Criativa)") +
  theme_minimal()

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# saldo municipios x criativos --------------------------------------------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tab_muni_criativo <- base[, .(
  total = .N,
  Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)) + 0.0,
  Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0)) + 0.0
), by = .(id_municipio, ocupacao_criativo)]

tab_muni_criativo[, Var_abs := Saldo_adm + Saldo_dem]
tab_muni_criativo

### gráfico boxplot ------------------------
ggplot(tab_muni_criativo, aes(x = ocupacao_criativo, y = Var_abs, colour = ocupacao_criativo)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot de Var_abs por Ocupação Criativa", x = "Ocupação Criativa", y = "Var_abs") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(-100, 6500, 500))



### gráfico espacial ------------------------
municipios_es <- read_municipality(code_muni = "ES", year = 2020)
tab_muni_df <- as.data.frame(tab_muni_criativo[ocupacao_criativo == "Ocupação Criativa", ])
municipios_es_joined <- municipios_es %>%
  left_join(tab_muni_df, by = c("code_muni" = "id_municipio"))

ggplot(municipios_es_joined) +
  geom_sf(aes(fill = Var_abs), color = "black") + 
  scale_fill_gradient2(
    low = "red",         # Valores negativos em vermelho
    mid = "white",       # Valor zero em branco
    high = "green",      # Valores positivos em verde
    midpoint = 0,        # Zero no meio do gradiente
    na.value = "grey90", # Cor para NA
    name = "Saldo",
    breaks = seq(-1000, 6000, 1000),
  ) +
  scale_x_continuous(limits = c(-42, -39)) +
  labs(title = "Saldo por Município no ES (Ocupação Criativa)") +
  theme_minimal() 
  

