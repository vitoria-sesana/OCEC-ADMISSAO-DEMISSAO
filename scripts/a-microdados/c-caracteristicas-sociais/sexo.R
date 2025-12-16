# rotina ------------------------------------------------------------------
source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8")

base[, sexo_char := fcase(sexo == 1, "Masculino", 
                          sexo == 3, "Feminino",
                          default = NA)]
# sexo --------------------------------------------------------------------

tab_sexo <- 
  base[, .(
    total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)) + 0.0,
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0)) + 0.0
  ), by = .(sexo)]

tab_sexo <- 
  base[, .(
    total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)) + 0.0,
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0)) + 0.0
  ), by = .(sexo, ocupacao_criativo)]

tab_sexo <- 
  base[, .(
    total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)) + 0.0,
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0)) + 0.0
  ), by = .(sexo, ocupacao_criativo, mes)]

tab_sexo <- 
  base[, .(
    total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)) + 0.0,
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0)) + 0.0
  ), by = .(sexo_char, mes)]



# variação absoluta: criativos e não criativos ----------------------------
tab_sexo[, Var_abs := Saldo_adm + Saldo_dem]
tab_sexo[, Var_abs_class := fifelse(Var_abs > 0, "Positivo", "Negativo") ]

## gráfico -----------------------------------------------------------------
tab_sexo <- 
  base[, .(
    total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)) + 0.0,
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0)) + 0.0
  ), by = .(sexo_char, mes)]

tab_sexo[, Var_abs := Saldo_adm + Saldo_dem]
tab_sexo[, Var_abs_class := fifelse(Var_abs > 0, "Positivo", "Negativo") ]

ggplot(tab_sexo,
       aes(x = mes, y = Var_abs, fill = sexo_char)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras Horizontal",
       x = "Categoria",
       y = "Valor") +
  theme_minimal() +
  theme( 
    panel.grid.major.x = element_blank()
  )

## gráfico -----------------------------------------------------------------
tab_sexo <- 
  base[, .(
    total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)) + 0.0,
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0)) + 0.0
  ), by = .(sexo_char, mes, ocupacao_criativo)]

tab_sexo[, Var_abs := Saldo_adm + Saldo_dem]
tab_sexo[, Var_abs_class := fifelse(Var_abs > 0, "Positivo", "Negativo") ]
tab_sexo[, classe_plot := paste(ocupacao_criativo, "-", sexo_char)]

ggplot(tab_sexo,
       aes(x = mes, y = Var_abs, fill = classe_plot)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras Horizontal",
       x = "Categoria",
       y = "Valor") +
  theme_minimal() +
  theme( 
    panel.grid.major.x = element_blank()
  )


## gráfico -----------------------------------------------------------------
tab_sexo <- 
  base[, .(
    total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)) + 0.0,
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0)) + 0.0
  ), by = .(sexo_char, mes, ocupacao_criativo)]

tab_sexo <- tab_sexo[ocupacao_criativo == "Ocupação Criativa",]
tab_sexo[, Var_abs := Saldo_adm + Saldo_dem]

ggplot(tab_sexo,
       aes(x = mes, y = Var_abs, fill = sexo_char)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras Horizontal",
       x = "Categoria",
       y = "Valor") +
  theme_minimal() +
  theme( 
    panel.grid.major.x = element_blank()
  )

