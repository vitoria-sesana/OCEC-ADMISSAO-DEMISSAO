

# variação absoluta -------------------------------------------------------
tab_var_abs <- base[, .(
  Total = .N,
  Saldo_adm = as.numeric(sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0))),
  Saldo_dem = as.numeric(sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0)))
), by = .(mes, ocupacao_criativo, grau_instrucao)]

tab_var_abs[, Var_abs := Saldo_adm + Saldo_dem]
tab_var_abs[, Var_abs_class := fifelse(Var_abs > 0, "Positivo", "Negativo") ]
tab_var_abs[, grau_instrucao := as.character(grau_instrucao)]


# gráfico -----------------------------------------------------------------
ggplot(tab_var_abs, aes(x = mes, y = Var_abs, fill = grau_instrucao)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico de Barras Horizontal",
       x = "Categoria",
       y = "Valor") +
  theme_minimal() +
  theme( 
    panel.grid.major.x = element_blank()
  )


# gráfico linha -----------------------------------------------------------

base[, saldo_movimentacao_class := fifelse(saldo_movimentacao > 0, "Positivo", "Negativo") ]

tab <- base[, .(
  Total = .N,
  Saldo = sum(saldo_movimentacao)
), by = .(mes, grau_instrucao)]
tab[, grau_instrucao := as.character(grau_instrucao)]


ggplot(tab, aes(x = mes, y = Saldo, fill = grau_instrucao)) +
  geom_line(size = 1) +
  labs(title = "Gráfico de Linha com 4 Classes",
       x = "Eixo X",
       y = "Eixo Y",
       color = "Classe") +
  theme_minimal() 

