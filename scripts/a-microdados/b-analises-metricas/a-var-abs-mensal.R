# rotina ------------------------------------------------------------------
source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8")

# variação absoluta -------------------------------------------------------
tab_var_abs <-
  base[, .(
    Total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(mes)]

tab_var_abs[, Var_abs := Saldo_adm + Saldo_dem]
tab_var_abs[, Var_abs_class := fifelse(Var_abs > 0, "Positivo", "Negativo") ]

## gráfico -----------------------------------------------------------------
ggplot(tab_var_abs, aes(x = mes, y = Var_abs, fill = Var_abs_class)) +
  geom_bar(stat = "identity") +
  labs(title = "Saldo mensal no ES",
       x = "Categoria",
       y = "Valor") +
  theme_minimal() +
  theme( 
    panel.grid.major.x = element_blank()
  )


# variação absoluta: criativos e não criativos ----------------------------
tab_var_abs_cri <-
  base[, .(
    Total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(mes,ocupacao_criativo)]

tab_var_abs_cri[, Var_abs := Saldo_adm + Saldo_dem]
tab_var_abs_cri[, Var_abs_class := fifelse(Var_abs > 0, "Positivo", "Negativo") ]
tab_var_abs_cri[, classe_plot := paste(ocupacao_criativo, "-", Var_abs_class)]

## gráfico -----------------------------------------------------------------
ggplot(tab_var_abs_cri,
       aes(x = mes, y = Var_abs, fill = ocupacao_criativo)) +
  geom_bar(stat = "identity") +
  labs(title = "Saldo mensal entre criativos e não criativos",
       x = "Categoria",
       y = "Valor") +
  theme_minimal() +
  theme( 
    panel.grid.major.x = element_blank()
  )


# tentativa de colocar 'estampas'  :
# 
# 
# tab_var_abs_cri <- tab_var_abs_cri %>%
#   mutate(pattern_use = ifelse(ocupacao_criativo == "Ocupação não Criativa", NA, ocupacao_criativo))
# 
# ggplot(tab_var_abs_cri,
#        aes(x = mes, y = Var_abs,
#            pattern = pattern_use, fill = Var_abs_class)) +
#   geom_bar_pattern(stat = "identity",
#                    position = "stack",
#                    # fill = "white",
#                    # color = "black",
#                    # pattern_fill = "black",
#                    pattern_angle = 45,
#                    pattern_density = 0.1,
#                    pattern_spacing = 0.02,
#                    pattern_key_scale_factor = 0.6) +
#   labs(title = "Gráfico de Barras Horizontal com Listras",
#        x = "Mês",
#        y = "Variação Absoluta",
#        pattern = "Ocupação Criativo") +
#   theme_minimal() +
#   theme(panel.grid.major.x = element_blank())

