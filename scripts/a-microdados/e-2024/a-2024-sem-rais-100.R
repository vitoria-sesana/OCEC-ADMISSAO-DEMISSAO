# rotina ------------------------------------------------------------------
source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8")

# variação absoluta -------------------------------------------------------
tab_var_abs <-
  base[, .(
    Total = .N,
    admitidos = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
    demitidos = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(setor_criativo, mes)]

# variação absoluta 
tab_var_abs[, var_abs := admitidos + demitidos] 

# indicador de base 100
valores_iniciais <- tab_var_abs[mes == 1,.(setor_criativo,
                                           e0_adm = admitidos,
                                           e0_dem = demitidos,
                                           e0_var_abs = var_abs)]

tab_var_abs <- left_join(tab_var_abs, valores_iniciais, by = "setor_criativo")

# calculando resultados adimissão, demissão e variância absoluta
tab_var_abs[, resultados_adm := round((admitidos/e0_adm), 3)]
tab_var_abs[, resultados_dem := round((demitidos/e0_dem), 3)]
tab_var_abs[, resultados_var_abs := round((var_abs/e0_var_abs), 3)]
tab_var_abs[, mes := as.integer(mes)]

cores <- c("Setor criativo" = "#1f77b4",
           "Setor não criativo" = "#ff7f0e")

ggplot(tab_var_abs, aes(x = mes, 
                   y = resultados_adm,
                   color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(breaks = seq(0.5,1.5,0.1), limits = c(0.5, 1.5))
  # scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-8.1, 2.2))


ggplot(tab_var_abs, aes(x = mes, 
                        y = resultados_dem,
                        color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(breaks = seq(0.5,1.5,0.1), limits = c(0.5, 1.5))
  # scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-8.1, 2.2))
