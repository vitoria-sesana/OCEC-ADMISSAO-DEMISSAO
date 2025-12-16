# rotina ------------------------------------------------------------------
source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8")

# variação absoluta -------------------------------------------------------
tab_var_abs <-
  base[, .(
    Total = .N,
    total_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
    total_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(setor_criativo, mes)]

tab_var_abs[, var_abs := total_adm + total_dem]
tab_var_abs[, var_abs_class := fifelse(var_abs > 0, "Positivo", "Negativo")]
tab_var_abs <- setorder(tab_var_abs,mes)


# total de empregos por mes -----------------------------------------------
qnt_2023 <- 
  base_total_empregos_ES_anual_cnae_2006_2024_final |>
  filter(ano == 2023) |>
  rename(qntd_total_clt_dez_2023 = quantidade_total_vinculos_clt)

tab <- tab_var_abs[,.(setor_criativo,mes,total_adm, total_dem, var_abs)]
tab <- left_join(tab, qnt_2023, by = "setor_criativo")
tab <- copy(tab)
tab[, total_emprego_final_mes := qntd_total_clt_dez_2023 + cumsum(var_abs),
    by = setor_criativo]
tab[, total_emprego_comeco_mes := total_emprego_final_mes - var_abs]
tab[, var_relativo := (var_abs/total_emprego_comeco_mes)*100]
tab[, ind_emprego := (total_emprego_final_mes/total_emprego_comeco_mes)*100]
tab[, min_adm_dem := pmin(total_adm, abs(total_dem))]
tab[, taxa_rotatividade := (min_adm_dem/total_emprego_comeco_mes)*100]
tab


# indicador de base 100 ---------------------------------------------------
valores_iniciais <- 
  tab[mes == 1,
      .(setor_criativo,
        e0_adm = total_adm,
        e0_dem = total_dem,
        e0_var_abs = var_abs,
        e0_var_relativo = var_relativo,
        e0_ind_emprego = ind_emprego,
        e0_taxa_rotatividade = taxa_rotatividade
        )]

tab <- left_join(tab, valores_iniciais, by = "setor_criativo")

# calculando resultados adimissão, demissão e variância absoluta
tab <- copy(tab)
tab[, resultados_adm := round((total_adm/e0_adm), 3)]
tab[, resultados_dem := round((total_dem/e0_dem), 3)]
tab[, resultados_var_abs := round((var_abs/e0_var_abs), 3)]
tab[, resultados_var_relativo := round((var_relativo/e0_var_relativo), 3)]
tab[, resultados_ind_emprego:= round((ind_emprego/e0_ind_emprego), 3)]
tab[, resultados_taxa_rotatividade := round((taxa_rotatividade/e0_taxa_rotatividade), 3)]
tab[, mes := as.integer(mes)]
tab


cores <- c("Setor criativo" = "#1f77b4",
           "Setor não criativo" = "#ff7f0e")


## Índice de emprego ----
ggplot(tab, aes(x = mes,
                y = resultados_ind_emprego,
                color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(breaks = seq(0.5,1.5,0.1), limits = c(0.5, 1.5))+
  ylab("Índice de emprego  (indicador base 100)") +
  xlab("Meses") + 
  ggtitle("Índice de emprego em 2024")

## Taxa de rotatividade ----
ggplot(tab, aes(x = mes,
                y = resultados_taxa_rotatividade,
                color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(breaks = seq(0.5,1.5,0.1), limits = c(0.5, 1.5))+
  ylab("Taxa de rotatividade (indicador base 100)") +
  xlab("Meses") + 
  ggtitle("Taxa de rotatividade em 2024")
