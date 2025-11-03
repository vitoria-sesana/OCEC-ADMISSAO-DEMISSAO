# rotina ------------------------------------------------------------------
source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8")

# variação absoluta -------------------------------------------------------
tab_var_abs <-
  base[, .(
    Total = .N,
    total_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
    total_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(mes)]

tab_var_abs[, var_abs := total_adm + total_dem]
tab_var_abs[, var_abs_class := fifelse(var_abs > 0, "Positivo", "Negativo")]
tab_var_abs <- setorder(tab_var_abs,mes)


# total de empregos por mes -----------------------------------------------
base_total_empregos_ES_2023

tab <- tab_var_abs[,.(mes,total_adm, total_dem, var_abs)]
tab[, total_emprego_final_mes := base_total_empregos_ES_2023 + cumsum(var_abs)]
tab[, total_emprego_comeco_mes := total_emprego_final_mes - var_abs]
tab[, var_relativo := (var_abs/total_emprego_comeco_mes)*100]
tab[, ind_emprego := (total_emprego_final_mes/total_emprego_comeco_mes)*100]
tab[, taxa_rotatividade := (min(total_adm, abs(total_dem))/total_emprego_comeco_mes)*100]
tab
