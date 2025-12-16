# rotina ------------------------------------------------------------------
source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8")

# variação absoluta -------------------------------------------------------
tab_var_abs <-
  base[, .(
    Total = .N,
    Saldo_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
    Saldo_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(mes, id_municipio)]

tab_var_abs[, Var_abs := Saldo_adm + Saldo_dem]
tab_var_abs[, Var_abs_class := fifelse(Var_abs > 0, "Positivo", "Negativo")]
