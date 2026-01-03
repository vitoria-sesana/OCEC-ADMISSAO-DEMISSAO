# rotina ------------------------------------------------------------------
source("scripts/a-microdados/h-completo/0-rotina_completa.R", encoding = "UTF-8")

# variação absoluta -------------------------------------------------------
tab_ind_2007 <-
  base_2007_2019[, .(
    Total = .N,
    total_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
    total_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(setor_criativo, mes, ano)]

tab_ind_2007[, var_abs := total_adm + total_dem]
tab_ind_2007[, var_abs_class := fifelse(var_abs > 0, "Positivo", "Negativo")]
tab_ind_2007 <- setorder(tab_ind_2007, ano, mes)

tab_ind_2020 <-
   base_2020_2024[, .(
    Total = .N,
    total_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
    total_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(setor_criativo, mes, ano)]

tab_ind_2020[, var_abs := total_adm + total_dem]
tab_ind_2020[, var_abs_class := fifelse(var_abs > 0, "Positivo", "Negativo")]
tab_ind_2020 <- setorder(tab_ind_2020, ano, mes)

tab_ind_completa <- bind_rows(tab_ind_2007, tab_ind_2020)
  
# total de empregos por mes -----------------------------------------------
qnt_2006_2023 <- 
  base_total_empregos_ES_anual_cnae_2006_2024_final |>
  filter(ano %in% c(2006:2023)) |>
  rename(qntd_total_clt_dez_ano_anterior = quantidade_total_vinculos_clt, ano_calculo = ano) |>
  mutate(ano = ano_calculo+1) # ano atribuído para calculo (ex: total de clt em dez de 2019 sera vinculado ano inicio de 2020), entao ano=2020.


# demais indicadores ------------------------------------------------------
tab <- tab_ind_completa[,.(setor_criativo,mes,ano,total_adm, total_dem, var_abs)]
tab <- left_join(tab, qnt_2006_2023, by = c("ano", "setor_criativo"))
tab <- copy(tab)
tab[, total_emprego_final_mes := qntd_total_clt_dez_ano_anterior + cumsum(var_abs),
    by = c("ano", "setor_criativo")]
tab[, total_emprego_comeco_mes := total_emprego_final_mes - var_abs]
tab[, var_relativo := (var_abs/total_emprego_comeco_mes)*100]
tab[, ind_emprego := (total_emprego_final_mes/total_emprego_comeco_mes)*100]
tab[, min_adm_dem := pmin(total_adm, abs(total_dem))]
tab[, taxa_rotatividade := (min_adm_dem/total_emprego_comeco_mes)*100]
tab


# indicador de base 100 ---------------------------------------------------
valores_iniciais <- 
  tab[mes == 1 & ano == 2007,
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
tab[, mes_ano := lubridate::my(paste0(mes,"/",ano))]


# gráficos -----------------------------------------------------------------
cores <- c("Setor criativo" = "#1f77b4",
           "Setor não criativo" = "#ff7f0e")

## total admissao ------
ggplot(tab, aes(x = mes_ano,
                y = total_adm,
                color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  # scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 50000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("2007-2024")

## total demissao -----
ggplot(tab, aes(x = mes_ano,
                y = abs(total_dem),
                color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  # scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 50000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("2007-2024")


## variancia absoluta -----
ggplot(tab, aes(x = mes_ano,
                y = var_abs,
                color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  # scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(-10000, 10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("2007-2024")

## taxa rotatividade -----
ggplot(tab, aes(x = mes_ano,
                y = taxa_rotatividade,
                color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  # scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("2007-2024")

## taxa rotatividade base 100 -----
ggplot(tab, aes(x = mes_ano,
                y = resultados_taxa_rotatividade,
                color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  # scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 1000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("2007-2024")
