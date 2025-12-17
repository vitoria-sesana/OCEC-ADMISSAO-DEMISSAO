# rotina ------------------------------------------------------------------
source("scripts/a-microdados/h-completo/0-rotina_completa.R", encoding = "UTF-8")


# Criativos e não criativos -----------------------------------------------

# bases 2007:2019 e 2020:2024 ---------------------------------------------
# 2007:2019 
tab_ind_2007 <-
  base_2007_2019[, .(
    Total = .N,
    total_adm = sum(ifelse(saldo_movimentacao == 1, saldo_movimentacao, 0)),
    total_dem = sum(ifelse(saldo_movimentacao == -1, saldo_movimentacao, 0))),
    by = .(setor_criativo, mes, ano)]

tab_ind_2007[, var_abs := total_adm + total_dem]
tab_ind_2007[, var_abs_class := fifelse(var_abs > 0, "Positivo", "Negativo")]
tab_ind_2007 <- setorder(tab_ind_2007, ano, mes)

# 2020:2024
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

## quantidade total de empregos anual RAIS -----------------------------------------------
qntd_anual_rais_setorizado <- 
  base_total_empregos_ES_anual_cnae_2006_2024_final 

qntd_referencia_rais_setorizado <- 
  qntd_anual_rais_setorizado |>
  filter(ano %in% c(2024)) |>
  rename(qntd_total_clt_dez_ano_anterior = quantidade_total_vinculos_clt, ano_calculo = ano)


## ordenação e seleção -----------------------------------------------------
tab <- tab_ind_completa[, .(setor_criativo, mes, ano, total_adm, total_dem, var_abs)]
tab <- setorder(tab, -ano, -mes)
tab <- left_join(tab, qntd_referencia_rais_setorizado, by = c("setor_criativo"))
tab <- copy(tab)
tab <- setorder(tab, "setor_criativo", -"ano", -"mes")

## cáulculo do estoque ------------------------------------------------------
tab[, estoque_inicial := qntd_total_clt_dez_ano_anterior - cumsum(var_abs), 
    by = c("setor_criativo")]
tab[, estoque_final := lag(estoque_inicial)]

## definindo quantidade de empregos do ano anterior -----------------------
tab[setor_criativo=="Setor criativo" & ano==2024 & mes==12, estoque_final := qntd_referencia_rais_setorizado$qntd_total_clt_dez_ano_anterior[1]]
tab[setor_criativo=="Setor não criativo" & ano==2024 & mes==12, estoque_final := qntd_referencia_rais_setorizado$qntd_total_clt_dez_ano_anterior[2]]

## indicadores ----------------------------------------------------------------
tab[, var_relativo := (var_abs/estoque_inicial)*100]
tab[, ind_emprego := (estoque_final/estoque_inicial)*100]
tab[, min_adm_dem := pmin(total_adm, abs(total_dem))]
tab[, taxa_rotatividade := (min_adm_dem/estoque_inicial)*100]
tab

## indicador de base 100 ---------------------------------------------------
valores_iniciais <- 
  tab[mes == 12 & ano == 2024,
      .(setor_criativo,
        e0_taxa_rotatividade = taxa_rotatividade
      )]

tab <- left_join(tab, valores_iniciais, by = "setor_criativo")

# calculando resultados adimissão, demissão e variância absoluta
tab <- copy(tab)
tab[, resultados_taxa_rotatividade := round((taxa_rotatividade/e0_taxa_rotatividade), 3)]
tab[, mes := as.integer(mes)]
tab[, mes_ano := lubridate::my(paste0(mes,"/",ano))]

tabela_final <- tab[ano >= 2008,]

## gráficos -----------------------------------------------------------------
cores <- c("Setor criativo" = "#1f77b4",
           "Setor não criativo" = "#ff7f0e")

ggplot(tabela_final, aes(x = mes_ano,
                y = taxa_rotatividade,
                color = setor_criativo)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("2008-2024")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# com análise geral ------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tab_ind_completa_geral <- 
  tab_ind_completa |>
  mutate(geral = "geral") |>
  group_by(geral, mes, ano) |>
  summarise(
    Total = sum(Total),
    total_adm = sum(total_adm),
    total_dem = sum(total_dem)
    ) |>
  as.data.table()

tab_ind_completa_geral[, var_abs := total_adm + total_dem]

## quantidade total de empregos anual RAIS -----------------------------------------------
qntd_anual_rais <- 
  base_total_empregos_ES_anual_cnae_2006_2024_final |>
  mutate(geral = "geral") |>
  group_by(geral, ano) |>
  summarise(qntd_total_clt_dez_ano_anterior = sum(quantidade_total_vinculos_clt)) |>
  rename(ano_calculo = ano)

qntd_referencia_rais <- 
  qntd_anual_rais |>
  filter(ano_calculo %in% c(2024))

## ordenação e seleção -----------------------------------------------------
tab_geral <- tab_ind_completa_geral[, .(geral, mes, ano, total_adm, total_dem, var_abs)]
tab_geral <- setorder(tab_geral, -ano, -mes)
tab_geral <- left_join(tab_geral, qntd_referencia_rais, by = c("geral"))
tab_geral <- copy(tab_geral)
tab_geral <- setorder(tab_geral, -"ano", -"mes")

## cálculo do estoque ------------------------------------------------------
tab_geral[, estoque_inicial := qntd_total_clt_dez_ano_anterior - cumsum(var_abs)]
tab_geral[, estoque_final := lag(estoque_inicial)]

## definindo quantidade de empregos do ano anterior -----------------------
tab_geral[ano==2024 & mes==12, estoque_final := qntd_referencia_rais$qntd_total_clt_dez_ano_anterior[1]]

## indicadores -------------------------------------------------------------
tab_geral[, var_relativo := (var_abs/estoque_inicial)*100]
tab_geral[, ind_emprego := (estoque_final/estoque_inicial)*100]
tab_geral[, min_adm_dem := pmin(total_adm, abs(total_dem))]
tab_geral[, taxa_rotatividade := (min_adm_dem/estoque_inicial)*100]
tab_geral


## indicador de base 100 ---------------------------------------------------
valores_iniciais <- 
  tab_geral[mes == 12 & ano == 2024,
      .(e0_taxa_rotatividade = taxa_rotatividade
      )]

tab_geral <- tab_geral[, e0_taxa_rotatividade := valores_iniciais$e0_taxa_rotatividade]

# calculando resultados adimissão, demissão e variância absoluta
tab_geral <- copy(tab_geral)
tab_geral[, resultados_taxa_rotatividade := round((taxa_rotatividade/e0_taxa_rotatividade), 3)]
tab_geral[, mes := as.integer(mes)]
tab_geral[, mes_ano := lubridate::my(paste0(mes,"/",ano))]

## finalização ---------------------------------------------------------------
tabela_final_geral <- tab_geral[ano >= 2008,]

## gráficos -----------------------------------------------------------------
cores <- c("Setor criativo" = "#1f77b4",
           "Setor não criativo" = "#ff7f0e")

ggplot(tabela_final_geral, aes(x = mes_ano,
                         y = taxa_rotatividade)) +
  geom_line(size = 1) +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("2008-2024")



# grafico taxa de rotatividade criativos, não criativos e geral -----------
cores <- c("Setor criativo" = "#1f77b4",
           "Setor não criativo" = "gray",
           "Geral" = "red")

t1 <- 
  tabela_final |>
  rename(classe = setor_criativo)

t2 <-
  tabela_final_geral |>
  rename(classe = geral) |>
  mutate(classe = "Geral")

tabela <- bind_rows(t1, t2)


ggplot(tabela, aes(x = mes_ano,
                         y = taxa_rotatividade,
                         color = classe)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  theme_minimal() +
  # scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    #breaks = seq(0.5,1.5,0.1), 
    limits = c(0, 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("2008-2024")


# saidas ------------------------------------------------------------------
write.csv2(tabela, "scripts/a-microdados/h-completo/tabela_geral.csv")
write.csv2(tabela_final, "scripts/a-microdados/h-completo/tabela_setores_criativos.csv")
