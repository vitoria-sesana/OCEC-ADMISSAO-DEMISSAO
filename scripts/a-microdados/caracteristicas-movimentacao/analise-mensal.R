
# rotina ------------------------------------------------------------------
source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8")

# gráfico linha cruzado mensal: movimentação x setor criativo -------------
g_01 <- 
  base[, .(Total = .N, Saldo = sum(saldo_movimentacao)),
       by = .(tipo_movimentacao_agrup, ocupacao_criativo, mes)]

g_01[, mes := as.integer(mes)]
g_01[, classe := paste(ocupacao_criativo,"-", tipo_movimentacao_agrup)]
g_01[, Percentual := Total / sum(Total) * 100, by = mes]

cores <- c("Ocupação não Criativa - Admissão" = "#1f77b4",
           "Ocupação não Criativa - Desligamento" = "#ff7f0e",
           "Ocupação Criativa - Desligamento" = "#2ca02c",
           "Ocupação Criativa - Admissão" = "#d62728")

ggplot(g_01, aes(x = mes, y = Total, color = classe)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  labs(title = "Gráfico de linha para a quantidade total de admitidos e demitidos entre as ocupações criativos e não criativos",
       x = "Mês",
       y = "Percentual (%)",
       color = "Classe") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1,12,1))

# percentual --------------------------------------------------------------
ggplot(g_01, aes(x = mes, y = Percentual, color = classe)) +
  geom_line(size = 1) +
  scale_color_manual(values = cores) +
  labs(title = "Gráfico de linha para o percentual de admitidos e demitidos entre as ocupações criativos e não criativos",
       x = "Mês",
       y = "Percentual (%)",
       color = "Classe") +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(1,12,1))

