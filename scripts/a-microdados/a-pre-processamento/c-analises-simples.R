source("scripts/a-microdados/0-rotina.R")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# análises base caged 2024 ------------------------------------------------
# variáveis de interesse: ocupacao criativa, admitidos e demitidos (com e sem agrupação)
print("Tabela frequência das ocupações criativas e não criativas.")
base[,.(Total=.N),by = ocupacao_criativo] 

print("Tabela frequência das ocupações criativas e não criativas, série histórica.")
tab_01 <- base[,.(Total=.N), by = c("ocupacao_criativo", "mes")]
setorder(tab_01, mes)
tab_01 

print("Tabela frequência dos tipos de movimentação.")
base[,.(Total=.N),by = tipo_movimentacao] 

print("Tabela frequência dos tipos de movimentação agrupados.")
base[,.(Total=.N),by = tipo_movimentacao_agrup] 

print("Tabela frequência dos tipos de movimentação agrupados e o saldo.")
base[,.(Total=.N, Sum=sum(saldo_movimentacao)),by = tipo_movimentacao_agrup] 

# outras variáveis 
print("Tabela frequência das fontes de movimentação.")
base[,.(Total=.N),by = origem_informacao] 

print("Tabela frequência indicador de aprendiz.")
base[,.(Total=.N),by = indicador_aprendiz] 


base[,.(Total=.N),by = c("indicador_aprendiz", "ocupacao_criativo")] 


# análise cruzada ---------------------------------------------------------
base[,.(Total=.N, Saldo=sum(saldo_movimentacao)), by = c("tipo_movimentacao_agrup", "ocupacao_criativo")] 

