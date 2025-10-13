
# pacotes -----------------------------------------------------------------
# install.packages("basedosdados")
library(basedosdados)


# leitura -----------------------------------------------------------------

caminho_caged_2024 <- 
  "bases/bq-results-20251013-102611-1760351321145.csv"

base_caged_2024 <- 
  read.csv(caminho_caged_2024)

