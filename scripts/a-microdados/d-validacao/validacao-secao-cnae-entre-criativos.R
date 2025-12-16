# verificação das cnaes: fazem parte das industrias ou serviços?

source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8") # chamando base caged

# agrupamento por secao e por setor criativo e não criativo
base %>% 
  mutate(cnae_criatvo = ifelse(grande_setor == "Setor não criativo", "Setor não criativo", "Setor criativo")) %>% 
  group_by(cnae_2_secao, cnae_criatvo) %>% 
  summarise(n=n()) %>% 
  View


# quantidade das atividades economicas das seções 'não serviços'
base %>% 
  mutate(cnae_criatvo = ifelse(grande_setor == "Setor não criativo", "Setor não criativo", "Setor criativo")) %>% 
  group_by(cnae_2_secao, cnae_criatvo) %>% 
  summarise(n=n()) %>% 
  filter(cnae_2_secao %in% c("A", "B", "C", "D", "E", "F")) # variável 'secao' oriunda da base caged
# descrição da atividade que não tá considerada como serviços
base %>% 
  filter((cnae_2_secao == "C") & (grande_setor != "Setor não criativo")) %>%
  select(descricao_da_atividade) %>% 
  unique()
  
# CNAEs que não estão configuradas como serviços
base %>% 
  filter((cnae_2_secao == "C") & (grande_setor != "Setor não criativo")) %>% 
  select(cnae_2_subclasse) %>% 
  unique()
