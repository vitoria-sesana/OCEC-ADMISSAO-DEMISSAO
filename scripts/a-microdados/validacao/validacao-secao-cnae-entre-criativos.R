source("scripts/a-microdados/0-rotina.R", encoding = "UTF-8")

base %>% 
  mutate(cnae_criatvo = ifelse(grande_setor == "Setor não criativo", "Setor não criativo", "Setor criativo")) %>% 
  group_by(cnae_2_secao, cnae_criatvo) %>% 
  summarise(n=n()) %>% 
  View
  


base %>% 
  mutate(cnae_criatvo = ifelse(grande_setor == "Setor não criativo", "Setor não criativo", "Setor criativo")) %>% 
  group_by(cnae_2_secao, cnae_criatvo) %>% 
  summarise(n=n()) %>% 
  filter(cnae_2_secao %in% c("A", "B", "C", "D", "E", "F"))


base %>% 
  filter(cnae_2_secao = "C" & grande_setor != "Setor não criativo")