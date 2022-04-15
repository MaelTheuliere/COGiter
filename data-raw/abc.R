# Zonage ABC --------------------------------------------------------------
# On repart de la version 2018 : un seul conflit sur la fusion de Ancennis et ?, on prend Ancennis en référence
zonage_abc<-read_excel("data-raw/source/2019/zonageabc_communes2019.xls") %>%
  mutate(zonage_abc=str_c("Zone ",zonage_abc) %>% as.factor(.),
         DEPCOM=as.factor(DEPCOM))

use_data(zonage_abc,internal = F,overwrite = T)
