# Zonage Pinel ------------------------------------------

zonage_pinel_r52<-read_excel("data-raw/source/2019/zonagepinel_pdl_communes2019.xls") %>%
  mutate(zonage_pinel=str_c("Zone ",zonage_pinel) %>% as.factor(.),
         DEPCOM=as.factor(DEPCOM))
use_data(zonage_pinel_r52,internal = F,overwrite = T)
