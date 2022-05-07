library(COGiter)
pop2015<-COGiter::pop2015
pop2015_2018_ancienne_version<-pop2015 %>%
  passer_au_cog_a_jour(garder_info_supra = F)

pop2015_2018_nouvelle_version<-COGiter::pop2015 %>%
  rename(DEPCOM_HIST=DEPCOM) %>%
  left_join(fusion_complete %>%
              rename(DEPCOM_HIST=com_av,DEPCOM=com_ap)) %>%
  mutate(DEPCOM=ifelse(is.na(DEPCOM),DEPCOM_HIST,DEPCOM)) %>%
  select(-DEPCOM_HIST) %>%
    group_by(DEPCOM)  %>%
    summarise_all(funs(sum))

pop2015_2018_ancienne_version %>%
  anti_join(pop2015_2018_nouvelle_version) %>%
  View()

pop2015_2018_nouvelle_version %>%
  anti_join(pop2015_2018_ancienne_version) %>%
  View()
# principalement les arrondissements PLM
# 17340 :fusion 2018 mais mars donc ok
# 50592 à éclairsire

COGiter::table_passage_com_historique %>% View()
test<-communes %>%
  filter(typecom=="COM") %>%
  select(depcom) %>%
  inner_join(fusion_complete %>%
              select(com_av) %>%
              rename(depcom=com_av)) %>%
  left_join(mvtcommunes %>%
              filter(mod==21) %>%
              rename(depcom=com_av) %>%
              select(typecom_av,depcom,com_ap)) %>%
  filter(is.na(typecom_av))

#14 code communes qui pose soucis
#les cas qui posent soucis :
#- CHEVEUGES SAINT AIGNAN	: séparation mal gérée
#- La Boissière : séparation mal gérée
#- MESNILS SUR MADON	: séparation mal gérée
#- LOISEY CULEY	: séparation


