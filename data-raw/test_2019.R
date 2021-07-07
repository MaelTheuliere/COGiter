rm(list = ls())
library(COGiter)
library(curl)
library(readxl)
library(dplyr)
library(sf)

# ce script permet la mise à jour annuelle des communes et des EPCI avant que l'insee ne publie l'ensemble des fichiers COG du nouveau millésime
# grâce à la table INSEE des communes nouvelles de l'année qui s'est achevée et au fichier de la DGCL de composition communale des EPCI
# publiés tous deux plus précocemment
# il ne tient pas compte d'un éventuel changement de la composition départementale des régions.
# En revanche, si une ou des communes change de département, le script met à jour la composition communale des départements et régions
# et met à jour les couches cartos départementale et régionale

# téléchargement de la liste des fusions de communes de la dernière année ########

if (!file.exists("data-raw/source/hist_COG_mil.xls")) { 
  url_hist_COG_mil <- "https://www.insee.fr/fr/statistiques/fichier/2549968/communes_nouvelles_2018.xls"
  curl_download(url_hist_COG_mil , "data-raw/source/hist_COG_mil.xls" , mode = "wb" )
}

table_passage_com_hist_mil <- read_excel("data-raw/source/hist_COG_mil.xls") %>% select(DepComA, DepComN, NomCN)

# mise à jour de la table de passage des COG historiques  ########
table_passage_com_hist_2 <- left_join(table_passage_com_historique, table_passage_com_hist_mil, by=c("DEPCOM"="DepComA")) %>%
  mutate(DEPCOM=if_else(is.na(DepComN), DEPCOM, DepComN)) %>%
  select(-DepComN, -NomCN)%>%
  arrange(DEPCOM) %>%
  mutate_all(as.factor) %>%
  as_tibble()

# téléchargement de la composition communale des EPCI  ########
# https://www.collectivites-locales.gouv.fr/liste-et-composition-des-epci-a-fiscalite-propre

if (!file.exists("data-raw/source/epci.xlsx")) { 
  url_epci <- "https://www.collectivites-locales.gouv.fr/files/files/statistiques/brochures/epcicom2019.xlsx"
  curl_download(url_epci , "data-raw/source/epci.xlsx" , mode = "wb" )
}

epci_com2<-read_excel("data-raw/source/epci.xlsx") %>%
  select(EPCI=siren, NOM_EPCI=raison_sociale, DEP=dep_com, DEPCOM=insee, NOM_COM=nom_membre, NATURE_EPCI=nature_juridique) %>%
  as_tibble()  %>%
  left_join(select(departements, DEP, REG))%>%
  mutate_at(vars(DEPCOM, EPCI, DEP, REG), as.factor)
         
# création de la table de passage com epci  ########
table_passage_com_epci_2 <- select(epci_com2, DEPCOM, EPCI) %>%
  arrange(DEPCOM) %>%
  mutate_all(as.factor) %>%
  as_tibble()

# création de la table des epci ########
epci2 <- select(epci_com2, -ends_with("COM")) %>%
  group_by(EPCI, NOM_EPCI, NATURE_EPCI) %>%
  summarise(DEPARTEMENTS_DE_L_EPCI=list(unique(as.character(DEP))), 
            REGIONS_DE_L_EPCI=list(unique(as.character(REG))))%>%
  ungroup()%>% as_tibble() %>%
  arrange(EPCI) %>%
  mutate(EPCI=as.factor(EPCI))


# créations de la table des communes #########
communes2 <- select(communes, DEPCOM_old=DEPCOM, NOM_DEPCOM_old=NOM_DEPCOM) %>%
  left_join(table_passage_com_hist_mil, by=c("DEPCOM_old"="DepComA")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(DEPCOM=if_else(is.na(DepComN), DEPCOM_old, DepComN),
         NOM_DEPCOM=if_else(is.na(DepComN), NOM_DEPCOM_old, NomCN)) %>%
  mutate(DEP=if_else(substr(DEPCOM, 1, 2)=="97",substr(DEPCOM, 1, 3), substr(DEPCOM, 1, 2))) %>%
  left_join(select(departements, DEP, NOM_DEP, REG )) %>%
  left_join(select(regions, REG, NOM_REG)) %>%
  left_join(table_passage_com_epci_2) %>%
  left_join(select(epci2, -NATURE_EPCI)) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(NOM_EPCI=if_else(is.na(EPCI), "Sans objet", NOM_EPCI),
         DEPARTEMENTS_DE_L_EPCI=if_else(is.na(EPCI), DEP, DEPARTEMENTS_DE_L_EPCI),
         REGIONS_DE_L_EPCI=if_else(is.na(EPCI), REG, REGIONS_DE_L_EPCI),
         EPCI=if_else(is.na(EPCI), "ZZZZZZZZZ", EPCI))%>%
  select(DEPCOM, NOM_DEPCOM, EPCI, NOM_EPCI, DEP, NOM_DEP, REG, NOM_REG, DEPARTEMENTS_DE_L_EPCI, REGIONS_DE_L_EPCI) %>%
  arrange(DEPCOM) %>%
  as_tibble()%>%
  arrange(DEPCOM)%>%
  mutate_at(vars("DEPCOM", "EPCI", "DEP", "REG"), as.factor)

# création de la table geo des communes  ########

communes_geo2 <- communes_geo %>%
  left_join(table_passage_com_hist_2, by=c("DEPCOM"="DEPCOM_HIST"), suffix = c("", ".y"))%>%
  select(DEPCOM=DEPCOM.y) %>%
  arrange(as.character(DEPCOM)) %>%
  group_by(DEPCOM) %>% summarise(do_union=T) %>% ungroup()

gc()

# création des table géo supra  ########
epci_geo2 <- filter(communes2, NOM_EPCI!="Sans objet")%>%
  inner_join(communes_geo2, ., by="DEPCOM") %>%
  select(EPCI) %>% arrange(as.character(EPCI)) %>%
  group_by(EPCI) %>% summarise(do_union=T) %>% ungroup()

departements_geo2 <- inner_join(communes_geo2, communes2, by="DEPCOM") %>%
  select(DEP) %>% arrange(as.character(DEP)) %>%
  group_by(DEP) %>% summarise(do_union=T) %>% ungroup()

regions_geo2 <- inner_join(departement_geo2, departements, by="DEP")%>%
  select(REG) %>%  arrange(as.character(REG)) %>%
  group_by(REG) %>% summarise(do_union=T) %>% ungroup()

rm(table_passage_com_hist_mil, epci_com2)

# Fonctions


passer_au_cog_a_jour <- function (.data, code_commune = DEPCOM, aggrege = T, garder_info_supra = T) 
{
  quo_code_commune <- enquo(code_commune)
  result <- .data %>% rename(DEPCOM_HIST = !(!quo_code_commune)) %>% 
    left_join(table_passage_com_hist_2) %>% select(-DEPCOM_HIST)
  if (aggrege == T) {
    result <- result %>% group_by_if(funs(!is.numeric(.))) %>% 
      summarise_all(funs(sum)) %>% ungroup()
  }
  if (garder_info_supra == T) {
    result <- result %>% left_join(communes %>% select(DEPCOM:REGIONS_DE_L_EPCI)) %>% 
      mutate(DEPCOM = as.factor(DEPCOM))
  }
  result
}

