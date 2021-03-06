# Chargement des librairies ----------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(usethis)
# Chargement des tables du COG ------------------------------------------
# https://www.insee.fr/fr/information/2560452
#
# Création des tables suivantes:
# régions : copie de la table régions du cog avec des noms de variables normalisées
# départements : copie de la table départements du cog avec des noms de variables normalisées
# epci : Construite à partir de la table epci du cog, pour chaque epci,liste les attributs et les départements et régions d'appartenance
# communes : copie de la table communes du cog avec des noms de variables normalisées
# communes_info_supra : donne pour toutes les communes les epci, départements, régions de rattachement ainsi que les info de rattachement de l'epci de rattachement


# Table des régions --------------------------------------------------

regions<-read_csv("data-raw/source/2020/COG/region2020.csv")  %>%
  as_tibble() %>%
  rename_all(funs(toupper)) %>%
  rename(NOM_REG=LIBELLE) %>%
  mutate(across(everything(),as.factor))

# Table des départements --------------------------------------------------

departements<-read_csv("data-raw/source/2020/COG/departement2020.csv")%>%
  as_tibble() %>%
  rename_all(funs(toupper)) %>%
  rename(NOM_DEP=LIBELLE) %>%
  mutate(across(everything(),as.factor))

# Table des Epci ----------------------------------------------------------

epci<-read_excel("data-raw/source/2020/COG/Intercommunalité - Métropole au 01-01-2020.xlsx",sheet=1,skip=5) %>%
  mutate(EPCI=as.factor(EPCI),
         LIBEPCI=as.factor(LIBEPCI),
         NATURE_EPCI=as.factor(NATURE_EPCI) %>%
           fct_relevel(c("ME", "CU", "CA", "CC", "ZZ"))
  )%>%
  rename(NOM_EPCI=LIBEPCI) %>%
  arrange(NATURE_EPCI,NOM_EPCI) %>%
  mutate(EPCI = fct_inorder(EPCI)) %>%
  select(EPCI,NOM_EPCI,NATURE_EPCI) %>%
  as_tibble()

communes_epci<-read_excel("data-raw/source/2020/COG/Intercommunalité - Métropole au 01-01-2020.xlsx",sheet=2,skip=5) %>%
  mutate(CODGEO=as.factor(CODGEO),
         EPCI=as.factor(EPCI),
         LIBEPCI=as.factor(LIBEPCI),
         REG=as.factor(REG),
         DEP=as.factor(DEP)) %>%
  rename(DEPCOM=CODGEO,
         NOM_DEPCOM=LIBGEO,
         NOM_EPCI=LIBEPCI) %>%
  as_tibble()

epci_rattachement_reg_dep<-communes_epci %>%
  select(-DEPCOM,-NOM_DEPCOM) %>%
  distinct() %>%
  group_by(EPCI,NOM_EPCI) %>%
  summarise(DEPARTEMENTS_DE_L_EPCI=list(unique(as.character(DEP))),
            REGIONS_DE_L_EPCI=list(unique(as.character(REG)))
            ) %>%
  ungroup()

epci<-epci %>%
  left_join(epci_rattachement_reg_dep) %>%
  filter(EPCI!="ZZZZZZZZZ")

table_passage_com_epci<-communes_epci %>%
  select(DEPCOM,EPCI)

# Table des communes  --------------------------------------------------

communes_cog<-read_csv("data-raw/source/2020/COG/communes2020.csv",
                   col_types = cols(
                     typecom = col_character(),
                     com = col_character(),
                     reg = col_character(),
                     dep = col_character(),
                     arr = col_character(),
                     tncc = col_double(),
                     ncc = col_character(),
                     nccenr = col_character(),
                     libelle = col_character(),
                     can = col_character(),
                     comparent = col_character()
                   )
                   ) %>%
  as_tibble() %>%
  rename_with(toupper) %>%
  rename(DEPCOM=COM,
         NOM_DEPCOM=LIBELLE) %>%
  mutate(across(everything(),as.factor))



# Table des mouvements de communes ----------------------------------------

mvtcommunes<-read_csv("data-raw/source/2020/COG/mvtcommune2020.csv",
                      col_types = cols(
                        mod = col_factor(),
                        date_eff = col_date(format = ""),
                        typecom_av = col_factor(),
                        com_av = col_factor(),
                        tncc_av = col_factor(),
                        ncc_av = col_factor(),
                        nccenr_av = col_factor(),
                        libelle_av = col_factor(),
                        typecom_ap = col_factor(),
                        com_ap = col_factor(),
                        tncc_ap = col_factor(),
                        ncc_ap = col_factor(),
                        nccenr_ap = col_factor(),
                        libelle_ap = col_factor()
                      )) %>%
  rename_all(toupper)


# Table des informations supra communales de chaque commune ---------------

communes<-communes_cog %>%
  filter(TYPECOM=="COM") %>%
#  select(DEPCOM,NOM_DEPCOM,DEP,REG) %>%
  mutate(DEPCOM=fct_drop(DEPCOM),
         NOM_DEPCOM=fct_drop(NOM_DEPCOM)) %>%
  left_join(communes_epci %>%
              select(DEPCOM,EPCI,NOM_EPCI)) %>%
  left_join(epci_rattachement_reg_dep) %>%
  left_join(departements %>%
              select(DEP,NOM_DEP)) %>%
  left_join(regions %>%
              select(REG,NOM_REG)) %>%
  select(DEPCOM,NOM_DEPCOM,EPCI,NOM_EPCI,DEP,NOM_DEP,
         REG,NOM_REG,DEPARTEMENTS_DE_L_EPCI,REGIONS_DE_L_EPCI,
         ARR,TNCC,NCC,NCCENR,CAN,COMPARENT) %>%
  mutate(DEPARTEMENTS_DE_L_EPCI=if_else(EPCI!="ZZZZZZZZZ", DEPARTEMENTS_DE_L_EPCI, as.list(as.character(DEP))),
         REGIONS_DE_L_EPCI=if_else(EPCI!="ZZZZZZZZZ", REGIONS_DE_L_EPCI, as.list(as.character(REG))))

communes_info_supra<-communes %>%
  select(DEPCOM,NOM_DEPCOM,EPCI,NOM_EPCI,
         DEP,NOM_DEP,REG,NOM_REG,DEPARTEMENTS_DE_L_EPCI,
         REGIONS_DE_L_EPCI)

# Table liste zone --------------------------------------------------------

liste_zone<-bind_rows(
  communes_info_supra %>%
    select(CodeZone=DEPCOM,Zone = NOM_DEPCOM,EPCI,DEP,REG) %>%
    distinct() %>%
    mutate(TypeZone="Communes",
           DEP=as.list(as.character(DEP)),
           REG=as.list(as.character(REG))
    ),
  epci %>%
    select(CodeZone=EPCI,Zone = NOM_EPCI,NATURE_EPCI,DEP=DEPARTEMENTS_DE_L_EPCI,REG=REGIONS_DE_L_EPCI) %>%
    distinct() %>%
    mutate(TypeZone="Epci",EPCI=CodeZone),
  departements %>%
    select(CodeZone=DEP,Zone = NOM_DEP,REG) %>%
    distinct() %>%
    mutate(TypeZone="Départements",
           DEP=as.list(as.character(CodeZone)),
           REG=as.list(as.character(REG))),
  regions %>%
    select(REG,Zone = NOM_REG) %>%
    distinct() %>%
    mutate(TypeZone="Régions",
           CodeZone=REG,
           REG=as.list(as.character(REG)))
) %>%
  mutate_if(is.character,as.factor) %>%
  select(CodeZone,Zone,TypeZone,EPCI,NATURE_EPCI,DEP,REG)

# Table de passage des communes historiques ---------------------------------------
# source : https://www.insee.fr/fr/information/2028028
table_passage_com_historique <- read_xlsx('data-raw/source/2020/COG/table_passage_geo2003_geo2020.xlsx',skip = 5) %>%
  select(DEPCOM_HIST = CODGEO_INI, DEPCOM = CODGEO_2020) %>%
  mutate(across(.cols = everything(),as.factor))

table_passage_arm_com <- read.csv2('data-raw/source/2020/COG/arm.csv') %>%
  select(DEPCOM_HIST = CODGEO_INI, DEPCOM = CODGEO_2020) %>%
  mutate(across(.cols = everything(),as.factor))

table_passage_com_historique <- table_passage_com_historique %>%
  bind_rows(table_passage_arm_com) %>%
  mutate(across(.cols = everything(),as.factor))

# Gestion des encodages ----------------------------------------------------
enc.fact.utf8 <- function(a) {
  x<-levels(a)
  Encoding(x)<-"UTF-8"
  levels(a)<-x }

enc.fact.utf8(communes_cog$NOM_DEPCOM)
enc.fact.utf8(communes_cog$NCC)
enc.fact.utf8(communes_cog$NCCENR)
enc.fact.utf8(communes$NOM_DEPCOM)
enc.fact.utf8(communes$NCC)
enc.fact.utf8(communes$NCCENR)
enc.fact.utf8(communes$NOM_EPCI)
enc.fact.utf8(communes$NOM_DEP)
enc.fact.utf8(communes$NOM_REG)
enc.fact.utf8(communes_info_supra$NOM_DEPCOM)
enc.fact.utf8(communes_info_supra$NOM_EPCI)
enc.fact.utf8(communes_info_supra$NOM_DEP)
enc.fact.utf8(communes_info_supra$NOM_REG)
enc.fact.utf8(epci$NOM_EPCI)
enc.fact.utf8(departements$NCC)
enc.fact.utf8(departements$NCCENR)
enc.fact.utf8(departements$NOM_DEP)
enc.fact.utf8(regions$NCC)
enc.fact.utf8(regions$NCCENR)
enc.fact.utf8(regions$NOM_REG)
enc.fact.utf8(mvtcommunes$NCC_AV)
enc.fact.utf8(mvtcommunes$NCCENR_AV)
enc.fact.utf8(mvtcommunes$LIBELLE_AV)
enc.fact.utf8(mvtcommunes$NCC_AP)
enc.fact.utf8(mvtcommunes$NCCENR_AP)
enc.fact.utf8(mvtcommunes$LIBELLE_AP)
enc.fact.utf8(liste_zone$Zone)


use_data(regions,internal=F,overwrite = T)
use_data(departements,internal=F,overwrite = T)
use_data(epci,internal=F,overwrite = T)
use_data(communes_cog,internal=F,overwrite = T)
use_data(communes,internal=F,overwrite = T)
use_data(communes_info_supra,internal=F,overwrite = T)
use_data(mvtcommunes,internal=F,overwrite = T)
use_data(table_passage_com_epci,internal=F,overwrite = T)
use_data(pop2015,internal=F,overwrite = T)
use_data(liste_zone,internal=F,overwrite = T)
use_data(table_passage_com_historique,internal=F,overwrite = T)


