# COG 2018 ----------------------

# Packages ----------------------
rm(list=ls())
library(readxl)
library(readr)
library(stringr)
library(tidyverse)
library(sf)
# Chargement Admin Express -------------------------------

communes_geo<-st_read(dsn="S:/REFERENTIELS/ADMINEXPRESS/1_DONNEES_LIVRAISON_2018-02-15/ADE_1-1_SHP_LAMB93_FR",layer="COMMUNE") %>%
  rename(DEPCOM=INSEE_COM,
         DEP=INSEE_DEP,
         REG=INSEE_REG) %>%
  select(DEPCOM)

epci_geo<-st_read(dsn="S:/REFERENTIELS/ADMINEXPRESS/1_DONNEES_LIVRAISON_2018-02-15/ADE_1-1_SHP_LAMB93_FR",layer="EPCI") %>%
  rename(EPCI=CODE_EPCI) %>%
  select(EPCI)

departements_geo<-st_read(dsn="S:/REFERENTIELS/ADMINEXPRESS/1_DONNEES_LIVRAISON_2018-02-15/ADE_1-1_SHP_LAMB93_FR",layer="DEPARTEMENT")%>%
  rename(DEP=INSEE_DEP,
         REG=INSEE_REG) %>%
  select(DEP)

regions_geo<-st_read(dsn="S:/REFERENTIELS/ADMINEXPRESS/1_DONNEES_LIVRAISON_2018-02-15/ADE_1-1_SHP_LAMB93_FR",layer="REGION")%>%
  rename(REG=INSEE_REG)%>%
  select(REG)

communes_geo<-st_transform(communes_geo,"+proj=longlat +datum=WGS84")
epci_geo<-st_transform(epci_geo,"+proj=longlat +datum=WGS84")
departements_geo<-st_transform(departements_geo,"+proj=longlat +datum=WGS84")
regions_geo<-st_transform(regions_geo,"+proj=longlat +datum=WGS84")


# COG Insee

# Chargement des tables---------------
epci<-read_excel("data/Intercommunalité - Métropole au 01-01-2018.xls",sheet=2,skip=5) %>%
  mutate(CODGEO=as.factor(CODGEO),
         EPCI=as.factor(EPCI),
         REG=as.factor(REG),
         DEP=as.factor(DEP))%>%
  as_tibble()

departements<-read.delim("data/depts2018.txt",fileEncoding = "Latin1") %>%
  mutate(REGION=REGION %>% as.character(.) %>% str_pad(.,2,"left",0) %>% as.factor) %>%
  rename(REG=REGION,
         NOM_DEP=NCCENR) %>%
  as_tibble()

regions<-read.delim("data/reg2018.txt",fileEncoding = "Latin1")  %>%
  mutate(REGION=REGION %>% as.character(.) %>% str_pad(.,2,"left",0) %>% as.factor)%>%
  rename(REG=REGION,
         NOM_REG=NCCENR) %>%
  as_tibble()

communes<-read.delim("data/comsimp2018.txt",fileEncoding="latin1") %>%
  mutate(DEPCOM=case_when(
    REG %in% c(1,2,3,4,6) ~ paste0(as.character(DEP),str_pad(as.character(COM),2,"left",pad="0")),
    T ~ paste0(as.character(DEP),str_pad(as.character(COM),3,"left",pad="0"))
  ),
  REG=REG %>% as.character(.) %>% str_pad(.,2,"left",0) %>% as.factor) %>%
  mutate_if(is.numeric,as.factor) %>%
  as_tibble()


# Création de la table de passage entre l'historique des communes et les communes existants--------------
# On commence par enlever les 8 communes périmées (actual = 3) et sans pole de rattachemen( pole à blanc)

table_passage_com_historique<-read.delim("data/France2018.txt",fileEncoding="latin1") %>%
  filter(!(ACTUAL==3 & POLE=="")) %>%
  mutate(depcom=case_when(
    REG %in% c(1,2,3,4,6) ~ paste0(as.character(DEP),str_pad(as.character(COM),2,"left",pad="0")),
    T ~ paste0(as.character(DEP),str_pad(as.character(COM),3,"left",pad="0"))
  ),
  depcom_a_jour=ifelse(POLE=="",
                       depcom,
                       as.character(str_pad(POLE,5,"left",0))),
  REG=REG %>% as.character(.) %>% str_pad(.,2,"left",0) %>% as.factor
  ) %>%
  filter(CDC==0|CDC==2|is.na(CDC)) %>%
  as_tibble()

#Gestion des fusions de fusion de communes et integration des donées des tables EPCI, DEP, ET REG----------------------
table_passage_com_historique<-table_passage_com_historique %>%
  left_join(select(table_passage_com_historique,depcom,depcom_a_jour),by=c("depcom_a_jour"="depcom")) %>%
  select(-depcom_a_jour) %>%
  rename(depcom_a_jour=depcom_a_jour.y) %>%
  left_join(select(table_passage_com_historique,depcom,NCCENR,DEP,REG),by=c("depcom_a_jour"="depcom")) %>%
  left_join(select(epci,CODGEO,EPCI,LIBEPCI),by=c("depcom_a_jour"="CODGEO")) %>%
  rename(nepci_a_jour=LIBEPCI,
         epci_a_jour=EPCI,
         ndepcom_a_jour=NCCENR.y,
         dep_a_jour=DEP.y,
         reg_a_jour=REG.y,
         REG=REG.x,
         DEP=DEP.x,
         NCCENR=NCCENR.x) %>%
  left_join(departements %>% select(dep_a_jour=DEP,ndep_a_jour=NOM_DEP)) %>%
  left_join(regions %>% select(reg_a_jour=REG,nreg_a_jour=NOM_REG))

#Intégration d'une colonne listant toutes les régions auxquelles appartiennent les epci-----------

epci<-table_passage_com_historique %>%
  filter(!is.na(nepci_a_jour),nepci_a_jour != "Sans objet") %>%
  select(epci_a_jour,nepci_a_jour,reg_a_jour) %>%
  distinct() %>%
  mutate(reg_a_jour=as.character(reg_a_jour)) %>%
  group_by(epci_a_jour,nepci_a_jour) %>%
  nest(.key=regions_de_l_epci_a_jour)

table_passage_com_historique<-table_passage_com_historique %>%
  left_join(epci %>% select(-nepci_a_jour)) %>%
  mutate_if(is.character,as.factor) %>%
  mutate(reg_a_jour=factor(reg_a_jour)) %>%
  select(depcom,
         depcom_a_jour,
         ndepcom_a_jour,
         epci_a_jour,
         nepci_a_jour,
         dep_a_jour,
         ndep_a_jour,
         reg_a_jour,
         nreg_a_jour,
         regions_de_l_epci_a_jour) %>%
  setNames(c("DEPCOM_HIST","DEPCOM","NOM_DEPCOM","EPCI","NOM_EPCI","DEP","NOM_DEP","REG","NOM_REG","REGIONS_DE_L_EPCI"))

epci<-epci %>%
  setNames(c("EPCI","NOM_EPCI","REGIONS_DE_L_EPCI"))

# Création de la table de passage entre les communes et leur EPCI au 1er janvier -------------

table_passage_com_epci<-read_excel("data/Intercommunalité - Métropole au 01-01-2018.xls",sheet=2,skip=5) %>%
  mutate(CODGEO=as.factor(CODGEO),
         EPCI=as.factor(EPCI),
         LIBEPCI=as.factor(LIBEPCI),
         REG=as.factor(REG),
         DEP=as.factor(DEP)) %>%
  rename(DEPCOM=CODGEO,NOM_DEPCOM=LIBGEO,NOM_EPCI=LIBEPCI) %>%
  as_tibble()

# rajout des données communales à jour de la table de passage dans la table commune
temp<-table_passage_com_historique %>%
  select(-DEPCOM_HIST,-REGIONS_DE_L_EPCI) %>%
  distinct

communes<-communes %>%
  inner_join(temp) %>%
  left_join(epci %>% select(EPCI,REGIONS_DE_L_EPCI)) %>%
  select(DEPCOM,NOM_DEPCOM,EPCI,NOM_EPCI,DEP,NOM_DEP,REG,NOM_REG,REGIONS_DE_L_EPCI,CDC,CHEFLIEU,COM,AR,CT,TNCC,ARTMAJ,NCC,ARTMIN,NCCENR)
rm(temp)

# suppression des données communales à jour de la table de passage
table_passage_com_historique<-table_passage_com_historique %>%
  select(DEPCOM_HIST,DEPCOM)

# Liste zone, une table helper pour sélectionner tous les territoires d'une région donnée.

liste_zone<-bind_rows(
  communes %>%
    select(CodeZone=DEPCOM,REG) %>%
    distinct() %>%
    mutate(TypeZone="Communes",
           REG=as.list(as.character(REG))
    ),
  epci %>%
    select(CodeZone=EPCI,REG=REGIONS_DE_L_EPCI) %>%
    distinct() %>%
    mutate(TypeZone="Epci"),
  departements %>%
    select(CodeZone=DEP,REG) %>%
    distinct() %>%
    mutate(TypeZone="Départements",
           REG=as.list(as.character(REG))),
  regions %>%
    select(REG) %>%
    distinct() %>%
    mutate(TypeZone="Régions",
           CodeZone=REG,
           REG=as.list(as.character(REG)))
) %>%
  mutate_if(is.character,as.factor)

#Zonages abc
zonage_abc_r52<-read_excel("data/zonageabc_pdl_communes2018.xls") %>%
  mutate(zonage_abc=str_c("Zone ",zonage_abc)) %>%
  mutate_all(funs(as.factor(.)))

zonage_pinel_r52<-read_excel("data/zonagepinel_pdl_communes2018.xls") %>%
  mutate(zonage_pinel=str_c("Zone ",zonage_pinel)) %>%
  mutate_all(funs(as.factor(.))) %>%
  passer_au_cog_a_jour(aggrege = F,garder_info_supra=F) %>%
  distinct

source("script/fonctions.R")
save.image(file="livrable/COG.RData")

write.csv2(table_passage_com_historique,file="livrable/table_passage_com_historique.csv")

use_data(communes,internal=F)
use_data(communes_geo,internal=F)
use_data(departements,internal=F)
use_data(departements_geo,internal=F)
use_data(epci,internal=F)
use_data(epci_geo,internal=F)
use_data(regions,internal=F)
use_data(regions_geo,internal=F)
use_data(table_passage_com_epci,internal=F)
use_data(table_passage_com_historique,internal=F)
use_data(zonage_abc_r52)
use_data(zonage_pinel_r52)
