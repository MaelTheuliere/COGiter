# COG 2018 ----------------------

# Packages ----------------------
rm(list=ls())
library(readxl)
library(readr)
library(stringr)
library(tidyverse)
library(sf)
library(COGiter)
library(curl)

# Chargement Admin Express -------------------------------

## téléchargement des couches IGN  ----

url_admin_express <- "https://wxs-telechargement.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-COG-PACK_2018-05-04$ADMIN-EXPRESS-COG_1-1__SHP__FRA_2018-04-03/file/ADMIN-EXPRESS-COG_1-1__SHP__FRA_2018-04-03.7z"
ie_get_proxy_for_url(url_admin_express)
curl_download(url_admin_express , "data-raw/source/adminexpress.7z" , mode = "wb" )

system('"C:/Program Files (x86)/7-Zip/7z.exe" e -aoa -odata-raw/source/adminexpress/metro data-raw/source/adminexpress.7z *FR/COMMUNE_CARTO* -r')
system('"C:/Program Files (x86)/7-Zip/7z.exe" e -aoa -odata-raw/source/adminexpress/971 data-raw/source/adminexpress.7z *D971/COMMUNE_CARTO* -r')
system('"C:/Program Files (x86)/7-Zip/7z.exe" e -aoa -odata-raw/source/adminexpress/972 data-raw/source/adminexpress.7z *D972/COMMUNE_CARTO* -r')
system('"C:/Program Files (x86)/7-Zip/7z.exe" e -aoa -odata-raw/source/adminexpress/973 data-raw/source/adminexpress.7z *D973/COMMUNE_CARTO* -r')
system('"C:/Program Files (x86)/7-Zip/7z.exe" e -aoa -odata-raw/source/adminexpress/974 data-raw/source/adminexpress.7z *D974/COMMUNE_CARTO* -r')
system('"C:/Program Files (x86)/7-Zip/7z.exe" e -aoa -odata-raw/source/adminexpress/976 data-raw/source/adminexpress.7z *D976/COMMUNE_CARTO* -r')

## compilation des couches communales metropole + DOM ----

com_metro<- st_read("data-raw/source/adminexpress/metro/COMMUNE_CARTO.shp") %>%
  st_set_crs(2154)

origine_metro <- c(st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1,1]], st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1,2]] )
doms<-c("971", "972", "973", "974", "976")

for (i in 1:5) {
  
  dom <- doms[[i]]
  
  com_dom <- st_read(paste0("data-raw/source/adminexpress/",dom,"/COMMUNE_CARTO.shp")) %>%
    st_set_crs(st_crs(com_metro)) 
  
  ctrd_com_dom <- st_centroid(st_geometry(com_dom)) # vecteur des centroïdes de communes du dom
  bbox_dom <- st_bbox(com_dom)  
  ctrd_dom <- st_centroid(st_as_sfc(bbox_dom)) # centre de la bbox du dom
  alpha <- 160000/(bbox_dom$ymax - bbox_dom$ymin) # rapport de proportionnalité (pour un emplacement de 210 km de hauteur)
  
  st_geometry(com_dom) <- (st_geometry(com_dom) - ctrd_com_dom ) * alpha + ctrd_com_dom * alpha  # agrandissement de la géometrie du dom
  st_geometry(com_dom) <- st_geometry(com_dom) - ctrd_dom * alpha + origine_metro + c(-175000,7110500-6049646-210000*(i-0.5)) # translation vers l'emplacement
  
  assign(paste0("com_",dom), com_dom)
  
}

communes_geo <- bind_rows(com_metro, com_971, com_972, com_973, com_974, com_976) %>% 
  as_tibble %>% 
  select(DEPCOM=INSEE_COM, geometry)%>%
  st_as_sf()%>%
  st_set_crs(2154)

rm(i, origine_metro, doms, dom, com_dom, com_metro, com_971, com_972, com_973, com_974, com_976, ctrd_com_dom, bbox_dom, ctrd_dom, alpha, url_admin_express)


## constitution des tables géo supra ----
data("communes")

epci_geo <- inner_join(communes_geo, communes, by="DEPCOM") %>%
  select(EPCI) %>% group_by(EPCI) %>% summarise(do_union=T) %>% ungroup()

departements_geo <- inner_join(communes_geo, communes, by="DEPCOM") %>%
  select(DEP) %>% group_by(DEP) %>% summarise(do_union=T) %>% ungroup()

regions_geo <- inner_join(communes_geo, communes, by="DEPCOM")%>%
  select(REG) %>% group_by(REG) %>% summarise(do_union=T) %>% ungroup()


# Chargement des tables du COG---------------
epci_type<-read_excel("data-raw/source/Intercommunalité - Métropole au 01-01-2018.xls",sheet=1,skip=5) %>%
  mutate(EPCI=as.factor(EPCI),
         LIBEPCI=as.factor(LIBEPCI),
         NATURE_EPCI=as.factor(NATURE_EPCI)
         )%>%
  select(EPCI,NATURE_EPCI) %>%
  as_tibble()


epci<-read_excel("data-raw/source/Intercommunalité - Métropole au 01-01-2018.xls",sheet=2,skip=5) %>%
  mutate(CODGEO=as.factor(CODGEO),
         EPCI=as.factor(EPCI),
         REG=as.factor(REG),
         DEP=as.factor(DEP))%>%
  as_tibble()

departements<-read.delim("data-raw/source/depts2018.txt",fileEncoding = "Latin1") %>%
  mutate(REGION=REGION %>% as.character(.) %>% str_pad(.,2,"left",0) %>% as.factor) %>%
  rename(REG=REGION,
         NOM_DEP=NCCENR) %>%
  as_tibble()

regions<-read.delim("data-raw/source/reg2018.txt",fileEncoding = "Latin1")  %>%
  mutate(REGION=REGION %>% as.character(.) %>% str_pad(.,2,"left",0) %>% as.factor)%>%
  rename(REG=REGION,
         NOM_REG=NCCENR) %>%
  as_tibble()

communes<-read.delim("data-raw/source/comsimp2018.txt",fileEncoding="latin1") %>%
  mutate(DEPCOM=case_when(
    REG %in% c(1,2,3,4,6) ~ paste0(as.character(DEP),str_pad(as.character(COM),2,"left",pad="0")),
    T ~ paste0(as.character(DEP),str_pad(as.character(COM),3,"left",pad="0"))
  ),
  REG=REG %>% as.character(.) %>% str_pad(.,2,"left",0) %>% as.factor) %>%
  mutate_if(is.numeric,as.factor) %>%
  as_tibble()


# Création de la table de passage entre l'historique des communes et les communes existants--------------
# On commence par enlever les 8 communes périmées (actual = 3) et sans pole de rattachemen( pole à blanc)

table_passage_com_historique<-read.delim("data-raw/source/France2018.txt",fileEncoding="latin1") %>%
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

#Gestion des fusions de fusion de communes et integration des données des tables EPCI, DEP, ET REG----------------------
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

epci<-left_join(
  table_passage_com_historique %>%
    filter(!is.na(nepci_a_jour),nepci_a_jour != "Sans objet") %>%
    select(epci_a_jour,nepci_a_jour,dep_a_jour) %>%
    distinct() %>%
    mutate(dep_a_jour=as.character(dep_a_jour)
    ) %>%
    group_by(epci_a_jour,nepci_a_jour) %>%
    summarise(departements_de_l_epci_a_jour=list(dep_a_jour)
    ) %>%
    ungroup,
  table_passage_com_historique %>%
    filter(!is.na(nepci_a_jour),nepci_a_jour != "Sans objet") %>%
    select(epci_a_jour,nepci_a_jour,reg_a_jour) %>%
    distinct() %>%
    mutate(reg_a_jour=as.character(reg_a_jour)
    ) %>%
    group_by(epci_a_jour,nepci_a_jour) %>%
    summarise(regions_de_l_epci_a_jour=list(reg_a_jour)
    ) %>%
    ungroup
)

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
         departements_de_l_epci_a_jour,
         regions_de_l_epci_a_jour) %>%
  setNames(c("DEPCOM_HIST","DEPCOM","NOM_DEPCOM","EPCI","NOM_EPCI","DEP","NOM_DEP","REG","NOM_REG","DEPARTEMENTS_DE_L_EPCI","REGIONS_DE_L_EPCI"))

epci<-epci %>%
  setNames(c("EPCI","NOM_EPCI","DEPARTEMENTS_DE_L_EPCI","REGIONS_DE_L_EPCI")) %>%
  left_join(epci_type)


# Création de la table de passage entre les communes et leur EPCI au 1er janvier -------------

table_passage_com_epci<-read_excel("data-raw/source/Intercommunalité - Métropole au 01-01-2018.xls",sheet=2,skip=5) %>%
  select(DEPCOM=CODGEO,EPCI) %>%
  as_tibble()

# rajout des données communales à jour de la table de passage dans la table commune
temp<-table_passage_com_historique %>%
  select(-DEPCOM_HIST,-REGIONS_DE_L_EPCI,-DEPARTEMENTS_DE_L_EPCI) %>%
  distinct

communes<-communes %>%
  inner_join(temp) %>%
  left_join(epci %>% select(EPCI,DEPARTEMENTS_DE_L_EPCI,REGIONS_DE_L_EPCI)) %>%
  select(DEPCOM,NOM_DEPCOM,EPCI,NOM_EPCI,DEP,NOM_DEP,REG,NOM_REG,DEPARTEMENTS_DE_L_EPCI,REGIONS_DE_L_EPCI,CDC,CHEFLIEU,COM,AR,CT,TNCC,ARTMAJ,NCC,ARTMIN,NCCENR)
rm(temp)

# suppression des données communales à jour de la table de passage
table_passage_com_historique<-table_passage_com_historique %>%
  select(DEPCOM_HIST,DEPCOM)

# Liste zone, une table helper pour sélectionner tous les territoires d'une région donnée.

liste_zone<-bind_rows(
  communes %>%
    select(CodeZone=DEPCOM,EPCI,DEP,REG) %>%
    distinct() %>%
    mutate(TypeZone="Communes",
           DEP=as.list(as.character(DEP)),
           REG=as.list(as.character(REG))
    ),
  epci %>%
    select(CodeZone=EPCI,DEP=DEPARTEMENTS_DE_L_EPCI,REG=REGIONS_DE_L_EPCI) %>%
    distinct() %>%
    mutate(TypeZone="Epci",EPCI=CodeZone),
  departements %>%
    select(CodeZone=DEP,REG) %>%
    distinct() %>%
    mutate(TypeZone="Départements",
           DEP=as.list(as.character(CodeZone)),
           REG=as.list(as.character(REG))),
  regions %>%
    select(REG) %>%
    distinct() %>%
    mutate(TypeZone="Régions",
           CodeZone=REG,
           REG=as.list(as.character(REG)))
) %>%
  mutate_if(is.character,as.factor) %>%
  select(CodeZone,TypeZone,EPCI,DEP,REG)

#Zonages abc
zonage_abc_r52<-read_excel("data-raw/source/zonageabc_pdl_communes2018.xls") %>%
  mutate(zonage_abc=str_c("Zone ",zonage_abc)) %>%
  mutate_all(funs(as.factor(.)))

zonage_pinel_r52<-read_excel("data-raw/source/zonagepinel_pdl_communes2018.xls") %>%
  mutate(zonage_pinel=str_c("Zone ",zonage_pinel)) %>%
  mutate_all(funs(as.factor(.))) %>%
  passer_au_cog_a_jour(aggrege = F,garder_info_supra=F) %>%
  distinct

pop2015 <- read_excel("data-raw/source/pop2015.xls",
                      col_types = c("text", "numeric", "numeric",
                                    "numeric", "text")) %>%
  mutate(DEPCOM=case_when(
    DEP %in% c("971","972","973","974","976") ~ paste0(as.character(DEP),substr(as.character(DEPCOM),2,3)),
    T ~ paste0(as.character(DEP),as.character(DEPCOM))
  ) %>%
    as.factor()
  ) %>%
  select(-DEP)


# Gestion encodage --------------------------------------------------------

x<-levels(communes$NOM_DEPCOM)
Encoding(x)<-"UTF-8"
levels(communes$NOM_DEPCOM)<-x


x<-levels(communes$NOM_DEPCOM)
Encoding(x)<-"UTF-8"
levels(communes$NOM_DEPCOM)<-x


x<-levels(communes$NOM_EPCI)
Encoding(x)<-"UTF-8"
levels(communes$NOM_EPCI)<-x

x<-levels(communes$NOM_DEP)
Encoding(x)<-"UTF-8"
levels(communes$NOM_DEP)<-x

x<-levels(communes$NOM_REG)
Encoding(x)<-"UTF-8"
levels(communes$NOM_REG)<-x

x<-levels(communes$NCC)
Encoding(x)<-"UTF-8"
levels(communes$NCC)<-x

x<-levels(communes$NCCENR)
Encoding(x)<-"UTF-8"
levels(communes$NCCENR)<-x

x<-levels(epci$NOM_EPCI)
Encoding(x)<-"UTF-8"
levels(epci$NOM_EPCI)<-x


x<-levels(departements$NCC)
Encoding(x)<-"UTF-8"
levels(departements$NCC)<-x

x<-levels(departements$NOM_DEP)
Encoding(x)<-"UTF-8"
levels(departements$NOM_DEP)<-x

x<-levels(regions$NCC)
Encoding(x)<-"UTF-8"
levels(regions$NCC)<-x

x<-levels(regions$NOM_REG)
Encoding(x)<-"UTF-8"
levels(regions$NOM_REG)<-x

use_data(communes_geo,internal=F)
use_data(departements_geo,internal=F)
use_data(epci_geo,internal=F)
use_data(regions_geo,internal=F)

use_data(communes,internal=F,overwrite = T)
use_data(departements,internal=F,overwrite = T)
use_data(epci,internal=F,overwrite = T)
use_data(regions,internal=F,overwrite = T)
use_data(table_passage_com_epci,internal=F,overwrite = T)
use_data(table_passage_com_historique,internal=F,overwrite = T)
use_data(zonage_abc_r52,overwrite = T)
use_data(zonage_pinel_r52,overwrite = T)
use_data(pop2015,overwrite = T)
use_data(liste_zone,overwrite = T)
