library(sf)
library(usethis)
library(tidyverse)
load("data/communes_info_supra.rda")
load("data/table_passage_com_historique.rda")

# Chargement Admin Express -------------------------------

## téléchargement des couches IGN  ----
## Chargements des données présentes sur le site IGN : ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS-COG_2-0__SHP__FRA_2019-04-17.7z.001
# Stockée ici :
## compilation des couches communales metropole + DOM ----
## data-raw/source/2019/adminexpress/
## probleme sur la table commune_carto sur la métropole, on utilise donc une simplification de commune via mapshaper

com_metro<- st_read("data-raw/source/2019/adminexpress/metro_simplifie/COMMUNE.shp") %>%
  st_set_crs(2154)

origine_metro <- c(st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1,1]], st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1,2]] )
doms<-c("971", "972", "973", "974", "976")


for (i in 1:5) {

  dom <- doms[[i]]
if (i<5){
  com_dom <- st_read(paste0("data-raw/source/2019/adminexpress/",dom,"/COMMUNE_CARTO.shp")) %>%
    st_set_crs(st_crs(com_metro))
}
  else {  com_dom <- st_read(paste0("data-raw/source/2019/adminexpress/",dom,"/COMMUNE.shp")) %>%
    st_set_crs(st_crs(com_metro))
  }
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

# gestion des arrondissements de Paris, Lyon, Marseille dorénavant intégré à admin express

communes_geo<-communes_geo %>%
  rename(DEPCOM_HIST=DEPCOM) %>%
  left_join(table_passage_com_historique) %>%
  select(-DEPCOM_HIST) %>%
  group_by(DEPCOM) %>%
  summarise(do_union=T)

epci_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
  inner_join(communes_geo, ., by="DEPCOM") %>%
  select(EPCI) %>% group_by(EPCI) %>% summarise(do_union=T) %>% ungroup()

departements_geo <- inner_join(communes_geo, communes_info_supra, by="DEPCOM") %>%
  select(DEP) %>% group_by(DEP) %>% summarise(do_union=T) %>% ungroup()

regions_geo <- inner_join(communes_geo, communes_info_supra, by="DEPCOM")%>%
  select(REG) %>% group_by(REG) %>% summarise(do_union=T) %>% ungroup()

# sauvegarde des données --------------------------------------------------------
use_data(communes_geo,internal=F, overwrite = T)
use_data(departements_geo,internal=F,overwrite = T)
use_data(epci_geo,internal=F,overwrite = T)
use_data(regions_geo,internal=F,overwrite = T)
