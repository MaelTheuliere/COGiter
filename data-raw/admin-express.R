# Exploitation des données du COG 2018 pour les remettre sur le périmètre 2019
library(sf)
library(tidyverse)
load("data/table_passage_com_historique.rda")
load("data/communes_info_supra.rda")
com_metro<- st_read("data-raw/source/2018/adminexpress/metro/COMMUNE_CARTO.shp") %>%
  st_set_crs(2154)

origine_metro <- c(st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1,1]], st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1,2]] )
doms<-c("971", "972", "973", "974", "976")

for (i in 1:5) {

  dom <- doms[[i]]

  com_dom <- st_read(paste0("data-raw/source/2018/adminexpress/",dom,"/COMMUNE_CARTO.shp")) %>%
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

communes_geo_2019<-communes_geo %>%
  left_join(table_passage_com_historique,c("DEPCOM"="DEPCOM_HIST")) %>%
  select(-DEPCOM) %>%
  rename(DEPCOM=DEPCOM.y) %>%
  group_by(DEPCOM) %>%
  summarise(do_union=T)


# Carte des Epci
epci_geo_2019<-communes_geo_2019 %>%
  left_join(communes_info_supra %>%
              select(DEPCOM,EPCI)) %>%
  filter(EPCI != "ZZZZZZZZZ") %>%
  select(-DEPCOM) %>%
  group_by(EPCI) %>%
  summarise(do_union=T)


# Carte des départements

departements_geo_2019<-communes_geo_2019 %>%
  left_join(communes_info_supra %>%
              select(DEPCOM,DEP)) %>%
  select(-DEPCOM) %>%
  group_by(DEP) %>%
  summarise(do_union=T)

# Carte des régions

regions_geo_2019<-communes_geo_2019 %>%
  left_join(communes_info_supra %>%
              select(DEPCOM,REG)) %>%
  select(-DEPCOM) %>%
  group_by(REG) %>%
  summarise(do_union=T)

communes_geo<-communes_geo_2019
departements_geo<-departements_geo_2019
epci_geo<-epci_geo_2019
regions_geo<-regions_geo_2019

# sauvegarde des données --------------------------------------------------------
use_data(communes_geo,internal=F, overwrite = T)
use_data(departements_geo,internal=F,overwrite = T)
use_data(epci_geo,internal=F,overwrite = T)
use_data(regions_geo,internal=F,overwrite = T)
