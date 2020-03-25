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
## probleme sur la table commune_carto sur la métropole, on utilise donc une simplification de commune via mapshaper :
#simplification utilisant une simplification à 5% en décochant la possibilité d'un

com_metro<- st_read("data-raw/source/2019/adminexpress/metro_simplifie/COMMUNE.shp")

origine_metro <- c(st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1,1]], st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1,2]] )

# arguments à passer pour chaque doms : code, centroid de destination, echelle, angle)





translate_drom <- function(code_dom,destination,scale=1,angle=0,epsg=2154) {

  if (code_dom != '976'){

    com_dom <- st_read(paste0("data-raw/source/2019/adminexpress/",code_dom,"/COMMUNE_CARTO.shp")) %>%
      st_transform(epsg)
  }
  else {

    com_dom <- st_read(paste0("data-raw/source/2019/adminexpress/",code_dom,"/COMMUNE.shp")) %>%
      st_transform(epsg)
  }

  # centroid d'origine
  centroid_com_dom_sfc <- st_centroid(st_geometry(com_dom %>%
                                                    summarise()))
  origine <- centroid_com_dom_sfc[[1]]

  com_dom_sfc <- st_geometry(com_dom)
  rotation = function(a){
    r = a * pi / 180 #degrees to radians
    matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
  }

  com_dom_sfc_middle <- (com_dom_sfc-centroid_com_dom_sfc)*scale* rotation(angle) + centroid_com_dom_sfc
  com_dom_sfc_trans <- com_dom_sfc_middle + c(destination[1]-origine[1], destination[2]-origine[2])
  com_dom_trans <- st_set_geometry(com_dom, com_dom_sfc_trans)
  st_crs(com_dom_trans) <- st_crs(com_dom)

  return(com_dom_trans)
}

arg <- list(code_dom=c("971", "972", "973", "974", "976"),
            destination=list(c(144008.229673723,6497197.13214097),
                             c(168304.796604606,6353491.45368567),
                             c(211011.372716378, 6164488.37843379),
                             c(279617.056312069, 6398160.72995736),
                             c(297376.58755285, 6507015.666758)
            ),
            scale=c(1.3,1.3,0.5,1,1.4),
            angle=c(-50,-50,-45,30,30))

l <- pmap(arg,translate_drom)
dom_geo <- rbind(l[[1]],l[[2]],l[[3]],l[[4]],l[[5]])


communes_geo <- rbind(com_metro, dom_geo) %>%
  as_tibble %>%
  select(DEPCOM=INSEE_COM, geometry) %>%
  st_as_sf() %>%
  st_set_crs(2154)


# gestion des arrondissements de Paris, Lyon, Marseille dorénavant intégré à admin express

communes_geo<-communes_geo %>%
  rename(DEPCOM_HIST=DEPCOM) %>%
  left_join(table_passage_com_historique) %>%
  select(-DEPCOM_HIST) %>%
  group_by(DEPCOM) %>%
  summarise(do_union=T) %>%
  mutate(AREA=st_area(geometry))


communes_metro_geo <- com_metro %>%
  as_tibble %>%
  select(DEPCOM=INSEE_COM, geometry)%>%
  st_as_sf()%>%
  st_set_crs(2154)

epci_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
  inner_join(communes_geo %>% select(DEPCOM), ., by="DEPCOM") %>%
  select(EPCI) %>%
  group_by(EPCI) %>%
  summarise(do_union=T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))

epci_metro_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
  inner_join(communes_metro_geo %>% select(DEPCOM), ., by="DEPCOM") %>%
  select(EPCI) %>%
  group_by(EPCI) %>%
  summarise(do_union=T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))

departements_geo <- inner_join(communes_geo %>% select(DEPCOM),
                               communes_info_supra,
                               by = "DEPCOM") %>%
  select(DEP) %>%
  group_by(DEP) %>%
  summarise(do_union=T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))
departements_metro_geo <- inner_join(communes_metro_geo %>% select(DEPCOM),
                                     communes_info_supra,
                                     by = "DEPCOM") %>%
  select(DEP) %>%
  group_by(DEP) %>%
  summarise(do_union=T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))
plot(departements_metro_geo)
regions_geo <- inner_join(communes_geo %>% select(DEPCOM),
                          communes_info_supra,
                          by = "DEPCOM") %>%
  select(REG) %>%
  group_by(REG) %>%
  summarise(do_union = T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))



# Communes DOM -----------------

communes_971_geo <- st_read(paste0("data-raw/source/2019/adminexpress/",'971',"/COMMUNE_CARTO.shp")) %>%
  select(DEPCOM=INSEE_COM, geometry)
communes_972_geo <- st_read(paste0("data-raw/source/2019/adminexpress/",'972',"/COMMUNE_CARTO.shp")) %>%
  select(DEPCOM=INSEE_COM, geometry)
communes_973_geo <- st_read(paste0("data-raw/source/2019/adminexpress/",'973',"/COMMUNE_CARTO.shp")) %>%
  select(DEPCOM=INSEE_COM, geometry)
communes_974_geo <- st_read(paste0("data-raw/source/2019/adminexpress/",'974',"/COMMUNE_CARTO.shp")) %>%
  select(DEPCOM=INSEE_COM, geometry)
communes_976_geo <- st_read(paste0("data-raw/source/2019/adminexpress/",'976',"/COMMUNE.shp")) %>%
  select(DEPCOM=INSEE_COM, geometry)

# Epci DOM --------------------

epci_971_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
  inner_join(communes_971_geo %>% select(DEPCOM), ., by="DEPCOM") %>%
  select(EPCI) %>%
  group_by(EPCI) %>%
  summarise(do_union=T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))

epci_972_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
  inner_join(communes_972_geo %>% select(DEPCOM), ., by="DEPCOM") %>%
  select(EPCI) %>%
  group_by(EPCI) %>%
  summarise(do_union=T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))

epci_973_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
  inner_join(communes_973_geo %>% select(DEPCOM), ., by="DEPCOM") %>%
  select(EPCI) %>%
  group_by(EPCI) %>%
  summarise(do_union=T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))

epci_974_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
  inner_join(communes_974_geo %>% select(DEPCOM), ., by="DEPCOM") %>%
  select(EPCI) %>%
  group_by(EPCI) %>%
  summarise(do_union=T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))

epci_976_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
  inner_join(communes_976_geo %>% select(DEPCOM), ., by="DEPCOM") %>%
  select(EPCI) %>%
  group_by(EPCI) %>%
  summarise(do_union=T) %>%
  ungroup() %>%
  mutate(AREA=st_area(geometry))

# sauvegarde des données --------------------------------------------------------
use_data(communes_geo,internal=F, overwrite = T)
use_data(communes_971_geo,internal=F, overwrite = T)
use_data(communes_972_geo,internal=F, overwrite = T)
use_data(communes_973_geo,internal=F, overwrite = T)
use_data(communes_974_geo,internal=F, overwrite = T)
use_data(communes_976_geo,internal=F, overwrite = T)
use_data(epci_geo,internal=F,overwrite = T)
use_data(epci_971_geo,internal=F,overwrite = T)
use_data(epci_972_geo,internal=F,overwrite = T)
use_data(epci_973_geo,internal=F,overwrite = T)
use_data(epci_974_geo,internal=F,overwrite = T)
use_data(epci_976_geo,internal=F,overwrite = T)
use_data(departements_geo,internal=F,overwrite = T)
use_data(regions_geo,internal=F,overwrite = T)

rm(i, origine_metro, doms, dom, com_dom, com_metro, com_971, com_972, com_973, com_974, com_976, ctrd_com_dom, bbox_dom, ctrd_dom, alpha, url_admin_express)
