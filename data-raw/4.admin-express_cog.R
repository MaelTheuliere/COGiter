library(sf)
library(geojsonsf)
library(rmapshaper)
library(mapview)
library(usethis)
library(tidyverse)
library(archive)
library(units)
load("data/communes_info_supra.rda")
load("data/table_passage_com_historique.rda")
millesime <- "2026"

## téléchargement des couches IGN admin express COG carto via flux geojson----
## Attention s'assurer que ADMINEXPRESS-COG-CARTO.LATEST correspond au millésime souhaité

options(timeout = 5*60)
get_page_adexpcog <- function(index = 0) {
  message("Interrogation des lignes ", index, " à ", (index + 5000))
  geojsonsf::geojson_sf(paste0("https://data.geopf.fr/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=ADMINEXPRESS-COG-CARTO.LATEST%3Acommune&OUTPUTFORMAT=application%2Fjson&SRSNAME=EPSG%3A4326&startIndex=",index)) %>%
    mutate(code_insee = as.character(code_insee))
}
indexes <- c(0, 5000, 10000, 15000, 20000, 25000, 30000)
com_fce_ent0 <- purrr::map(indexes, get_page_adexpcog)
com_fce_ent <- list_rbind(com_fce_ent0) %>%
  st_as_sf() %>%
  filter(code_insee_du_departement != "NR")

# Assemblage des couches communales métropole + DOM ---------

## sélection com métropole
com_metro <- filter(com_fce_ent, code_insee_de_la_region >= "10") %>%
  st_transform(2154)

origine_metro <- c(st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1, 1]], st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1, 2]])

## travail sur les DOM : translation + mise à l'échelle + changement EPSG
# arguments à passer pour chaque doms : code, centroid de destination, echelle, angle)

translate_drom <- function(code_dom, destination, scale = 1, angle = 0, epsg = 2154) {

  com_dom <- filter(com_fce_ent, code_insee_du_departement == code_dom) %>%
    st_transform(epsg)

  # centroid d'origine
  centroid_com_dom_sfc <- st_centroid(st_geometry(com_dom %>% summarise()))
  origine <- centroid_com_dom_sfc[[1]]

  com_dom_sfc <- st_geometry(com_dom)

  rotation = function(a) {
    r = a * pi / 180 # degrees to radians
    matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
  }

  com_dom_sfc_middle <- (com_dom_sfc - centroid_com_dom_sfc) * scale * rotation(angle) + centroid_com_dom_sfc
  com_dom_sfc_trans <- com_dom_sfc_middle + c(destination[1] - origine[1], destination[2] - origine[2])
  com_dom_trans <- st_set_geometry(com_dom, com_dom_sfc_trans)
  st_crs(com_dom_trans) <- st_crs(com_dom)

  return(com_dom_trans)
}

arg <- list(code_dom = c("971", "972", "973", "974", "976"),
            destination = list(c(-5000, 7100000),
                               c(-5000, 6875000),
                               c(-5000, 6650000),
                               c(-5000, 6425000),
                               c(-5000, 6200000)
            ),
            scale = c(2.2, 2.2, 0.3, 1, 3),
            angle = c(-50, -50, -45, 30, 30))

l <- pmap(arg, translate_drom)
dom_geo <- rbind(l[[1]], l[[2]], l[[3]], l[[4]], l[[5]])

# mapview::mapview(dom_geo$geometry)

## Assemblage com DOM et métro + simplification du contour
communes_geo_00 <- rbind(com_metro, dom_geo) %>%
  as_tibble %>%
  select(DEPCOM = code_insee, geometry) %>%
  st_as_sf() %>%
  st_make_valid()

gc() ; options(timeout = 60) ; rm(com_fce_ent0)  # ménage
save.image(".RData")

communes_geo_0 <- ms_simplify(communes_geo_00, keep = 0.02, keep_shapes = TRUE, weighting = 0.8, sys = TRUE, sys_mem = 10) %>% # installation de mapshaper sur PC nécessaire
  st_set_crs(2154)

object.size(communes_geo_0)
communes_geo_0 %>% filter(grepl("49...", DEPCOM)) %>%  mapview::mapview(alpha.region = 0.5)
save(communes_geo_0, file="data-raw/source/communes_geo_0_JU.RData" )
rm(com_metro, dom_geo, arg, l, translate_drom, communes_geo_00)
gc()

communes_geo <- communes_geo_0 %>%
  inner_join(st_drop_geometry(com_fce_ent), by = join_by("DEPCOM" == "code_insee")) %>%
  mutate(AREA = (10000 * as.double(superficie_cadastrale))) %>%
  select(DEPCOM, AREA)
rm(communes_geo_0)

communes_metro_geo <- communes_geo %>%
  filter(!grepl("97...", DEPCOM))

## EPCI
epci_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
  inner_join(communes_geo, ., by="DEPCOM") %>%
  select(EPCI, AREA) %>%
  group_by(EPCI) %>%
  summarise(AREA = sum(AREA), do_union = TRUE, .groups = "drop") %>%
  mutate(AREA = set_units(AREA, "m^2"))

epci_metro_geo <- communes_info_supra %>%
  filter(!grepl("97...", DEPCOM), NOM_EPCI != "Sans objet") %>%
  select(EPCI) %>%
  distinct() %>%
  right_join(epci_geo, ., by = "EPCI")

## Départements
departements_geo <- inner_join(communes_geo, communes_info_supra, by = "DEPCOM") %>%
  select(DEP, AREA) %>%
  group_by(DEP) %>%
  summarise(AREA = sum(AREA), do_union = TRUE, .groups = "drop") %>%
  mutate(AREA = set_units(AREA, "m^2"))

departements_metro_geo <- departements_geo %>%
  filter(!grepl("97.", DEP))

## Régions
regions_geo <- inner_join(communes_geo, communes_info_supra, by = "DEPCOM") %>%
  select(REG, AREA) %>%
  group_by(REG) %>%
  summarise(AREA = sum(AREA), do_union = TRUE, .groups = "drop") %>%
  st_buffer(0) %>%
  mutate(AREA = set_units(AREA, "m^2"))

regions_metro_geo <- regions_geo %>%
  filter(!grepl("0.", REG))

## Ajout de l'unite dans communes_geo
communes_geo <- communes_geo %>%
  mutate(AREA = set_units(AREA, "m^2"))

# DOM : des jeux de données spé qui respectent le CRS et plus détaillé-----------------
## Communes DOM---
com_geo_dom <- function(dep = "971", epsg = 5490) {
  com <- com_fce_ent %>%
    filter(code_insee_du_departement == dep) %>%
    select(DEPCOM = code_insee, superficie_cadastrale) %>%
    st_transform(epsg) %>%
    ms_simplify(keep = 0.05, keep_shapes = FALSE, weighting = 0.9) %>%
    mutate(AREA = (10000 * as.double(superficie_cadastrale))) %>%
    select(DEPCOM, AREA)
  # gestion de l'encodage des chaines wkt
  st_crs(com)$wkt <- gsub("°|º", "\\\u00b0", st_crs(com)$wkt)
  return(com)
}

communes_971_geo <- com_geo_dom(dep = "971", epsg = 5490)

communes_972_geo <- com_geo_dom(dep = "972", epsg = 5490)

communes_973_geo <- com_geo_dom(dep = "973", epsg = 2972)

communes_974_geo <- com_geo_dom(dep = "974", epsg = 2975)

communes_976_geo <- com_geo_dom(dep = "976", epsg = 4471)

rm(com_geo_dom)

## Epci DOM --------------------

epci_geo_dom <- function(com_geo = communes_971_geo) {
  epci <- filter(communes_info_supra, NOM_EPCI != "Sans objet")%>%
    inner_join(com_geo, ., by = "DEPCOM") %>%
    select(EPCI, AREA) %>%
    group_by(EPCI) %>%
    summarise(AREA = sum(AREA), do_union = TRUE, .groups = "drop")
  # gestion de l'encodage des chaines wkt
  st_crs(epci)$wkt <- gsub("°|º", "\\\u00b0", st_crs(epci)$wkt)
  return(epci)
}

epci_971_geo <- epci_geo_dom(communes_971_geo)

epci_972_geo <- epci_geo_dom(communes_972_geo)

epci_973_geo <- epci_geo_dom(communes_973_geo)

epci_974_geo <- epci_geo_dom(communes_974_geo)

epci_976_geo <- epci_geo_dom(communes_976_geo)

## Départements DOM -----------------

departements_971_geo <- communes_971_geo %>%
  summarise(DEP = "971", AREA = sum(AREA), do_union = TRUE, .groups = "drop")

departements_972_geo <- communes_972_geo %>%
  summarise(DEP = "972", AREA = sum(AREA), do_union = TRUE, .groups = "drop")

departements_973_geo <- communes_973_geo %>%
  summarise(DEP = "973", AREA = sum(AREA), do_union = TRUE, .groups = "drop")

departements_974_geo <- communes_974_geo %>%
  summarise(DEP = "974", AREA = sum(AREA), do_union = TRUE, .groups = "drop")

departements_976_geo <- communes_976_geo %>%
  summarise(DEP = "976", AREA = sum(AREA), do_union = TRUE, .groups = "drop")


## Régions DOM -----------------

reg_dom_geo <- function(dom = "971"){
  dep_reg <- select(communes_info_supra, DEP, REG) %>%
    distinct
  depgeo <- get(paste0("departements_", dom, "_geo"))
  depgeo %>%
    left_join(dep_reg, by = "DEP") %>%
    select(REG, everything(), -DEP) %>%
    st_buffer(0.0000000001)
}

regions_971_geo <- reg_dom_geo("971")

regions_972_geo <- reg_dom_geo("972")

regions_973_geo <- reg_dom_geo("973")

regions_974_geo <- reg_dom_geo("974")

regions_976_geo <- reg_dom_geo("976")

# sauvegarde des données --------------------------------------------------------
use_data(communes_geo, internal = FALSE, overwrite = TRUE)
use_data(communes_metro_geo, internal = FALSE, overwrite = TRUE)
use_data(communes_971_geo, internal = FALSE, overwrite = TRUE)
use_data(communes_972_geo, internal = FALSE, overwrite = TRUE)
use_data(communes_973_geo, internal = FALSE, overwrite = TRUE)
use_data(communes_974_geo, internal = FALSE, overwrite = TRUE)
use_data(communes_976_geo, internal = FALSE, overwrite = TRUE)
use_data(epci_geo, internal = FALSE, overwrite = TRUE)
use_data(epci_metro_geo, internal = FALSE, overwrite = TRUE)
use_data(epci_971_geo, internal = FALSE, overwrite = TRUE)
use_data(epci_972_geo, internal = FALSE, overwrite = TRUE)
use_data(epci_973_geo, internal = FALSE, overwrite = TRUE)
use_data(epci_974_geo, internal = FALSE, overwrite = TRUE)
use_data(epci_976_geo, internal = FALSE, overwrite = TRUE)
use_data(departements_geo, internal = FALSE, overwrite = TRUE)
use_data(departements_metro_geo, internal = FALSE, overwrite = TRUE)
use_data(departements_971_geo, internal = FALSE, overwrite = TRUE)
use_data(departements_972_geo, internal = FALSE, overwrite = TRUE)
use_data(departements_973_geo, internal = FALSE, overwrite = TRUE)
use_data(departements_974_geo, internal = FALSE, overwrite = TRUE)
use_data(departements_976_geo, internal = FALSE, overwrite = TRUE)
use_data(regions_geo, internal = FALSE, overwrite = TRUE)
use_data(regions_metro_geo, internal = FALSE, overwrite = TRUE)
use_data(regions_971_geo, internal = FALSE, overwrite = TRUE)
use_data(regions_972_geo, internal = FALSE, overwrite = TRUE)
use_data(regions_973_geo, internal = FALSE, overwrite = TRUE)
use_data(regions_974_geo, internal = FALSE, overwrite = TRUE)
use_data(regions_976_geo, internal = FALSE, overwrite = TRUE)

rm(origine_metro, millesime, reg_dom_geo, epci_geo_dom, com_fce_ent,
   table_passage_com_historique, communes_info_supra)
