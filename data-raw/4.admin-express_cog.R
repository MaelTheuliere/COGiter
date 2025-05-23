library(sf)
library(rmapshaper)
library(mapview)
library(usethis)
library(tidyverse)
library(archive)
library(units)
load("data/communes_info_supra.rda")
load("data/table_passage_com_historique.rda")
millesime <- "2025"


# (télé)chargement Admin Express -------------------------------
repo_mil <- paste0("data-raw/source/", millesime, "/adminexpress")
repo_dest <- "/ADMIN-EXPRESS-COG-CARTO_3-2__SHP_WGS84G_FRA_2025-04-02"

## téléchargement des couches IGN admin express COG carto ----
## Chargements des données présentes sur le site IGN :https://geoservices.ign.fr/adminexpress (10min hors RIE)
# download.file(paste0("https://data.geopf.fr/telechargement/download/ADMIN-EXPRESS-COG-CARTO/ADMIN-EXPRESS-COG-CARTO_3-2__SHP_WGS84G_FRA_2025-04-02/ADMIN-EXPRESS-COG-CARTO_3-2__SHP_WGS84G_FRA_2025-04-02.7z"),
              # destfile = paste0(repo_mil, repo_dest, ".7z"), method = "curl")

## lecture du zip et dezippage
contenu_list <- archive(paste0(repo_mil, repo_dest, ".7z"))
path_com <- filter(contenu_list, grepl("/COMMUNE.", path, fixed = TRUE)) %>%
  arrange(desc(size)) %>% pull(path)

archive_extract(archive = paste0(repo_mil, repo_dest, ".7z"),
                dir = repo_mil, file = path_com)

com_fce_ent <- st_read(paste0(repo_mil,"/", path_com[1]))

## ménage
list.dirs(paste0(repo_mil, repo_dest)) %>%
  unlink(., recursive = TRUE, force = TRUE)

# Assemblage des couches communales métropole + DOM ---------

## sélection com métropole
com_metro <- filter(com_fce_ent, INSEE_REG >= "10") %>%
  st_transform(2154)

origine_metro <- c(st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1, 1]], st_as_sfc(st_bbox(com_metro))[[1]][[1]][[1, 2]])

## travail sur les DOM : translation + mise à l'échelle + changement EPSG
# arguments à passer pour chaque doms : code, centroid de destination, echelle, angle)

translate_drom <- function(code_dom, destination, scale = 1, angle = 0, epsg = 2154) {

  com_dom <- filter(com_fce_ent, INSEE_DEP == code_dom) %>%
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
  select(DEPCOM = INSEE_COM, geometry) %>%
  st_as_sf() %>%
  st_make_valid()

gc()
save.image(".RData")

communes_geo_0 <- ms_simplify(communes_geo_00, keep = 0.03, keep_shapes = TRUE, weighting = 0.8) %>%  #, sys = TRUE, sys_mem = 10) # installation de mapshaper sur PC nécessaire
  st_set_crs(2154)

object.size(communes_geo_0)
communes_geo_0 %>% filter(grepl("49...", DEPCOM)) %>%  mapview::mapview(alpha.region = 0.5)
save(communes_geo_0, file="data-raw/source/communes_geo_0.RData" )
rm(com_metro, dom_geo, arg, l, translate_drom, communes_geo_00)
gc()

# Constitution des datasets geo du package incluant la surface du territoire--------------

## Communes

# chargement des surfaces communales issues de la bd carto 2025 - attention ref communes COG 2024
# en csv et par département (limite de l'api IGN wfs 5000 éléments)
gep_surf_com_dptmt <- function(dept = "15") {
  message("Interrogation sur le departement ", dept)
  readr::read_csv(paste0("https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAME=BDCARTO_V5:commune&PROPERTYNAME=BDCARTO_V5:code_insee,BDCARTO_V5:nom_officiel,BDCARTO_V5:surface_en_ha&OUTPUTFORMAT=csv&FILTER=%3CFilter%3E%3CPropertyIsEqualTo%3E%3CValueReference%3EBDCARTO_V5_V3:code_insee_du_departement%3C/ValueReference%3E%3CLiteral%3E", dept,"%3C/Literal%3E%3C/PropertyIsEqualTo%3E%3C/Filter%3E"), show_col_types = FALSE, col_types = "cccc") %>%
    mutate(code_insee = as.character(code_insee))
}
load("data/departements.rda")
depsurf <- purrr::map_dfr(departements$DEP, gep_surf_com_dptmt) %>%
  mutate(DEPCOMB = code_insee, AREA = (10000 * as.double(surface_en_ha))) %>%
  select(DEPCOMB, AREA)

# si millésime précédent, agrégation sur communes fusionnées et ajout des communes issues de scission
load("data/communes.rda")
superf_communes <- depsurf %>%
  left_join(table_passage_com_historique, by = join_by(DEPCOMB == DEPCOM_HIST)) %>%
  summarise(AREA = sum(AREA), .by = DEPCOM) %>%
  right_join(communes) %>%
  select(DEPCOM, AREA) %>%
  mutate(AREA = set_units(AREA, "m^2"))

#gestion des 4 communes du cantal après scission (fusion fin 2016, on récupère la bdtopo 2016 sur le département 15)
#https://geoservices.ign.fr/bdtopo#telechargement2016
com2016 <- read_sf("data-raw/source/2016/bdtopo/015/COMMUNE.shp")
les4enscission<-com2016 %>% filter(CODE_INSEE %in% c("15031","15035","15047","15171"))
les4enscission<-les4enscission %>% mutate(DEPCOM = CODE_INSEE, AREA = st_area(les4enscission)) %>% select(DEPCOM, AREA) %>%
           st_drop_geometry()
superf_communes <- superf_communes %>%
  left_join(les4enscission, by = join_by(DEPCOM == DEPCOM))  %>%
  mutate(AREA = ifelse(is.na(AREA.x),AREA.y,AREA.x)) %>% select(DEPCOM, AREA)%>%
  mutate(AREA = set_units(AREA, "m^2"))
# tests
nrow(superf_communes) == nrow(communes_info_supra)
communes_geo <- communes_geo_0 %>%
  left_join(superf_communes, by = c("DEPCOM")) %>%
  select(DEPCOM, AREA)

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

# DOM : des jeux de données spé qui respecte le CRS et plus détaillé-----------------
## Communes DOM---

com_geo_dom <- function(dep = "971", epsg = 5490) {
  com <- com_fce_ent %>%
    filter(INSEE_DEP == dep) %>%
    select(DEPCOM = INSEE_COM) %>%
    st_transform(epsg) %>%
    ms_simplify(keep = 0.05, keep_shapes = FALSE, weighting = 0.9) %>%
    inner_join(superf_communes, by = "DEPCOM")%>%
    relocate(AREA, .after = DEPCOM)
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

rm(origine_metro, millesime, communes_geo_0, reg_dom_geo, epci_geo_dom, com_fce_ent, superf_communes, contenu_list, table_passage_com_historique,
   communes_info_supra, path_com, repo_dest, repo_mil)
