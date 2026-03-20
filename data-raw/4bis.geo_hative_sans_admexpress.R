# Script qui shunte le 4e script 4.admin-express_cog.R pour livrer une 1ere mise à jour hâtive de COGiter
# si pas de scission de commune en début d'année N+1, on peut reconstituer les contours N+1 à partir des contours de l'année N.

library(sf)
library(rmapshaper)
library(mapview)
library(usethis)
library(tidyverse)
library(archive)
library(units)
load("data/communes_info_supra.rda")
load("data/table_passage_com_historique.rda") # nouvelle version de la table de passage com / com hist péprarée dans le 2e script
millesime <- "2026"

# mise à jour du contour des communes par agrégation des contours de l'année précédente
communes_geo <- COGiter::communes_geo %>% # version mil précédent
  rename(DEPCOM_HIST = DEPCOM) %>%
  left_join(table_passage_com_historique, by = "DEPCOM_HIST") %>%
  select(-DEPCOM_HIST) %>%
  relocate(DEPCOM, .before = everything()) %>%
  group_by(DEPCOM) %>%
  summarise(AREA = sum(AREA), do_union = TRUE, .groups = "drop") %>%
  st_buffer(0)

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

communes_971_geo <- COGiter::communes_971_geo
communes_972_geo <- COGiter::communes_972_geo
communes_973_geo <- COGiter::communes_973_geo
communes_974_geo <- COGiter::communes_974_geo
communes_976_geo <- COGiter::communes_976_geo


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

rm(origine_metro, millesime, reg_dom_geo, epci_geo_dom, com_fce_ent, superf_communes, contenu_list,
   table_passage_com_historique, communes_info_supra, path_com, repo_dest, repo_mil)
