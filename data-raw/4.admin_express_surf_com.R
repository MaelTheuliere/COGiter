library(sf)
library(tidyverse)
library(archive)
library(units)
load("data/communes.rda")
load("data/table_passage_com_historique.rda")

millesime <- "2025"
dir.create(paste0("data-raw/source/", millesime, "/adminexpress"), showWarnings = F)

## recomposition à partir du millesime pdt
surf_com <- COGiter::communes_geo %>%
  st_drop_geometry() %>%
  # ajout des communes ressucitées
  full_join(select(communes, DEPCOM), by = "DEPCOM") %>%
  left_join(table_passage_com_historique, by = c("DEPCOM" = "DEPCOM_HIST")) %>%
  group_by(DEPCOM.y) %>%
  summarise(AREA = sum(drop_units(AREA))) %>%
  mutate(set_units(AREA, "m^2")) %>%
  select(DEPCOM = DEPCOM.y, AREA)


save(surf_com, file = paste0("data-raw/source/", millesime, "/adminexpress/superf_communes.RData"))



# # méthode 2024
# ## Téléchargement de toutes les couches admin express en projections légales pour mesures des surfaces
# # https://geoservices.ign.fr/telechargement-api/ADMIN-EXPRESS
# repos = list(
#   com_metro = "data-raw/source/2024/adminexpress/surf/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25",
#   com_guada = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_RGAF09UTM20_GLP_2024-03-25",
#   com_martnq = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_RGAF09UTM20_MTQ_2024-03-25",
#   com_mayotte = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_RGM04UTM38S_MYT_2024-03-25",
#   com_reunion = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_RGR92UTM40S_REU_2024-03-25",
#   com_guyane = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_UTM22RGFG95_GUF_2024-03-25"
# )
#
#
# ## lecture, mesure et combinaison
# lire_shp_com <- function(repo = repos$com_guada) {
#   st_read(paste0(repo,"/", "COMMUNE.shp")) %>%
#     mutate(surface_m2 = st_area(geometry)) %>%
#     st_drop_geometry() %>%
#     select(DEPCOM = INSEE_COM, surface_m2)
# }
#
# surf_com <- map_dfr(.x = repos, .f = lire_shp_com) %>%
#   arrange(DEPCOM)
# save(surf_com, file = paste0("data-raw/source/", millesime, "/adminexpress/superf_communes.RData"))



# Methode courant 2025 depuis flux WFS IGN
# load("data/departements.rda")
# depsurf<<-data.frame()
# purrr::map(departements$DEP,function(x){
#   depplus <-  readr::read_csv(paste0("
# https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAME=BDCARTO_V5:commune&PROPERTYNAME=BDCARTO_V5:code_insee,BDCARTO_V5:nom_officiel,BDCARTO_V5:surface_en_ha&OUTPUTFORMAT=csv&FILTER=%3CFilter%3E%3CPropertyIsEqualTo%3E%3CValueReference%3EBDCARTO_V5_V3:code_insee_du_departement%3C/ValueReference%3E%3CLiteral%3E%22,x,%22%3C/Literal%3E%3C/PropertyIsEqualTo%3E%3C/Filter%3E
# "),show_col_types = FALSE, col_types = "cccc")
#   depplus %>% mutate(code_insee = as.character(code_insee))
#   depsurf <<- depsurf %>% dplyr::bind_rows(depplus)
# })
# superf_communes <- depsurf %>%
#   mutate(DEPCOM = code_insee, AREA = (10000*as.double(surface_en_ha))) %>%
#   select(DEPCOM,AREA)
