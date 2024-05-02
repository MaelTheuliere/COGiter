library(sf)
library(tidyverse)
library(archive)
library(units)

millesime <- "2024"

## Téléchargement de toutes les couches admin express en projections légales pour mesures des surfaces
# https://geoservices.ign.fr/telechargement-api/ADMIN-EXPRESS
repos = list(
  com_metro = "data-raw/source/2024/adminexpress/surf/ADMIN-EXPRESS_3-2__SHP_LAMB93_FXX_2024-03-25",
  com_guada = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_RGAF09UTM20_GLP_2024-03-25",
  com_martnq = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_RGAF09UTM20_MTQ_2024-03-25",
  com_mayotte = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_RGM04UTM38S_MYT_2024-03-25",
  com_reunion = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_RGR92UTM40S_REU_2024-03-25",
  com_guyane = "data-raw/source/2024/adminexpress/surf//ADMIN-EXPRESS_3-2__SHP_UTM22RGFG95_GUF_2024-03-25"
)


## lecture, mesure et combinaison
lire_shp_com <- function(repo = repos$com_guada) {
  st_read(paste0(repo,"/", "COMMUNE.shp")) %>%
    mutate(surface_m2 = st_area(geometry)) %>%
    st_drop_geometry() %>%
    select(DEPCOM = INSEE_COM, surface_m2)
}

surf_com <- map_dfr(.x = repos, .f = lire_shp_com) %>%
  arrange(DEPCOM)
save(surf_com, file = paste0("data-raw/source/", millesime, "/adminexpress/superf_communes.RData"))


