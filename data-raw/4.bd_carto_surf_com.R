library(sf)
library(tidyverse)
library(units)
library(datalibaba)

millesime <- "2023"
repo_mil_bdcarto <- paste0("data-raw/source/", millesime, "/bd_carto")
dir.create(repo_mil_bdcarto, recursive = TRUE)

# (télé)chargement BD CARTO® SQL - France Entière - Septembre 2022 https://geoservices.ign.fr/ressource/217904  -------------------------------
# - import du fichier commune.sql du dossier ADMINISTRATIF commune dans postgis local
#

superf_communes <- importer_data(table = "commune", schema = "public", db = "public", server = "localhost", user = "does") %>%
    st_drop_geometry() %>%
  select(INSEE_COM = code_insee, surface_en_ha) %>%
  mutate(SUPERFICIE = set_units(surface_en_ha, "m^2")*10000) %>%
  select(INSEE_COM, SUPERFICIE)

save(superf_communes, file = paste0(repo_mil_bdcarto, "/superf_communes.RData"))
rm(repo_mil_bdcarto)
