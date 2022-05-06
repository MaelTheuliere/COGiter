library(sf)
library(usethis)
library(tidyverse)
library(archive)
library(units)
repo_mil_bdcarto <- paste0("data-raw/source/", 2022, "/bd_carto")
dir.create(repo_mil_bdcarto, recursive = TRUE)

# (télé)chargement BD Carto régionales -------------------------------


df_url <- tibble::tribble(
  ~REG, ~NOM_REG, ~CRS, ~EPSG, ~URL, ~DESTFILE,
  '11', 'Île-de-France', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R11_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R11_2021-05-10.7z',
  '24', 'Centre-Val de Loire', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R24_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R24_2021-05-10.7z',
  '27', 'Bourgogne-Franche-Comté', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R27_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R27_2021-05-10.7z',
  '28', 'Normandie', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R28_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R28_2021-05-10.7z',
  '32', 'Hauts-de-France', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R32_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R32_2021-05-10.7z',
  '44', 'Grand Est', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R44_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R44_2021-05-10.7z',
  '52', 'Pays de la Loire', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R52_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R52_2021-05-10.7z',
  '53', 'Bretagne', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R53_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R53_2021-05-10.7z',
  '75', 'Nouvelle-Aquitaine', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R75_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R75_2021-05-10.7z',
  '76', 'Occitanie', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R76_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R76_2021-05-10.7z',
  '84', 'Auvergne-Rhône-Alpes', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R84_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R84_2021-05-10.7z',
  '93', 'Provence-Alpes-Côte d’Azur', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R93_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R93_2021-05-10.7z',
  '94', 'Corse', 'LAMB93', 2154, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R94_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R94_2021-05-10.7z',
  '01', 'Guadeloupe', 'RGAF09UTM20', 5490, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_RGAF09UTM20_R01_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_RGAF09UTM20_R01_2021-05-10.7z',
  '02', 'Martinique', 'RGAF09UTM20', 5490, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_RGAF09UTM20_R02_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_RGAF09UTM20_R02_2021-05-10.7z',
  '03', 'Guyane', 'UTM22RGFG95', 2972, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_UTM22RGFG95_R03_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_UTM22RGFG95_R03_2021-05-10.7z',
  '04', 'La Réunion', 'RGR92UTM40S', 2975, 'ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_RGR92UTM40S_R04_2021-05-10.7z', 'BDCARTO_4-0_TOUSTHEMES_SHP_RGR92UTM40S_R04_2021-05-10.7z' ) %>%
  arrange(REG)


get_superficie_bd_carto <- function(index = 1){

  # téléchargement de la bd carto reg
  download.file(url = df_url$URL[index], destfile = paste0(repo_mil_bdcarto, "/", df_url$DESTFILE[index]))

  # decouverte du contenu du zip
  contenu_list <- archive(paste0(repo_mil_bdcarto, "/", df_url$DESTFILE[index]))

  # sélection des ardesses de fichiers à extraire et lire
  path_fic_extr <- filter(contenu_list, grepl("COMMUNE.|DEPARTEMENT.|REGION.", path)) %>% pull(path)
  path_com <- filter(contenu_list, grepl("COMMUNE.shp", path)) %>% pull(path)

  # dézippage
  archive_extract(archive = paste0(repo_mil_bdcarto, "/", df_url$DESTFILE[index]), d = repo_mil_bdcarto, files = path_fic_extr)

  # lecture de la couche communes, la seule à contenir un attribut de surface
  com_surf <- st_read(paste0(repo_mil_bdcarto, "/", path_com)) %>%
    st_drop_geometry() %>%
    select(INSEE_COM, SUPERFICIE)

  # nettoyage du repertoire : suppression des fichiers téléchargés
  file.remove(paste0(repo_mil_bdcarto, "/", df_url$DESTFILE[index]))
  file.remove(paste0(repo_mil_bdcarto, "/", path_fic_extr))
  file.remove(list.files("~/bes_pdl_2022/data-raw/source/2022/bd_carto/", include.dirs = TRUE, full.names = TRUE, recursive = TRUE) %>% rev())

  # # export de la table des surfaces communales de la région
  # save(com_surf, file = paste0(repo_mil_bdcarto, "/", df_url$NOM_REG[index], ".RData"))

  return(com_surf)
}

superf_communes <- map_dfr(.x = 1:nrow(df_url), .f = get_superficie_bd_carto) %>%
  mutate(superf_communes, SUPERFICIE = set_units(SUPERFICIE, "m^2")*10000)


# Ajout des surfaces des com de mayotte qui n'ont pas de bd carto
# telechargement et dézippage version admin express la plus precise pour Mayotte
com_mayotte <- st_read(paste0(repo_mil_bdcarto, "/mayotte/COMMUNE.shp")) %>%
  mutate(SUPERFICIE = st_area(.)) %>%
  st_drop_geometry() %>%
  select(INSEE_COM, SUPERFICIE)

# Assemblage
superf_communes <- bind_rows(superf_communes, com_mayotte)

save(superf_communes, file = paste0(repo_mil_bdcarto, "/superf_communes.RData"))
rm(list = setdiff(ls(), "superf_communes"))
