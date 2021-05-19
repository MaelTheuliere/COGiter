
# datapréparation de la table de passage des communes vers les zonages supra de l'insee

library(readxl)
library(purrr)
library(dplyr)

# lecture des données avec les codes des zonages
table_passage_com_zonages <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", skip = 5)

# lecture des données avec les libellés des zonages onglet Zones_supra_communales  --------

libelle_cv <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "CV") %>%
  select( CV = CODGEO,LIB_CV = LIBGEO)

libelle_arr <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "ARR") %>%
  select(ARR = CODGEO,LIB_ARR = LIBGEO)

libelle_ze2020 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "ZE2020") %>%
  select( ZE2020 = CODGEO,LIB_ZE2020 = LIBGEO)

libelle_uu2020 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "UU2020") %>%
  select( UU2020 = CODGEO,LIB_UU2020 = LIBGEO)

libelle_bv2012 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "BV2012") %>%
  select( BV2012 = CODGEO,LIB_BV2012 = LIBGEO)

libelle_aav2020 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "AAV2020") %>%
  select(AAV2020 = CODGEO,LIB_AAV2020 = LIBGEO)

# lecture des données avec les libellés des zonages onglet documentation  --------

libelle_tdaav2017 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", range = "A16:A21", sheet = "Documentation", col_names = FALSE, .name_repair = ~c("x")) %>%
  mutate(TDAAV2017 = stringr::str_split_fixed(x,n=2," - ")[,1],
         LIB_TDAAV2017 = stringr::str_split_fixed(x,n=2," - ")[,2]
  ) %>%
  select(TDAAV2017, LIB_TDAAV2017)

libelle_tdaav2017 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", range = "A16:A21", sheet = "Documentation", col_names = FALSE, .name_repair = ~c("x")) %>%
  mutate(TAAV2017 = stringr::str_split_fixed(x,n=2," - ")[,1],
         LIB_TAAV2017 = stringr::str_split_fixed(x,n=2," - ")[,2]
  ) %>%
  select(TAAV2017, LIB_TAAV2017)

libelle_tdaav2017 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", range = "A26:A42", sheet = "Documentation", col_names = FALSE, .name_repair = ~c("x")) %>%
  mutate(TDAAV2017 = stringr::str_split_fixed(x,n=2," - ")[,1],
         LIB_TDAAV2017 = stringr::str_split_fixed(x,n=2," - ")[,2]
  ) %>%
  select(TDAAV2017, LIB_TDAAV2017)

libelle_tdaav2017 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", range = "A26:A42", sheet = "Documentation", col_names = FALSE, .name_repair = ~c("x")) %>%
  mutate(TDAAV2017 = stringr::str_split_fixed(x,n=2," - ")[,1],
         LIB_TDAAV2017 = stringr::str_split_fixed(x,n=2," - ")[,2]
  ) %>%
  select(TDAAV2017, LIB_TDAAV2017)

libelle_cateaav2020 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", range = "A47:A51", sheet = "Documentation", col_names = FALSE, .name_repair = ~c("x")) %>%
  mutate(CATEAAV2020 = stringr::str_split_fixed(x,n=2," - ")[,1],
         LIB_CATEAAV2020 = stringr::str_split_fixed(x,n=2," - ")[,2]
  ) %>%
  select(CATEAAV2020, LIB_CATEAAV2020)

libelle_tuu2017 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", range = "A56:A64", sheet = "Documentation", col_names = FALSE, .name_repair = ~c("x")) %>%
  mutate(TUU2017 = stringr::str_split_fixed(x,n=2," ")[,1],
         LIB_TUU2017 = stringr::str_split_fixed(x,n=2," ")[,2]
  ) %>%
  select(TUU2017, LIB_TUU2017)

libelle_tduu2017 <- read_excel("data-raw/source/2021/COG/table-appartenance-geo-communes-21.xlsx", range = "A72:A92", sheet = "Documentation", col_names = FALSE, .name_repair = ~c("x")) %>%
  mutate(TDUU2017 = stringr::str_split_fixed(x,n=2," ")[,1],
         LIB_TDUU2017 = stringr::str_split_fixed(x,n=2," ")[,2]
  ) %>%
  select(TDUU2017, LIB_TDUU2017)

# intégration des libellés à la table de passage -------------

table_passage_communes_zonages <- list(table_passage_com_zonages,
             libelle_aav2020,
             libelle_arr,
             libelle_bv2012,
             libelle_cateaav2020,
             libelle_cv,
             libelle_tdaav2017,
             libelle_tduu2017,
             libelle_tuu2017,
             libelle_uu2020,
             libelle_ze2020) %>%
  reduce(inner_join) %>%
  mutate(across(where(is.character),as.factor)) %>%
  rename(DEPCOM = CODGEO, NOM_DEPCOM = LIBGEO) %>%
  select(-NOM_DEPCOM,-DEP,-REG,-EPCI,-NATURE_EPCI)

nrow(table_passage_communes_zonages) == nrow(table_passage_com_zonages)

use_data(table_passage_communes_zonages, overwrite = TRUE)
utilitaires.ju::use_data_doc("table_passage_communes_zonages", description = "Table de passage des communes vers des zonages supra", source = "Insee : https://www.insee.fr/fr/information/2028028")


