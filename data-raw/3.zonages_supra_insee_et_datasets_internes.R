
# datapréparation de la table de passage des communes vers les zonages supra de l'insee

millesime <- "2024"
repo_mil <- paste0("data-raw/source/", millesime, "/COG")
fich_mil <- paste0("table-appartenance-geo-communes-", millesime)
path_fic_xls <- paste0(repo_mil, "/", fich_mil, ".xlsx")

library(readxl)
library(purrr)
library(dplyr)
library(forcats)

fact.enc.utf8 <- function(a) {
  a <- as.factor(a) %>%
    fct_relabel(.fun = enc2utf8)
}

# intégration de la table d'appartenance (https://www.insee.fr/fr/information/7671844, publié vers la mi mars en 2023)
download.file(paste0("https://www.insee.fr/fr/statistiques/fichier/7671844/", fich_mil, ".zip"),
              destfile = paste0(repo_mil, "/", fich_mil, ".zip"))

unzip(zipfile = paste0(repo_mil, "/", fich_mil, ".zip"),
      exdir = repo_mil)

table_passage_com_zonages <- read_excel(path_fic_xls, skip = 5) %>%
  mutate(across(everything(), fact.enc.utf8))


# lecture des libellés des zonages onglet Zones_supra_communales  --------

libelle_cv <- read_excel(path_fic_xls, skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "CANOV") %>%
  select(CANOV = CODGEO, LIB_CV = LIBGEO)

libelle_arr <- read_excel(path_fic_xls, skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "ARR") %>%
  select(ARR = CODGEO, LIB_ARR = LIBGEO)

libelle_ze2020 <- read_excel(path_fic_xls, skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "ZE2020") %>%
  select(ZE2020 = CODGEO, LIB_ZE2020 = LIBGEO)

libelle_uu2020 <- read_excel(path_fic_xls, skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "UU2020") %>%
  select(UU2020 = CODGEO, LIB_UU2020 = LIBGEO)

libelle_bv2022 <- read_excel(path_fic_xls, skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "BV2022") %>%
  select(BV2022 = CODGEO, LIB_BV2022 = LIBGEO)

libelle_aav2020 <- read_excel(path_fic_xls, skip = 5, sheet = "Zones_supra_communales") %>%
  filter(NIVGEO == "AAV2020") %>%
  select(AAV2020 = CODGEO, LIB_AAV2020 = LIBGEO)

# lecture des libellés des zonages onglet documentation  --------

libelle_taav2017 <- read_excel(path_fic_xls, range = "A16:A21", sheet = "Documentation", col_names = FALSE, .name_repair = ~ c("x")) %>%
  mutate(TAAV2017 = stringr::str_split_fixed(x, n = 2, " - ")[, 1],
    LIB_TAAV2017 = stringr::str_split_fixed(x, n = 2, " - ")[, 2]
  ) %>%
  select(TAAV2017, LIB_TAAV2017)


libelle_tdaav2017 <- read_excel(path_fic_xls, range = "A26:A42", sheet = "Documentation", col_names = FALSE, .name_repair = ~ c("x")) %>%
  mutate(TDAAV2017 = stringr::str_split_fixed(x, n = 2, " - ")[, 1],
    LIB_TDAAV2017 = stringr::str_split_fixed(x, n = 2, " - ")[, 2]
  ) %>%
  select(TDAAV2017, LIB_TDAAV2017)

libelle_cateaav2020 <- read_excel(path_fic_xls, range = "A47:A51", sheet = "Documentation", col_names = FALSE, .name_repair = ~ c("x")) %>%
  mutate(CATEAAV2020 = stringr::str_split_fixed(x, n = 2, " - ")[, 1],
    LIB_CATEAAV2020 = stringr::str_split_fixed(x, n = 2, " - ")[, 2]
  ) %>%
  select(CATEAAV2020, LIB_CATEAAV2020)

libelle_tuu2017 <- read_excel(path_fic_xls, range = "A56:A64", sheet = "Documentation", col_names = FALSE, .name_repair = ~ c("x")) %>%
  mutate(TUU2017 = stringr::str_split_fixed(x, n = 2, " ")[, 1],
    LIB_TUU2017 = stringr::str_split_fixed(x, n = 2, " ")[, 2]
  ) %>%
  select(TUU2017, LIB_TUU2017)

libelle_tduu2017 <- read_excel(path_fic_xls, range = "A72:A92", sheet = "Documentation", col_names = FALSE, .name_repair = ~ c("x")) %>%
  mutate(TDUU2017 = stringr::str_split_fixed(x, n = 2, " ")[, 1],
    LIB_TDUU2017 = stringr::str_split_fixed(x, n = 2, " ")[, 2]
  ) %>%
  select(TDUU2017, LIB_TDUU2017)

# TYPUU2020
libelle_typuu2020 <- read_excel(path_fic_xls, range = "A95:A100", sheet = "Documentation", col_names = FALSE, .name_repair = ~ c("x")) %>%
  mutate(TYPUU2020 = stringr::str_split_fixed(x, n = 2, " ")[, 1],
         LIB_TYPUU2020 = stringr::str_split_fixed(x, n = 2, " ")[, 2]
  ) %>%
  select(TYPUU2020, LIB_TYPUU2020)

# COMUU2020
libelle_comuu2020 <- read_excel(path_fic_xls, range = "A105:A108", sheet = "Documentation", col_names = FALSE, .name_repair = ~ c("x")) %>%
  mutate(COMUU2020 = stringr::str_split_fixed(x, n = 2, " ")[, 1],
         LIB_COMUU2020 = stringr::str_split_fixed(x, n = 2, " ")[, 2]
  ) %>%
  select(COMUU2020, LIB_COMUU2020)


# intégration des libellés à la table de passage -------------

table_passage_communes_zonages <- list(table_passage_com_zonages,
                                       libelle_aav2020,
                                       libelle_arr,
                                       libelle_bv2022,
                                       libelle_cateaav2020,
                                       libelle_cv,
                                       libelle_taav2017,
                                       libelle_tdaav2017,
                                       libelle_uu2020,
                                       libelle_typuu2020,
                                       libelle_comuu2020,
                                       libelle_tduu2017,
                                       libelle_tuu2017,
                                       libelle_ze2020) %>%
  reduce(inner_join) %>%
  mutate(across(where(is.character), as.factor)) %>%
  rename(DEPCOM = CODGEO) %>%
  select(DEPCOM, ARR, LIB_ARR, CV = CANOV, LIB_CV, ZE2020, LIB_ZE2020, UU2020,  LIB_UU2020, COMUU2020, LIB_COMUU2020, TYPUU2020, LIB_TYPUU2020, TUU2017, LIB_TUU2017, TDUU2017, LIB_TDUU2017,
         AAV2020, LIB_AAV2020 = LIBAAV2020, TAAV2017, LIB_TAAV2017, TDAAV2017, LIB_TDAAV2017, CATEAAV2020, LIB_CATEAAV2020, BV2022, LIB_BV2022)


nrow(table_passage_communes_zonages) == nrow(table_passage_com_zonages)

names(COGiter::table_passage_communes_zonages) == names(table_passage_communes_zonages)
glimpse(COGiter::table_passage_communes_zonages)
glimpse(table_passage_communes_zonages)

usethis::use_data(table_passage_communes_zonages, overwrite = TRUE)
# utilitaires.ju::use_data_doc("table_passage_communes_zonages", description = "Table de passage des communes vers des zonages supra", source = "Insee : https://www.insee.fr/fr/information/2028028")

liste_zonages <- tibble::tribble(
  ~"Code du zonage",~"Nom du zonage",
  "ARR","Arrondissement",
  "CV","Canton ville",
  "ZE2020","Zone d\'emploi 20220",
  "UU2020","Unit\u00e9 urbaine 2020",
  "COMUU2020", "Statut de la commune dans l'unit\u00e9 urbaine",
  "TYPUU2020", "Type d'unité urbaine",
  "TUU2017","Tranche d\'unit\u00e9 urbaine 2020 calcul\u00e9e sur la population 2017",
  "TDUU2017","Tranche d\u00e9taill\u00e9e d\'unit\u00e9 urbaine 2017",
  "AAV2020","Aire d\'attraction des villes 2020",
  "TAAV2017","Tranche d\'aire d\'attraction des villes 2020 calcul\u00e9e sur la population 2017",
  "TDAAV2017","Tranche d\u00e9taill\u00e9e d\'aire d\'attraction des villes 2020 calcul\u00e9e sur la population 2017",
  "CATEAAV2020","Cat\u00e9gorie commune dans aire d\'attraction des villes 2020",
  "BV2022","Bassin de vie 2022"
)

# un autre dataset interne, nécessaire pour les fonctions filtrer_cog
com_limitrophes_epci_a_cheval <- select(communes, DEPCOM, NOM_DEPCOM, DEP, DEPARTEMENTS_DE_L_EPCI) %>%
  tidyr::unnest(DEPARTEMENTS_DE_L_EPCI) %>%
  filter(DEP != DEPARTEMENTS_DE_L_EPCI) %>%
  select(-DEP) %>%
  rename(DEP = DEPARTEMENTS_DE_L_EPCI) %>%
  left_join(departements %>% select(DEP, REG)) %>%
  mutate(
    TypeZone = factor("Communes", levels = levels(liste_zone$TypeZone)),
    DEP = as.list(as.character(DEP)),
    REG = as.list(as.character(REG))
  ) %>%
  rename(CodeZone = DEPCOM, Zone = NOM_DEPCOM)

usethis::use_data(liste_zonages, com_limitrophes_epci_a_cheval, internal = TRUE, overwrite = TRUE)
