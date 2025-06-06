# Chargement des librairies ----------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(arrow)
library(usethis)

millesime <- "2025"
repo_mil <- paste0("data-raw/source/", millesime, "/COG/")

fact.enc.utf8 <- function(a) {
  a <- as.factor(a) %>%
   fct_relabel(.fun = enc2utf8)
}


# Chargement des tables du COG ------------------------------------------
# https://www.insee.fr/fr/information/2560452

# dir.create(repo_mil, recursive = TRUE)
# download.file("https://www.insee.fr/fr/statistiques/fichier/8377162/cog_ensemble_2025_csv.zip",
              # destfile = paste0(repo_mil, "/cog_ensemble_", millesime, "_csv.zip"))
unzip(zipfile = paste0(repo_mil, "/cog_ensemble_", millesime, "_csv.zip"),
      exdir = repo_mil, overwrite = TRUE)

# Création des tables suivantes:
# régions : copie de la table régions du cog avec des noms de variables normalisées
# départements : copie de la table départements du cog avec des noms de variables normalisées
# epci : Construite à partir de la table epci du cog, pour chaque epci, liste les attributs et les départements et régions d'appartenance
# communes : copie de la table communes du cog avec des noms de variables normalisées
# communes_info_supra : donne pour toutes les communes les epci, départements, régions de rattachement ainsi que les info de rattachement de l'epci de rattachement


# Table des régions --------------------------------------------------

regions <- read_csv(paste0(repo_mil, "/v_region_", millesime, ".csv"))  %>%
  as_tibble() %>%
  rename_with(toupper) %>%
  rename(NOM_REG = LIBELLE) %>%
  mutate(across(everything(), fact.enc.utf8))

# supervision pour vérifier évolution structure
names(COGiter::regions) == names(regions)
str(COGiter::regions)
str(regions)

# Table des départements --------------------------------------------------

departements <- read_csv(paste0(repo_mil, "/v_departement_", millesime, ".csv")) %>%
  as_tibble() %>%
  rename_with(toupper) %>%
  rename(NOM_DEP=LIBELLE) %>%
  mutate(across(everything(), fact.enc.utf8))

# supervision pour vérifier évolution structure
names(COGiter::departements) == names(departements)
str(COGiter::departements)
str(departements)

# Table des Epci ----------------------------------------------------------

# table de la composition communale des EPCI : https://www.collectivites-locales.gouv.fr/institutions/liste-et-composition-des-epci-fiscalite-propre
download.file(url = paste0("https://www.collectivites-locales.gouv.fr/files/Accueil/DESL/", millesime, "/epcicom", millesime, ".xlsx"),
              destfile = paste0(repo_mil, "/epcicom", millesime, ".xlsx"), method = "curl")

epci_0 <- read_excel(path = paste0(repo_mil, "/epcicom", millesime, ".xlsx"), sheet = 1) %>%
  mutate(
    DEP_SIEGE_EPCI = as.factor(dept),
    EPCI = as.factor(siren),
    NOM_EPCI = as.factor(raison_sociale),
    NATURE_EPCI = gsub("MET69|METRO", "ME", nature_juridique) %>% as.factor() %>%
      fct_relevel(c("ME", "CU", "CA", "CC")) %>% fct_expand("ZZ")
    ) %>%
  arrange(NATURE_EPCI, NOM_EPCI) %>%
  mutate(EPCI = fct_inorder(EPCI)) %>%
  select(EPCI, NOM_EPCI, NATURE_EPCI, DEP_SIEGE_EPCI) %>%
  distinct() %>%
  as_tibble() %>%
  mutate(across(where(is.factor), fact.enc.utf8))

communes_epci <- read_excel(path = paste0(repo_mil, "/epcicom", millesime, ".xlsx"), sheet = 1) %>%
  transmute(
    DEPCOM = as.factor(insee),
    NOM_DEPCOM = nom_membre,
    EPCI = as.factor(siren),
    NOM_EPCI = as.factor(raison_sociale),
    DEP = as.factor(dep_com)
  ) %>%
  left_join(departements %>% select(DEP, REG), by = "DEP") %>%
  as_tibble()

epci_rattachement_reg_dep <- communes_epci %>%
  select(-DEPCOM, -NOM_DEPCOM) %>%
  distinct() %>%
  group_by(EPCI, NOM_EPCI) %>%
  summarise(DEPARTEMENTS_DE_L_EPCI = list(unique(as.character(DEP))),
            REGIONS_DE_L_EPCI = list(unique(as.character(REG))), .groups = "drop") %>%
  ungroup()

epci <- epci_0 %>%
  left_join(epci_rattachement_reg_dep) %>%
  relocate(DEP_SIEGE_EPCI, .after = dplyr::everything()) # %>%
  # filter(EPCI != "ZZZZZZZZZ") # devenu inutile /!\, changement du fichier, plus les communes hors epci


# supervision pour vérifier évolution structure
names(COGiter::epci) == names(epci)
glimpse(COGiter::epci)
glimpse(epci)

# Table des communes  --------------------------------------------------

communes_cog <- read_csv(paste0("data-raw/source/", millesime, "/COG/v_commune_", millesime,".csv"),
  col_types = cols(
    TYPECOM = col_character(),
    COM = col_character(),
    REG = col_character(),
    DEP = col_character(),
    CTCD = col_character(),
    ARR = col_character(),
    TNCC = col_double(),
    NCC = col_character(),
    NCCENR = col_character(),
    LIBELLE = col_character(),
    CAN = col_character(),
    COMPARENT = col_character()
  )
) %>%
  as_tibble() %>%
  rename(
    DEPCOM = COM,
    NOM_DEPCOM = LIBELLE
  ) %>%
  mutate(across(everything(), fact.enc.utf8))


# complément tables com EPCI

communes_epci <- communes_cog %>%
  filter(TYPECOM == "COM") %>%
  select(DEPCOM, NOM_DEPCOM, DEP, REG) %>%
  full_join(communes_epci, by = "DEPCOM", suffix = c("", ".dgcl")) %>%
  # le nom de commune fourni par la DGCL ne gère pas bien les E dans l'O et la table dgcl ne contient pas les communes insulaires
  select(-contains(".dgcl")) %>%
  mutate(EPCI = coalesce(EPCI, "ZZZZZZZZZ"),
         NOM_EPCI = coalesce(NOM_EPCI, "Sans objet")) %>%
  mutate(across(everything(), fact.enc.utf8))

table_passage_com_epci <- communes_epci  %>%
  select(DEPCOM, EPCI)


# Table des informations supra communales de chaque commune ---------------

communes <- communes_cog %>%
  filter(TYPECOM == "COM") %>%
  #  select(DEPCOM, NOM_DEPCOM, DEP, REG) %>%
  mutate(
    DEPCOM = fct_drop(DEPCOM),
    NOM_DEPCOM = fct_drop(NOM_DEPCOM)
  ) %>%
  left_join(communes_epci %>%
    select(DEPCOM, EPCI, NOM_EPCI)) %>%
  left_join(epci_rattachement_reg_dep) %>%
  left_join(departements %>% select(DEP, NOM_DEP)) %>%
  left_join(regions %>% select(REG, NOM_REG)) %>%
  select(
    DEPCOM, NOM_DEPCOM, EPCI, NOM_EPCI, DEP, NOM_DEP,
    REG, NOM_REG, DEPARTEMENTS_DE_L_EPCI, REGIONS_DE_L_EPCI,
    ARR, TNCC, NCC, NCCENR, CAN, COMPARENT
  ) %>%
  mutate(
    DEPARTEMENTS_DE_L_EPCI = if_else(EPCI != "ZZZZZZZZZ", DEPARTEMENTS_DE_L_EPCI, as.list(as.character(DEP))),
    REGIONS_DE_L_EPCI = if_else(EPCI != "ZZZZZZZZZ", REGIONS_DE_L_EPCI, as.list(as.character(REG)))
  ) %>%
  mutate(across(where(is.factor), ~fct_relabel(.f = .x, .fun = enc2utf8)),
         across(where(is.character), fact.enc.utf8))

communes_info_supra <- communes %>%
  select(
    DEPCOM, NOM_DEPCOM, EPCI, NOM_EPCI,
    DEP, NOM_DEP, REG, NOM_REG, DEPARTEMENTS_DE_L_EPCI,
    REGIONS_DE_L_EPCI
  )

# supervision pour vérifier évolution structure
names(COGiter::communes_info_supra) == names(communes_info_supra)
glimpse(COGiter::communes_info_supra)
glimpse(communes_info_supra)

names(COGiter::communes) == names(communes)
glimpse(COGiter::communes)
glimpse(communes)


# Table liste zone --------------------------------------------------------

liste_zone <- bind_rows(
  communes_info_supra %>%
    select(CodeZone = DEPCOM, Zone = NOM_DEPCOM, EPCI, DEP, REG) %>%
    distinct() %>%
    mutate(
      TypeZone = "Communes",
      DEP = as.list(as.character(DEP)),
      REG = as.list(as.character(REG))
    ),
  epci %>%
    select(CodeZone = EPCI, Zone = NOM_EPCI, NATURE_EPCI, DEP = DEPARTEMENTS_DE_L_EPCI, REG = REGIONS_DE_L_EPCI) %>%
    distinct() %>%
    mutate(TypeZone = "Epci", EPCI = CodeZone),
  departements %>%
    select(CodeZone = DEP, Zone = NOM_DEP, REG) %>%
    distinct() %>%
    mutate(
      TypeZone = "Départements",
      DEP = as.list(as.character(CodeZone)),
      REG = as.list(as.character(REG))
    ),
  regions %>%
    select(REG, Zone = NOM_REG) %>%
    distinct() %>%
    mutate(
      TypeZone = "Régions",
      CodeZone = REG,
      REG = as.list(as.character(REG))
    )
) %>%
  mutate(across(where(is.character), fact.enc.utf8)) %>%
  select(CodeZone, Zone, TypeZone, EPCI, NATURE_EPCI, DEP, REG)

names(COGiter::liste_zone) == names(liste_zone)
glimpse(COGiter::liste_zone)
glimpse(liste_zone)


# Table des mouvements de communes ----------------------------------------
# Table du millésime précédent
table_passage_com_hist_old <- COGiter::table_passage_com_historique
mil_precdt <- as.numeric(millesime) - 1
# save(table_passage_com_hist_old, file = paste0("data-raw/source/", millesime, "/COG/table_passage_com_historique_", mil_precdt, ".RData"))

mvtcommunes <- read_csv(paste0("data-raw/source/", millesime, "/COG/v_mvt_commune_", millesime, ".csv"),
                        col_types = cols(
                          MOD = col_factor(),
                          DATE_EFF = col_date(format = ""),
                          TYPECOM_AV = col_factor(),
                          COM_AV = col_character(),
                          TNCC_AV = col_factor(),
                          NCC_AV = col_factor(),
                          NCCENR_AV = col_factor(),
                          LIBELLE_AV = col_factor(),
                          TYPECOM_AP = col_factor(),
                          COM_AP = col_character(),
                          TNCC_AP = col_factor(),
                          NCC_AP = col_factor(),
                          NCCENR_AP = col_factor(),
                          LIBELLE_AP = col_factor()
                        ))  %>%
  mutate(across(where(is.factor), fact.enc.utf8))

# on filtre cette table pour ne garder que :
  # - les évolutions de l'année écoulée,
  # - concernant les seules communes,
  # - autres que des changements de nom (MOD == 10)

table_evol_com_mil <- mvtcommunes %>%
  filter(DATE_EFF > paste0(mil_precdt, "-01-01"), TYPECOM_AP == "COM", TYPECOM_AV == "COM", MOD != 10) %>%
  select(MOD, COM_AV, COM_AP) %>%
  distinct()

# communes rétablies : identification
com_retablies <- filter(table_evol_com_mil, MOD == 21, COM_AV != COM_AP) %>%
  pull(COM_AP)
# communes rétablies : fonction d'identification des lignes de correspondance entre communes à supprimer
corresp_a_suppr <- function(df, idcom = "60694") {
  mutate(df, a_virer = case_when(COM_AP == idcom & DEPCOM_HIST != idcom ~ TRUE,
                                 COM_AP != idcom & DEPCOM_HIST == idcom ~ TRUE,
                                 TRUE ~ FALSE)
  ) %>%
    filter(a_virer) %>%
    pull(rowid)
}

# on applique les évolutions de l'année écoulée à la table de passage du millésime précédent
table_passage_com_historique0 <- table_passage_com_hist_old %>%
  left_join(table_evol_com_mil, by = c("DEPCOM" = "COM_AV"), relationship = "many-to-many") %>%
  mutate(COM_AP = coalesce(COM_AP, DEPCOM)) %>%
  rowid_to_column()

# gestion des rétablissements de communes : COM 60694 sortie de 60054, COM 85165 et 85212 sorties de 85084 en 2024
id_a_virer <- map(.x = com_retablies, .f = ~corresp_a_suppr(df = table_passage_com_historique0, idcom = .x)) %>%
  unlist()

table_passage_com_historique <- table_passage_com_historique0 %>%
  mutate(A_virer = rowid %in% id_a_virer) %>%
  filter(!A_virer) %>%
  select(DEPCOM_HIST, DEPCOM = COM_AP) %>%
  distinct() %>%
  mutate(across(everything(), fact.enc.utf8))

# Vérification : DEPCOM_HIST ne doit pas comporter de doublons
table_passage_com_historique %>%
  add_count(DEPCOM_HIST) %>%
  filter(n > 1)

names(COGiter::table_passage_com_historique) == names(table_passage_com_historique)
glimpse(COGiter::table_passage_com_historique)
glimpse(table_passage_com_historique)


# Gestion des arrondissements de Paris Lyon Marseille
# Les arrondissements sont gérés comme des anciens codes communes et rattachés à leur commune

arn_plm <- communes_cog %>%
  filter(TYPECOM == "ARM") %>%
  rename(ARN = DEPCOM) %>%
  mutate(DEPCOM = case_when(
    DEP == "75" ~ "75056", # Paris
    DEP == "69" ~ "69123", # Lyon
    DEP == "13" ~ "13055" # Marseille
  )) %>%
  select(ARN, DEPCOM)

table_passage_com_historique <- table_passage_com_historique %>%
  bind_rows(arn_plm %>% select(DEPCOM_HIST = ARN, DEPCOM)) %>%
  arrange(DEPCOM_HIST) %>%
  distinct() %>%
  mutate(across(everything(), fact.enc.utf8))

# Vérification : DEPCOM_HIST ne doit pas comporter de doublons
table_passage_com_historique %>%
  add_count(DEPCOM_HIST) %>%
  filter(n>1)

usethis::use_data(regions, internal = FALSE, overwrite = TRUE)
usethis::use_data(departements, internal = FALSE, overwrite = TRUE)
usethis::use_data(epci, internal = FALSE, overwrite = TRUE)
usethis::use_data(communes_cog, internal = FALSE, overwrite = TRUE)
usethis::use_data(communes, internal = FALSE, overwrite = TRUE)
usethis::use_data(communes_info_supra, internal = FALSE, overwrite = TRUE)
usethis::use_data(mvtcommunes, internal = FALSE, overwrite = TRUE)
usethis::use_data(table_passage_com_epci, internal = FALSE, overwrite = TRUE)
# usethis::use_data(pop2015, internal = FALSE, overwrite = TRUE)
usethis::use_data(liste_zone, internal = FALSE, overwrite = TRUE)
usethis::use_data(table_passage_com_historique, internal = FALSE, overwrite = TRUE)
usethis::use_data(arn_plm, internal = FALSE, overwrite = TRUE)


## Chargement des couches communes et table_passage_com_historique dans production.scte_cogiter---
library(datalibaba)
communes2 <- rowwise(communes) %>%
  mutate(across(where(is.list), ~paste0(.x, collapse = ", ") %>% as.factor),
         across(where(is.factor), as.character)) %>%
  ungroup()
poster_data(data = communes2, table = "r_communes_000", schema = "scte_cogiter", pk = "DEPCOM", post_row_name = FALSE, droits_schema = TRUE, db = "production", overwrite = TRUE)
poster_data(data = communes2, table = paste0("r_communes_000_", millesime), schema = "scte_cogiter", pk = "DEPCOM", post_row_name = FALSE, droits_schema = TRUE, db = "production", overwrite = TRUE)

table_passage_com_historique2 <- table_passage_com_historique %>%
  mutate(across(where(is.factor), as.character))
poster_data(data = table_passage_com_historique2, table = "r_tb_passage_com_hist_000", schema = "scte_cogiter", pk = "DEPCOM_HIST", post_row_name = FALSE, droits_schema = TRUE, db = "production", overwrite = TRUE)
poster_data(data = table_passage_com_historique2, table = paste0("r_tb_passage_com_hist_000_", millesime), schema = "scte_cogiter", pk = "DEPCOM_HIST", post_row_name = FALSE, droits_schema = TRUE, db = "production", overwrite = TRUE)

poster_data(data = table_passage_com_historique2, table = "r_cogiter_communes_000", schema = "donnee_generique", pk = "DEPCOM_HIST", post_row_name = FALSE,
            droits_schema = TRUE, db = "consultation", overwrite = TRUE, user = "csd")
