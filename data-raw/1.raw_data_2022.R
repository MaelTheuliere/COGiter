# Chargement des librairies ----------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(usethis)

millesime <- "2022"
repo_mil <- paste0("data-raw/source/", millesime, "/COG")

fact.enc.utf8 <- function(a) {
  a <- as.factor(a) %>%
   fct_relabel(.fun = enc2utf8)
}


# Chargement des tables du COG ------------------------------------------
# https://www.insee.fr/fr/information/2560452

# dir.create(repo_mil)
# download.file(paste0("https://www.insee.fr/fr/statistiques/fichier/6051727/cog_ensemble_2022_csv.zip"),
#               destfile = paste0(repo_mil, "/cog_ensemble_", millesime, "_csv.zip"))
# unzip(zipfile = paste0(repo_mil, "/cog_ensemble_", millesime, "_csv.zip"),
#       exdir = repo_mil)

# Création des tables suivantes:
# régions : copie de la table régions du cog avec des noms de variables normalisées
# départements : copie de la table départements du cog avec des noms de variables normalisées
# epci : Construite à partir de la table epci du cog, pour chaque epci,liste les attributs et les départements et régions d'appartenance
# communes : copie de la table communes du cog avec des noms de variables normalisées
# communes_info_supra : donne pour toutes les communes les epci, départements, régions de rattachement ainsi que les info de rattachement de l'epci de rattachement




# Table des régions --------------------------------------------------

regions <- read_csv(paste0(repo_mil, "/region_", millesime, ".csv"))  %>%
  as_tibble() %>%
  rename_with(toupper) %>%
  rename(NOM_REG = LIBELLE) %>%
  mutate(across(everything(), fact.enc.utf8))

# Table des départements --------------------------------------------------

departements <- read_csv(paste0(repo_mil, "/departement_", millesime, ".csv")) %>%
  as_tibble() %>%
  rename_with(toupper) %>%
  rename(NOM_DEP=LIBELLE) %>%
  mutate(across(everything(), fact.enc.utf8))

# Table des Epci ----------------------------------------------------------
# download.file(url = paste0("https://www.collectivites-locales.gouv.fr/files/", millesime, "/epcicom", millesime, ".xlsx"),
              # destfile = paste0(repo_mil, "/epcicom", millesime, ".xlsx"), method = "curl")


epci <- read_excel(path = paste0(repo_mil, "/epcicom", millesime, ".xlsx"), sheet = 1) %>%
  mutate(
    EPCI = as.factor(siren),
    NOM_EPCI = as.factor(raison_sociale),
    NATURE_EPCI = gsub("MET69|METRO", "ME", nature_juridique) %>% as.factor() %>%
      fct_relevel(c("ME", "CU", "CA", "CC")) %>% fct_expand("ZZ")
    ) %>%
  arrange(NATURE_EPCI, NOM_EPCI) %>%
  mutate(EPCI = fct_inorder(EPCI)) %>%
  select(EPCI, NOM_EPCI, NATURE_EPCI) %>%
  distinct() %>%
  as_tibble() %>%
  mutate(across(where(is.factor), ~fct_relabel(.f = .x, .fun = enc2utf8)))

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

epci <- epci %>%
  left_join(epci_rattachement_reg_dep) # %>%
  # filter(EPCI != "ZZZZZZZZZ") # devenu inutile /!\, changement du fichier, plus les communes hors epci



# Table des communes  --------------------------------------------------

communes_cog <- read_csv(paste0("data-raw/source/", millesime, "/COG/commune_", millesime,".csv"),
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
  # le nom de commune fourni par la DGCL ne gère pas bien les E dans l'O et la table dgcl ne contient pas les communes insulaire
  select(-contains(".dgcl")) %>%
  mutate(EPCI = coalesce(EPCI, "ZZZZZZZZZ"),
         NOM_EPCI = coalesce(NOM_EPCI, "Sans objet")) %>%
  mutate(across(everything(), fact.enc.utf8))

table_passage_com_epci <- communes_epci  %>%
  select(DEPCOM, EPCI)

# Table des mouvements de communes ----------------------------------------

mvtcommunes <- read_csv(paste0("data-raw/source/", millesime, "/COG/mvtcommune_", millesime, ".csv"),
                      col_types = cols(
                        MOD = col_factor(),
                        DATE_EFF = col_date(format = ""),
                        TYPECOM_AV = col_factor(),
                        COM_AV = col_factor(),
                        TNCC_AV = col_factor(),
                        NCC_AV = col_factor(),
                        NCCENR_AV = col_factor(),
                        LIBELLE_AV = col_factor(),
                        TYPECOM_AP = col_factor(),
                        COM_AP = col_factor(),
                        TNCC_AP = col_factor(),
                        NCC_AP = col_factor(),
                        NCCENR_AP = col_factor(),
                        LIBELLE_AP = col_factor()
                      ))  %>%
  mutate(across(where(is.factor), fact.enc.utf8))


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

# Table de passage des communes historiques ---------------------------------------
# source : https://www.insee.fr/fr/information/2028028

# download.file(paste0("https://www.insee.fr/fr/statistiques/fichier/2028028/table_passage_geo2003_geo", millesime, ".zip"),
#               destfile = paste0(repo_mil, "/table_passage_geo2003_geo", millesime, ".zip"))
# unzip(zipfile = paste0(repo_mil, "/table_passage_geo2003_geo", millesime, ".zip"),
#       exdir = repo_mil)

table_passage_com_historique <- read_xlsx(paste0(repo_mil, "/table_passage_geo2003_geo", millesime, ".xlsx"), skip = 5) %>%
  select(DEPCOM_HIST = CODGEO_INI, DEPCOM = paste0("CODGEO_", millesime)) %>%
  mutate(DEPCOM = case_when( # correction bug toujours présent de saint martin et saint barthélemy qui ne sont pas rattachés sur leurs nouveaux codes commune.
    DEPCOM_HIST == '97123'~'97701',
    DEPCOM_HIST == '97127'~'97801',
    TRUE ~ DEPCOM
  )) %>%
  mutate(across(everything(), fact.enc.utf8))

# Gestion des arrondissements de PLM
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
  mutate(across(everything(), fact.enc.utf8))


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


## Chargement des couches communes et table_passage_com_historique dans production.cogiter
library(datalibaba)
communes2 <- rowwise(communes) %>%
  mutate(across(where(is.list), ~paste0(.x, collapse = ", ") %>% as.factor),
         across(where(is.factor), as.character)) %>%
  ungroup()
poster_data(data = communes2, table = "r_communes_000", schema = "cogiter", pk = "DEPCOM", post_row_name = FALSE, droits_schema = TRUE, db = "production", overwrite = TRUE)
poster_data(data = communes2, table = paste0("r_communes_000_", millesime), schema = "cogiter", pk = "DEPCOM", post_row_name = FALSE, droits_schema = TRUE, db = "production", overwrite = TRUE)

table_passage_com_historique2 <- table_passage_com_historique %>%
  mutate(across(where(is.factor), as.character))
poster_data(data = table_passage_com_historique2, table = "r_tb_passage_com_hist_000", schema = "cogiter", pk = "DEPCOM_HIST", post_row_name = FALSE, droits_schema = TRUE, db = "production", overwrite = TRUE)
poster_data(data = table_passage_com_historique2, table = paste0("r_tb_passage_com_hist_000_", millesime), schema = "cogiter", pk = "DEPCOM_HIST", post_row_name = FALSE, droits_schema = TRUE, db = "production", overwrite = TRUE)

poster_data(data = table_passage_com_historique2, table = "r_cogiter_communes_000", schema = "donnee_generique", pk = "DEPCOM_HIST", post_row_name = FALSE,
            droits_schema = TRUE, db = "consultation", overwrite = TRUE, user = "admin")
