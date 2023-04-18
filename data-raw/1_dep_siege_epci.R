library(readr)
library(dplyr)
library(tricky)
library(tidyr)

millesime <- 2023

telech <- tempfile()
download.file(url = paste0('https://www.banatic.interieur.gouv.fr/V5/fichiers-en-telechargement/telecharger.php?zone=N&date=01/01/', millesime,'&format=E'),
              destfile = paste0(telech, "siege_epci.csv"))

siege_epci <- readr::read_tsv(paste0(telech, "siege_epci.csv"), locale = locale("fr", encoding = "latin1"), show_col_types = FALSE) %>%
  set_standard_names() %>%
  select(EPCI = `n°_siren`, departement_siege) %>%
  separate(departement_siege, into = c("DEP_SIEGE_EPCI", NA), sep = " - ") %>%
  distinct %>%
  mutate(across(c(EPCI, DEP_SIEGE_EPCI), ~as.character(.x) %>% as.factor()))


save(siege_epci, file = paste0("data-raw/source/", millesime,"/siege_epci.RData"))
