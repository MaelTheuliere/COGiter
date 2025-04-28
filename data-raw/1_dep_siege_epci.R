library(readr)
library(dplyr)
library(tricky)

millesime <- 2025
telech <- tempfile()
# https://www.data.gouv.fr/fr/datasets/base-nationale-sur-les-intercommunalites/
download.file(url = paste0('https://www.data.gouv.fr/fr/datasets/r/6e05c448-62cc-4470-aa0f-4f31adea0bc4'),
              destfile = paste0(telech, "siege_epci.csv"))

siege_epci <- readr::read_delim(file = paste0(telech, "siege_epci.csv"), delim = ";",
                                locale = locale("fr", encoding = "latin1"), show_col_types = FALSE) %>%
  select(EPCI = siren, DEP_SIEGE_EPCI = dept) %>%
  distinct %>%
  mutate(across(c(EPCI, DEP_SIEGE_EPCI), ~as.character(.x) %>% as.factor()))

dir.create(paste0("data-raw/source/", millesime))
save(siege_epci, file = paste0("data-raw/source/", millesime,"/siege_epci.RData"))

