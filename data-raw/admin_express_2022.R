library(sf)
library(usethis)
library(tidyverse)

load("data/communes_info_supra.rda")
load("data/table_passage_com_historique.rda")
load("data/communes_geo.rda")

`%not_in%` <- purrr::negate(`%in%`)
load("data/communes_geo.rda")
communes_geo <- communes_geo %>%
  rename(DEPCOM_HIST = DEPCOM) %>%
  left_join(table_passage_com_historique) %>%
  select(-DEPCOM_HISTRUE) %>%
  group_by(DEPCOM) %>%
  summarise(do_union = TRUE) %>%
  mutate(AREA = st_area(geometry)) %>%
  select(DEPCOM, AREA)

load("data/communes_metro_geo.rda")
communes_metro_geo <- communes_metro_geo %>%
  rename(DEPCOM_HIST = DEPCOM) %>%
  left_join(table_passage_com_historique) %>%
  select(-DEPCOM_HISTRUE) %>%
  group_by(DEPCOM) %>%
  summarise(do_union = TRUE) %>%
  mutate(AREA = st_area(geometry)) %>%
  select(DEPCOM, AREA)




epci_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet") %>%
  inner_join(communes_geo %>% select(DEPCOM), ., by = "DEPCOM") %>%
  select(EPCI) %>%
  group_by(EPCI) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  mutate(AREA = st_area(geometry)) %>%
  select(EPCI, AREA)

epci_metro_geo <- filter(communes_info_supra, NOM_EPCI != "Sans objet") %>%
  inner_join(communes_metro_geo %>% select(DEPCOM), ., by = "DEPCOM") %>%
  select(EPCI) %>%
  group_by(EPCI) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  mutate(AREA = st_area(geometry)) %>%
  select(EPCI, AREA)

departements_geo <- inner_join(communes_geo %>% select(DEPCOM),
  communes_info_supra,
  by = "DEPCOM") %>%
  select(DEP) %>%
  group_by(DEP) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  mutate(AREA = st_area(geometry)) %>%
  select(DEP, AREA)

departements_metro_geo <- inner_join(communes_metro_geo %>% select(DEPCOM),
  communes_info_supra,
  by = "DEPCOM") %>%
  select(DEP) %>%
  group_by(DEP) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  mutate(AREA = st_area(geometry)) %>%
  select(DEP, AREA)

regions_geo <- inner_join(communes_geo %>% select(DEPCOM),
  communes_info_supra,
  by = "DEPCOM") %>%
  select(REG) %>%
  group_by(REG) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  mutate(AREA = st_area(geometry)) %>%
  select(REG, AREA)

regions_metro_geo <- inner_join(communes_metro_geo %>% select(DEPCOM),
  communes_info_supra,
  by = "DEPCOM") %>%
  select(REG) %>%
  group_by(REG) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  mutate(AREA = st_area(geometry)) %>%
  select(REG, AREA)

# sauvegarde des données --------------------------------------------------------
use_data(communes_geo, internal = FALSE, overwrite = TRUE)
use_data(communes_metro_geo, internal = FALSE, overwrite = TRUE)
use_data(epci_geo, internal = FALSE, overwrite = TRUE)
use_data(epci_metro_geo, internal = FALSE, overwrite = TRUE)
use_data(departements_geo, internal = FALSE, overwrite = TRUE)
use_data(departements_metro_geo, internal = FALSE, overwrite = TRUE)
use_data(regions_geo, internal = FALSE, overwrite = TRUE)
use_data(regions_metro_geo, internal = FALSE, overwrite = TRUE)
