library(dplyr)
library(readxl)
library(rlang)
# Pop 2015 ----------------------------------------------------------------

pop2015 <- read_excel("data-raw/source/pop2015.xls",
  col_types = c("text", "numeric", "numeric", "numeric", "text")) %>%
  mutate(DEPCOM = case_when(
    DEP %in% c("971", "972", "973", "974", "976") ~ paste0(as.character(DEP), substr(as.character(DEPCOM), 2, 3)),
    TRUE ~ paste0(as.character(DEP), as.character(DEPCOM))) %>%
    as.factor()
    ) %>%
  select(-DEP) %>%
  set_names(c("DEPCOM", "pop2015", "pop2015_a_part", "pop2015_totale"))

usethis::use_data(pop2015, internal = FALSE, overwrite = TRUE)
