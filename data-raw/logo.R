library(COGiter)
library(sf)
library(ggplot2)
library(dplyr)
library(hexSticker)
epci_lannion <- epci_geo %>%
  filter(EPCI == '200065928')
dep_code_armor <- departements_geo %>%
  filter(DEP %in% c("22","29"))
plot(dep_code_armor)

communes_lannion <- communes %>%
  filter(EPCI == "200065928") %>%
  pull(DEPCOM)
communes_lannion <- communes_geo %>%
  filter(DEPCOM %in% communes_lannion)
plot(communes_lannion)

bbox <- st_bbox(epci_lannion)
p <- ggplot() +
  geom_sf(data = regions_geo,fill = "white",color = NA) +
  geom_sf(data = communes_lannion, fill = "white", color = "light grey", size = .1) +
  geom_sf(data = epci_lannion, fill = NA, color = "dark grey", size = .5) +
  geom_sf(data = dep_code_armor, fill = NA, color = "black", size = 1) +
  coord_sf(xlim = c(bbox[1],bbox[3]),ylim = c(bbox[2],bbox[4])) +
  theme_void() + theme_transparent() +
  theme(panel.background = element_rect(colour = NA ))

p <- ggplot() +
  geom_sf(data = regions_geo,fill = NA,color = NA) +
  geom_sf(data = communes_lannion, fill = NA, color = "light grey", size = .1) +
  geom_sf(data = epci_lannion, fill = NA, color = "dark grey", size = .2) +
  geom_sf(data = dep_code_armor, fill = NA, color = "black", size = .3) +
  coord_sf(xlim = c(bbox[1]*0.99,bbox[3]*1.01),ylim = c(bbox[2],bbox[4])) +
  theme_void() +
  theme_transparent() +
  theme(panel.background = element_rect(colour = NA ))

library(gouvdown)
library(extrafont)
extrafont::font_import()
sticker(p,
        package="COGiter",
        p_family = "Marianne",
        p_color = "#466964",
        h_color = "#169B62",
        p_size=20,
        h_fill = "white",
        spotlight = TRUE,
        s_x=1,
        s_y=.75,
        s_width=4,
        s_height=1.02,
        filename="logo.png")

usethis::use_logo('logo.png')
file.remove('logo.png')
