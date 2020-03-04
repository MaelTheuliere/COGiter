#' @return Renvoie une table de donnees filtrées
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom dplyr pull
#' @importFrom sf st_bbox
#' @importFrom sf st_crop
#' @encoding UTF-8

filtrer_cog_metro_geo <- function(depcom=NULL,
                                  epci=NULL,
                                  dep=NULL,
                                  reg=NULL,
                                  metro=NULL,
                                  garder_supra='non'
) {
  stop_if_any(c(depcom,epci,dep,reg,metro),is.numeric,msg="Les paramètres doivent être des chaines de caractère")
  reg_drom <- c('01','02','03','04','06')
  dep_drom <- c('971','972','973','974','976')
  epci_drom <- liste_zone %>% filter(str_detect(DEP,dep_drom),TypeZone=='Epci') %>% pull(CodeZone)
  depcom_drom <- liste_zone %>% filter(str_detect(DEP,dep_drom),TypeZone=='Communes') %>% pull(CodeZone)
  if (!is.null(reg)){
    if (garder_supra=="non"){
      liste_dep <- filter(liste_zone,str_detect(REG,reg),TypeZone=="D\u00e9partements") %>% pull(CodeZone)
      liste_epci <- filter(liste_zone,str_detect(REG,reg),TypeZone=="Epci") %>% pull(CodeZone)
      liste_depcom <- filter(liste_zone,str_detect(REG,reg),TypeZone=="Communes") %>% pull(CodeZone)

      reg <- regions_geo %>% filter(REG==reg)
      dep <- departements_geo %>% filter(DEP %in% liste_dep)
      epci <- epci_geo %>% filter(EPCI %in% liste_epci)
      communes <- communes_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes,epci=epci,departements=dep,regions=reg)
    }
    if (garder_supra=="oui"){
      reg <- regions_geo %>% filter(REG==reg)
      bbox <- st_bbox(reg)
      reg <- st_crop(regions_geo
                     %>% filter(!(REG %in% reg_drom)),
                     bbox)
      dep <- st_crop(departements_geo %>%
                       filter(!(DEP %in% dep_drom)),
                     bbox)
      epci <- st_crop(epci_geo %>%
                        filter(!(EPCI %in% epci_drom)),
                      bbox)
      communes <- st_crop(communes_geo %>%
                            filter(!(DEPCOM %in% depcom_drom)),
                          bbox)
      result <- list(communes=communes,epci=epci,departements=dep,regions=reg)
    }
    return(result)

  }
  if (!is.null(dep)){
    if (garder_supra=="non"){
      liste_epci <- filter(liste_zone,str_detect(DEP,dep),TypeZone=="Epci") %>% pull(CodeZone)
      liste_depcom <- filter(liste_zone,str_detect(DEP,dep),TypeZone=="Communes") %>% pull(CodeZone)
      dep <- departements_geo %>% filter(DEP == dep)
      epci <- epci_geo %>% filter(EPCI %in% liste_epci)
      communes <- communes_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes,epci=epci,departements=dep)
    }
    if (garder_supra=="oui"){
      dep <- departements_geo %>% filter(DEP==dep)
      bbox <- st_bbox(dep)
      dep <- st_crop(departements_geo %>%
                       filter(!(DEP %in% dep_drom)),
                     bbox)
      epci <- st_crop(epci_geo %>%
                        filter(!(EPCI %in% epci_drom)),
                      bbox)
      communes <- st_crop(communes_geo %>%
                            filter(!(DEPCOM %in% depcom_drom)),
                          bbox)
      result <- list(communes=communes,epci=epci,departements=dep)
    }
    return(result)

  }
  if (!is.null(epci)){
    if (garder_supra=="non"){
      liste_depcom <- filter(liste_zone,EPCI==epci,TypeZone=="Communes") %>% pull(CodeZone)
      epci <- epci_geo %>% filter(EPCI == epci)
      communes <- communes_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes,epci=epci)
    }
    if (garder_supra=="oui"){
      epci <- epci_geo %>% filter(EPCI==epci)
      bbox <- st_bbox(epci)
      epci <- st_crop(epci_geo %>%
                        filter(!(EPCI %in% epci_drom)),
                      bbox)
      communes <- st_crop(communes_geo %>%
                            filter(!(DEPCOM %in% depcom_drom)),
                          bbox)
      result <- list(communes=communes,epci=epci)
    }
  }
  if (!is.null(depcom)){
    if (garder_supra=="non"){
      liste_depcom <- filter(liste_zone,CodeZone==depcom,TypeZone=="Communes") %>% pull(CodeZone)
      communes <- communes_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes)
    }
    if (garder_supra=="oui"){
      com <- communes_geo %>% filter(DEPCOM==depcom)
      bbox <- st_bbox(com)
      communes <- st_crop(communes_geo %>%
                            filter(!(DEPCOM %in% depcom_drom)),
                          bbox)
      result <- list(communes=communes)
    }
  }
  return(result)

}


res<-filtrer_cog_metro_geo(epci=200000438,garder_supra = 'oui')
res<-filtrer_cog_metro_geo(epci='200000438',garder_supra = 'non')

res<-filtrer_cog_metro_geo(depcom='44001',garder_supra = 'oui')
plot(res$communes)
res$epci$EPCI

