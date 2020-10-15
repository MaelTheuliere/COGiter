#' filtrer les fonds de carte sur un territoire métropolitain
#'
#' @param depcom la commune sur laquelle filtrer les donn\encoding{é}es
#' @param epci l'epci sur lequel filtrer les donn\encoding{é}es
#' @param dep le departement sur lequel filtrer les donn\encoding{é}es
#' @param reg la region sur laquelle filtrer les donn\encoding{é}es
#' @param metro
#' @param garder_supra TRUE si on souhaite une carte centrée sur le territoire mais avec les territoires environnant visible, non si on souhaite garder que le territoire sélectionné
#' @return Renvoie une table de donnees filtrées
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom dplyr pull
#' @importFrom sf st_bbox
#' @importFrom sf st_crop
#' @importFrom attempt stop_if_any
#' @encoding UTF-8

filtrer_cog_metro_geo <- function(depcom=NULL,
                                  epci=NULL,
                                  dep=NULL,
                                  reg=NULL,
                                  garder_supra=FALSE
) {
  stop_if_any(c(depcom,epci,dep,reg),is.numeric,msg="Les paramètres doivent être des chaines de caractère")
  reg_drom <- c('01','02','03','04','06')
  dep_drom <- c('971','972','973','974','976')
  epci_drom <- liste_zone %>% filter(str_detect(DEP,dep_drom),TypeZone=='Epci') %>% pull(CodeZone)
  depcom_drom <- liste_zone %>% filter(str_detect(DEP,dep_drom),TypeZone=='Communes') %>% pull(CodeZone)
  if (!is.null(reg)){
    if (!garder_supra) {
      liste_dep <- filter(liste_zone,str_detect(REG,reg),TypeZone=="D\u00e9partements") %>% pull(CodeZone)
      liste_epci <- filter(liste_zone,str_detect(REG,reg),TypeZone=="Epci") %>% pull(CodeZone)
      liste_depcom <- filter(liste_zone,str_detect(REG,reg),TypeZone=="Communes") %>% pull(CodeZone)

      reg <- regions_metro_geo %>% filter(REG==reg)
      dep <- departements_metro_geo %>% filter(DEP %in% liste_dep)
      epci <- epci_metro_geo %>% filter(EPCI %in% liste_epci)
      communes <- communes_metro_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes,epci=epci,departements=dep,regions=reg)
    }
    if (garder_supra){
      reg <- regions_metro_geo %>% filter(REG==reg)
      bbox <- st_bbox(reg)
      reg <- st_crop(regions_metro_geo,
                     bbox)
      dep <- st_crop(departements_metro_geo,
                     bbox)
      epci <- st_crop(epci_metro_geo,
                      bbox)
      communes <- st_crop(communes_metro_geo,
                          bbox)
      result <- list(communes=communes,epci=epci,departements=dep,regions=reg)
    }
    return(result)

  }
  if (!is.null(dep)){
    if (!garder_supra){
      liste_epci <- filter(liste_zone,str_detect(DEP,dep),TypeZone=="Epci") %>% pull(CodeZone)
      liste_depcom <- filter(liste_zone,str_detect(DEP,dep),TypeZone=="Communes") %>% pull(CodeZone)
      dep <- departements_metro_geo %>% filter(DEP == dep)
      epci <- epci_metro_geo %>% filter(EPCI %in% liste_epci)
      communes <- communes_metro_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes,epci=epci,departements=dep)
    }
    if (garder_supra){
      dep <- departements_metro_geo %>% filter(DEP==dep)
      bbox <- st_bbox(dep)
      dep <- st_crop(departements_metro_geo,
                     bbox)
      epci <- st_crop(epci_metro_geo,
                      bbox)
      communes <- st_crop(communes_metro_geo,
                          bbox)
      result <- list(communes=communes,epci=epci,departements=dep)
    }
    return(result)

  }
  if (!is.null(epci)){
    if (!garder_supra){
      liste_depcom <- filter(liste_zone,EPCI==epci,TypeZone=="Communes") %>% pull(CodeZone)
      epci <- epci_metro_geo %>% filter(EPCI == epci)
      communes <- communes_metro_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes,epci=epci)
    }
    if (garder_supra){
      epci <- epci_metro_geo %>% filter(EPCI==epci)
      bbox <- st_bbox(epci)
      epci <- st_crop(epci_metro_geo,
                      bbox)
      communes <- st_crop(communes_metro_geo,
                          bbox)
      result <- list(communes=communes,epci=epci)
    }
  }
  if (!is.null(depcom)){
    if (!garder_supra){
      liste_depcom <- filter(liste_zone,CodeZone==depcom,TypeZone=="Communes") %>% pull(CodeZone)
      communes <- communes_metro_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes)
    }
    if (garder_supra){
      com <- communes_metro_geo %>% filter(DEPCOM==depcom)
      bbox <- st_bbox(com)
      communes <- st_crop(communes_metro_geo,
                          bbox)
      result <- list(communes=communes)
    }
  }
  return(result)

}

