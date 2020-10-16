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
  # on sélectionne les tables de départ suivant que le territoire est en métropole ou en drom
  if (!(reg %in% reg_drom)) {
    reg_geo <- regions_metro_geo
    dep_geo <- departements_metro_geo
    epci_geo <- epci_metro_geo
    com_geo <- communes_metro_geo
  }
  if (reg == '01') {
    reg_geo <- regions_971_geo
    dep_geo <- departements_971_geo
    epci_geo <- epci_971_geo
    com_geo <- communes_971_geo
  }
  if (!is.null(reg)){
    if (!garder_supra) {
      liste_dep <- filter(liste_zone,str_detect(REG,reg),TypeZone=="D\u00e9partements") %>% pull(CodeZone)
      liste_epci <- filter(liste_zone,str_detect(REG,reg),TypeZone=="Epci") %>% pull(CodeZone)
      liste_depcom <- filter(liste_zone,str_detect(REG,reg),TypeZone=="Communes") %>% pull(CodeZone)

      reg <- reg_geo %>% filter(REG==reg)
      dep <- dep_geo %>% filter(DEP %in% liste_dep)
      epci <- epci_geo %>% filter(EPCI %in% liste_epci)
      communes <- com_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes,epci=epci,departements=dep,regions=reg)
    }
    if (garder_supra){
      reg <- reg_geo %>% filter(REG==reg)
      bbox <- st_bbox(reg)
      reg <- st_crop(reg_geo,
                     bbox)
      dep <- st_crop(dep_geo,
                     bbox)
      epci <- st_crop(epci_geo,
                      bbox)
      communes <- st_crop(com_geo,
                          bbox)
      result <- list(communes=communes,epci=epci,departements=dep,regions=reg)
    }
    return(result)

  }
  if (!is.null(dep)){
    if (!garder_supra){
      liste_epci <- filter(liste_zone,str_detect(DEP,dep),TypeZone=="Epci") %>% pull(CodeZone)
      liste_depcom <- filter(liste_zone,str_detect(DEP,dep),TypeZone=="Communes") %>% pull(CodeZone)
      dep <- dep_geo %>% filter(DEP == dep)
      epci <- epci_geo %>% filter(EPCI %in% liste_epci)
      communes <- com_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes,epci=epci,departements=dep)
    }
    if (garder_supra){
      dep <- dep_geo %>% filter(DEP==dep)
      bbox <- st_bbox(dep)
      dep <- st_crop(dep_geo,
                     bbox)
      epci <- st_crop(epci_geo,
                      bbox)
      communes <- st_crop(com_geo,
                          bbox)
      result <- list(communes=communes,epci=epci,departements=dep)
    }
    return(result)

  }
  if (!is.null(epci)){
    if (!garder_supra){
      liste_depcom <- filter(liste_zone,EPCI==epci,TypeZone=="Communes") %>% pull(CodeZone)
      epci <- epci_geo %>% filter(EPCI == epci)
      communes <- com_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes,epci=epci)
    }
    if (garder_supra){
      epci <- epci_geo %>% filter(EPCI==epci)
      bbox <- st_bbox(epci)
      epci <- st_crop(epci_geo,
                      bbox)
      communes <- st_crop(com_geo,
                          bbox)
      result <- list(communes=communes,epci=epci)
    }
  }
  if (!is.null(depcom)){
    if (!garder_supra){
      liste_depcom <- filter(liste_zone,CodeZone==depcom,TypeZone=="Communes") %>% pull(CodeZone)
      communes <- com_geo %>% filter(DEPCOM %in% liste_depcom)
      result <- list(communes=communes)
    }
    if (garder_supra){
      com <- com_geo %>% filter(DEPCOM==depcom)
      bbox <- st_bbox(com)
      communes <- st_crop(com_geo,
                          bbox)
      result <- list(communes=communes)
    }
  }
  return(result)

}

#' Obtenir les cartes en fonction de la localisation en métropole ou dans les DROM
#'
#' @param depcom la commune sur laquelle filtrer les donn\encoding{é}es
#' @param epci l'epci sur lequel filtrer les donn\encoding{é}es
#' @param dep le departement sur lequel filtrer les donn\encoding{é}es
#' @param reg la region sur laquelle filtrer les donn\encoding{é}es
#'
#' @return une liste de spatial dataframe
#' @export
#'
#' @examples
get_map <- function(depcom=NULL,
                    epci=NULL,
                    dep=NULL,
                    reg=NULL) {
  reg_drom <- c('01','02','03','04','06')
  dep_drom <- c('971','972','973','974','976')
  epci_drom <- liste_zone %>% filter(str_detect(DEP,dep_drom),TypeZone=='Epci') %>% pull(CodeZone)
  epci_drom <- liste_zone %>% filter(str_detect(DEP,dep_drom),TypeZone=='Epci') %>% pull(CodeZone)
  epci_971 <- liste_zone %>% filter(str_detect(DEP,'971'),TypeZone=='Epci') %>% pull(CodeZone)
  epci_972 <- liste_zone %>% filter(str_detect(DEP,'972'),TypeZone=='Epci') %>% pull(CodeZone)
  epci_973 <- liste_zone %>% filter(str_detect(DEP,'973'),TypeZone=='Epci') %>% pull(CodeZone)
  epci_974 <- liste_zone %>% filter(str_detect(DEP,'974'),TypeZone=='Epci') %>% pull(CodeZone)
  epci_976 <- liste_zone %>% filter(str_detect(DEP,'976'),TypeZone=='Epci') %>% pull(CodeZone)
  depcom_drom <- liste_zone %>% filter(str_detect(DEP,dep_drom),TypeZone=='Communes') %>% pull(CodeZone)
  depcom_971 <- liste_zone %>% filter(str_detect(DEP,'971'),TypeZone=='Communes') %>% pull(CodeZone)
  depcom_972 <- liste_zone %>% filter(str_detect(DEP,'972'),TypeZone=='Communes') %>% pull(CodeZone)
  depcom_973 <- liste_zone %>% filter(str_detect(DEP,'973'),TypeZone=='Communes') %>% pull(CodeZone)
  depcom_974 <- liste_zone %>% filter(str_detect(DEP,'974'),TypeZone=='Communes') %>% pull(CodeZone)
  depcom_976 <- liste_zone %>% filter(str_detect(DEP,'976'),TypeZone=='Communes') %>% pull(CodeZone)
  if (not_null(reg)){
    if (reg %not_in% reg_drom) {
      reg_geo <- regions_metro_geo
      dep_geo <- departements_metro_geo
      epci_geo <- epci_metro_geo
      com_geo <- communes_metro_geo
    }
    if (reg ==01) {
      reg_geo <- regions_971_geo
      dep_geo <- departements_971_geo
      epci_geo <- epci_971_geo
      com_geo <- communes_971_geo
    }
    if (reg == 02) {
      reg_geo <- regions_972_geo
      dep_geo <- departements_972_geo
      epci_geo <- epci_972_geo
      com_geo <- communes_972_geo
    }
    if (reg == 03) {
      reg_geo <- regions_973_geo
      dep_geo <- departements_973_geo
      epci_geo <- epci_973_geo
      com_geo <- communes_973_geo
    }
    if (reg == 04) {
      reg_geo <- regions_974_geo
      dep_geo <- departements_974_geo
      epci_geo <- epci_974_geo
      com_geo <- communes_974_geo
    }
    if (reg == 06) {
      reg_geo <- regions_976_geo
      dep_geo <- departements_976_geo
      epci_geo <- epci_976_geo
      com_geo <- communes_976_geo
    }
  }
  if (not_null(dep)) {
    if (dep %not_in% dep_drom) {
      reg_geo <- regions_metro_geo
      dep_geo <- departements_metro_geo
      epci_geo <- epci_metro_geo
      com_geo <- communes_metro_geo
    }
    if (dep == '971') {
      reg_geo <- regions_971_geo
      dep_geo <- departements_971_geo
      epci_geo <- epci_971_geo
      com_geo <- communes_971_geo
    }
    if (dep == '972') {
      reg_geo <- regions_972_geo
      dep_geo <- departements_972_geo
      epci_geo <- epci_972_geo
      com_geo <- communes_972_geo
    }
    if (dep == '973') {
      reg_geo <- regions_973_geo
      dep_geo <- departements_973_geo
      epci_geo <- epci_973_geo
      com_geo <- communes_973_geo
    }
    if (dep == '974') {
      reg_geo <- regions_974_geo
      dep_geo <- departements_974_geo
      epci_geo <- epci_974_geo
      com_geo <- communes_974_geo
    }
    if (dep == '976') {
      reg_geo <- regions_976_geo
      dep_geo <- departements_976_geo
      epci_geo <- epci_976_geo
      com_geo <- communes_976_geo
    }
  }
  if (not_null(depcom)) {
    if (epci %not_in% depcom_drom) {
      reg_geo <- regions_metro_geo
      dep_geo <- departements_metro_geo
      epci_geo <- epci_metro_geo
      com_geo <- communes_metro_geo
    }
    if (depcom %in% depcom_971) {
      reg_geo <- regions_971_geo
      dep_geo <- departements_971_geo
      epci_geo <- epci_971_geo
      com_geo <- communes_971_geo
    }
    if (depcom %in% depcom_972) {
      reg_geo <- regions_972_geo
      dep_geo <- departements_972_geo
      epci_geo <- epci_972_geo
      com_geo <- communes_972_geo
    }
    if (depcom %in% depcom_973) {
      reg_geo <- regions_973_geo
      dep_geo <- departements_973_geo
      epci_geo <- epci_973_geo
      com_geo <- communes_973_geo
    }
    if (depcom %in% depcom_974) {
      reg_geo <- regions_974_geo
      dep_geo <- departements_974_geo
      epci_geo <- epci_974_geo
      com_geo <- communes_974_geo
    }
    if (depcom %in% depcom_976) {
      reg_geo <- regions_976_geo
      dep_geo <- departements_976_geo
      epci_geo <- epci_976_geo
      com_geo <- communes_976_geo
    }
  }
  return(list(com_geo = com_geo, epci_geo=epci_geo,dep_geo = dep_geo, reg_geo = reg_geo))
}
