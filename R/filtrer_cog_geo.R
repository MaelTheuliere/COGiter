#' Filtrer les fonds de carte sur un territoire métropolitain
#'
#' @param depcom la commune sur laquelle filtrer les données
#' @param epci l'epci sur lequel filtrer les données
#' @param dep le departement sur lequel filtrer les données
#' @param reg la region sur laquelle filtrer les données
#' @param garder_supra TRUE si on souhaite une carte centrée sur le territoire mais avec les territoires environnant visible, non si on souhaite garder que le territoire sélectionné
#' @param lg_buffer si garder_supra est a true, la largeur du buffer a garder autour du territoire (metres)
#' @return une liste de spatial dataframe
#' @export
#' @importFrom dplyr filter pull
#' @importFrom sf st_bbox st_crop st_buffer
#' @importFrom attempt stop_if_any
#' @examples
#' nantes_metropole <- filtrer_cog_geo(epci = '244400404')
#' plot(nantes_metropole$communes)

filtrer_cog_geo <- function(depcom = NULL,
                            epci = NULL,
                            dep = NULL,
                            reg = NULL,
                            garder_supra = FALSE,
                            lg_buffer = 0) {
  attempt::stop_if_any(c(depcom, epci, dep, reg), is.numeric, msg = "Les param\u00e8tres doivent \u00eatre des chaines de caract\u00e8re")
  # on sélectionne les tables de départ suivant que le territoire est en métropole ou en drom
  map <- get_map(depcom, epci, dep, reg)
  com_geo <- map$com_geo
  epci_geo <- map$epci_geo
  dep_geo <- map$dep_geo
  reg_geo <- map$reg_geo
  if (!is.null(reg)) {
    if (!garder_supra) {
      liste_dep <- dplyr::filter(COGiter::liste_zone, grepl(reg, REG), TypeZone == "D\u00e9partements") %>% dplyr::pull(CodeZone)
      liste_epci <- dplyr::filter(COGiter::liste_zone, grepl(reg, REG), TypeZone == "Epci") %>% dplyr::pull(CodeZone)
      liste_depcom <- dplyr::filter(COGiter::liste_zone, grepl(reg, REG), TypeZone == "Communes") %>% dplyr::pull(CodeZone)

      reg <- reg_geo %>% dplyr::filter(REG == reg)
      dep <- dep_geo %>% dplyr::filter(DEP %in% liste_dep)
      epci <- epci_geo %>% dplyr::filter(EPCI %in% liste_epci)
      communes <- com_geo %>% dplyr::filter(DEPCOM %in% liste_depcom)
      result <- list(communes = communes, epci = epci, departements = dep, regions = reg)
    }
    if (garder_supra) {
      reg <- reg_geo %>% dplyr::filter(REG == reg)

      bbox <- sf::st_buffer(reg, dist = lg_buffer) %>%
        sf::st_bbox()
      reg <- sf::st_crop(
        reg_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      dep <- sf::st_crop(
        dep_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      epci <- sf::st_crop(
        epci_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      communes <- sf::st_crop(
        com_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      result <- list(communes = communes, epci = epci, departements = dep, regions = reg)
    }
    return(result)
  }
  if (!is.null(dep)) {
    if (!garder_supra) {
      liste_epci <- dplyr::filter(COGiter::liste_zone, grepl(dep, DEP), TypeZone == "Epci") %>% dplyr::pull(CodeZone)
      liste_depcom <- dplyr::filter(COGiter::liste_zone, grepl(dep, DEP), TypeZone == "Communes") %>% dplyr::pull(CodeZone)
      dep <- dep_geo %>% dplyr::filter(DEP == dep)
      epci <- epci_geo %>% dplyr::filter(EPCI %in% liste_epci)
      communes <- com_geo %>% dplyr::filter(DEPCOM %in% liste_depcom)
      result <- list(communes = communes, epci = epci, departements = dep)
    }
    if (garder_supra) {
      dep <- dep_geo %>% dplyr::filter(DEP == dep)
      bbox <- sf::st_buffer(dep, dist = lg_buffer) %>%
        sf::st_bbox()
      dep <- sf::st_crop(
        dep_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      epci <- sf::st_crop(
        epci_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      communes <- sf::st_crop(
        com_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      result <- list(communes = communes, epci = epci, departements = dep)
    }
    return(result)
  }
  if (!is.null(epci)) {
    if (!garder_supra) {
      liste_depcom <- dplyr::filter(COGiter::liste_zone, EPCI == epci, TypeZone == "Communes") %>% dplyr::pull(CodeZone)
      epci <- epci_geo %>% dplyr::filter(EPCI == epci)
      communes <- com_geo %>% dplyr::filter(DEPCOM %in% liste_depcom)
      result <- list(communes = communes, epci = epci)
    }
    if (garder_supra) {
      epci <- epci_geo %>% dplyr::filter(EPCI == epci)
      bbox <- sf::st_buffer(epci, dist = lg_buffer) %>%
        sf::st_bbox()
      epci <- sf::st_crop(
        epci_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      communes <- sf::st_crop(
        com_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      result <- list(communes = communes, epci = epci)
    }
  }
  if (!is.null(depcom)) {
    if (!garder_supra) {
      liste_depcom <- dplyr::filter(COGiter::liste_zone, CodeZone == depcom, TypeZone == "Communes") %>% dplyr::pull(CodeZone)
      communes <- com_geo %>% dplyr::filter(DEPCOM %in% liste_depcom)
      result <- list(communes = communes)
    }
    if (garder_supra) {
      com <- com_geo %>% dplyr::filter(DEPCOM == depcom)
      bbox <- sf::st_buffer(com, dist = lg_buffer) %>%
        sf::st_bbox()
      communes <- sf::st_crop(
        com_geo,
        bbox
      ) %>%
        sf::st_buffer(dist = 0)
      result <- list(communes = communes)
    }
  }
  return(result)
}

#' Obtenir les cartes en fonction de la localisation en métropole ou dans les DROM
#'
#' @param depcom la commune sur laquelle filtrer les données
#' @param epci l'epci sur lequel filtrer les données
#' @param dep le departement sur lequel filtrer les données
#' @param reg la region sur laquelle filtrer les données
#'
#' @importFrom dplyr filter pull
#' @return une liste de spatial dataframe
#' @export
#'
#' @examples
#' get_map(epci = "244400404")
get_map <- function(depcom = NULL,
                    epci = NULL,
                    dep = NULL,
                    reg = NULL) {
  attempt::stop_if_any(c(depcom, epci, dep, reg), is.numeric, msg = "Les param\u00e8tres doivent \u00eatre des chaines de caract\u00e8re")
  reg_drom <- "01|02|03|04|06"
  dep_drom <- "971|972|973|974|976"
  epci_drom <- COGiter::liste_zone %>%
    dplyr::filter(grepl(dep_drom, DEP), TypeZone == "Epci") %>%
    dplyr::pull(CodeZone)
  epci_971 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("971", DEP), TypeZone == "Epci") %>%
    dplyr::pull(CodeZone)
  epci_972 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("972", DEP), TypeZone == "Epci") %>%
    dplyr::pull(CodeZone)
  epci_973 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("973", DEP), TypeZone == "Epci") %>%
    dplyr::pull(CodeZone)
  epci_974 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("974", DEP), TypeZone == "Epci") %>%
    dplyr::pull(CodeZone)
  epci_976 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("976", DEP), TypeZone == "Epci") %>%
    dplyr::pull(CodeZone)
  depcom_drom <- COGiter::liste_zone %>%
    dplyr::filter(grepl(dep_drom, DEP), TypeZone == "Communes") %>%
    dplyr::pull(CodeZone)
  depcom_971 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("971", DEP), TypeZone == "Communes") %>%
    dplyr::pull(CodeZone)
  depcom_972 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("972", DEP), TypeZone == "Communes") %>%
    dplyr::pull(CodeZone)
  depcom_973 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("973", DEP), TypeZone == "Communes") %>%
    dplyr::pull(CodeZone)
  depcom_974 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("974", DEP), TypeZone == "Communes") %>%
    dplyr::pull(CodeZone)
  depcom_976 <- COGiter::liste_zone %>%
    dplyr::filter(grepl("976", DEP), TypeZone == "Communes") %>%
    dplyr::pull(CodeZone)
  if (not_null(reg)) {
    if (reg %not_in% reg_drom) {
      reg_geo <- COGiter::regions_metro_geo
      dep_geo <- COGiter::departements_metro_geo
      epci_geo <- COGiter::epci_metro_geo
      com_geo <- COGiter::communes_metro_geo
    }
    if (reg == "01") {
      reg_geo <- COGiter::regions_971_geo %>%
        st_transform(5490)
      dep_geo <- COGiter::departements_971_geo %>%
        st_transform(5490)
      epci_geo <- COGiter::epci_971_geo %>%
        st_transform(5490)
      com_geo <- COGiter::communes_971_geo %>%
        st_transform(5490)
    }
    if (reg == "02") {
      reg_geo <- COGiter::regions_972_geo %>%
        st_transform(5490)
      dep_geo <- COGiter::departements_972_geo %>%
        st_transform(5490)
      epci_geo <- COGiter::epci_972_geo %>%
        st_transform(5490)
      com_geo <- COGiter::communes_972_geo %>%
        st_transform(5490)
    }
    if (reg == "03") {
      reg_geo <- COGiter::regions_973_geo %>%
        st_transform(2972)
      dep_geo <- COGiter::departements_973_geo %>%
        st_transform(2972)
      epci_geo <- COGiter::epci_973_geo %>%
        st_transform(2972)
      com_geo <- COGiter::communes_973_geo %>%
        st_transform(2972)
    }
    if (reg == "04") {
      reg_geo <- COGiter::regions_974_geo %>%
        st_transform(2975)
      dep_geo <- COGiter::departements_974_geo %>%
        st_transform(2975)
      epci_geo <- COGiter::epci_974_geo %>%
        st_transform(2975)
      com_geo <- COGiter::communes_974_geo %>%
        st_transform(2975)
    }
    if (reg == "06") {
      reg_geo <- COGiter::regions_976_geo %>%
        st_transform(4471)
      dep_geo <- COGiter::departements_976_geo %>%
        st_transform(4471)
      epci_geo <- COGiter::epci_976_geo %>%
        st_transform(4471)
      com_geo <- COGiter::communes_976_geo %>%
        st_transform(4471)
    }
  }
  if (not_null(dep)) {
    if (dep %not_in% dep_drom) {
      reg_geo <- COGiter::regions_metro_geo
      dep_geo <- COGiter::departements_metro_geo
      epci_geo <- COGiter::epci_metro_geo
      com_geo <- COGiter::communes_metro_geo
    }
    if (dep == "971") {
      reg_geo <- COGiter::regions_971_geo %>%
        st_transform(5490)
      dep_geo <- COGiter::departements_971_geo %>%
        st_transform(5490)
      epci_geo <- COGiter::epci_971_geo %>%
        st_transform(5490)
      com_geo <- COGiter::communes_971_geo %>%
        st_transform(5490)
    }
    if (dep == "972") {
      reg_geo <- COGiter::regions_972_geo %>%
        st_transform(5490)
      dep_geo <- COGiter::departements_972_geo %>%
        st_transform(5490)
      epci_geo <- COGiter::epci_972_geo %>%
        st_transform(5490)
      com_geo <- COGiter::communes_972_geo %>%
        st_transform(5490)
    }
    if (dep == "973") {
      reg_geo <- COGiter::regions_973_geo %>%
        st_transform(2972)
      dep_geo <- COGiter::departements_973_geo %>%
        st_transform(2972)
      epci_geo <- COGiter::epci_973_geo %>%
        st_transform(2972)
      com_geo <- COGiter::communes_973_geo %>%
        st_transform(2972)
    }
    if (dep == "974") {
      reg_geo <- COGiter::regions_974_geo %>%
        st_transform(2975)
      dep_geo <- COGiter::departements_974_geo %>%
        st_transform(2975)
      epci_geo <- COGiter::epci_974_geo %>%
        st_transform(2975)
      com_geo <- COGiter::communes_974_geo %>%
        st_transform(2975)
    }
    if (dep == "976") {
      reg_geo <- COGiter::regions_976_geo %>%
        st_transform(4471)
      dep_geo <- COGiter::departements_976_geo %>%
        st_transform(4471)
      epci_geo <- COGiter::epci_976_geo %>%
        st_transform(4471)
      com_geo <- COGiter::communes_976_geo %>%
        st_transform(4471)
    }
  }
  if (not_null(epci)) {
    if (epci %not_in% epci_drom) {
      reg_geo <- COGiter::regions_metro_geo
      dep_geo <- COGiter::departements_metro_geo
      epci_geo <- COGiter::epci_metro_geo
      com_geo <- COGiter::communes_metro_geo
    }
    if (epci %in% epci_971) {
      reg_geo <- COGiter::regions_971_geo %>%
        st_transform(5490)
      dep_geo <- COGiter::departements_971_geo %>%
        st_transform(5490)
      epci_geo <- COGiter::epci_971_geo %>%
        st_transform(5490)
      com_geo <- COGiter::communes_971_geo %>%
        st_transform(5490)
    }
    if (epci %in% epci_972) {
      reg_geo <- COGiter::regions_972_geo %>%
        st_transform(5490)
      dep_geo <- COGiter::departements_972_geo %>%
        st_transform(5490)
      epci_geo <- COGiter::epci_972_geo %>%
        st_transform(5490)
      com_geo <- COGiter::communes_972_geo %>%
        st_transform(5490)
    }
    if (epci %in% epci_973) {
      reg_geo <- COGiter::regions_973_geo %>%
        st_transform(2972)
      dep_geo <- COGiter::departements_973_geo %>%
        st_transform(2972)
      epci_geo <- COGiter::epci_973_geo %>%
        st_transform(2972)
      com_geo <- COGiter::communes_973_geo %>%
        st_transform(2972)
    }
    if (epci %in% epci_974) {
      reg_geo <- COGiter::regions_974_geo %>%
        st_transform(2975)
      dep_geo <- COGiter::departements_974_geo %>%
        st_transform(2975)
      epci_geo <- COGiter::epci_974_geo %>%
        st_transform(2975)
      com_geo <- COGiter::communes_974_geo %>%
        st_transform(2975)
    }
    if (epci %in% epci_976) {
      reg_geo <- COGiter::regions_976_geo %>%
        st_transform(4471)
      dep_geo <- COGiter::departements_976_geo %>%
        st_transform(4471)
      epci_geo <- COGiter::epci_976_geo %>%
        st_transform(4471)
      com_geo <- COGiter::communes_976_geo %>%
        st_transform(4471)
    }
  }
  if (not_null(depcom)) {
    if (depcom %not_in% depcom_drom) {
      reg_geo <- COGiter::regions_metro_geo
      dep_geo <- COGiter::departements_metro_geo
      epci_geo <- COGiter::epci_metro_geo
      com_geo <- COGiter::communes_metro_geo
    }
    if (depcom %in% depcom_971) {
      reg_geo <- COGiter::regions_971_geo %>%
        st_transform(5490)
      dep_geo <- COGiter::departements_971_geo %>%
        st_transform(5490)
      epci_geo <- COGiter::epci_971_geo %>%
        st_transform(5490)
      com_geo <- COGiter::communes_971_geo %>%
        st_transform(5490)
    }
    if (depcom %in% depcom_972) {
      reg_geo <- COGiter::regions_972_geo %>%
        st_transform(5490)
      dep_geo <- COGiter::departements_972_geo %>%
        st_transform(5490)
      epci_geo <- COGiter::epci_972_geo %>%
        st_transform(5490)
      com_geo <- COGiter::communes_972_geo %>%
        st_transform(5490)
    }
    if (depcom %in% depcom_973) {
      reg_geo <- COGiter::regions_973_geo %>%
        st_transform(2972)
      dep_geo <- COGiter::departements_973_geo %>%
        st_transform(2972)
      epci_geo <- COGiter::epci_973_geo %>%
        st_transform(2972)
      com_geo <- COGiter::communes_973_geo %>%
        st_transform(2972)
    }
    if (depcom %in% depcom_974) {
      reg_geo <- COGiter::regions_974_geo %>%
        st_transform(2975)
      dep_geo <- COGiter::departements_974_geo %>%
        st_transform(2975)
      epci_geo <- COGiter::epci_974_geo %>%
        st_transform(2975)
      com_geo <- COGiter::communes_974_geo %>%
        st_transform(2975)
    }
    if (depcom %in% depcom_976) {
      reg_geo <- COGiter::regions_976_geo %>%
        st_transform(4471)
      dep_geo <- COGiter::departements_976_geo %>%
        st_transform(4471)
      epci_geo <- COGiter::epci_976_geo %>%
        st_transform(4471)
      com_geo <- COGiter::communes_976_geo %>%
        st_transform(4471)
    }
  }
  return(list(com_geo = com_geo, epci_geo = epci_geo, dep_geo = dep_geo, reg_geo = reg_geo))
}
