#' Préparer les données pour passer d'un type dataframe a un type liste
#'
#' @param .data la table de données a convertir
#' @param typezone le type de zonage
#'
#' @return Renvoie une table de données renommée
#' @importFrom dplyr filter mutate select everything across
#' @importFrom forcats fct_drop
#' @keywords internal


zone_df_to_list <- function(.data, typezone) {
  if (typezone == "communes") {
    d <- .data %>% dplyr::filter(TypeZone == "Communes")
    if (!is.null(d)) {
      d <- d %>%
        dplyr::mutate(NOM_DEPCOM = Zone, DEPCOM = CodeZone) %>%
        dplyr::select(-Zone, -CodeZone, -TypeZone) %>%
        dplyr::select(DEPCOM, NOM_DEPCOM, dplyr::everything()) %>%
        dplyr::mutate(dplyr::across(.cols = c(NOM_DEPCOM, DEPCOM), .fns = forcats::fct_drop))
    }
  }
  if (typezone == "epci") {
    d <- .data %>% dplyr::filter(TypeZone == "Epci")
    if (!is.null(d)) {
      d <- d %>%
        dplyr::mutate(NOM_EPCI = Zone, EPCI = CodeZone) %>%
        dplyr::select(-Zone, -CodeZone, -TypeZone) %>%
        dplyr::select(EPCI, NOM_EPCI, dplyr::everything()) %>%
        dplyr::mutate(dplyr::across(.cols = c(EPCI, NOM_EPCI), .fns = forcats::fct_drop))
    }
  }
  if (typezone == "departements") {
    d <- .data %>% dplyr::filter(TypeZone == "D\u00e9partements")
    if (!is.null(d)) {
      d <- d %>%
        dplyr::mutate(NOM_DEP = Zone, DEP = CodeZone) %>%
        dplyr::select(-Zone, -CodeZone, -TypeZone) %>%
        dplyr::select(DEP, NOM_DEP, dplyr::everything()) %>%
        dplyr::mutate(dplyr::across(.cols = c(DEP, NOM_DEP), .fns = forcats::fct_drop))
    }
  }
  if (typezone == "regions") {
    d <- .data %>% dplyr::filter(TypeZone == "R\u00e9gions")
    if (!is.null(d)) {
      d <- d %>%
        dplyr::mutate(NOM_REG = Zone, REG = CodeZone) %>%
        dplyr::select(-Zone, -CodeZone, -TypeZone) %>%
        dplyr::select(REG, NOM_REG, dplyr::everything()) %>%
        dplyr::mutate(dplyr::across(.cols = c(REG, NOM_REG), .fns = forcats::fct_drop))
    }
  }
  if (typezone == "metro") {
    d <- .data %>% dplyr::filter(Zone == "France m\u00e9tropolitaine")
    if (!is.null(d)) {
      d <- d %>%
        dplyr::select(-Zone, -CodeZone, -TypeZone)
    }
  }
  if (typezone == "metrodrom") {
    d <- .data %>% dplyr::filter(Zone == "France m\u00e9tropolitaine et DROM")
    if (!is.null(d)) {
      d <- d %>%
        dplyr::select(-Zone, -CodeZone, -TypeZone)
    }
  }
  if (typezone == "franceprovince") {
    d <- .data %>% dplyr::filter(Zone == "France de province")
    if (!is.null(d)) {
      d <- d %>%
        dplyr::select(-Zone, -CodeZone, -TypeZone)
    }
  }
  if (typezone == "drom") {
    d <- .data %>% dplyr::filter(Zone == "D\u00e9partements et r\u00e9gions d'outre-mer")
    if (!is.null(d)) {
      d <- d %>%
        dplyr::select(-Zone, -CodeZone, -TypeZone)
    }
  }
  if (nrow(d) == 0) {d <- NULL}
  return(d)
}

#' Convertir les données du COG d'un type dataframe a un type liste
#'
#' @param .data la liste de données a convertir
#'
#' @return Renvoie une liste de dataframe
#' @export
#' @importFrom purrr map set_names
cog_df_to_list <- function(.data) {
  purrr::map(c("communes", "epci", "departements", "regions", "metro", "metrodrom", "franceprovince", "drom"), ~ zone_df_to_list(.data, typezone = .x)) %>%
    purrr::set_names(c("communes", "epci", "departements", "regions", "metro", "metrodrom", "franceprovince", "drom"))
}
