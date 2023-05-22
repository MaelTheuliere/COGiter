#' Preparer les données pour passer d'un type liste a un type dataframe
#'
#' @param .data la table de données a convertir
#' @param typezone le type de zonage
#'
#' @return Renvoie une table de données renommée
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyselect everything
#' @keywords internal
zone_list_to_df <- function(.data, typezone) {
  if (typezone == "communes") {
    if (is.null(.data)){d <- NULL}
    else {
      d <- .data %>%
        dplyr::mutate(Zone = NOM_DEPCOM, CodeZone = DEPCOM, TypeZone = "Communes") %>%
        dplyr::select(-NOM_DEPCOM, -DEPCOM) %>%
        dplyr::select(TypeZone, Zone, CodeZone, tidyselect::everything())
    }
  }
  if (typezone == "epci") {
    if (is.null(.data)){d <- NULL}
    else {
      d <- .data %>%
        dplyr::mutate(Zone = NOM_EPCI, CodeZone = EPCI, TypeZone = "Epci") %>%
        dplyr::select(-NOM_EPCI, -EPCI) %>%
        dplyr::select(TypeZone, Zone, CodeZone, tidyselect::everything())
    }
  }
  if (typezone == "departements") {
    if (is.null(.data)){d <- NULL}
    else {
      d <- .data %>%
        dplyr::mutate(Zone = NOM_DEP, CodeZone = DEP, TypeZone = "D\u00e9partements") %>%
        dplyr::select(-NOM_DEP, -DEP) %>%
        dplyr::select(TypeZone, Zone, CodeZone, tidyselect::everything())
    }
  }
  if (typezone == "regions") {
    if (is.null(.data)){d <- NULL}
    else {
      d <- .data %>%
        dplyr::mutate(Zone = NOM_REG, CodeZone = REG, TypeZone = "R\u00e9gions") %>%
        dplyr::select(-NOM_REG, -REG) %>%
        dplyr::select(TypeZone, Zone, CodeZone, tidyselect::everything())
    }
  }
  if (typezone == "metro") {
    if (is.null(.data)){d <- NULL}
    else {
      d <- .data %>%
        dplyr::mutate(Zone = "France m\u00e9tropolitaine", CodeZone = "FRMETRO", TypeZone = "France") %>%
        dplyr::select(TypeZone, Zone, CodeZone, tidyselect::everything())
    }
  }
  if (typezone == "metrodrom") {
    if (is.null(.data)){d <- NULL}
    else {
      d <- .data %>%
        dplyr::mutate(Zone = "France m\u00e9tropolitaine et DROM", CodeZone = "FRMETRODROM", TypeZone = "France") %>%
        dplyr::select(TypeZone, Zone, CodeZone, tidyselect::everything())
    }
  }
  if (typezone == "franceprovince") {
    if (is.null(.data)){d <- NULL}
    else {
      d <- .data %>%
        dplyr::mutate(Zone = "France de province", CodeZone = "FRPROV", TypeZone = "France") %>%
        dplyr::select(TypeZone, Zone, CodeZone, tidyselect::everything())
    }
  }
  if (typezone == "drom") {
    if (is.null(.data)){d <- NULL}
    else {
      d <- .data %>%
        dplyr::mutate(Zone = "D\u00e9partements et r\u00e9gions d'outre-mer", CodeZone = "DROM", TypeZone = "France") %>%
        dplyr::select(TypeZone, Zone, CodeZone, tidyselect::everything())
    }
  }
  return(d)
}

#' Convertir les donnees du COG d'un type liste a un type dataframe
#'
#' @param list la liste de donnees a convertir
#'
#' @return Renvoie une table de donnees
#' @export
#' @importFrom purrr map2_df
#' @importFrom dplyr mutate across
cog_list_to_df <- function(list) {
  purrr::map2_df(list, names(list), ~ zone_list_to_df(.data  =  .x, typezone  =  .y)) %>%
    dplyr::mutate(dplyr::across(.cols = c(TypeZone, Zone, CodeZone), .fns = as.factor))
}
