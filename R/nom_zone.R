# Pour retrouver les code zone et nom zone des totaux qui ne sont pas présent dans liste_zone
augmenter_liste_zone <- function() {
  augment <- tibble::tribble(
    ~CodeZone,~Zone,~TypeZone,~EPCI,~NATURE_EPCI,~DEP,~REG,
    "FRMETRO","France m\u00e9tropolitaine","France",NA,NA,NA,NA,
    "FRMETRODROM","France m\u00e9tropolitaine et DROM","France",NA,NA,NA,NA,
    "FRPROV","France de province","France",NA,NA,NA,NA,
    "DROM","D\u00e9partements et r\u00e9gions d'outre-mer","France",NA,NA,NA,NA
  )
  res <- liste_zone %>%
    dplyr::bind_rows(augment)
  return(res)
}
#' Obtenir le nom d'une zone
#'
#' @param type_zone Type de Zone
#' @param code_zone Code de la Zone
#'
#' @return une chaîne de caractère
#' @importFrom dplyr filter pull
#' @export
#'
#' @examples
#' nom_zone(c("Communes","Communes"),c("75056","44109"))
nom_zone <- function(type_zone,code_zone) {
  filter <- data.frame(TypeZone = type_zone, CodeZone = code_zone)
  liste_zone <- augmenter_liste_zone()
  res <- filter %>%
    dplyr::left_join(liste_zone) %>%
    dplyr::pull(Zone)
  return(res)
}
#' Obtenir le code d'une zone
#'
#' @param type_zone Type de Zone
#' @param zone Nom de la Zone
#'
#' @return une chaîne de caractère
#' @importFrom dplyr filter pull
#' @export
#'
#' @examples
#' code_zone("Communes","Nantes")
code_zone <- function(type_zone,zone) {
  filter <- data.frame(TypeZone = type_zone, Zone = zone)
  liste_zone <- augmenter_liste_zone()
  res <- filter %>%
    dplyr::left_join(liste_zone) %>%
    dplyr::pull(CodeZone)
  return(res)
}

#' Trouver une zone à partir d'une chaîne de caractère
#'
#' @param pattern la chaîne de caractère à identifier
#'
#' @return un tibble
#' @export
#' @importFrom dplyr filter select
#' @importFrom stringr str_detect
#'
#' @examples
#' trouver_zone(pattern = "Belleville")
trouver_zone <- function(pattern) {
  liste_zone <- augmenter_liste_zone()
  res <- liste_zone %>%
    dplyr::filter(stringr::str_detect(Zone,pattern)) %>%
    dplyr::select(TypeZone,CodeZone,Zone)
  return(res)
}
