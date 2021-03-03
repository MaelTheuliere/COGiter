#' Obtenir une commune avec son contour geo de l'api geo.api.gouv.fr
#'
#' @param code_commune le code commune insee
#'
#' @return un spatial dataframe
#' @export
#' @importFrom glue glue
#' @importFrom httr GET
#' @importFrom geojsonsf geojson_sf
#' @examples
#' \dontrun{
#' get_geom_commune('44109')
#' }
get_geom_commune <- function(code_commune) {
  url <- glue::glue('https://geo.api.gouv.fr/communes?code={code_commune}&format=geojson&geometry=contour&fields=code,nom,contour')
  res <- httr::GET(url)
  res <- geojsonsf::geojson_sf(rawToChar(res$content))
  return(res)
}

#' Obtenir la liste des communes d'un département
#'
#' @param code_departement le code département insee
#'
#' @return un dataframe
#' @export
#' @importFrom glue glue
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' get_communes_departement('44')
#' }
get_communes_departement <- function(code_departement) {
  url <- glue::glue('https://geo.api.gouv.fr/departements/{code_departement}/communes')
  res <- httr::GET(url)
  res <- rawToChar(res$content) %>%
    jsonlite::fromJSON()
  return(res)
}

#' Obtenir la liste des départements d'une région
#'
#' @param code_region code région insee
#'
#' @return un dataframe
#' @importFrom glue glue
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' get_departements_region('52')
#' }
get_departements_region <- function(code_region) {
  url <- glue::glue('https://geo.api.gouv.fr/regions/{code_region}/departements')
  res <- httr::GET(url)
  res <- rawToChar(res$content) %>%
    jsonlite::fromJSON()
  return(res)
}

#' Obtenir la liste des communes d'une région
#'
#' @param code_region code région insee
#'
#' @return
#' @export
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{
#' get_communes_region('52')
#' }
get_communes_region <- function(code_region) {
  liste_dep <- get_departements_region(code_region)$code
  res <- purrr::map_dfr(liste_dep,get_communes_departement)
  return(res)
}

#' Obtenir la liste des contours communaux pour un département
#'
#' @param code_departement le code département insee
#'
#' @return
#' @export
#' @importFrom purrr map_dfr
#' @examples
#' \dontrun{
#' geom_communes_d44 <- get_geom_communes_departement('44')
#' plot(geom_communes_d44)
#' }
get_geom_communes_departement <- function(code_departement) {
  liste_communes <- get_communes_departement(code_departement)$code
  purrr::map_dfr(liste_communes,get_geom_commune)
}


#' Obtenir la liste des contours communaux pour une région
#'
#' @param code_region code région insee
#'
#' @return
#' @export
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{
#' geom_communes_r52 <- get_geom_communes_region('52')
#' plot(geom_communes_r52)
#' }
get_geom_communes_region <- function(code_region) {
  liste_communes <- get_communes_region(code_region)$code
  purrr::map_dfr(liste_communes,get_geom_commune)
}

