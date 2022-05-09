#' Filtrer un fichier du cog sur un sous ensemble du territoire
#'
#' @param .data la table de données a filtrer
#' @param depcom la commune sur laquelle filtrer les données
#' @param epci l'epci sur lequel filtrer les données
#' @param dep le departement sur lequel filtrer les données
#' @param reg la region sur laquelle filtrer les données
#' @param garder_supra ">" si on souhaite garder les territoires supra, ">=" si on souhaite garder les territoires suppra et du même niveau que celui sélectionné
#' @examples
#'  pop2015_cogifiee_Corse <- cogifier(pop2015,  code_commune = DEPCOM, as_df = TRUE) %>%
#'    filtrer_cog(reg = "94", garder_supra = '>')
#'
#' @return Renvoie une table de donnees filtrées
#' @export
#' @importFrom dplyr inner_join across mutate filter select bind_rows
#' @importFrom forcats fct_drop
#' @importFrom rlang enquo
#' @importFrom rlang !!


filtrer_cog <- function(.data, depcom = NULL, epci = NULL, dep = NULL, reg = NULL, garder_supra = "non") {
  quo_depcom <- enquo(depcom)
  quo_epci <- enquo(epci)
  quo_dep <- enquo(dep)
  quo_reg <- enquo(reg)
  result <- .data
  if (!is.null(reg)) {
    if (garder_supra == "non") {
      result <- result %>%
        inner_join(
          COGiter::liste_zone %>%
            filter(grepl(reg, REG)) %>%
            select(TypeZone, CodeZone)
        )
    }
    if (garder_supra == ">") {
      result <- result %>%
        inner_join(
          COGiter::liste_zone %>%
            filter(grepl(reg, REG)) %>%
            select(TypeZone, CodeZone)) %>%
        bind_rows(result %>%
          filter(TypeZone %in% c("France"))
        )
    }
    if (garder_supra == ">=") {
      result <- result %>%
        inner_join(
          COGiter::liste_zone %>%
            filter(grepl(reg, REG)) %>%
            select(TypeZone, CodeZone)) %>%
        bind_rows(result %>%
          filter(TypeZone %in% c("France") |
            (TypeZone %in% c("R\u00e9gions") & !(CodeZone == !!quo_reg))
          )
        )
    }
  }
  if (!is.null(dep)) {
    if (garder_supra == "non") {
      result <- result %>%
        inner_join(
          COGiter::liste_zone %>%
            filter(grepl(dep, DEP)) %>%
            select(TypeZone, CodeZone)
        )
    }
    if (garder_supra == ">") {
      result <- result %>%
        inner_join(
          COGiter::liste_zone %>%
            filter(grepl(dep, DEP)) %>%
            select(TypeZone, CodeZone)) %>%
        bind_rows(result %>%
          filter(TypeZone %in% c("France", "R\u00e9gions"))
        )
    }
    if (garder_supra == ">=") {
      result <- result %>%
        inner_join(
          COGiter::liste_zone %>%
            filter(grepl(dep, DEP)) %>%
            select(TypeZone, CodeZone)) %>%
        bind_rows(result %>%
          filter(TypeZone %in% c("France", "R\u00e9gions") |
            (TypeZone %in% c("D\u00e9partements") & !(CodeZone == !!quo_dep))
          )
        )
    }
  }
  if (!is.null(epci)) {
    if (garder_supra == "non") {
      result <- result %>%
        inner_join(
          COGiter::liste_zone %>%
            filter(EPCI == !!quo_epci) %>%
            select(TypeZone, CodeZone)
        )
    }
    if (garder_supra == ">") {
      result <- result %>%
        inner_join(
          COGiter::liste_zone %>%
            filter(EPCI == !!quo_epci) %>%
            select(TypeZone, CodeZone)) %>%
        bind_rows(result %>%
          filter(TypeZone %in% c("France", "R\u00e9gions", "D\u00e9partements"))
        )
    }
    if (garder_supra == ">=") {
      result <- result %>%
        inner_join(
          COGiter::liste_zone %>%
            filter(EPCI == !!quo_epci) %>%
            select(TypeZone, CodeZone)) %>%
        bind_rows(result %>%
          filter(TypeZone %in% c("France", "R\u00e9gions", "D\u00e9partements") |
            (TypeZone %in% c("Epci") & !(CodeZone == !!quo_epci))
          )
        )
    }
  }
  result <- result %>%
    dplyr::mutate(dplyr::across(c(TypeZone, Zone, CodeZone), as.factor)) %>%
    dplyr::mutate(dplyr::across(c(TypeZone, Zone, CodeZone), forcats::fct_drop))
  return(result)
}
