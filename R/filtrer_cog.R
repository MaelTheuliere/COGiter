#' Filtrer un fichier du cog sur un sous ensemble du territoire
#'
#' @details Les parametres depcom, epci, dep et reg sont exclusifs les uns des autres.
#' @param .data la table de données a filtrer
#' @param depcom la commune sur laquelle filtrer les données
#' @param epci l'epci sur lequel filtrer les données
#' @param dep le departement sur lequel filtrer les données
#' @param reg la region sur laquelle filtrer les données
#' @param garder_supra ">" si on souhaite garder les territoires supra, ">=" si on souhaite garder les territoires supra et du même niveau que celui sélectionné, sauf si le filtre est opéré sur une commune, auquel cas seuls les territoires supra auquel appartient la commune sont renvoyés. "non" par défaut.
#' @param epci_complet booleen, TRUE si on souhaite garder toutes les communes des EPCI du département ou de la région
#' @examples
#'  pop2015_cogifiee_Corse <- cogifier(pop2015,  code_commune = DEPCOM, as_df = TRUE) %>%
#'    filtrer_cog(reg = "94", garder_supra = '>')
#'
#' @return Renvoie une table de donnees filtrées
#' @export
#' @importFrom attempt stop_if
#' @importFrom dplyr inner_join across mutate filter select bind_rows transmute
#' @importFrom forcats fct_drop
#' @importFrom rlang enquo !!


filtrer_cog <- function(.data, depcom = NULL, epci = NULL, dep = NULL, reg = NULL, garder_supra = "non", epci_complet = FALSE) {
  nb_ter_null <- is.null(depcom) + is.null(epci) + is.null(dep) + is.null(reg)
  attempt::stop_if(nb_ter_null < 3, msg = "Les param\u00e8tres depcom, epci, dep, reg sont exclusifs les uns des autres. \n Merci de ne choisir qu'un seul territoire sur lequel filtrer vos donnees.")
  quo_depcom <- rlang::enquo(depcom)
  quo_epci <- rlang::enquo(epci)
  quo_dep <- rlang::enquo(dep)
  quo_reg <- rlang::enquo(reg)
  result <- .data

  if(epci_complet) {
    liste_zone <- COGiter::liste_zone %>%
      dplyr::bind_rows(com_limitrophes_epci_a_cheval)
  } else {
    liste_zone <- COGiter::liste_zone
  }


  if (!is.null(reg)) {
    if (garder_supra == "non") {
      result <- result %>%
        dplyr::inner_join(
          liste_zone %>%
            dplyr::filter(grepl(reg, REG)) %>%
            dplyr::select(TypeZone, CodeZone)
        )
    }
    if (garder_supra == ">") {
      result <- result %>%
        dplyr::inner_join(
          liste_zone %>%
            dplyr::filter(grepl(reg, REG)) %>%
            dplyr::select(TypeZone, CodeZone)) %>%
        dplyr::bind_rows(result %>%
          dplyr::filter(TypeZone %in% c("France"))
        )
    }
    if (garder_supra == ">=") {
      result <- result %>%
        dplyr::inner_join(
          liste_zone %>%
            dplyr::filter(grepl(reg, REG)) %>%
            dplyr::select(TypeZone, CodeZone)) %>%
        dplyr::bind_rows(result %>%
          dplyr::filter(TypeZone %in% c("France") |
            (TypeZone %in% c("R\u00e9gions") & !(CodeZone == !!quo_reg))
          )
        )
    }
  }
  if (!is.null(dep)) {
    if (garder_supra == "non") {
      result <- result %>%
        dplyr::inner_join(
          liste_zone %>%
            dplyr::filter(grepl(dep, DEP)) %>%
            dplyr::select(TypeZone, CodeZone)
        )
    }
    if (garder_supra == ">") {
      result <- result %>%
        dplyr::inner_join(
          liste_zone %>%
            dplyr::filter(grepl(dep, DEP)) %>%
            dplyr::select(TypeZone, CodeZone)) %>%
        dplyr::bind_rows(result %>%
          dplyr::filter(TypeZone %in% c("France", "R\u00e9gions"))
        )
    }
    if (garder_supra == ">=") {
      result <- result %>%
        dplyr::inner_join(
          liste_zone %>%
            dplyr::filter(grepl(dep, DEP)) %>%
            dplyr::select(TypeZone, CodeZone)) %>%
        dplyr::bind_rows(result %>%
          dplyr::filter(TypeZone %in% c("France", "R\u00e9gions") |
            (TypeZone %in% c("D\u00e9partements") & !(CodeZone == !!quo_dep))
          )
        )
    }
  }
  if (!is.null(epci)) {
    if (garder_supra == "non") {
      result <- result %>%
        dplyr::inner_join(
          COGiter::liste_zone %>%
            dplyr::filter(EPCI == !!quo_epci) %>%
            dplyr::select(TypeZone, CodeZone)
        )
    }
    if (garder_supra == ">") {
      result <- result %>%
        dplyr::inner_join(
          COGiter::liste_zone %>%
            dplyr::filter(EPCI == !!quo_epci) %>%
            dplyr::select(TypeZone, CodeZone)) %>%
        dplyr::bind_rows(result %>%
          dplyr::filter(TypeZone %in% c("France", "R\u00e9gions", "D\u00e9partements"))
        )
    }
    if (garder_supra == ">=") {
      result <- result %>%
        dplyr::inner_join(
          COGiter::liste_zone %>%
            dplyr::filter(EPCI == !!quo_epci) %>%
            dplyr::select(TypeZone, CodeZone)) %>%
        dplyr::bind_rows(result %>%
          dplyr::filter(TypeZone %in% c("France", "R\u00e9gions", "D\u00e9partements") |
            (TypeZone %in% c("Epci") & !(CodeZone == !!quo_epci))
          )
        )
    }
  }
  if (!is.null(depcom)) {
    if (garder_supra == "non") {
      result <- result %>%
        dplyr::inner_join(
          COGiter::liste_zone %>%
            dplyr::filter(CodeZone == !!quo_depcom, TypeZone == "Communes") %>%
            dplyr::select(TypeZone, CodeZone)
        )
    }
    if (grepl(">", garder_supra)) {
      com_a_garder <- COGiter::liste_zone %>%
        dplyr::filter(CodeZone == !!quo_depcom, TypeZone == "Communes")
      epci_a_garder <- com_a_garder %>%
        dplyr::select(CodeZone = EPCI) %>%
        dplyr::left_join(COGiter::liste_zone)
      dep_a_garder <- com_a_garder %>%
        dplyr::transmute(CodeZone = unlist(DEP)) %>%
        dplyr::mutate(TypeZone = factor("D\u00e9partements", levels = levels(liste_zone$TypeZone))) %>%
        dplyr::left_join(COGiter::liste_zone)
      reg_a_garder <- com_a_garder %>%
        dplyr::transmute(CodeZone = unlist(REG)) %>%
        dplyr::mutate(TypeZone = factor("R\u00e9gions", levels = levels(liste_zone$TypeZone))) %>%
        dplyr::left_join(COGiter::liste_zone)
      ter_a_garder <- dplyr::bind_rows(com_a_garder, epci_a_garder, dep_a_garder, reg_a_garder) %>%
        dplyr::select(TypeZone, CodeZone)

      result <- result %>%
        dplyr::inner_join(ter_a_garder) %>%
        dplyr::bind_rows(result %>%
                    dplyr::filter(TypeZone %in% "France"))

    }

  }
  result <- result %>%
    dplyr::mutate(dplyr::across(.cols = c(TypeZone, Zone, CodeZone), .fns = as.factor)) %>%
    dplyr::mutate(dplyr::across(.cols = c(TypeZone, Zone, CodeZone), .fns = forcats::fct_drop))
  return(result)
}
