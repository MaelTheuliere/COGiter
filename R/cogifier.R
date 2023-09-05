#' Consolider une table de données à la commune à tous les échelles du cog
#'
#' @param .data la table de données à convertir
#' @param code_commune le nom de la variable contenant le code commune sur 5 charactères
#' @param communes booléen TRUE si on souhaite des données à la commune
#' @param epci booléen TRUE si on souhaite des données à l'epci
#' @param departements booléen TRUE si on souhaite des données au département
#' @param regions booléen TRUE si on souhaite des données à la région
#' @param metro booléen TRUE si on souhaite des données France métropolitaine
#' @param metrodrom booléen TRUE si on souhaite des données France métropolitaine et des DROM
#' @param franceprovince booléen TRUE si on souhaite des données France de province
#' @param drom booléen TRUE si on souhaite des données Départements et régions d'outre mer
#' @param as_df booléen TRUE si on souhaite des données sous un seul dataframe, FALSE si on souhaite une liste de dataframe par type de zone
#' @param na.rm argument(s) passé(s) à la fonction d'aggrégation (sum), na.rm=F par défaut
#'
#' @return Renvoie un dataframe ou une liste de dataframe
#'
#' @examples
#' pop2015_cogifiee <- cogifier(pop2015, code_commune = DEPCOM)
#'
#' @export
#' @importFrom dplyr select group_by across summarise ungroup filter everything any_of
#' @importFrom rlang enquo !!
#' @importFrom tidyselect vars_select_helpers
cogifier <- function(.data, code_commune = DEPCOM,
                     communes = TRUE,
                     epci = TRUE,
                     departements = TRUE,
                     regions = TRUE,
                     metro = TRUE,
                     metrodrom = FALSE,
                     franceprovince = FALSE,
                     drom = FALSE,
                     as_df = TRUE,
                     na.rm = FALSE) {
  quo_code_commune <- rlang::enquo(code_commune)
  au_cog <- passer_au_cog_a_jour(.data = .data, code_commune = !!quo_code_commune,
                                 garder_info_supra = TRUE, aggrege = FALSE, na.rm = na.rm)

  # colonnes d'appartenance geo à enlever, a priori ce ne sont pas des dimensions à conserver
  dim_geo <- intersect(names(.data), names(COGiter::communes_info_supra)) %>%
    setdiff("DEPCOM")
  dim_geo_suffixee <- paste0(dim_geo, "_hist") # passer_au_cog_a_jour suffixe les noms de colonnes communs avec communes_info_supra
  if(length(dim_geo) == 1) {
    message("Votre jeu de donnees '.data' contient une colonne d'appartenance geo qui va etre supprimee avant traitement : '", dim_geo,
            "',\nrenommez-la au prealable si vous souhaitez conserver cet axe d'analyse." )
    au_cog <- au_cog %>%
      dplyr::select(-dplyr::all_of(dim_geo_suffixee))
  } else if(length(dim_geo) > 1){
    message("Votre jeu de donnees '.data' contient des colonnes d'appartenance geo qui vont etre supprimees avant traitement : '", paste(dim_geo, collapse = "', '"),
            "',\nrenommez-les au prealable si vous souhaitez conserver ces axes d'analyse." )
    au_cog <- au_cog %>%
      dplyr::select(-dplyr::all_of(dim_geo_suffixee))
    }

  c <- NULL
  e <- NULL
  d <- NULL
  r <- NULL
  m <- NULL
  md <- NULL
  fp <- NULL
  dr <- NULL
  if (communes == TRUE) {
    c <- au_cog %>%
      dplyr::select(-REG, -NOM_REG, -DEP, -NOM_DEP, -EPCI, -NOM_EPCI, -DEPARTEMENTS_DE_L_EPCI, -REGIONS_DE_L_EPCI) %>%
      dplyr::group_by(dplyr::across(.cols = !tidyselect::vars_select_helpers$where(is.numeric))) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = ~ sum(.x, na.rm = na.rm))) %>%
      dplyr::ungroup()
  }
  if (epci == TRUE) {
    e <- au_cog %>%
      dplyr::select(-REG, -NOM_REG, -DEP, -NOM_DEP, -DEPCOM, -NOM_DEPCOM, -DEPARTEMENTS_DE_L_EPCI, -REGIONS_DE_L_EPCI) %>%
      dplyr::filter(EPCI != "ZZZZZZZZZ") %>%
      dplyr::group_by(dplyr::across(.cols = !tidyselect::vars_select_helpers$where(is.numeric))) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = ~ sum(.x, na.rm = na.rm))) %>%
      dplyr::ungroup()
  }
  if (departements == TRUE) {
    d <- au_cog %>%
      dplyr::select(-REG, -NOM_REG, -DEPCOM, -NOM_DEPCOM, -EPCI, -NOM_EPCI, -DEPARTEMENTS_DE_L_EPCI, -REGIONS_DE_L_EPCI) %>%
      dplyr::group_by(dplyr::across(.cols = !tidyselect::vars_select_helpers$where(is.numeric))) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = ~ sum(.x, na.rm = na.rm))) %>%
      dplyr::ungroup()
  }
  if (regions == TRUE) {
    r <- au_cog %>%
      dplyr::select(-DEP, -NOM_DEP, -DEPCOM, -NOM_DEPCOM, -EPCI, -NOM_EPCI, -DEPARTEMENTS_DE_L_EPCI, -REGIONS_DE_L_EPCI) %>%
      dplyr::group_by(dplyr::across(.cols = !tidyselect::vars_select_helpers$where(is.numeric))) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::everything(),.fns = ~ sum(.x, na.rm = na.rm))) %>%
      dplyr::ungroup()
  }
  if (metro == TRUE) {
    m <- au_cog %>%
      dplyr::filter(!(REG %in% c("01", "02", "03", "04", "05", "06"))) %>%
      dplyr::select(-REG, -NOM_REG, -DEP, -NOM_DEP, -DEPCOM, -NOM_DEPCOM, -EPCI, -NOM_EPCI, -DEPARTEMENTS_DE_L_EPCI, -REGIONS_DE_L_EPCI) %>%
      dplyr::group_by(dplyr::across(.cols = !tidyselect::vars_select_helpers$where(is.numeric))) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::everything(),.fns = ~ sum(.x, na.rm = na.rm))) %>%
      dplyr::ungroup()
  }
  if (metrodrom == TRUE) {
    md <- au_cog %>%
      dplyr::select(-REG, -NOM_REG, -DEP, -NOM_DEP, -DEPCOM, -NOM_DEPCOM, -EPCI, -NOM_EPCI, -DEPARTEMENTS_DE_L_EPCI, -REGIONS_DE_L_EPCI) %>%
      dplyr::group_by(dplyr::across(.cols = !tidyselect::vars_select_helpers$where(is.numeric))) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = ~ sum(.x, na.rm = na.rm))) %>%
      dplyr::ungroup()
  }
  if (franceprovince == TRUE) {
    fp <- au_cog %>%
      dplyr::filter(!(REG %in% c("01", "02", "03", "04", "05", "06", "11"))) %>%
      dplyr::select(-REG, -NOM_REG, -DEP, -NOM_DEP, -DEPCOM, -NOM_DEPCOM, -EPCI, -NOM_EPCI, -DEPARTEMENTS_DE_L_EPCI, -REGIONS_DE_L_EPCI) %>%
      dplyr::group_by(dplyr::across(.cols = !tidyselect::vars_select_helpers$where(is.numeric))) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::everything(), .fns = ~ sum(.x, na.rm = na.rm))) %>%
      dplyr::ungroup()
  }
  if (drom == TRUE) {
    dr <- au_cog %>%
      dplyr::filter(REG %in% c("01", "02", "03", "04", "05", "06")) %>%
      dplyr::select(-REG, -NOM_REG, -DEP, -NOM_DEP, -DEPCOM, -NOM_DEPCOM, -EPCI, -NOM_EPCI, -DEPARTEMENTS_DE_L_EPCI, -REGIONS_DE_L_EPCI) %>%
      dplyr::group_by(dplyr::across(.cols = !tidyselect::vars_select_helpers$where(is.numeric))) %>%
      dplyr::summarise(dplyr::across(.cols = dplyr::everything(),.fns = ~ sum(.x, na.rm = na.rm))) %>%
      dplyr::ungroup()
  }


  result <- list("communes" = c, "epci" = e, "departements" = d, "regions" = r, "metro" = m, "metrodrom" = md, "franceprovince" = fp, "drom" = dr)

  if (as_df == TRUE) {
    result <- cog_list_to_df(result)
  }

  return(result)
}
