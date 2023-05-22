
#' Fonction de passage d'une table de données à la commune vers le millésime le plus récent du COG
#'
#' Cette fonction vous permet de convertir vos dataframes ayant une variable correspondant au code commune Insee vers le COG le plus récent.
#' Champ : codes communes de la France métropolitaine et des DROM
#'
#' @param .data la table de données à convertir
#' @param code_commune le nom de la variable contenant le code commune sur 5 caractères
#' @param aggrege booléen TRUE si on souhaite réaggréger les colonnes numériques sur la nouvelle carte communale
#' @param garder_info_supra booléen TRUE si on souhaite garder les informations sur les territoires supra des communes
#' @param na.rm argument passé à la fonction d'agrégation (sum), na.rm=FALSE par défaut
#' @return Renvoie la table de données convertie pour les codes communes valide en entrée
#' @examples
#' pop2015_a_jour <- passer_au_cog_a_jour(pop2015, code_commune = DEPCOM,
#'                                        aggrege = TRUE, garder_info_supra = FALSE)
#'
#' @export
#' @importFrom dplyr rename inner_join select group_by across summarise ungroup left_join is.grouped_df group_vars
#' @importFrom rlang enquo sym !!
#' @importFrom tidyselect vars_select_helpers everything
passer_au_cog_a_jour <- function(.data, code_commune = DEPCOM, aggrege = TRUE, garder_info_supra = TRUE, na.rm=FALSE) {
  quo_code_commune <- rlang::enquo(code_commune)
  result <- .data %>%
    dplyr::rename(DEPCOM_HIST = !!quo_code_commune) %>%
    dplyr::inner_join(COGiter::table_passage_com_historique, by = "DEPCOM_HIST") %>%
    dplyr::select(-DEPCOM_HIST)

  if (aggrege == TRUE) {
    result <- result %>%
      dplyr::group_by(dplyr::across(.cols = !tidyselect::vars_select_helpers$where(is.numeric))) %>%
      dplyr::summarise(dplyr::across(.cols = tidyselect::everything(), .fns = ~ sum(.x, na.rm = na.rm))) %>%
      dplyr::ungroup()
  }
  if (garder_info_supra == TRUE) {
    result <- result %>%
      dplyr::left_join(COGiter::communes_info_supra, by = "DEPCOM", suffix = c("_hist", ""))
  }
  if (dplyr::is.grouped_df(.data)) {
    gr_data <- dplyr::group_vars(.data)
    result <- result %>%
      dplyr::group_by(!!rlang::sym(gr_data))
  }
  return(result)
}
