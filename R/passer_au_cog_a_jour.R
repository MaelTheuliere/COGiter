
#' Fonction de passage d'une table de donnée à la commune vers le millésime le plus récent du COG
#'
#' Cette fonction vous permet de convertir vos dataframe ayant une variable correspondant au code commun Insee vers le COG le plus récent.
#' Champ : code communes de la France métropolitaine et des DROM
#'
#' @param .data la table de données à convertir
#' @param code_commune le nom de la variable contenant le code commune sur 5 charactères
#' @param aggrege booléen TRUE si on souhaite réaggréger les colonnes numériques sur la nouvelle carte communale
#' @param garder_info_supra booléen TRUE si on souhaite garder les informations sur les territoires supra des communes
#' @param ... argument(s) passé(s) à la fonction d'aggrégation (sum), na.rm=F par défaut
#' @return Renvoie la table de données convertie pour les codes communes valide en entrée
#' @export
#' @importFrom dplyr rename left_join inner_join select group_by summarise ungroup group_vars across is.grouped_df
#' @importFrom tidyselect vars_select_helpers
#' @importFrom rlang enquo sym !!
passer_au_cog_a_jour <- function(.data, code_commune = DEPCOM, aggrege = T, garder_info_supra = T, ...) {
  quo_code_commune <- enquo(code_commune)
  result <- .data %>%
    rename(DEPCOM_HIST = !!quo_code_commune) %>%
    inner_join(COGiter::table_passage_com_historique) %>%
    select(-DEPCOM_HIST)

  if (aggrege == T) {
    result <- result %>%
      group_by(across(!tidyselect::vars_select_helpers$where(is.numeric)), .add = TRUE) %>%
      summarise(across(.fns = ~ sum(.x, ...))) %>%
      ungroup()
  }
  if (garder_info_supra == T) {
    result <- result %>%
      left_join(COGiter::communes_info_supra)
  }
  if (is.grouped_df(.data)) {
    gr_data <- group_vars(.data)
    result <- result %>%
      group_by(!!sym(gr_data))
  }
  return(result)
}
