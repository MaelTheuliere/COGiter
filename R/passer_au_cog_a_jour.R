
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
#' @importFrom dplyr rename left_join inner_join select group_by summarise ungroup group_vars across is.grouped_df
#' @importFrom tidyselect vars_select_helpers
#' @importFrom rlang enquo sym !!
#'
passer_au_cog_a_jour <- function(.data, code_commune = DEPCOM, aggrege = TRUE, garder_info_supra = TRUE, na.rm=FALSE) {
  quo_code_commune <- enquo(code_commune)
  result <- .data %>%
    rename(DEPCOM_HIST = !!quo_code_commune) %>%
    inner_join(COGiter::table_passage_com_historique, by = "DEPCOM_HIST") %>%
    select(-DEPCOM_HIST)

  if (aggrege == T) {
    result <- result %>%
      group_by(across(!tidyselect::vars_select_helpers$where(is.numeric))) %>%
      summarise(across(.fns = ~ sum(.x, na.rm = na.rm))) %>%
      ungroup()
  }
  if (garder_info_supra == T) {
    result <- result %>%
      left_join(COGiter::communes_info_supra, by = "DEPCOM", suffix = c("_hist", ""))
  }
  if (is.grouped_df(.data)) {
    gr_data <- group_vars(.data)
    result <- result %>%
      group_by(!!sym(gr_data))
  }
  return(result)
}
