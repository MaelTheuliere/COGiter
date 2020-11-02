
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
#'
#' @return Renvoie la table de données convertie pour les codes communes valide en entrée
#' @export
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr group_by_if
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr funs
#' @importFrom rlang enquo
#' @importFrom rlang !!
#'


passer_au_cog_a_jour<-function(.data,code_commune=DEPCOM,aggrege=T,garder_info_supra=T,...) {
  quo_code_commune<-enquo(code_commune)
  result<-.data %>%
    rename(DEPCOM_HIST=!!quo_code_commune) %>%
    inner_join(table_passage_com_historique) %>%
    select(-DEPCOM_HIST)

  if (aggrege==T) {
    result<-result %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_all(funs(sum(.,...))) %>%
      ungroup()
  }
  if (garder_info_supra==T) {
    result<-result %>%
      left_join(communes_info_supra)
  }
  result
}
