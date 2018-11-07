
#' Fonction de passage d'une table de donnée à la commune vers le millésime le plus récent du COG
#'
#' @param .data la table de données à convertire
#' @param code_commune le nom de la variable contenant le code commune sur 5 charactères
#' @param aggrege booléen TRUE si on souhaite réaggréger les colonnes numériques sur la nouvelle carte communale
#' @param garder_info_supra booléen TRUE si on souhaite garder les informations sur les territoires supra des communes
#'
#' @return la table de données convertie
#' @export
#' @import magrittr
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
#' @examples
passer_au_cog_a_jour<-function(.data,code_commune=DEPCOM,aggrege=T,garder_info_supra=T) {
  quo_code_commune<-enquo(code_commune)
  result<-.data %>%
    rename(DEPCOM_HIST=!!quo_code_commune) %>%
    left_join(table_passage_com_historique) %>%
    select(-DEPCOM_HIST)

  if (aggrege==T) {
    result<-result %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_all(funs(sum)) %>%
      ungroup()
  }
  if (garder_info_supra==T) {
    result<-result %>%
      left_join(communes %>% select(DEPCOM:REGIONS_DE_L_EPCI)) %>%
      mutate(DEPCOM=as.factor(DEPCOM))
  }
  result
}
