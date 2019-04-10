
#' Fonction de passage d'une table de donn\encoding{é}e à la commune vers le mill\encoding{é}sime le plus r\encoding{é}cent du COG
#'
#' @param .data la table de donn\encoding{é}es à convertir
#' @param code_commune le nom de la variable contenant le code commune sur 5 charactères
#' @param aggrege bool\encoding{é}en TRUE si on souhaite r\encoding{é}aggr\encoding{é}ger les colonnes num\encoding{é}riques sur la nouvelle carte communale
#' @param garder_info_supra bool\encoding{é}en TRUE si on souhaite garder les informations sur les territoires supra des communes
#'
#' @return la table de donn\encoding{é}es convertie
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
#' @encoding UTF-8

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
      left_join(communes_info_supra)
  }
  result
}
