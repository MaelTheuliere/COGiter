#' table_scissions_com
#'
#' Table qui liste par année, les nouvelles communes issues de scission.
#'
#' @format A data frame with 622 rows and 6 variables:
#' \describe{
#'   \item{ ANNEE_SCISSION }{  numeric, annee au debut de laquelle la separation communale est enregistree }
#'   \item{ COM_AV }{  character, code de la commune scindee avant la separation}
#'   \item{ COM_AV_LIB }{  character, libelle de la commune scindee avant la separation }
#'   \item{ COM_AP }{  character, code commune de la commune nouvelle apres la separation }
#'   \item{ COM_AP_LIB }{  libelle de la commune nouvelle apres la separation  }
#' }
#' @source Table des mouvement de communes issue du [COG Insee](https://www.insee.fr/fr/information/2560452) et filtrée sur les modalités 20 et 21
"table_scissions_com"
