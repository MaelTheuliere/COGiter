#' Liste des communes du dernier millésime connu.
#'
#' Un dataset contenant pour chaque commune de France actuelle son code Insee, son nom et les identifiants des territoire supra
#'
#' @format A data frame with 35357 rows and 20 variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee}
#'   \item{NOM_DEPCOM}{Nom de la commune}
#'   \item{EPCI}{Code siren de l'EPCI de rattachement}
#'   \item{NOM_EPCI}{Nom de l'EPCI de rattachement}
#'   \item{DEP}{Code Insee du département de rattachement}
#'   \item{NOM_DEP}{Nom du département de rattachement}
#'   \item{REG}{Code Insee de la région de rattachement}
#'   \item{NOM_REG}{Nom de la région de rattachement}
#'   \item{DEPARTEMENTS_DE_L_EPCI}{Liste des départements dans lesquels  se retrouve au moins une commune de l'epci}
#'   \item{REGIONS_DE_L_EPCI}{Liste des régions dans lesquelles  se retrouve au moins une commune de l'epci}
#'   \item{CDC}{Découpage de la commune en cantons}
#'   \item{CHEFLIEU}{Chef-lieu d'arrondissement, de département, de région ou bureau centralisateur}
#'   \item{COM}{Code commune}
#'   \item{AR}{Code arrondissement}
#'   \item{CT}{Code canton}
#'   \item{TNCC}{Type de nom en clair}
#'   \item{ARTMAJ}{Article (majuscules)}
#'   \item{NCC}{Nom en clair (majuscules)}
#'   \item{ARTMIN}{Article (typographie riche)}
#'   \item{NCCENR}{Nom en clair (typographie riche)}
#' }
#' @source \url{https://www.insee.fr/fr/information/3363419#titre-bloc-7}
"communes"


#' Liste des départements du dernier millésime connu.
#'
#' Un dataset contenant pour chaque départements de France actuelle son code Insee, son nom et les identifiants des territoire supra
#'
#' @format A data frame with 101 rows and 6 variables:
#' \describe{
#'   \item{REG}{Code Insee de la région de rattachement}
#'   \item{DEP}{Code Insee du département}
#'   \item{CHEFLIEU}{Chef-lieu d'arrondissement, de département, de région ou bureau centralisateur}
#'   \item{TNCC}{Type de nom en clair}
#'   \item{NCC}{Nom en clair (majuscules)}
#'   \item{NOM_DEP}{Nom du département}
#' }
#' @source \url{https://www.insee.fr/fr/information/3363419#titre-bloc-23}
"departements"

#' Liste des epci du dernier millésime connu.
#'
#' Un dataset contenant pour chaque epci de France actuel son code Insee, son nom et les identifiants des territoire supra
#'
#' @format A data frame with 1264 rows and 4 variables:
#' \describe{
#'   \item{EPCI}{Code siren de l'EPCI}
#'   \item{NOM_EPCI}{Nom de l'EPCI}
#'   \item{DEPARTEMENTS_DE_L_EPCI}{Liste des départements dans lesquels  se retrouve au moins une commune de l'epci}
#'   \item{REGIONS_DE_L_EPCI}{Liste des régions dans lesquelles  se retrouve au moins une commune de l'epci}
#' }
#' @source \url{https://www.insee.fr/fr/information/2510634}
"epci"


#' Liste des régions du dernier millésime connu.
#'
#' Un dataset contenant pour chaque régions de France actuelle son code Insee, son nom
#'
#' @format A data frame with 18 rows and 5 variables:
#' \describe{
#'   \item{REG}{Code Insee de la région}
#'   \item{CHEFLIEU}{Chef-lieu d'arrondissement, de département, de région ou bureau centralisateur}
#'   \item{TNCC}{Type de nom en clair}
#'   \item{NCC}{Nom en clair (majuscules)}
#'   \item{NOM_REG}{Nom de la région}
#' }
#' @source \url{https://www.insee.fr/fr/information/3363419#titre-bloc-26}
"regions"

#' Rattachement des communes à leur epci.
#'
#' Un dataset contenant pour chaque commune de France actuelle le code sirene de son EPCI
#'
#' @format A data frame with 35357 rows and 2 variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee de la commune}
#'   \item{EPCI}{Code siren de l'EPCI de rattachement}
#' }
#' @source \url{https://www.insee.fr/fr/information/2510634}
"table_passage_com_epci"


#' Table de passage des fusions de communes.
#'
#' Un dataset contenant pour chaque commune historique de France le code de la commune actuelle de rattachement
#'
#' @format A data frame with 39161 rows and 2 variables:
#' \describe{
#'   \item{DEPCOM_HIST}{Code Insee de la commune historique}
#'   \item{DEPCOM}{Code Insee de la commune}
#' }
#' @source \url{https://www.insee.fr/fr/information/3363419#titre-bloc-3}
"table_passage_com_historique"

#' Une table permettant de lister pour chaque département ou région les zonages infra
#'
#' Une table permettant de lister pour chaque département ou région les zonages infra
#'
#' @format A data frame with 39161 rows and 2 variables:
#' \describe{
#'   \item{TypeZone}{Type de la zone : communes,epci,départements,régions}
#'   \item{CodeZone}{Code de la zone}
#'   \item{DEP}{liste des départements ayant cette zone en territoire infra}
#'   \item{REG}{liste des régions ayant cette zone en territoire infra}
#' }
"liste_zone"


#' Zonage ABC région Pays de la Loire.
#'
#' Un dataset contenant pour chaque commune actuelle de la région Pays de la Loire le zonage ABC correspondant
#'
#' @format A data frame with 1272 rows and 2 variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee de la commune}
#'   \item{zonage_abc}{Zonage ABC}
#' }
"zonage_abc_r52"

#' Zonage Pinel région Pays de la Loire.
#'
#' Un dataset contenant pour chaque commune actuelle de la région Pays de la Loire le zonage Pinel correspondant
#'
#' @format A data frame with 1272 rows and 2 variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee de la commune}
#'   \item{zonage_pinel}{Zonage Pinel}
#' }
"zonage_pinel_r52"

#' Contour des communes métropolitaines pour le dernier millésime connu
#'
#' Un dataset contenant pour chaque commune de France métropolitaine actuelle son contour
#'
#' @format A data frame with 35273 rows and 2 variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee}
#'   \item{geometry}{géométrie}
#' }
"communes_geo"

#' Contour des epci métropolitains pour le dernier millésime connu
#'
#' Un dataset contenant pour chaque epci de France métropolitaine actuelle son contour
#'
#' @format A data frame with 1241 rows and 2 variables:
#' \describe{
#'   \item{EPCI}{Code Sirene}
#'   \item{geometry}{géométrie}
#' }
"epci_geo"

#' Contour des départements métropolitains pour le dernier millésime connu
#'
#' Un dataset contenant pour chaque département de France métropolitaine actuelle son contour
#'
#' @format A data frame with 96 rows and 2 variables:
#' \describe{
#'   \item{DEP}{Code Insee}
#'   \item{geometry}{géométrie}
#' }
"departements_geo"

#' Contour des régions métropolitaines pour le dernier millésime connu
#'
#' Un dataset contenant pour chaque région  de France métropolitaine actuelle son contour
#'
#' @format A data frame with 13 rows and 2 variables:
#' \describe{
#'   \item{REG}{Code Insee}
#'   \item{geometry}{géométrie}
#' }
"departements_geo"
