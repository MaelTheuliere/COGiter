#' Liste des communes du dernier millésime connu.
#'
#' Un dataset contenant chaque commune, communes déléguées,arrondissements, de France
#'
#' @format A data frame with `r nrow(communes_cog)` rows and `r ncol(communes_cog)` variables:
#' \describe{
#'   \item{TYPECOM}{Code Insee}
#'   \item{DEPCOM}{Code Insee}
#'   \item{NOM_DEPCOM}{Nom en clair (typographie riche) avec article }
#'   \item{REG}{Code Insee de la région de rattachement}
#'   \item{DEP}{Code Insee du département de rattachement}
#'   \item{CTCD}{Code de la collectivité territoriale ayant les compétences départementales}
#'   \item{ARR}{Code arrondissement}
#'   \item{TNCC}{Type de nom en clair}
#'   \item{NCC}{Nom en clair (majuscules)}
#'   \item{NCCENR}{Nom en clair (typographie riche)}
#'   \item{CAN}{Code canton. Pour les communes « multi-cantonales » code décliné de 99 à 90 (pseudo-canton) ou de 89 à 80 (communes nouvelles) }
#'   \item{COMPARENT}{Code de la commune parente pour les arrondissements municipaux et les communes associées ou déléguées. }
#' }
#' ```{r}
#' head(communes_cog)
#' ```
#' @source \url{https://www.insee.fr/fr/information/3720946#titre-bloc-3}
"communes_cog"

#' Liste des communes du dernier millésime connu.
#'
#' Un dataset contenant chaque commune de France
#'
#' @format A data frame with `r nrow(communes)` rows and `r ncol(communes)` variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee}
#'   \item{NOM_DEPCOM}{Nom en clair (typographie riche) avec article }
#'   \item{EPCI}{Code Insee de la région de rattachement}
#'   \item{NOM_EPCI}{Nom de l'EPCI}
#'   \item{DEP}{Code Insee du département de rattachement}
#'   \item{NOM_DEP}{Nom en clair (typographie riche) avec article du département}
#'   \item{REG}{Code Insee de la région de rattachement}
#'   \item{NOM_REG}{Nom en clair (typographie riche) avec article de la région}
#'   \item{DEPARTEMENTS_DE_L_EPCI}{Liste des départements dans lesquels se retrouve au moins une commune de l'epci de rattachement de la commune}
#'   \item{REGIONS_DE_L_EPCI}{Liste des régions dans lesquelles se retrouve au moins une commune de l'epci de rattachement de la commune}
#'   \item{ARR}{Code arrondissement}
#'   \item{TNCC}{Type de nom en clair}
#'   \item{NCC}{Nom en clair (majuscules)}
#'   \item{NCCENR}{Nom en clair (typographie riche)}
#'   \item{CAN}{Code canton. Pour les communes « multi-cantonales » code décliné de 99 à 90 (pseudo-canton) ou de 89 à 80 (communes nouvelles) }
#'   \item{COMPARENT}{Code de la commune parente pour les arrondissements municipaux et les communes associées ou déléguées. }
#' }
#' @source \url{https://www.insee.fr/fr/information/3720946#titre-bloc-3}
"communes"


#' Information supracommunales pour les communes actuelles.
#'
#' Un dataset contenant chaque commune les informations de rattachement supracommunales
#'
#' @format A data frame with `r nrow(communes_info_supra)` rows and `r ncol(communes_info_supra)` variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee}
#'   \item{NOM_DEPCOM}{Nom en clair (typographie riche) avec article }
#'   \item{EPCI}{Code Insee de la région de rattachement}
#'   \item{NOM_EPCI}{Nom de l'EPCI}
#'   \item{DEP}{Code Insee du département de rattachement}
#'   \item{NOM_DEP}{Nom en clair (typographie riche) avec article du département}
#'   \item{REG}{Code Insee de la région de rattachement}
#'   \item{NOM_REG}{Nom en clair (typographie riche) avec article de la région}
#'   \item{DEPARTEMENTS_DE_L_EPCI}{Liste des départements dans lesquels se retrouve au moins une commune de l'epci de rattachement de la commune}
#'   \item{REGIONS_DE_L_EPCI}{Liste des régions dans lesquelles se retrouve au moins une commune de l'epci de rattachement de la commune}
#' }
"communes_info_supra"

#' Liste des départements du dernier millésime connu.
#'
#' Un dataset contenant chaque départements de France
#'
#' @format A data frame with `r nrow(departements)` rows and `r ncol(departements)` variables:
#' \describe{
#'   \item{DEP}{Code Insee du département}
#'   \item{REG}{Code Insee de la région de rattachement}
#'   \item{CHEFLIEU}{Chef-lieu d'arrondissement, de département, de région ou bureau centralisateur}
#'   \item{TNCC}{Type de nom en clair}
#'   \item{NCC}{Nom en clair (majuscules)}
#'   \item{NCCENR}{Nom en clair (typographie riche)}
#'   \item{NOM_DEP}{Nom en clair (typographie riche) avec article}
#' }
#' @source \url{https://www.insee.fr/fr/information/3720946#titre-bloc-14}
"departements"

#' Liste des epci du dernier millésime connu.
#'
#' Un dataset contenant chaque epci de France
#'
#' @format A data frame with `r nrow(epci)` rows and `r ncol(departements)` variables:
#' \describe{
#'   \item{EPCI}{Code siren de l'EPCI}
#'   \item{NOM_EPCI}{Nom de l'EPCI}
#'   \item{DEP_SIEGE_EPCI}{Code du departement de la commune siege de l'EPCI}
#'   \item{NATURE_EPCI}{Type de l'epci : ME,CA,CU,CC}
#'   \item{DEPARTEMENTS_DE_L_EPCI}{Liste des départements dans lesquels se retrouve au moins une commune de l'epci}
#'   \item{REGIONS_DE_L_EPCI}{Liste des régions dans lesquelles se retrouve au moins une commune de l'epci}
#' }
#' @source \url{https://www.insee.fr/fr/information/2510634}
"epci"


#' Liste des régions du dernier millésime connu.
#'
#' Un dataset contenant chaque régions de France
#'
#' @format A data frame with `r nrow(regions)` rows and `r ncol(regions)` variables:
#' \describe{
#'   \item{REG}{Code Insee de la région}
#'   \item{CHEFLIEU}{Chef-lieu d'arrondissement, de département, de région ou bureau centralisateur}
#'   \item{TNCC}{Type de nom en clair}
#'   \item{NCC}{Nom en clair (majuscules)}
#'   \item{NCCENR}{Nom en clair (typographie riche)}
#'   \item{NOM_REG}{Nom en clair (typographie riche) avec article}
#' }
#' @source \url{https://www.insee.fr/fr/information/3720946#titre-bloc-17}
"regions"

#' Rattachement des communes à leur epci.
#'
#' Un dataset contenant pour chaque commune de France actuelle le code sirene de son EPCI
#'
#' @format A data frame with `r nrow(table_passage_com_epci)` rows and `r ncol(table_passage_com_epci)` variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee de la commune}
#'   \item{EPCI}{Code siren de l'EPCI de rattachement}
#' }
#' @source \url{https://www.insee.fr/fr/information/2510634}
"table_passage_com_epci"


#' Table des mouvements des communes
#'
#' Un dataset listant les mouvements opérés sur la carte communale
#' @format A data frame with `r nrow(mvtcommunes)` rows and `r ncol(mvtcommunes)` variables:
#' \describe{
#'   \item{MOD}{Type d'événement de communes}
#'   \item{DATE_EFF}{Date d'effet}
#'   \item{TYPECOM_AV}{Type de la commune avant événement}
#'   \item{COM_AV}{Code de la commune avant événement}
#'   \item{TNCC_AV}{Type de nom en clair }
#'   \item{NCC_AV}{Nom en clair (majuscules)}
#'   \item{NCCENR_AV}{Nom en clair (typographie riche)}
#'   \item{LIBELLE_AV}{Nom en clair (typographie riche) avec article}
#'   \item{TYPECOM_AP}{Type de la commune après événement}
#'   \item{COM_AP}{Code de la commune après événement}
#'   \item{TNCC_AP}{Type de nom en clair }
#'   \item{NCC_AP}{Nom en clair (majuscules)}
#'   \item{NCCENR_AP}{Nom en clair (typographie riche)}
#'   \item{LIBELLE_AP}{Nom en clair (typographie riche) avec article}
#' }
#' @source \url{https://www.insee.fr/fr/information/3720946#titre-bloc-23}
"mvtcommunes"

#' Table de passage des fusions de communes.
#'
#' Un dataset contenant pour chaque commune historique de France le code de la commune actuelle de rattachement
#' Cette table permet de gérer le suivi des fusions de communes.
#' Elle se base sur un exploitation de la table historique du COG 2018 et de la table des mouvements du COG 2019
#'
#' @format A data frame with `r nrow(table_passage_com_historique)` rows and `r ncol(table_passage_com_historique)` variables:
#' \describe{
#'   \item{DEPCOM_HIST}{Code Insee de la commune historique}
#'   \item{DEPCOM}{Code Insee de la commune}
#' }
#' @source \url{https://www.insee.fr/fr/information/3720946#titre-bloc-23}
"table_passage_com_historique"

#' Table de passage des arrondissements de Paris, Lyon, Marseille vers leur code commune.
#'
#' Un dataset contenant pour chaque arrondissement de Paris, Lyon et Marseille le code de la commune de rattachement
#'
#' @format A data frame with `r nrow(arn_plm)` rows and `r ncol(arn_plm)` variables:
#' \describe{
#'   \item{ARN}{Code Insee de l'arrondissement}
#'   \item{DEPCOM}{Code Insee de la commune}
#' }
"arn_plm"

#' Une table permettant de lister pour chaque département ou région les zonages infra
#'
#' Une table permettant de lister pour chaque département ou région les zonages infra
#'
#' @format A data frame with `r nrow(liste_zone)` rows and `r ncol(liste_zone)` variables:
#' \describe{
#'   \item{TypeZone}{Type de la zone : communes,epci,départements,régions}
#'   \item{CodeZone}{Code de la zone}
#'   \item{Zone}{Libellé de la zone}
#'   \item{EPCI}{liste des EPCI ayant cette zone en territoire infra}
#'   \item{NATURE_EPCI}{nature de l'Epci pour les EPCI}
#'   \item{DEP}{liste des départements ayant cette zone en territoire infra}
#'   \item{REG}{liste des régions ayant cette zone en territoire infra}
#' }
"liste_zone"

#' Une table du recensement de la population 2015
#'
#' Une table du recencement de la population de 2015
#'
#' @format A data frame with `r nrow(pop2015)` rows and `r ncol(pop2015)` variables:
#' \describe{
#'   \item{DEPCOM}{Code commune (géographie 2014)}
#'   \item{pop2015}{population 2015}
#'   \item{pop2015_a_part}{population 2015 comptée à part}
#'   \item{pop2015_totale}{population 2015 total}
#' }
"pop2015"


#' Zonage ABC région Pays de la Loire.
#'
#' Un dataset contenant pour chaque commune du GOG 2019 de la région Pays de la Loire le zonage ABC 2014 révisé en 2019 correspondant
#'
#' @format A data frame with `r nrow(zonage_abc_r52)` rows and `r ncol(zonage_abc_r52)` variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee de la commune}
#'   \item{zonage_abc}{Zonage ABC}
#' }
"zonage_abc_r52"

#' Zonage ABC France entière
#'
#' Un dataset contenant pour chaque commune du COG 2019 le zonage ABC 2014 révisé en 2019 correspondant
#'
#' @format A data frame with `r nrow(zonage_abc)` rows and `r ncol(zonage_abc)` variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee de la commune}
#'   \item{zonage_abc}{Zonage ABC}
#' }
"zonage_abc"

#' Zonage Pinel région Pays de la Loire.
#'
#' Un dataset contenant pour la liste des communes 2019 éligible au pinel en 2019
#'
#' @format A data frame with `r nrow(zonage_pinel_r52)` rows and `r ncol(zonage_pinel_r52)` variables:
#' \describe{
#'   \item{DEPCOM}{Code Insee de la commune}
#'   \item{zonage_pinel}{Zonage Pinel}
#' }
"zonage_pinel_r52"

#' Contour des communes de France pour le dernier millésime connu.
#' @name communes_geo_xx
#' @details Des datasets contenant pour chaque commune actuelle son contour.
#'  Un dataset pour la france métropolitaine, un pour chaque DROM, et un assemblage de ces jeux de données pour pourvoir réaliser des cartes France Métropolitaine et DROM.
#' @format Un dataframe avec une ligne par commune et 3 variables.
#' \describe{
#'   \item{DEPCOM}{Code Insee}
#'   \item{geometry}{géométrie}
#'   \item{AREA}{Aire de la commune en m2}
#' }
NULL

#' @rdname communes_geo_xx
#' @format NULL
"communes_geo"

#' @rdname communes_geo_xx
#' @format NULL
"communes_metro_geo"

#' @rdname communes_geo_xx
#' @format NULL
"communes_971_geo"

#' @rdname communes_geo_xx
#' @format NULL
"communes_972_geo"

#' @rdname communes_geo_xx
#' @format NULL
"communes_973_geo"

#' @rdname communes_geo_xx
#' @format NULL
"communes_974_geo"

#' @rdname communes_geo_xx
#' @format NULL
"communes_976_geo"

#' Contour des epci de France pour le dernier millésime connu.
#' @name epci_geo_xx
#' @details Des datasets contenant pour chaque epci actuel son contour.
#'  Un dataset pour la france métropolitaine, un pour chaque DROM, et un assemblage de ces jeux de données pour pourvoir réaliser des cartes France Métropolitaine et DROM.
#' @format Un dataframe avec une ligne par epci et 3 variables.
#' \describe{
#'   \item{EPCI}{Code Sirene}
#'   \item{AREA}{Aire de l'EPCI en m2}
#'   \item{geometry}{géométrie}
#' }
NULL

#' @rdname epci_geo_xx
#' @format NULL
"epci_geo"

#' @rdname epci_geo_xx
#' @format NULL
"epci_metro_geo"

#' @rdname epci_geo_xx
#' @format NULL
"epci_971_geo"

#' @rdname epci_geo_xx
#' @format NULL
"epci_972_geo"

#' @rdname epci_geo_xx
#' @format NULL
"epci_973_geo"

#' @rdname epci_geo_xx
#' @format NULL
"epci_974_geo"

#' @rdname epci_geo_xx
#' @format NULL
"epci_976_geo"

#' Contour des départements de France pour le dernier millésime connu.
#' @name departements_geo_xx
#' @details Un dataset des contours des départements pour la france métropolitaine, un pour chaque DROM, et un assemblage de ces jeux de données pour pourvoir réaliser des cartes France Métropolitaine et DROM.
#' @format Un dataframe avec une ligne par departement et 3 variables.
#' \describe{
#'   \item{DEP}{Code Insee}
#'   \item{AREA}{Aire du departement en m2}
#'   \item{geometry}{géométrie}
#' }
NULL

#' @rdname departements_geo_xx
#' @format NULL
"departements_geo"

#' @rdname departements_geo_xx
#' @format NULL
"departements_metro_geo"

#' @rdname departements_geo_xx
#' @format NULL
"departements_971_geo"

#' @rdname departements_geo_xx
#' @format NULL
"departements_972_geo"

#' @rdname departements_geo_xx
#' @format NULL
"departements_973_geo"

#' @rdname departements_geo_xx
#' @format NULL
"departements_974_geo"

#' @rdname departements_geo_xx
#' @format NULL
"departements_976_geo"


#' Contour des régions de France pour le dernier millésime connu.
#' @name regions_geo_xx
#' @details Dataset des contours régionaux : un pour la france métropolitaine, un pour chaque DROM, et un assemblage de ces jeux de données pour pourvoir réaliser des cartes France Métropolitaine et DROM.
#' @format Un dataframe avec une ligne par région et 3 variables.
#' \describe{
#'   \item{REG}{Code Insee}
#'   \item{AREA}{Aire de la region en m2}
#'   \item{geometry}{géométrie}
#' }
NULL


#' @rdname regions_geo_xx
#' @format NULL
"regions_geo"


#' @rdname regions_geo_xx
#' @format NULL
"regions_metro_geo"

#' @rdname regions_geo_xx
#' @format NULL
"regions_971_geo"

#' @rdname regions_geo_xx
#' @format NULL
"regions_972_geo"

#' @rdname regions_geo_xx
#' @format NULL
"regions_973_geo"

#' @rdname regions_geo_xx
#' @format NULL
"regions_974_geo"

#' @rdname regions_geo_xx
#' @format NULL
"regions_976_geo"
