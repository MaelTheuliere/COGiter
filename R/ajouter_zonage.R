#' Ajouter un zonage supra communal spécifique à une table cogifiée
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param .data la table de données cogifiee, comportant des donnees communales, a partir de laquelle calculer les agregats selon le nouveau zonage souhaite
#' @param zonage_df le dataframe contenant le rattachement entre le code commune et le nouveau zonage, contenant 4 informations : le code commune, le code zone dont relève la commune, le libellé de la zone dont relève la commune, le nom du zonage (par exemple 'bassins versants')
#' @param var_depcom le nom de la variable code commune dans zonage_df
#' @param var_code_zone le nom de la variable désignant le code zone dans zonage_df
#' @param var_zone le nom de la variable désignant le libelle de la zone dans zonage_df
#' @param var_type_zone le nom de la variable désignant le nom du zonage dans zonage_df
#' @return Renvoie une table de données cogifiée augmentée des agrégats selon ce nouveau zonage
#' @export
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_if
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr vars
#' @importFrom forcats fct_expand
#' @importFrom rlang enquo
#' @importFrom rlang !!
#' @importFrom rlang .data

ajouter_zonage <- function(.data,
                           zonage_df,
                           var_depcom = DEPCOM,
                           var_code_zone = CodeZone,
                           var_type_zone = TypeZone,
                           var_zone = Zone) {
  quo_Depcom <- enquo(var_depcom)
  quo_CodeZone <- enquo(var_code_zone)
  quo_TypeZone <- enquo(var_type_zone)
  quo_Zone <- enquo(var_zone)

  a_ajouter <- .data %>%
    filter(.data$TypeZone == "Communes") %>%
    select(-"Zone", -"TypeZone") %>%
    rename(DEPCOM = "CodeZone") %>%
    inner_join(zonage_df %>%
      rename(DEPCOM = !!quo_Depcom)) %>%
    select(-"DEPCOM") %>%
    group_by(across(!tidyselect::vars_select_helpers$where(is.numeric))) %>%
    summarise(across(.fns = sum)) %>%
    ungroup() %>%
    rename(
      CodeZone = !!quo_CodeZone,
      TypeZone = !!quo_TypeZone,
      Zone = !!quo_Zone
    ) %>%
    mutate(across(c("CodeZone", "TypeZone", "Zone"), as.factor))

  tmp <- .data %>%
    mutate(
      CodeZone = forcats::fct_expand(
        .data$CodeZone,
        levels(a_ajouter$CodeZone)
      ),
      TypeZone = forcats::fct_expand(
        .data$TypeZone,
        levels(a_ajouter$TypeZone)
      ),
      Zone = forcats::fct_expand(
        .data$Zone,
        levels(a_ajouter$Zone)
      )
    )

  a_ajouter <- mutate(a_ajouter,
    CodeZone = factor(.data$CodeZone, levels = levels(tmp$CodeZone)),
    Zone = factor(.data$Zone, levels = levels(tmp$Zone)),
    TypeZone = factor(.data$TypeZone, levels = levels(tmp$TypeZone)),
  )
  res <- bind_rows(tmp, a_ajouter)
  return(res)
}


#' Ajouter une typologie supra communale spécifique à une table cogifiee
#'
#' Permet de rajouter à une table COGifiée de nouveaux type de zonages basé sur des agrégations par epci, départements, régions... de typologie de commune (exemple : aire d'attraction des villes de l'insee, zonage ABC pour le logement)
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param .data la table de donnees cogifiee, comportant des donnees communales, a partir de laquelle calculer les indicateurs selon la typologie souhaitee
#' @param epci booléen TRUE si on souhaite des données de chaque classe de la typologie à l'epci
#' @param departements booléen TRUE si on souhaite des données de chaque classe de la typologie  au département
#' @param regions booléen TRUE si on souhaite des données de chaque classe de la typologie  à la région
#' @param metro booléen TRUE si on souhaite des données de chaque classe de la typologie sur la France métropolitaine
#' @param metrodrom booléen TRUE si on souhaite des données de chaque classe de la typologie sur la France métropolitaine et des DROM
#' @param franceprovince booléen TRUE si on souhaite des données de chaque classe de la typologie sur la France de province
#' @param drom booléen TRUE si on souhaite des données de chaque classe de la typologie pour les Départements et régions d'outre mer
#' @param zonage_df le dataframe contenant le rattachement entre le code commune et la typologie, contenant 4 informations : le code commune, le code typologique dont relève la commune, le libellé typologique dont relève la commune, le nom de la typologie
#' @param var_depcom le nom de la variable code commune dans zonage_df
#' @param var_code_zone le nom de la variable code zone dans zonage_df
#' @param var_zone le nom de la variable zone dans zonage_df
#' @param var_type_zone le nom de la variable type zone dans zonage_df
#' @return Renvoie une table de donnees cogifiée augmentée des calculs pour ces nouvelles typologies
#' @export
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_if
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr vars
#' @importFrom forcats fct_expand
#' @importFrom rlang enquo
#' @importFrom rlang !!
#' @importFrom rlang .data
#' @examples
#' library(COGiter)
#' zonage_aav <- charger_zonage("AAV2020")
#' cogpop2015 <- cogifier(pop2015)
#' cogpop2015aav <- ajouter_typologie(.data = cogpop2015, zonage_df = zonage_aav)

ajouter_typologie <- function(.data,
                              epci = FALSE,
                              departements = TRUE,
                              regions = TRUE,
                              metro = TRUE,
                              metrodrom = TRUE,
                              drom = TRUE,
                              franceprovince = TRUE,
                              zonage_df,
                              var_depcom = DEPCOM,
                              var_code_zone = CodeZone,
                              var_type_zone = TypeZone,
                              var_zone = Zone) {
  quo_Depcom <- rlang::enquo(var_depcom)
  quo_CodeZone <- rlang::enquo(var_code_zone)
  quo_TypeZone <- rlang::enquo(var_type_zone)
  quo_Zone <- rlang::enquo(var_zone)

  a_ajouter <- .data %>%
    cog_df_to_list() %>%
    .$communes %>%
    dplyr::select(-"NOM_DEPCOM") %>%
    dplyr::inner_join(zonage_df %>%
      dplyr::rename(DEPCOM = {{ var_depcom }}) %>%
      dplyr::mutate(
        CodeZoneTypo = as.factor({{ var_code_zone }}),
        TypeZoneTypo = as.factor({{ var_type_zone }}),
        ZoneTypo = as.factor({{ var_zone }})
      )) %>%
    dplyr::select(-{{ var_code_zone }}, -{{ var_type_zone }}, -{{ var_zone }}) %>%
    cogifier(
      communes = FALSE,
      epci = epci,
      departements = departements,
      regions = regions,
      metro = metro,
      metrodrom = metrodrom,
      drom = drom,
      franceprovince = franceprovince
    ) %>%
    dplyr::mutate(
      TypeZone = paste(.data$TypeZoneTypo, .data$TypeZone, sep = " - ") %>% as.factor(),
      Zone = paste(.data$ZoneTypo, .data$Zone, sep = " - ") %>% as.factor(),
      CodeZone = paste(.data$CodeZoneTypo, .data$CodeZone, sep = " - ") %>% as.factor()
    ) %>%
    dplyr::select(-"CodeZoneTypo", "TypeZoneTypo", "ZoneTypo")

  tmp <- .data %>%
    dplyr::mutate(
      CodeZone = forcats::fct_expand(
        .data$CodeZone,
        levels(a_ajouter$CodeZone)
      ),
      TypeZone = forcats::fct_expand(
        .data$TypeZone,
        levels(a_ajouter$TypeZone)
      ),
      Zone = forcats::fct_expand(
        .data$Zone,
        levels(a_ajouter$Zone)
      )
    )

  a_ajouter <- dplyr::mutate(a_ajouter,
    CodeZone = factor(.data$CodeZone, levels = levels(tmp$CodeZone)),
    Zone = factor(.data$Zone, levels = levels(tmp$Zone)),
    TypeZone = factor(.data$TypeZone, levels = levels(tmp$TypeZone)),
  )
  res <- dplyr::bind_rows(tmp, a_ajouter)
  return(res)
}


#' Créer un zonage supra-communal adapté aux fonctions ajouter_zonage() et ajouter_typologie()
#' `r lifecycle::badge("experimental")`
#'
#' @param zonage Caractère - Zonages parmi ceux disponibles
#'
#' @return Un dataframe
#' @importFrom dplyr select mutate
#' @importFrom purrr set_names
#' @importFrom rlang sym
#' @export
#' @examples
#' charger_zonage("ARR")
charger_zonage <- function(zonage) {
  liste_zonages_disponibles <- lister_zonages()
  zonages_disponibles <- paste0(liste_zonages_disponibles$`Code du zonage`," (",liste_zonages_disponibles$`Nom du zonage`," )", collapse = ", ")
  if (!zonage %in% liste_zonages_disponibles$`Code du zonage`) stop(glue::glue("Zonage non disponible, merci de s\u00e9lectionner un zonage dans la liste suivante : \n{zonages_disponibles}"))
  res <- COGiter::table_passage_communes_zonages %>%
    dplyr::select("DEPCOM",
                  rlang::sym(zonage),
                  rlang::sym(paste0("LIB_", zonage))
    ) %>%
    dplyr::mutate(TypeZone = zonage) %>%
    purrr::set_names("DEPCOM","CodeZone","Zone","TypeZone")
  return(res)
}

#' liste des zonages disponibles
#' @export
lister_zonages <- function(){
  liste_zonages
}
