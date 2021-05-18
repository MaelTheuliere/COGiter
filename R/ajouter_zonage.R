#' Ajouter un zonage supra communal spécifique à une table cogifiée
#'
#' @param .data la table de données a filtrer
#' @param zonage_df le dataframe contenant le rattachement entre le code commune et le nouveau zonage
#' @param var_depcom le nom de la variable code commune dans zonage_df
#' @param var_code_zone le nom de la variable code zone dans zonage_df
#' @param var_type_zone le nom de la variable type zone dans zonage_df
#' @param var_zone le nom de la variable zone dans zonage_df
#'
#'
#' @return Renvoie une table de donnees cogifiée augmentée des calculs pour ce nouveau zonage
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
    select(-.data$Zone, -.data$TypeZone) %>%
    rename(DEPCOM = .data$CodeZone) %>%
    inner_join(zonage_df %>%
      rename(DEPCOM = !!quo_Depcom)) %>%
    select(-.data$DEPCOM) %>%
    group_by_if(funs(!is.numeric(.))) %>%
    summarise_all(funs(sum)) %>%
    ungroup() %>%
    rename(
      CodeZone = !!quo_CodeZone,
      TypeZone = !!quo_TypeZone,
      Zone = !!quo_Zone
    ) %>%
    mutate_at(vars(.data$CodeZone, .data$TypeZone, .data$Zone), funs(as.factor))

  tmp <- .data %>%
    mutate(
      CodeZone = fct_expand(
        .data$CodeZone,
        levels(a_ajouter$CodeZone)
      ),
      TypeZone = fct_expand(
        .data$TypeZone,
        levels(a_ajouter$TypeZone)
      ),
      Zone = fct_expand(
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


#' Ajouter une typologie supra communale spécifique à une table cogifiée
#'
#' @param .data la table de données a filtrer
#' @param epci booléen TRUE si on souhaite des données de chaque classe de la typologiee à l'epci
#' @param departements booléen TRUE si on souhaite des données de chaque classe de la typologiee  au département
#' @param regions booléen TRUE si on souhaite des données de chaque classe de la typologiee  à la région
#' @param metro booléen TRUE si on souhaite des données de chaque classe de la typologiee sur la France métropolitaine
#' @param metrodrom booléen TRUE si on souhaite des données de chaque classe de la typologiee sur la France métropolitaine et des DROM
#' @param franceprovince booléen TRUE si on souhaite des données de chaque classe de la typologiee sur la France de province
#' @param drom booléen TRUE si on souhaite des données de chaque classe de la typologiee pour les Départements et régions d'outre mer
#' @param zonage_df le dataframe contenant le rattachement entre le code commune et la typologie
#' @param var_depcom le nom de la variable code commune dans zonage_df
#' @param var_code_zone le nom de la variable code zone dans zonage_df
#' @param var_type_zone le nom de la variable type zone dans zonage_df
#' @param var_zone le nom de la variable zone dans zonage_df
#'
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
  quo_Depcom <- enquo(var_depcom)
  quo_CodeZone <- enquo(var_code_zone)
  quo_TypeZone <- enquo(var_type_zone)
  quo_Zone <- enquo(var_zone)

  a_ajouter <- .data %>%
    cog_df_to_list() %>%
    .$communes %>%
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
      TypeZone = paste(TypeZoneTypo, TypeZone, sep = " - ") %>% as.factor(),
      Zone = paste(ZoneTypo, Zone, sep = " - ") %>% as.factor(),
      CodeZone = paste(CodeZoneTypo, CodeZone, sep = " - ") %>% as.factor()
    ) %>%
    dplyr::select(-CodeZoneTypo, -TypeZoneTypo, -ZoneTypo)

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
