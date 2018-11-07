#' Préparer les données pour passer d'un type liste à un type dataframe
#'
#' @param .data la table de données à convertir
#' @param typezone le type de zonage
#'
#' @return la fonction renvoie une table de données renommée
#' @export
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom dplyr select

zone_list_to_df<-function(.data,typezone) {
  if (typezone=="communes") {
    if (is.null(.data)){d<-NULL}
    else {
      d<-.data %>%
        mutate(Zone=NOM_DEPCOM,CodeZone=DEPCOM,TypeZone="Communes") %>%
        select(-NOM_DEPCOM,-DEPCOM) %>%
        select(TypeZone,Zone,CodeZone,everything())
    }
  }
  if (typezone=="epci") {
    if (is.null(.data)){d<-NULL}
    else {
      d<-.data %>%
        mutate(Zone=NOM_EPCI,CodeZone=EPCI,TypeZone="Epci") %>%
        select(-NOM_EPCI,-EPCI) %>%
        select(TypeZone,Zone,CodeZone,everything())
    }
  }
  if (typezone=="departements") {
    if (is.null(.data)){d<-NULL}
    else {
      d<-.data %>%
        mutate(Zone=NOM_DEP,CodeZone=DEP,TypeZone="Départements") %>%
        select(-NOM_DEP,-DEP) %>%
        select(TypeZone,Zone,CodeZone,everything())
    }
  }
  if (typezone=="regions") {
    if (is.null(.data)){d<-NULL}
    else {
      d<-.data %>%
        mutate(Zone=NOM_REG,CodeZone=REG,TypeZone="Régions") %>%
        select(-NOM_REG,-REG) %>%
        select(TypeZone,Zone,CodeZone,everything())
    }
  }
  if (typezone=="metro") {
    if (is.null(.data)){d<-NULL}
    else {
      d<-.data %>%
        mutate(Zone="France métropolitaine",CodeZone="FRMETRO",TypeZone="France") %>%
        select(TypeZone,Zone,CodeZone,everything())
    }
  }
  if (typezone=="metrodrom") {
    if (is.null(.data)){d<-NULL}
    else {
      d<-.data %>%
        mutate(Zone="France métropolitaine et DROM",CodeZone="FRMETRODROM",TypeZone="France") %>%
        select(TypeZone,Zone,CodeZone,everything())
    }
  }
  return(d)
}

#' Convertir les données du COG d'un type liste à un type dataframe
#'
#' @param list la liste de données à convertir
#'
#' @return la fonction renvoie une table de données
#' @export
#' @import magrittr
#' @importFrom purrr map2_df
#' @importFrom dplyr mutate_at

cog_list_to_df<-function(list) {
  map2_df(list,names(list),~ zone_list_to_df(.data = .x,typezone = .y)) %>%
    mutate_at(vars(TypeZone,Zone,CodeZone),as.factor)
}
