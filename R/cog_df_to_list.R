#' Préparer les données pour passer d'un type dataframe à un type liste
#'
#' @param .data la table de données à convertir
#' @param typezone le type de zonage
#'
#' @return la fonction renvoie une table de données renommée
#' @export
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select


zone_df_to_list<-function(.data,typezone) {
  if (typezone=="communes") {
    d<-.data %>% filter(TypeZone=="Communes")
    if (!is.null(d)){
      d<-d %>%
        mutate(NOM_DEPCOM=Zone,DEPCOM=CodeZone) %>%
        select(-Zone,-CodeZone,-TypeZone) %>%
        select(DEPCOM,NOM_DEPCOM,everything()) %>%
        mutate_at(vars(NOM_DEPCOM,DEPCOM),funs(fct_drop))
    }
  }
  if (typezone=="epci") {
    d<-.data %>% filter(TypeZone=="Epci")
    if (!is.null(d)){
      d<-d %>%
        mutate(NOM_EPCI=Zone,EPCI=CodeZone) %>%
        select(-Zone,-CodeZone,-TypeZone) %>%
        select(EPCI,NOM_EPCI,everything()) %>%
        mutate_at(vars(EPCI,NOM_EPCI),funs(fct_drop))
    }
  }
  if (typezone=="departements") {
    d<-.data %>% filter(TypeZone=="Départements")
    if (!is.null(d)){
      d<-d %>%
        mutate(NOM_DEP=Zone,DEP=CodeZone) %>%
        select(-Zone,-CodeZone,-TypeZone) %>%
        select(DEP,NOM_DEP,everything()) %>%
        mutate_at(vars(DEP,NOM_DEP),funs(fct_drop))
    }
  }
  if (typezone=="regions") {
    d<-.data %>% filter(TypeZone=="Régions")
    if (!is.null(d)){
      d<-d %>%
        mutate(NOM_REG=Zone,REG=CodeZone) %>%
        select(-Zone,-CodeZone,-TypeZone) %>%
        select(REG,NOM_REG,everything()) %>%
        mutate_at(vars(REG,NOM_REG),funs(fct_drop))
    }
  }
  if (typezone=="metro") {
    d<-.data %>% filter(Zone=="France métropolitaine")
    if (!is.null(d)){
      d<-d %>%
        select(-Zone,-CodeZone,-TypeZone)
    }
  }
  if (typezone=="metrodrom") {
    d<-.data %>% filter(Zone=="France métropolitaine et DROM")
    if (!is.null(d)){
      d<-d %>%
        select(-Zone,-CodeZone,-TypeZone)
    }
  }
  if (nrow(d)==0){d<-NULL}
  return(d)
}

#' Convertir les données du COG d'un type dataframe à un type liste
#'
#' @param data la liste de données à convertir
#'
#' @return la fonction renvoie une liste de dataframe
#' @export
#' @import magrittr
#' @importFrom purrr map
#' @importFrom purrr set_names

cog_df_to_list<-function(.data) {
  map(c("communes","epci","departements","regions","metro","metrodrom"),~zone_df_to_list(.data,typezone = .x)) %>%
    set_names(c("communes","epci","departements","regions","metro","metrodrom"))
}
