#' Préparer les données pour passer d'un type dataframe a un type liste
#'
#' @param .data la table de données a convertir
#' @param typezone le type de zonage
#'
#' @return Renvoie une table de données renommée
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select
#' @importFrom tidyselect everything
#' @keywords internal

zone_df_to_list<-function(.data,typezone) {
  if (typezone=="communes") {
    d<-.data %>% dplyr::filter(TypeZone=="Communes")
    if (!is.null(d)){
      d<-d %>%
        mutate(NOM_DEPCOM=Zone,DEPCOM=CodeZone) %>%
        select(-Zone,-CodeZone,-TypeZone) %>%
        select(DEPCOM,NOM_DEPCOM,everything()) %>%
        mutate_at(vars(NOM_DEPCOM,DEPCOM),funs(fct_drop))
    }
  }
  if (typezone=="epci") {
    d<-.data %>% dplyr::filter(TypeZone=="Epci")
    if (!is.null(d)){
      d<-d %>%
        mutate(NOM_EPCI=Zone,EPCI=CodeZone) %>%
        select(-Zone,-CodeZone,-TypeZone) %>%
        select(EPCI,NOM_EPCI,everything()) %>%
        mutate_at(vars(EPCI,NOM_EPCI),funs(fct_drop))
    }
  }
  if (typezone=="departements") {
    d<-.data %>% dplyr::filter(TypeZone=="D\u00e9partements")
    if (!is.null(d)){
      d<-d %>%
        mutate(NOM_DEP=Zone,DEP=CodeZone) %>%
        select(-Zone,-CodeZone,-TypeZone) %>%
        select(DEP,NOM_DEP,everything()) %>%
        mutate_at(vars(DEP,NOM_DEP),funs(fct_drop))
    }
  }
  if (typezone=="regions") {
    d<-.data %>% dplyr::filter(TypeZone=="R\u00e9gions")
    if (!is.null(d)){
      d<-d %>%
        mutate(NOM_REG=Zone,REG=CodeZone) %>%
        select(-Zone,-CodeZone,-TypeZone) %>%
        select(REG,NOM_REG,everything()) %>%
        mutate_at(vars(REG,NOM_REG),funs(fct_drop))
    }
  }
  if (typezone=="metro") {
    d<-.data %>% dplyr::filter(Zone=="France m\u00e9tropolitaine")
    if (!is.null(d)){
      d<-d %>%
        select(-Zone,-CodeZone,-TypeZone)
    }
  }
  if (typezone=="metrodrom") {
    d<-.data %>% dplyr::filter(Zone=="France m\u00e9tropolitaine et DROM")
    if (!is.null(d)){
      d<-d %>%
        select(-Zone,-CodeZone,-TypeZone)
    }
  }
  if (typezone=="franceprovince") {
    d<-.data %>% dplyr::filter(Zone=="France de province")
    if (!is.null(d)){
      d<-d %>%
        select(-Zone,-CodeZone,-TypeZone)
    }
  }
  if (typezone=="drom") {
    d<-.data %>% dplyr::filter(Zone=="D\u00e9partements et r\u00e9gions d'outre-mer")
    if (!is.null(d)){
      d<-d %>%
        select(-Zone,-CodeZone,-TypeZone)
    }
  }
  if (nrow(d)==0){d<-NULL}
  return(d)
}

#' Convertir les données du COG d'un type dataframe a un type liste
#'
#' @param .data la liste de données a convertir
#'
#' @return Renvoie une liste de dataframe
#' @export
#' @importFrom purrr map
#' @importFrom purrr set_names
cog_df_to_list<-function(.data) {
  map(c("communes","epci","departements","regions","metro","metrodrom","franceprovince","drom"),~zone_df_to_list(.data,typezone = .x)) %>%
    set_names(c("communes","epci","departements","regions","metro","metrodrom","franceprovince","drom"))
}
