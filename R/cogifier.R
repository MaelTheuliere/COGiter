#' Consolider une table de données à la commune à tous les échelles du cog
#'
#' @param .data la table de données à convertir
#' @param code_commune le nom de la variable contenant le code commune sur 5 charactères
#' @param communes booléen TRUE si on souhaite des données à la commune
#' @param epci booléen TRUE si on souhaite des données à l'epci
#' @param departements booléen TRUE si on souhaite des données au département
#' @param regions booléen TRUE si on souhaite des données à la région
#' @param metro booléen TRUE si on souhaite des données France métropolitaine
#' @param metrodrom booléen TRUE si on souhaite des données France métropolitaine et des DROM
#' @param as_df booléen TRUE si on souhaite des données sous un seul dataframe, FALSE si on souhaite une liste de dataframes par type de zone

#'
#' @return la fonction renvoie un dataframe ou une liste de dataframe
#' @export
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr group_by_if
#' @importFrom dplyr summarise_if
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate_at
#' @importFrom dplyr bind_rows
#' @importFrom dplyr funs
#' @importFrom dplyr vars
#' @importFrom rlang enquo
#' @importFrom rlang !!

cogifier<-function(.data,code_commune=DEPCOM,
                   communes=T,
                   epci=T,
                   departements=T,
                   regions=T,
                   metro=T,
                   metrodrom=F,
                   as_df=T){
  quo_code_commune<-enquo(code_commune)
  au_cog<-passer_au_cog_a_jour(.data=.data,code_commune=!!quo_code_commune,
                               garder_info_supra = T)
  c<-NULL
  e<-NULL
  d<-NULL
  r<-NULL
  m<-NULL
  md<-NULL
  if (communes==T) {
    c<-au_cog %>%
      select(-REG,-NOM_REG,-DEP,-NOM_DEP,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(.))) %>%
      ungroup
  }
  if (epci==T) {
    e<-au_cog %>%
      select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(.))) %>%
      ungroup
  }
  if (departements==T) {
    d<-au_cog %>%
      select(-REG,-NOM_REG,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(.))) %>%
      ungroup
  }
  if (regions==T) {
    r<-au_cog %>%
      select(-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(.))) %>%
      ungroup
    if(metro==T) {
      m<-au_cog %>%
        dplyr::filter(!(REG %in% c("01","02","03","04","05","06"))) %>%
        select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
        group_by_if(funs(!is.numeric(.))) %>%
        summarise_if(is.numeric,funs(sum(.))) %>%
        ungroup
    }
    if(metrodrom==T) {
      md<-au_cog %>%
        select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
        group_by_if(funs(!is.numeric(.))) %>%
        summarise_if(is.numeric,funs(sum(.))) %>%
        ungroup
    }
  }

  result<-list("communes"=c,"epci"=e,"departements"=d,"regions"=r,"metro"=m,"metrodrom"=md)

  if (as_df==T) {
    result<-cog_list_to_df(result)
  }

  return(result)
}
