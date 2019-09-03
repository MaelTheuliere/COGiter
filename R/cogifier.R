#' Consolider une table de donn\encoding{é}es à la commune à tous les \encoding{é}chelles du cog
#'
#' @param .data la table de donn\encoding{é}es à convertir
#' @param code_commune le nom de la variable contenant le code commune sur 5 charactères
#' @param communes bool\encoding{é}en TRUE si on souhaite des donn\encoding{é}es à la commune
#' @param epci bool\encoding{é}en TRUE si on souhaite des donn\encoding{é}es à l'epci
#' @param departements bool\encoding{é}en TRUE si on souhaite des donn\encoding{é}es au d\encoding{é}partement
#' @param regions bool\encoding{é}en TRUE si on souhaite des donn\encoding{é}es à la r\encoding{é}gion
#' @param metro bool\encoding{é}en TRUE si on souhaite des donn\encoding{é}es France m\encoding{é}tropolitaine
#' @param metrodrom bool\encoding{é}en TRUE si on souhaite des donn\encoding{é}es France m\encoding{é}tropolitaine et des DROM
#' @param as_df bool\encoding{é}en TRUE si on souhaite des donn\encoding{é}es sous un seul dataframe, FALSE si on souhaite une liste de dataframe par type de zone
#' @param ... argument(s) pass\encoding{é}(s) à la fonction d'aggr\encoding{é}gation (sum), na.rm=F par défaut
#'
#' @return Renvoie un dataframe ou une liste de dataframe
#' @export
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
#' @encoding UTF-8

cogifier<-function(.data,code_commune=DEPCOM,
                   communes=T,
                   epci=T,
                   departements=T,
                   regions=T,
                   metro=T,
                   metrodrom=F,
                   as_df=T,
                   ...){
  quo_code_commune<-enquo(code_commune)
  au_cog<-passer_au_cog_a_jour(.data=.data,code_commune=!!quo_code_commune,
                               garder_info_supra = T, aggrege = F)
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
      summarise_if(is.numeric,funs(sum(., ...))) %>%
      ungroup
  }
  if (epci==T) {
    e<-au_cog %>%
      select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      filter(EPCI!="ZZZZZZZZZ") %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(., ...))) %>%
      ungroup
  }
  if (departements==T) {
    d<-au_cog %>%
      select(-REG,-NOM_REG,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(., ...))) %>%
      ungroup
  }
  if (regions==T) {
    r<-au_cog %>%
      select(-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(., ...))) %>%
      ungroup
    if(metro==T) {
      m<-au_cog %>%
        dplyr::filter(!(REG %in% c("01","02","03","04","05","06"))) %>%
        select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
        group_by_if(funs(!is.numeric(.))) %>%
        summarise_if(is.numeric,funs(sum(., ...))) %>%
        ungroup
    }
    if(metrodrom==T) {
      md<-au_cog %>%
        select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
        group_by_if(funs(!is.numeric(.))) %>%
        summarise_if(is.numeric,funs(sum(., ...))) %>%
        ungroup
    }
  }

  result<-list("communes"=c,"epci"=e,"departements"=d,"regions"=r,"metro"=m,"metrodrom"=md)

  if (as_df==T) {
    result<-cog_list_to_df(result)
  }

  return(result)
}
