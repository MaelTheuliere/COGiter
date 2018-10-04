#' Consolider une table de données à la commune à tous les échelles du cog
#'
#' @param .data la table de données à convertire
#' @param code_commune le nom de la variable contenant le code commune sur 5 charactères
#' @param metro booléen TRUE si on souhaite une aggrégation sur le contour de la France métropolitaine
#' @param metrodrom booléen TRUE si on souhaite une aggrégation sur le contour de la France métropolitaine et des DROM
#'
#' @return la fonction renvoie une table de données
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
#' @examples
cogifier<-function(.data,code_commune=DEPCOM,metro=T,metrodrom=F){
  quo_code_commune<-enquo(code_commune)
  au_cog<-passer_au_cog_a_jour(.data=.data,code_commune=!!quo_code_commune)
  if (metro==T & metrodrom==F) {
    temp1<-au_cog %>%
      dplyr::filter(!(REG %in% c("01","02","03","04","05","06"))) %>%
      mutate(Zone="France métropolitaine" %>% as.factor,
             CodeZone="FRMETRO" %>% as.factor,
             TypeZone="France" %>% as.factor) %>%
      select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(.))) %>%
      ungroup
  }
  if (metro==F & metrodrom==T) {
    temp1<-au_cog %>%
      mutate(Zone="France métropolitaine et DROM" %>% as.factor,
             CodeZone="FRMETREDROM" %>% as.factor,
             TypeZone="France" %>% as.factor) %>%
      select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(.))) %>%
      ungroup
  }
  if (metro==T & metrodrom==T) {
    temp1a<-au_cog %>%
      dplyr::filter(!(REG %in% c("01","02","03","04","05","06"))) %>%
      mutate(Zone="France métropolitaine" %>% as.factor,
             CodeZone="FRMETRO" %>% as.factor,
             TypeZone="France" %>% as.factor) %>%
      select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(.))) %>%
      ungroup
    temp1b<-au_cog %>%
      mutate(Zone="France métropolitaine et DROM" %>% as.factor,
             CodeZone="FRMETREDROM" %>% as.factor,
             TypeZone="France" %>% as.factor) %>%
      select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
      group_by_if(funs(!is.numeric(.))) %>%
      summarise_if(is.numeric,funs(sum(.))) %>%
      ungroup
    temp1<-bind_rows(temp1a,temp1b)
  }
  if (metro==F & metrodrom==F) {
    temp1<-NULL
  }
  temp2<-au_cog %>%
    mutate(Zone=NOM_REG,
           CodeZone=REG,
           TypeZone="Régions" %>% as.factor) %>%
    select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
    group_by_if(funs(!is.numeric(.))) %>%
    summarise_if(is.numeric,funs(sum(.))) %>%
    ungroup
  temp3<-au_cog %>%
    mutate(Zone=NOM_DEP,
           CodeZone=DEP,
           TypeZone="Départements" %>% as.factor) %>%
    select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
    group_by_if(funs(!is.numeric(.))) %>%
    summarise_if(is.numeric,funs(sum(.))) %>%
    ungroup
  temp4<-au_cog %>%
    mutate(Zone=NOM_EPCI,
           CodeZone=EPCI,
           TypeZone="Epci" %>% as.factor) %>%
    select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
    group_by_if(funs(!is.numeric(.))) %>%
    summarise_if(is.numeric,funs(sum(.))) %>%
    ungroup
  temp5<-au_cog %>%
    mutate(Zone=NOM_DEPCOM,
           CodeZone=DEPCOM,
           TypeZone="Communes" %>% as.factor) %>%
    select(-REG,-NOM_REG,-DEP,-NOM_DEP,-DEPCOM,-NOM_DEPCOM,-EPCI,-NOM_EPCI,-DEPARTEMENTS_DE_L_EPCI,-REGIONS_DE_L_EPCI) %>%
    group_by_if(funs(!is.numeric(.))) %>%
    summarise_if(is.numeric,funs(sum(.))) %>%
    ungroup

  result<-bind_rows(temp1,temp2,temp3,temp4,temp5) %>%
    mutate_at(vars(Zone,CodeZone,TypeZone),funs(as.factor))
  result
}
