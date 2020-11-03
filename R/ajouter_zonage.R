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


ajouter_zonage<-function(.data,
                         zonage_df,
                         var_depcom=DEPCOM,
                         var_code_zone=CodeZone,
                         var_type_zone=TypeZone,
                         var_zone=Zone){
  quo_Depcom<-enquo(var_depcom)
  quo_CodeZone<-enquo(var_code_zone)
  quo_TypeZone<-enquo(var_type_zone)
  quo_Zone<-enquo(var_zone)

  a_ajouter<-.data %>%
    filter(.data$TypeZone=="Communes") %>%
    select(-.data$Zone,-.data$TypeZone) %>%
    rename(DEPCOM=.data$CodeZone) %>%
    inner_join(zonage_df %>%
                 rename(DEPCOM=!!quo_Depcom)) %>%
    select(-.data$DEPCOM) %>%
    group_by_if(funs(!is.numeric(.))) %>%
    summarise_all(funs(sum)) %>%
    ungroup %>%
    rename(CodeZone=!!quo_CodeZone,
           TypeZone=!!quo_TypeZone,
           Zone=!!quo_Zone) %>%
    mutate_at(vars(.data$CodeZone,.data$TypeZone,.data$Zone),funs(as.factor))

tmp<-.data %>%
 mutate(CodeZone=fct_expand(.data$CodeZone,
                            levels(a_ajouter$CodeZone)),
        TypeZone=fct_expand(.data$TypeZone,
                            levels(a_ajouter$TypeZone)),
        Zone=fct_expand(.data$Zone,
                            levels(a_ajouter$Zone))
 )

a_ajouter<-mutate(a_ajouter,
                  CodeZone=factor(.data$CodeZone,levels=levels(tmp$CodeZone)),
                  Zone=factor(.data$Zone,levels=levels(tmp$Zone)),
                  TypeZone=factor(.data$TypeZone,levels=levels(tmp$TypeZone)),
)
res<-bind_rows(tmp,a_ajouter)
return(res)
}