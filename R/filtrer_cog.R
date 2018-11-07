#' Filtrer une fichier du cog sur un sous ensemble du territoire
#'
#' @param .data la table de données à filtrer
#' @param depcom la commune sur laquelle filtrer les données
#' @param epci lepci sur lequel filtrer les données
#' @param dep le département sur lequel filtrer les données
#' @param reg la région sur laquelle filtrer les données
#'
#'
#' @return la fonction renvoie une table de données filtrer
#' @export
#' @import magrittr
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom rlang enquo
#' @importFrom rlang !!

filtrer_cog<-function(.data,depcom=NULL,epci=NULL,dep=NULL,reg=NULL,garder_supra="non"){
  quo_depcom<-enquo(depcom)
  quo_epci<-enquo(epci)
  quo_dep<-enquo(dep)
  quo_reg<-enquo(reg)
  result<-.data
  if (!is.null(reg)){
    if (garder_supra=="non"){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(str_detect(REG,quo_reg)) %>%
            select(TypeZone,CodeZone)
        )
    }
    if (garder_supra==">"){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(str_detect(REG,quo_reg)) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France"))
        )
    }
    if (garder_supra==">="){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(str_detect(REG,quo_reg)) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France") |
                             (TypeZone %in% c("Régions") & !(CodeZone==!!quo_reg))
                           )
        )
    }
  }
  if (!is.null(dep)){
    if (garder_supra=="non"){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(str_detect(DEP,dep)) %>%
            select(TypeZone,CodeZone)
        )
    }
    if (garder_supra==">"){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(str_detect(DEP,dep)) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France","Régions"))
        )
    }
    if (garder_supra==">="){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(str_detect(DEP,dep)) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France","Régions") |
                             (TypeZone %in% c("Départements") & !(CodeZone==!!quo_dep))
                    )
        )
    }
  }
  if (!is.null(epci)){
    if (garder_supra=="non"){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(EPCI==!!quo_epci) %>%
            select(TypeZone,CodeZone)
        )
    }
    if (garder_supra==">"){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(EPCI==!!quo_epci) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France","Régions","Départements"))
        )
    }
    if (garder_supra==">="){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(EPCI==!!quo_epci) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France","Régions","Départements") |
                             (TypeZone %in% c("Epci") & !(CodeZone==!!quo_epci))
                    )
        )
    }
  }
  result<- result %>%
    mutate_at(vars(TypeZone,Zone,CodeZone),funs(fct_drop(as.factor(.))))
  return(result)
}
