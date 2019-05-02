#' Filtrer une fichier du cog sur un sous ensemble du territoire
#'
#' @param .data la table de donn\encoding{é}es a filtrer
#' @param depcom la commune sur laquelle filtrer les donn\encoding{é}es
#' @param epci l'epci sur lequel filtrer les donn\encoding{é}es
#' @param dep le departement sur lequel filtrer les donn\encoding{é}es
#' @param reg la region sur laquelle filtrer les donn\encoding{é}es
#' @param garder_supra ">" si on souhaite garder les territoires supra, ">=" si on souhaite garder les territoires suppra et du même niveau que celui s\encoding{é}lectionn\encoding{é}
#'
#'
#' @return Renvoie une table de donnees filtrées
#' @export
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_detect
#' @importFrom forcats fct_drop
#' @importFrom rlang enquo
#' @importFrom rlang !!
#' @encoding UTF-8

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
            filter(str_detect(REG,reg)) %>%
            select(TypeZone,CodeZone)
        )
    }
    if (garder_supra==">"){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(str_detect(REG,reg)) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France"))
        )
    }
    if (garder_supra==">="){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(str_detect(REG,reg)) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France") |
                             (TypeZone %in% c("R\u00e9gions") & !(CodeZone==!!quo_reg))
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
                    filter(TypeZone %in% c("France","R\u00e9gions"))
        )
    }
    if (garder_supra==">="){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(str_detect(DEP,dep)) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France","R\u00e9gions") |
                             (TypeZone %in% c("D\u00e9partements") & !(CodeZone==!!quo_dep))
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
                    filter(TypeZone %in% c("France","R\u00e9gions","D\u00e9partements"))
        )
    }
    if (garder_supra==">="){
      result<-result %>%
        inner_join(
          liste_zone %>%
            filter(EPCI==!!quo_epci) %>%
            select(TypeZone,CodeZone)) %>%
        bind_rows(result %>%
                    filter(TypeZone %in% c("France","R\u00e9gions","D\u00e9partements") |
                             (TypeZone %in% c("Epci") & !(CodeZone==!!quo_epci))
                    )
        )
    }
  }
  result<- result %>%
    mutate_at(vars(TypeZone,Zone,CodeZone),funs(fct_drop(as.factor(.))))
  return(result)
}
