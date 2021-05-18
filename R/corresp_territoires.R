#' teste que le code region existe
#'
#' @param region Character. Le code de la region concernee
#' @return TRUE/FALSE
#' @importFrom dplyr distinct pull
#' @importFrom tidyr unnest
#' @examples code_reg_existe("75")
#' @export

code_reg_existe <- function(region){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(REG) %>%
    distinct(REG) %>%
    pull() %>%
    as.character()

  if (region %in% list_code) { return(TRUE) }
  else { return(FALSE)  }
}


#' teste que le code departement existe
#'
#' @param departement Character. Le code du departement concerné
#' @return TRUE/FALSE
#' @importFrom dplyr distinct pull
#' @importFrom tidyr unnest
#' @examples code_dep_existe("24")
#' @export

code_dep_existe <- function(departement){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(DEP) %>%
    distinct(DEP) %>%
    pull() %>%
    as.character()

  if (departement %in% list_code) { return(TRUE) }
  else { return(FALSE) }
}


#' teste que le code EPCI existe
#'
#' @param epci Character. Le code de l'EPCI concerné
#' @return TRUE/FALSE
#' @importFrom dplyr distinct pull
#' @importFrom tidyr unnest
#' @examples code_epci_existe("249740077")
#' @export

code_epci_existe <- function(epci){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(EPCI) %>%
    distinct(EPCI) %>%
    pull() %>%
    as.character()

  if (epci %in% list_code) { return(TRUE) }
  else { return(FALSE) }
}


#' teste que le code commune existe
#'
#' @param commune Character. Le code de la commune concernée
#' @return TRUE/FALSE
#' @importFrom dplyr filter distinct pull
#' @importFrom tidyr unnest
#' @examples code_com_existe("75056")
#' @export

code_com_existe <- function(commune){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(CodeZone) %>%
    filter(TypeZone== "Communes") %>%
    distinct(CodeZone) %>%
    pull() %>%
    as.character()

  if (commune %in% list_code) { return(TRUE) }
  else { return(FALSE)  }
}

#' test si le code commune est un code ayant existé mais qui n'est plus valide
#'
#' @param commune Character. code commune
#' @return TRUE/FALSE
#' @export
#'
#' @examples code_com_non_a_jour("75056")
code_com_non_a_jour <- function(commune) {
  liste_non_a_jour <- setdiff(table_passage_com_historique$DEPCOM_HIST,
                              table_passage_com_historique$DEPCOM)
  if (!(commune %in% table_passage_com_historique$DEPCOM_HIST)) {
    stop("commune n'est pas un code commune ayant exist\u00e9")
  }
  return(commune %in% liste_non_a_jour)
}

#' test si le code commune est celui d'un arrondissement de Paris, Lyon ou Marseille
#'
#' @param commune Character. code commune
#' @return TRUE/FALSE
#' @export
#'
#' @examples code_com_plm("75120")
code_com_plm <- function(commune) {
  if (!(commune %in% table_passage_com_historique$DEPCOM_HIST)) {
    stop("commune n'est pas un code commune valide")
  }
  return(commune %in% arn_plm$ARN)
}

#' Renvoie la liste des departements pour une region
#'
#' @param region Character. Le code de la region concernee
#' @return une liste de caractere
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples list_dep_in_reg("75")
#' @export

list_dep_in_reg <- function(region){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(REG) %>%
    filter(TypeZone== "D\u00e9partements", REG == as.character(region)) %>%
    pull(CodeZone) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}


#' Renvoie le code de la région d appartenance d un département
#'
#' @param departement Character. Le code du département concerné
#' @return un caractere
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples code_reg_of_dep("24")
#' @export

code_reg_of_dep <- function(departement){

  list_code <-  COGiter::liste_zone %>%
    filter(TypeZone== "D\u00e9partements", DEP == as.character(departement)) %>%
    tidyr::unnest(REG) %>%
    pull(REG) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}


#' Renvoie la liste des EPCI pour un département
#'
#' @param departement Character. Le code du département concerné
#' @return une liste de caractère
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples list_epci_in_dep("24")
#' @export

list_epci_in_dep <- function(departement){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(DEP) %>%
    filter(TypeZone== "Epci", DEP == as.character(departement)) %>%
    pull(CodeZone) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}


#' Renvoie le code du département d appartenance d un epci
#'
#' @param epci Character. Le code de l'epci concerné
#' @return un caractere
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples code_dep_of_epci("200070647")
#' @export

code_dep_of_epci <- function(epci){

  list_code <-  COGiter::liste_zone %>%
    filter(TypeZone== "Epci", EPCI == as.character(epci)) %>%
    tidyr::unnest(DEP) %>%
    pull(DEP) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}

#' Renvoie la liste des EPCI pour une région
#'
#' @param regions Character. Le code de la région concernée
#' @return une liste de caractère
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples list_epci_in_reg("52")
#' @export

list_epci_in_reg <- function(regions){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(REG) %>%
    filter(TypeZone== "Epci", REG == as.character(regions)) %>%
    pull(CodeZone) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}

#' Renvoie le code de la région d appartenance d un epci
#'
#' @param epci Character. Le code de l'epci concerné
#' @return un caractere
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples code_reg_of_epci("200070647")
#' @export

code_reg_of_epci <- function(epci){

  list_code <-  COGiter::liste_zone %>%
    filter(TypeZone== "Epci", EPCI == as.character(epci)) %>%
    tidyr::unnest(REG) %>%
    pull(REG) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}

#' Renvoie la liste des communes pour un epci
#'
#' @param epci Character. Le code de l'epci concerné
#' @return une liste de caractère
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples list_com_in_epci("200070647")
#' @export

list_com_in_epci <- function(epci){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(EPCI) %>%
    filter(TypeZone== "Communes", EPCI == as.character(epci)) %>%
    pull(CodeZone) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}


#' Renvoie le code de l'epci d appartenance d une commune
#'
#' @param com Character. Le code de la commune concernée
#' @return un caractere
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples code_epci_of_com("24037")
#' @export

code_epci_of_com <- function(com){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(CodeZone) %>%
    filter(TypeZone== "Communes", CodeZone == as.character(com)) %>%
    pull(EPCI) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}


#' Renvoie le code du département d appartenance d une commune
#'
#' @param com Character. Le code de la commune concernée
#' @return un caractere
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples code_dep_of_com("24037")
#' @export

code_dep_of_com <- function(com){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(CodeZone) %>%
    filter(TypeZone== "Communes", CodeZone == as.character(com)) %>%
    pull(DEP) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}


#' Renvoie la liste des communes pour un département
#'
#' @param departement Character. Le code du département concerné
#' @return une liste de caractère
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples list_com_in_dep("24")
#' @export

list_com_in_dep <- function(departement){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(DEP) %>%
    filter(TypeZone== "Communes", DEP == as.character(departement)) %>%
    pull(CodeZone) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}

#' Renvoie le code de la région d appartenance d une commune
#'
#' @param com Character. Le code de la commune concernée
#' @return un caractere
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples code_reg_of_com("24037")
#' @export

code_reg_of_com <- function(com){

  list_code <-  COGiter::liste_zone %>%
    filter(TypeZone== "Communes", CodeZone == as.character(com)) %>%
    tidyr::unnest(REG) %>%
    pull(REG) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}


#' Renvoie la liste des communes pour une région
#'
#' @param regions Character. Le code de la région concernée
#' @return une liste de caractère
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples list_com_in_reg("52")
#' @export

list_com_in_reg <- function(regions){

  list_code <-  COGiter::liste_zone %>%
    tidyr::unnest(REG) %>%
    filter(TypeZone== "Communes", REG == as.character(regions)) %>%
    pull(CodeZone) %>%
    as.character()

  if (length(list_code)!=0) { return(list_code) }
  else { warning("Code non existant")  }
}

