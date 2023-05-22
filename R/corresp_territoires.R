#' teste que le code du territoire existe
#' @param reg Vecteur de caractère. Le code de la region concernée.
#' @param dep Vecteur de caractère. Le code du departement concernée.
#' @param epci Vecteur de caractère. Le code de l'epci concernée.
#' @param depcom Vecteur de caractère. Le code de la commune concernée.
#' @return Vecteur de booléen. TRUE si le code existe.
#' @importFrom purrr map_lgl
#' @importFrom dplyr distinct pull
#' @importFrom tidyr unnest
#' @name code_xx_existe
NULL



#' Renvoie la liste des territoires pour un ou des territoires
#' @param reg Vecteur de caractère. Le code de la région concernée.
#' @param dep Vecteur de caractère. Le code du département concernée.
#' @param epci Vecteur de caractère. Le code de l'epci concernée.
#' @return Un vecteur de caractère.
#' @importFrom purrr map_lgl
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @importFrom glue glue
#' @name list_xx_in_yy
NULL

code_reg_existe_int <- function(reg) {
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(REG) %>%
    dplyr::distinct(REG) %>%
    dplyr::pull() %>%
    as.character()

  if (reg %in% list_code) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

#' teste que des codes regions existent
#' @rdname code_xx_existe
#' @export
#' @examples
#' code_reg_existe(c("11", "99"))
code_reg_existe <- function(reg) {
  purrr::map_lgl(reg, code_reg_existe_int)
}


code_dep_existe_int <- function(dep) {
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(DEP) %>%
    dplyr::distinct(DEP) %>%
    dplyr::pull() %>%
    as.character()

  if (dep %in% list_code) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

#' teste que des codes département existent
#'
#' @rdname code_xx_existe
#' @export
#' @examples
#' code_dep_existe(c("75", "99"))
code_dep_existe <- function(dep) {
  purrr::map_lgl(dep, code_dep_existe_int)
}


code_epci_existe_int <- function(epci) {
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(EPCI) %>%
    dplyr::distinct(EPCI) %>%
    dplyr::pull() %>%
    as.character()

  if (epci %in% list_code) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}
#' Test que les codes EPCI existent
#' @rdname code_xx_existe
#' @examples
#' code_epci_existe("249740077")
#' @export
code_epci_existe <- function(epci) {
  purrr::map_lgl(epci, code_epci_existe_int)
}


code_com_existe_int <- function(depcom) {
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(CodeZone) %>%
    dplyr::filter(TypeZone == "Communes") %>%
    dplyr::distinct(CodeZone) %>%
    dplyr::pull() %>%
    as.character()

  if (depcom %in% list_code) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}
#' teste que les codes commune existent
#' @rdname code_xx_existe
#' @examples
#' code_com_existe("75056")
#' @export
code_com_existe <- function(depcom) {
  purrr::map_lgl(depcom, code_com_existe_int)
}


code_com_non_a_jour_int <- function(depcom) {
  liste_non_a_jour <- setdiff(
    table_passage_com_historique$DEPCOM_HIST,
    table_passage_com_historique$DEPCOM
  )
  if (!(depcom %in% table_passage_com_historique$DEPCOM_HIST)) {
    stop("depcom n'est pas un code commune ayant exist\u00e9")
  }
  return(depcom %in% liste_non_a_jour)
}
#' Test si les codes communes sont des codes ayant existé mais qui ne sont plus valide
#'
#' @param depcom Un vecteur de caractères. Un vecteur de codes commune.
#' @return Un vecteur de booléens.
#' @importFrom purrr map_lgl
#' @examples
#' code_com_non_a_jour("75056")
#' @export
code_com_non_a_jour <- function(depcom) {
  purrr::map_lgl(depcom, code_com_non_a_jour_int)
}


code_com_plm_int <- function(depcom) {
  if (!(depcom %in% table_passage_com_historique$DEPCOM_HIST)) {
    stop("depcom n'est pas un code commune valide")
  }
  return(depcom %in% arn_plm$ARN)
}
#' test si les codes communes sont ceux d'un arrondissement de Paris, Lyon ou Marseille
#'
#' @param depcom Character. Un vecteur de codes commune.
#' @return Un vecteur de booléens.
#' @importFrom purrr map_lgl
#' @examples
#' code_com_plm(c("75056", "75112"))
#' @export
code_com_plm <- function(depcom) {
  purrr::map_lgl(depcom, code_com_plm_int)
}

#' Renvoie la liste des départements pour une ou des régions
#' @examples
#' list_dep_in_reg("75")
#' @export
#' @rdname list_xx_in_yy
list_dep_in_reg <- function(reg) {
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(REG) %>%
    dplyr::filter(TypeZone == "D\u00e9partements", REG %in% as.character(reg)) %>%
    dplyr::pull(CodeZone) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}



code_reg_of_dep_int <- function(dep) {
  list_code <- COGiter::liste_zone %>%
    dplyr::filter(TypeZone == "D\u00e9partements", DEP == as.character(dep)) %>%
    tidyr::unnest(REG) %>%
    dplyr::pull(REG) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}

#' Renvoie les codes de la région d appartenance de chaque département
#'
#' @param dep Character. Un vecteur de codes département
#' @return Un vecteur de caractère.
#' @importFrom purrr map_chr
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @importFrom glue glue
#' @examples
#' code_reg_of_dep(c("75", "44"))
#' @export
code_reg_of_dep <- function(dep) {
  purrr::map_chr(dep, code_reg_of_dep_int)
}
#' Renvoie la liste des EPCI pour un département
#' @examples
#' list_epci_in_dep("24")
#' @export
#' @rdname list_xx_in_yy
list_epci_in_dep <- function(dep) {
  if (!all(dep %in% COGiter::departements$DEP)) {
    if (sum(!(dep %in% COGiter::departements$DEP))==1) {
      warning(glue::glue("{dep[!(dep %in% departements$DEP)]} n'est pas un code d\u00e9partement valide"))
    }
    if (sum(!(dep %in% COGiter::departements$DEP))>1) {
      warning(glue::glue("{paste(dep[!(dep %in% departements$DEP)], collapse = ', ')} ne sont pas des codes d\u00e9partement valides"))
    }
  }
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(DEP) %>%
    dplyr::filter(TypeZone == "Epci", DEP %in% as.character(dep)) %>%
    dplyr::pull(CodeZone) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
}


code_dep_of_epci_int <- function(epci) {
  list_code <- COGiter::liste_zone %>%
    dplyr::filter(TypeZone == "Epci", EPCI == as.character(epci)) %>%
    tidyr::unnest(DEP) %>%
    dplyr::pull(DEP) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}
#' Renvoie le code du département d appartenance de chaque epci
#'
#' @param epci Character. Un vecteur de code epci.
#' @return Un vecteur.
#' @importFrom purrr map
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples
#' code_dep_of_epci(c("200070647"))
#' @export
code_dep_of_epci <- function(epci) {
  purrr::map(epci, code_dep_of_epci_int)
}
#' Renvoie la liste des EPCI pour une région
#'
#' @examples
#' list_epci_in_reg("52")
#' @importFrom glue glue
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @export
#' @rdname list_xx_in_yy
list_epci_in_reg <- function(reg) {
  if (!all(reg %in% COGiter::regions$REG)) {
    if (sum(!(reg %in% COGiter::regions$REG)) == 1) {
      warning(glue::glue("{reg[!(reg %in% regions$REG)]} n'est pas un code r\u00e9gion valide"))
    }
    if (sum(!(reg %in% COGiter::regions$REG))>1) {
      warning(glue::glue("{paste(reg[!(reg %in% regions$REG)], collapse = ', ')} ne sont pas des codes r\u00e9gion valides"))
    }
  }
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(REG) %>%
    dplyr::filter(TypeZone == "Epci", REG %in% as.character(reg)) %>%
    dplyr::pull(CodeZone) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}

code_reg_of_epci_int <- function(epci) {
  list_code <- COGiter::liste_zone %>%
    dplyr::filter(TypeZone == "Epci", EPCI == as.character(epci)) %>%
    tidyr::unnest(REG) %>%
    dplyr::pull(REG) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}
#' Renvoie le code du région d appartenance de chaque epci
#'
#' @param epci Character. Un vecteur de codes epci.
#' @return une liste
#' @importFrom purrr map
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @importFrom glue glue
#' @examples
#' code_reg_of_epci(c("200070647"))
#' @export
code_reg_of_epci <- function(epci) {
  purrr::map(epci, code_reg_of_epci_int)
}
#' Renvoie la liste des communes pour un epci
#' @examples
#' list_com_in_epci("200070647")
#' @export
#' @name list_xx_in_yy
list_com_in_epci <- function(epci) {
  if (!all(epci %in% COGiter::epci$EPCI)) {
    if (sum(!(epci %in% COGiter::epci$EPCI))==1) {
      warning(glue::glue("{epci[!(epci %in% COGiter::epci$EPCI)]} n'est pas un code epci valide"))
    }
    if (sum(!(epci %in% COGiter::epci$EPCI))>1) {
      warning(glue::glue("{paste(epci[!(epci %in% COGiter::epci$EPCI)], collapse = ', ')} ne sont pas des codes epci valides"))
    }
  }

  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(EPCI) %>%
    dplyr::filter(TypeZone == "Communes", EPCI == as.character(epci)) %>%
    dplyr::pull(CodeZone) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}


code_epci_of_com_int <- function(depcom) {
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(CodeZone) %>%
    dplyr::filter(TypeZone == "Communes", CodeZone == as.character(depcom)) %>%
    dplyr::pull(EPCI) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}
#' Renvoie le code de l'epci d appartenance d une commune
#'
#' @param depcom Character. Un vecteur de code commune.
#' @return Un vecteur
#' @importFrom purrr map_chr
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples
#' code_epci_of_com(c("24037"))
#' @export
code_epci_of_com <- function(depcom) {
  purrr::map_chr(depcom, code_epci_of_com_int)
}

code_dep_of_com_int <- function(depcom) {
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(CodeZone) %>%
    dplyr::filter(TypeZone == "Communes", CodeZone %in% as.character(depcom)) %>%
    dplyr::pull(DEP) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}

#' Renvoie le code du département d appartenance d une commune
#'
#' @param depcom Character. Un vecteur de codes commune.
#' @return Un vecteur.
#' @importFrom purrr map_chr
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples
#' code_dep_of_com(c("24037", "44109"))
#' @export
code_dep_of_com <- function(depcom) {
  purrr::map_chr(depcom, code_dep_of_com_int)
}

#' Renvoie la liste des communes pour un ou des départements
#' @examples
#' list_com_in_dep("24")
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @export
#' @rdname list_xx_in_yy
list_com_in_dep <- function(dep) {
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(DEP) %>%
    dplyr::filter(TypeZone == "Communes", DEP %in% as.character(dep)) %>%
    dplyr::pull(CodeZone) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}

code_reg_of_com_int <- function(depcom) {
  list_code <- COGiter::liste_zone %>%
    dplyr::filter(TypeZone == "Communes", CodeZone == as.character(depcom)) %>%
    tidyr::unnest(REG) %>%
    dplyr::pull(REG) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}
#' Renvoie le code région d appartenance d une commune
#'
#' @param depcom Character. Un vecteur de codes commune.
#' @return Un vecteur.
#' @importFrom purrr map_chr
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @examples
#' code_reg_of_com(c("24037", "44109"))
#' @export
code_reg_of_com <- function(depcom) {
  purrr::map_chr(depcom, code_reg_of_com_int)
}


#' Renvoie la liste des communes pour une région
#' @examples
#' list_com_in_reg("52")
#' @export
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @rdname list_xx_in_yy
list_com_in_reg <- function(reg) {
  list_code <- COGiter::liste_zone %>%
    tidyr::unnest(REG) %>%
    dplyr::filter(TypeZone == "Communes", REG %in% as.character(reg)) %>%
    dplyr::pull(CodeZone) %>%
    as.character()

  if (length(list_code) != 0) {
    return(list_code)
  }
  else {
    warning("Code non existant")
  }
}


#' Ajouter des variables sur le rattachement supra communal dans un dataframe a la commune
#' @param .data Table en entrée.
#' @param code_commune Nom de la variable avec le code commune.
#' @return un dataframe
#' @name add_xx_yy
NULL

#' `add_epci_of_com()` ajouter le code epci
#' @export
#' @importFrom dplyr left_join
#' @importFrom rlang set_names ensym
#' @rdname add_xx_yy
add_epci_of_com <- function(.data,
                            code_commune){
  .data %>%
    dplyr::left_join(COGiter::communes[, c("DEPCOM", "EPCI")],
                     by = rlang::set_names(nm =  rlang::ensym(code_commune), "DEPCOM")
    )
}

#' `add_dep_of_com()` ajouter le code département
#' @export
#' @importFrom dplyr left_join
#' @importFrom rlang set_names ensym
#' @rdname add_xx_yy
add_dep_of_com <- function(.data,
                           code_commune){
  .data %>%
    dplyr::left_join(COGiter::communes[, c("DEPCOM", "DEP")],
                     by = rlang::set_names(nm =  rlang::ensym(code_commune), "DEPCOM")
    )
}

#' `add_reg_of_com()` ajouter le code région
#' @export
#' @importFrom dplyr left_join
#' @importFrom rlang set_names ensym
#' @rdname add_xx_yy
add_reg_of_com <- function(.data,
                           code_commune){
  .data %>%
    dplyr::left_join(COGiter::communes[, c("DEPCOM", "REG")],
                     by = rlang::set_names(nm = rlang::ensym(code_commune), "DEPCOM")
    )
}
