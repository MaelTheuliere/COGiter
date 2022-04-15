#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' TRUE if x is NULL or x in y
#' @noRd
#'
#' @examples
#' 1 %in_or_null% 1:10
#' d <- NULL
#' d %in_or_null% 1:10
`%in_or_null%` <- function(x,y) {
  if (is.null(x)) return(TRUE)
  else (x %in% y)
}

#' TRUE if x is NULL or x not in y
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' d <- NULL
#' d %not_in_or_null% 1:10
`%not_in_or_null%` <- function(x,y) {
  if (is.null(x)) return(TRUE)
  else (x %not_in% y)
}

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}
