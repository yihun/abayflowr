#' Count total rules
#'
#' Counts all variable-level rules in a rule object.
#'
#' @param rules Rule object
#'
#' @return Integer count
#'
#' @keywords internal
count_rules <- function(rules) {
  sum(vapply(
    rules$variables,
    function(v) length(v$rules),
    integer(1)
  ))
}
