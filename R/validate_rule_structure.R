#' Validate the structure of YAML rules
#'
#' Ensures that the rules YAML has the required sections and correct format.
#'
#' @param rules A list read from YAML
#' @return The same rules list if valid
#' @export
#' @examples
#' rules <- list(
#'   variables = list(
#'     age = list(
#'       type = "numeric",
#'       rules = list(list(rule = "min", value = 0))
#'     )
#'   )
#' )
#' validate_rule_structure(rules)
validate_rule_structure <- function(rules) {

  if (is.null(rules$variables)) {
    stop("Rules must contain a 'variables' section.")
  }

  if (!is.list(rules$variables)) {
    stop("'variables' must be a named list.")
  }

  for (var in names(rules$variables)) {

    v <- rules$variables[[var]]

    if (is.null(v$type)) {
      stop("Variable '", var, "' is missing 'type'.")
    }

    if (is.null(v$rules)) {
      stop("Variable '", var, "' must define rules.")
    }

    if (!is.list(v$rules)) {
      stop("Rules for variable '", var, "' must be a list.")
    }
  }

  # Optional: check cross_rules if exists
  if (!is.null(rules$cross_rules)) {
    if (!is.list(rules$cross_rules)) {
      stop("cross_rules must be a list.")
    }
    for (cr in rules$cross_rules) {
      if (is.null(cr$rule)) {
        stop("Each cross rule must have a 'rule' field.")
      }
    }
  }

  return(rules)
}
