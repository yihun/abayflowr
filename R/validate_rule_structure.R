#' Validate the structure of YAML rules
#'
#' Validates the structure of a rules object loaded from a YAML configuration
#' file. This function ensures that the expected sections and fields exist
#' and are correctly formatted before they are used by the validation engine.
#'
#' Expected YAML structure:
#'
#' \preformatted{
#' variables:
#'   age:
#'     type: numeric
#'     rules:
#'       - rule: min
#'         value: 0
#'
#' cross_rules:
#'   - rule: compare
#'     left: start_date
#'     right: end_date
#' }
#'
#' @param rules A list read from a YAML file (typically using `yaml::read_yaml()`).
#'
#' @return The validated rules list. If the structure is invalid, the function
#' stops with an informative error.
#'
#' @export
#'
#' @examples
#' rules <- list(
#'   variables = list(
#'     age = list(
#'       type = "numeric",
#'       rules = list(
#'         list(rule = "min", value = 0)
#'       )
#'     )
#'   )
#' )
#'
#' validate_rule_structure(rules)
validate_rule_structure <- function(rules) {
  if (!is.list(rules)) {
    stop("Rules must be a list (typically read from YAML).", call. = FALSE)
  }

  if (is.null(rules$variables)) {
    stop("Rules must contain a 'variables' section.", call. = FALSE)
  }

  if (!is.list(rules$variables) || is.null(names(rules$variables))) {
    stop("'variables' must be a named list.", call. = FALSE)
  }

  for (var in names(rules$variables)) {
    v <- rules$variables[[var]]

    if (!is.list(v)) {
      stop("Definition for variable '", var, "' must be a list.", call. = FALSE)
    }

    if (is.null(v$type)) {
      stop(
        "Variable '",
        var,
        "' is missing required field 'type'.",
        call. = FALSE
      )
    }

    if (is.null(v$rules)) {
      stop("Variable '", var, "' must define a 'rules' section.", call. = FALSE)
    }

    if (!is.list(v$rules)) {
      stop("Rules for variable '", var, "' must be a list.", call. = FALSE)
    }

    for (i in seq_along(v$rules)) {
      rule <- v$rules[[i]]

      if (!is.list(rule)) {
        stop(
          "Rule ",
          i,
          " for variable '",
          var,
          "' must be a list.",
          call. = FALSE
        )
      }

      if (is.null(rule$rule)) {
        stop(
          "Rule ",
          i,
          " for variable '",
          var,
          "' must contain a 'rule' field.",
          call. = FALSE
        )
      }
    }
  }

  # Validate cross-variable rules if present
  if (!is.null(rules$cross_rules)) {
    if (!is.list(rules$cross_rules)) {
      stop("'cross_rules' must be a list.", call. = FALSE)
    }

    for (i in seq_along(rules$cross_rules)) {
      cr <- rules$cross_rules[[i]]

      if (!is.list(cr)) {
        stop("cross_rule ", i, " must be a list.", call. = FALSE)
      }

      if (is.null(cr$rule)) {
        stop("cross_rule ", i, " must contain a 'rule' field.", call. = FALSE)
      }
    }
  }

  rules
}
