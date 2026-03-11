#' Print abayflowr rules
#'
#' Displays a summary of loaded validation rules.
#'
#' @param x An object of class `datavalid_rules`
#' @param ... Not used
#'
#' @export
print.datavalid_rules <- function(x, ...) {
  cat("abayflowr validation rules\n")
  cat("--------------------------\n")

  source <- attr(x, "source")

  if (!is.null(source)) {
    cat("Source:", source, "\n")
  }

  cat("Variables:", length(x$variables), "\n")

  n_rules <- sum(vapply(x$variables, function(v) length(v$rules), integer(1)))

  cat("Variable rules:", n_rules, "\n")

  if (!is.null(x$cross_rules)) {
    cat("Cross rules:", length(x$cross_rules), "\n")
  }
}
