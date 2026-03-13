#' Print abayflowr rules
#'
#' Provides a concise summary when a rules object is printed.
#'
#' @param x An object of class "abayflowr_rules"
#' @param ... Not used
#'
#' @export
print.abayflowr_rules <- function(x, ...) {
  dataset <- x$dataset %||% "unknown"

  n_variables <- length(x$variables)

  n_rules <- sum(
    vapply(
      x$variables,
      function(v) length(v$rules),
      integer(1)
    )
  )

  n_cross <- length(x$cross_rules)

  cat("\nabayflowr validation rules\n")
  cat("--------------------------\n")
  cat("Dataset:        ", dataset, "\n")
  cat("Variables:      ", n_variables, "\n")
  cat("Variable rules: ", n_rules, "\n")
  cat("Cross rules:    ", n_cross, "\n")
  cat("\n")

  invisible(x)
}
