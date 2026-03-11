#' Normalize rule structure
#'
#' Standardizes the structure of rules loaded from YAML so that downstream
#' validation functions can work with a consistent format.
#'
#' This function ensures:
#' - every variable rule has a rule name
#' - rule fields exist even if missing
#' - cross_rules exist as an empty list if not provided
#'
#' @param rules A validated rules list
#'
#' @return A normalized rules list
#'
#' @keywords internal
normalize_rules <- function(rules) {
  # Ensure cross_rules exists
  if (is.null(rules$cross_rules)) {
    rules$cross_rules <- list()
  }

  # Normalize variable rules
  for (var in names(rules$variables)) {
    v <- rules$variables[[var]]

    for (i in seq_along(v$rules)) {
      r <- v$rules[[i]]

      # Auto-generate rule name if missing
      if (is.null(r$name)) {
        r$name <- paste0(var, "_", r$rule, "_", i)
      }

      # Ensure message field exists
      if (is.null(r$message)) {
        r$message <- NULL
      }

      v$rules[[i]] <- r
    }
    rules$variables[[var]] <- v
  }
  rules
}
