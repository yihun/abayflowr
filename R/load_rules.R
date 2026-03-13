#' Load validation rules from a YAML file
#'
#' @param file Path to YAML file containing rules
#' @param validate Logical. Validate the structure of the rules file
#' @param verbose Logical. Print loading messages
#'
#' @return A list of rules with class "abayflowr_rules" and attribute "source"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rules <- load_rules("rules.yaml")
#' }
load_rules <- function(file, validate = TRUE, verbose = TRUE) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required.", call. = FALSE)
  }

  if (!file.exists(file)) {
    stop("Rules file does not exist: ", file, call. = FALSE)
  }

  if (verbose) {
    cli_start("Loading validation rules")
    cli_info("Reading YAML file")
  }

  rules <- yaml::read_yaml(file)

  if (validate) {
    if (verbose) {
      cli_info("Validating rule structure")
    }
    rules <- validate_rule_structure(rules)
  }

  rules <- normalize_rules(rules)

  attr(rules, "source") <- normalizePath(file)
  class(rules) <- c("abayflowr_rules", class(rules))

  n_rules <- sum(
    vapply(
      rules$variables,
      function(v) length(v$rules),
      integer(1)
    )
  )

  if (verbose) {
    cli_success("Rules loaded successfully")

    cli_rule("Dataset", rules$dataset %||% "unknown")
    cli_rule("Variables", length(rules$variables))
    cli_rule("Rules", n_rules)
  }

  invisible(rules)
}
