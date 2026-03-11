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
    stop(
      "Package 'yaml' is required. Install it with install.packages('yaml')",
      call. = FALSE
    )
  }

  if (!file.exists(file)) {
    stop("Rules file does not exist: ", file, call. = FALSE)
  }

  rules <- yaml::read_yaml(file)

  if (validate) {
    rules <- validate_rule_structure(rules)
  }

  rules <- normalize_rules(rules)

  attr(rules, "source") <- normalizePath(file)
  class(rules) <- c("abayflowr_rules", class(rules))

  if (verbose) {
    message("abayflowr rules loaded successfully")
    message("Dataset: ", rules$dataset %||% "unknown")
    message("Number of rules: ", length(rules$rules))
  }

  rules
}
