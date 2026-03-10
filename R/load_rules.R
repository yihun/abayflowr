
#' Title: Load yaml files
#'
#' @param file: structured yaml file containing rules
#' @param validate logical: whether to validate the structure of the rules file
#' @param verbose logical: whether to print messages about the loading process
#'
#' @returns A list of rules with class "datavalid_rules" and an attribute "source" containing the file path
#'
#' @export
#' @examples
#' #' # Assuming you have a rules.yaml file structured correctly
#' rules <- load_rules("rules.yaml")
#'
load_rules <- function(file, validate = TRUE, verbose = TRUE) {

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required.")
  }

  if (!file.exists(file)) {
    stop("Rules file does not exist: ", file)
  }

  rules <- yaml::read_yaml(file)

  if (validate) {
    rules <- validate_rule_structure(rules)
  }

  rules <- normalize_rules(rules)

  attr(rules, "source") <- normalizePath(file)
  class(rules) <- "datavalid_rules"

  if (verbose) {
    message("datavalidR rules loaded successfully")
    message("Dataset: ", rules$dataset)
    message("Variables: ", length(rules$variables))
  }

  return(rules)
}
