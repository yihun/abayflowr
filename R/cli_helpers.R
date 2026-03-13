cli_start <- function(title) {
  cli::cli_h1(title)
}

cli_success <- function(text) {
  cli::cli_alert_success(text)
}

cli_info <- function(text) {
  cli::cli_alert_info(text)
}

cli_warning <- function(text) {
  cli::cli_alert_warning(text)
}

cli_error <- function(text) {
  cli::cli_alert_danger(text)
}

cli_rule <- function(name, value) {
  cli::cli_text("{.field {name}}: {.val {value}}")
}
