#' Validate a data frame against a YAML rule file
#'
#' @param data A data frame to validate.
#' @param rules Either a path to a YAML file, or a named list already parsed
#'   from such a file (e.g. via [load_rules()]).
#'
#' @return A `validation_result` object

#' @export
#' @examples
#' data <- read_toy_data()
#' rules <- load_rules(system.file("extdata/toy_survey.yaml",
#'                                  package = "datavalidatoR"))
#' result <- validate_data(data, rules)
#' print(result)
validate_data <- function(data, rules) {
  if (is.character(rules)) {
    rules <- load_rules(rules)
  }

  violations <- list()

  # ------------------------------------------------------------------
  # 1. Per-variable rules
  # ------------------------------------------------------------------
  var_rules <- rules[["variables"]]
  if (!is.null(var_rules)) {
    for (var_name in names(var_rules)) {
      var_def <- var_rules[[var_name]]
      var_type <- var_def[["type"]]

      if (!var_name %in% names(data)) {
        violations <- c(
          violations,
          list(tibble::tibble(
            row = NA_integer_,
            variable = var_name,
            rule = "column_exists",
            value = NA_character_,
            message = glue_msg("Column '{var_name}' not found in data.")
          ))
        )
        next
      }

      col <- data[[var_name]]

      for (rule_def in var_def[["rules"]]) {
        viol <- apply_variable_rule(col, var_name, rule_def, var_type)
        if (nrow(viol) > 0) violations <- c(violations, list(viol))
      }
    }
  }

  # ------------------------------------------------------------------
  # 2. Cross-variable rules
  # ------------------------------------------------------------------
  cross_rules <- rules[["cross_rules"]]
  if (!is.null(cross_rules)) {
    for (cr in cross_rules) {
      viol <- apply_cross_rule(data, cr)
      if (nrow(viol) > 0) violations <- c(violations, list(viol))
    }
  }

  # ------------------------------------------------------------------
  # 3. Assemble result
  # ------------------------------------------------------------------
  if (length(violations) == 0) {
    viol_df <- empty_violation_tibble()
  } else {
    viol_df <- dplyr::bind_rows(violations)
    # Attach original values for context
    viol_df <- attach_row_values(viol_df, data)
  }

  summary_df <- tibble::tibble(
    dataset = rules[["dataset"]] %||% NA_character_,
    n_rows = nrow(data),
    n_violations = nrow(viol_df),
    n_rows_affected = length(unique(stats::na.omit(viol_df$row))),
    n_vars_affected = length(unique(stats::na.omit(viol_df$variable)))
  )

  structure(
    list(
      summary = summary_df,
      violations = viol_df,
      data = data,
      dataset = rules[["dataset"]] %||% NA_character_
    ),
    class = "validation_result"
  )
}


# ---- helpers -----------------------------------------------------------------

apply_variable_rule <- function(col, var_name, rule_def, var_type) {
  rule <- rule_def[["rule"]]
  rows <- seq_along(col)

  viol_rows <- switch(
    rule,

    # --- type check ---
    type = {
      if (var_type == "numeric") {
        which(!is.na(col) & suppressWarnings(is.na(as.numeric(col))))
      } else {
        integer(0)
      }
    },

    # --- min / max ---
    min = {
      val <- as.numeric(rule_def[["value"]])
      num <- suppressWarnings(as.numeric(col))
      which(!is.na(num) & num < val)
    },

    max = {
      val <- as.numeric(rule_def[["value"]])
      num <- suppressWarnings(as.numeric(col))
      which(!is.na(num) & num > val)
    },

    # --- allowed_values ---
    allowed_values = {
      allowed <- as.character(rule_def[["values"]])
      which(!is.na(col) & col != "" & !(col %in% allowed))
    },

    # --- not_missing ---
    not_missing = {
      which(is.na(col) | col == "")
    },

    # --- unknown rule ---
    {
      cli::cli_warn(
        "Unknown rule '{rule}' for variable '{var_name}' – skipped."
      )
      integer(0)
    }
  )

  if (length(viol_rows) == 0) {
    return(empty_violation_tibble())
  }

  msg <- switch(
    rule,
    min = paste0("Value < ", rule_def[["value"]], " (min)"),
    max = paste0("Value > ", rule_def[["value"]], " (max)"),
    allowed_values = paste0(
      "Value not in allowed set: [",
      paste(rule_def[["values"]], collapse = ", "),
      "]"
    ),
    not_missing = "Value is missing (NA or blank)",
    type = paste0("Cannot coerce to ", var_type),
    paste0("Rule '", rule, "' violated")
  )

  tibble::tibble(
    row = as.integer(viol_rows),
    variable = var_name,
    rule = rule,
    value = as.character(col[viol_rows]),
    message = msg
  )
}


apply_cross_rule <- function(data, cr) {
  rule_type <- cr[["rule"]]

  if (rule_type == "skip_if") {
    return(apply_skip_if(data, cr))
  }

  cli::cli_warn(
    "Unknown cross rule '{rule_type}' (name: '{cr[['name']]}') – skipped."
  )
  empty_violation_tibble()
}


apply_skip_if <- function(data, cr) {
  cond_block <- cr[["if"]]
  then_var <- cr[["then"]][["variable"]]
  must_be <- cr[["then"]][["must_be"]]

  if (!then_var %in% names(data)) {
    return(empty_violation_tibble())
  }
  col <- data[[then_var]]

  make_bad_rows_msg <- function(mask, cond_label) {
    if (identical(must_be, "NA") || is.na(must_be)) {
      bad <- which(mask & !(is.na(col) | col == ""))
      msg <- paste0(
        "'",
        then_var,
        "' must be NA when [",
        cond_label,
        "] (rule: ",
        cr[["name"]],
        ")"
      )
    } else {
      bad <- which(mask & !(col %in% must_be) & !is.na(col))
      msg <- paste0(
        "'",
        then_var,
        "' must equal '",
        must_be,
        "' when [",
        cond_label,
        "] (rule: ",
        cr[["name"]],
        ")"
      )
    }
    if (length(bad) == 0) {
      return(empty_violation_tibble())
    }
    tibble::tibble(
      row = as.integer(bad),
      variable = then_var,
      rule = paste0("cross:", cr[["name"]]),
      value = as.character(col[bad]),
      message = msg
    )
  }

  # 'all' block: ALL sub-conditions must hold simultaneously (AND).
  # Emit one combined violation per row where the whole block is TRUE.
  if ("all" %in% names(cond_block)) {
    sub_conds <- cond_block[["all"]]
    masks <- lapply(sub_conds, eval_single_condition, data = data)
    labels <- vapply(sub_conds, condition_label, character(1))
    combined_mask <- Reduce("&", masks)
    combined_label <- paste(labels, collapse = " AND ")
    return(make_bad_rows_msg(combined_mask, combined_label))
  }

  # 'any' block: each sub-condition is evaluated independently.
  # A row that satisfies multiple sub-conditions gets one violation per match,
  # so the user can see exactly which conditions fired.
  if ("any" %in% names(cond_block)) {
    sub_conds <- cond_block[["any"]]
    viols <- mapply(
      function(cond) {
        mask <- eval_single_condition(data, cond)
        label <- condition_label(cond)
        make_bad_rows_msg(mask, label)
      },
      sub_conds,
      SIMPLIFY = FALSE
    )
    result <- dplyr::bind_rows(viols)
    # De-duplicate: keep distinct (row, message) pairs so that if two
    # sub-conditions produce the same message on the same row we don't double-count.
    result <- dplyr::distinct(
      result,
      .data$row,
      .data$message,
      .keep_all = TRUE
    )
    return(result)
  }

  # Bare single condition (no all/any wrapper)
  mask <- eval_single_condition(data, cond_block)
  label <- condition_label(cond_block)
  make_bad_rows_msg(mask, label)
}


# Build a human-readable label for a single condition clause
condition_label <- function(cond) {
  var <- cond[["variable"]]
  if ("equals" %in% names(cond)) {
    return(paste0(var, " = ", cond[["equals"]]))
  }
  if ("not_equals" %in% names(cond)) {
    return(paste0(var, " != ", cond[["not_equals"]]))
  }
  if ("less_than" %in% names(cond)) {
    return(paste0(var, " < ", cond[["less_than"]]))
  }
  if ("greater_than" %in% names(cond)) {
    return(paste0(var, " > ", cond[["greater_than"]]))
  }
  if ("is_missing" %in% names(cond)) {
    return(paste0(
      var,
      " is",
      if (isTRUE(cond[["is_missing"]])) "" else " not",
      " missing"
    ))
  }
  paste0(var, " (unknown operator)")
}


eval_condition <- function(data, cond_block) {
  if ("all" %in% names(cond_block)) {
    parts <- lapply(cond_block[["all"]], eval_single_condition, data = data)
    return(Reduce("&", parts))
  }
  if ("any" %in% names(cond_block)) {
    parts <- lapply(cond_block[["any"]], eval_single_condition, data = data)
    return(Reduce("|", parts))
  }
  eval_single_condition(data, cond_block)
}


eval_single_condition <- function(data, cond) {
  var <- cond[["variable"]]
  col <- data[[var]]
  n <- nrow(data)

  if ("equals" %in% names(cond)) {
    return(!is.na(col) & col == as.character(cond[["equals"]]))
  }
  if ("not_equals" %in% names(cond)) {
    return(is.na(col) | col != as.character(cond[["not_equals"]]))
  }
  if ("less_than" %in% names(cond)) {
    num <- suppressWarnings(as.numeric(col))
    return(!is.na(num) & num < as.numeric(cond[["less_than"]]))
  }
  if ("greater_than" %in% names(cond)) {
    num <- suppressWarnings(as.numeric(col))
    return(!is.na(num) & num > as.numeric(cond[["greater_than"]]))
  }
  if ("is_missing" %in% names(cond)) {
    res <- is.na(col) | col == ""
    return(if (isTRUE(cond[["is_missing"]])) res else !res)
  }

  cli::cli_warn(
    "Unknown condition operator in: {paste(names(cond), collapse=', ')}. Returning FALSE."
  )
  rep(FALSE, n)
}


attach_row_values <- function(viol_df, data) {
  # viol_df$value already set per-rule; just ensure row IDs are present
  viol_df
}

empty_violation_tibble <- function() {
  tibble::tibble(
    row = integer(0),
    variable = character(0),
    rule = character(0),
    value = character(0),
    message = character(0)
  )
}

glue_msg <- function(...) paste0(...)

`%||%` <- function(a, b) if (!is.null(a)) a else b
