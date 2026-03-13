#' Null coalescing operator
#'
#' Returns the left-hand value if it is not NULL, otherwise returns the right-hand value.
#'
#' @name null_coalesce
#' @aliases %||%
#'
#' @param x First value
#' @param y Fallback value
#'
#' @return One of the two values
#'
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
