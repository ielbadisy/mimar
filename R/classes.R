#' @export
print.mimar_missing <- function(x, ...) {
  cat("mimar missing-data description\n")
  cat("Rows:", x$n, " Columns:", x$p, "\n")
  cat("Complete cases:", x$complete_cases, sprintf("(%.1f%%)\n", 100 * x$complete_case_prop))
  invisible(x)
}

#' @export
summary.mimar_missing <- function(object, ...) object$variable_summary

#' @export
print.mimar_amputation <- function(x, ...) {
  cat("mimar amputation\n")
  cat("Mechanism:", x$mechanism, " Added missing cells:", sum(x$mask_added), "\n")
  invisible(x)
}

#' @export
summary.mimar_amputation <- function(object, ...) {
  data.frame(variable = names(object$data), original = colSums(object$mask_original),
             added = colSums(object$mask_added), total = colSums(object$mask_total), row.names = NULL)
}

#' @export
print.mimar_imputation <- function(x, ...) {
  cat("mimar imputation\n")
  cat("Imputer:", x$imputer, " m:", x$m, " stochastic:", x$stochastic, "\n")
  invisible(x)
}

#' @export
summary.mimar_imputation <- function(object, ...) object$imputed_cells

#' @export
print.mimar_evaluation <- function(x, ...) {
  cat("mimar imputation evaluation\n")
  if (!is.null(x$summary)) print(x$summary)
  invisible(x)
}

#' @export
summary.mimar_evaluation <- function(object, ...) object$summary

#' @export
print.mimar_pool <- function(x, ...) {
  cat("mimar pooled results\n")
  print(x$pooled)
  invisible(x)
}

#' @export
summary.mimar_pool <- function(object, ...) object$pooled

#' @export
print.mimar_imputers <- function(x, ...) {
  cat("mimar available imputers\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}
