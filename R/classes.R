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
  .as_tibble(data.frame(variable = names(object$data), original = colSums(object$mask_original),
                        added = colSums(object$mask_added), total = colSums(object$mask_total), row.names = NULL))
}

#' @export
print.mimar_imputation <- function(x, ...) {
  cat("mimar imputation\n")
  overview <- .imputation_overview(x)
  imputed <- .imputation_variable_summary(x)
  vars <- imputed$variable[imputed$n_missing_before > 0]
  cat("Completed datasets:", x$m, "\n")
  cat("Rows x columns:", overview$rows, "x", overview$columns, "\n")
  cat("Imputer:", x$imputer, " maxit:", x$maxit, " stochastic:", x$stochastic, "\n")
  cat("Missing cells imputed:", overview$total_imputed, "/", overview$total_missing_before, "\n")
  if (length(vars)) {
    cat("Variables imputed:", paste(vars, collapse = ", "), "\n")
  } else {
    cat("Variables imputed: none\n")
  }
  cat("Use complete(x) to extract completed data.\n")
  if (x$m == 1) {
    cat("\nCompleted data preview:\n")
    print(utils::head(x$data, 6))
  }
  invisible(x)
}

#' @export
summary.mimar_imputation <- function(object, ...) {
  out <- list(
    overview = .imputation_overview(object),
    variables = .imputation_variable_summary(object),
    diagnostics = object$diagnostics
  )
  class(out) <- c("summary_mimar_imputation", "list")
  out
}

#' @export
print.summary_mimar_imputation <- function(x, ...) {
  cat("mimar imputation summary\n")
  print(x$overview)
  cat("\nVariables:\n")
  print(x$variables)
  invisible(x)
}

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
  print(.as_tibble(x))
  invisible(x)
}
