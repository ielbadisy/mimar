#' @export
describe.data.frame <- function(x, ...) {
  .check_data_frame(x)
  variable_summary <- data.frame(
    variable = names(x),
    type = vapply(x, .variable_type, character(1)),
    n_missing = colSums(is.na(x)),
    prop_missing = colMeans(is.na(x)),
    row.names = NULL
  )
  complete_cases <- sum(stats::complete.cases(x))
  out <- list(
    call = match.call(),
    n = nrow(x),
    p = ncol(x),
    variable_summary = variable_summary,
    pattern = .missing_pattern(x),
    row_summary = .row_summary(x),
    complete_cases = complete_cases,
    complete_case_prop = complete_cases / max(nrow(x), 1),
    data = x
  )
  class(out) <- c("mimar_missing", "list")
  out
}

#' @export
describe.character <- function(x, ...) {
  if (length(x) != 1 || !identical(tolower(x), "imputers")) {
    .mimar_stop('Only describe("imputers") is supported for character input.')
  }
  out <- imputer_registry()
  class(out) <- c("mimar_imputers", "data.frame")
  out
}

#' @export
describe.mimar_amputation <- function(x, ...) describe.data.frame(x$data, ...)

#' @export
describe.mimar_imputation <- function(x, ...) describe.data.frame(x$imputations[[1]], ...)

#' @export
describe.mimar_evaluation <- function(x, ...) x$summary

#' @export
describe.mimar_pool <- function(x, ...) x$pooled
