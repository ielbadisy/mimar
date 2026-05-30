#' @export
complete.mimar_imputation <- function(x, action = 1, ...) {
  if (is.character(action)) {
    action <- match.arg(action, c("all", "long"))
    if (identical(action, "all")) return(x$imputations)
    out <- do.call(rbind, lapply(seq_along(x$imputations), function(i) {
      cbind(.imp = i, x$imputations[[i]], row.names = NULL)
    }))
    row.names(out) <- NULL
    return(.as_tibble(out))
  }

  if (!is.numeric(action) || length(action) != 1 || is.na(action)) {
    .mimar_stop("`action` must be a positive integer, 'all', or 'long'.")
  }
  action <- as.integer(action)
  if (action < 1 || action > length(x$imputations)) {
    .mimar_stop("`action` must select an imputation between 1 and ", length(x$imputations), ".")
  }
  .as_tibble(x$imputations[[action]])
}
