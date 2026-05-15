#' @export
evaluate.mimar_imputation <- function(x, truth = NULL, ...) {
  truth <- truth %||% x$truth %||% if (!is.null(x$amputation)) x$amputation$data_original else NULL
  mask <- x$mask_added %||% if (!is.null(x$amputation)) x$amputation$mask_added else NULL
  original <- x$data_original
  first <- x$imputations[[1]]
  distribution <- .rbind_or_empty(lapply(names(original), function(nm) {
    obs <- original[[nm]][!is.na(original[[nm]])]
    imputed_values <- unlist(lapply(x$imputations, function(d) {
      d[[nm]][is.na(original[[nm]]) & !is.na(d[[nm]])]
    }), use.names = FALSE)
    numeric_var <- is.numeric(original[[nm]]) || is.integer(original[[nm]]) || inherits(original[[nm]], "Date")
    data.frame(
      variable = nm,
      type = .variable_type(original[[nm]]),
      missing = sum(is.na(original[[nm]])),
      observed_unique = length(unique(obs)),
      imputed_unique = length(unique(imputed_values)),
      observed_mean = if (numeric_var) mean(as.numeric(obs), na.rm = TRUE) else NA_real_,
      imputed_mean = if (numeric_var) mean(as.numeric(imputed_values), na.rm = TRUE) else NA_real_,
      observed_sd = if (numeric_var) stats::sd(as.numeric(obs), na.rm = TRUE) else NA_real_,
      imputed_sd = if (numeric_var) stats::sd(as.numeric(imputed_values), na.rm = TRUE) else NA_real_,
      row.names = NULL
    )
  }))
  variability <- .rbind_or_empty(lapply(names(first), function(nm) {
    vals <- vapply(x$imputations, function(d) {
      v <- d[[nm]]
      if (is.numeric(v) || is.integer(v)) mean(v, na.rm = TRUE) else length(unique(v[!is.na(v)]))
    }, numeric(1))
    data.frame(variable = nm, between_imputation_sd = stats::sd(vals), row.names = NULL)
  }))
  recovery <- data.frame()
  recovery_by_imputation <- data.frame()
  if (!is.null(truth) && !is.null(mask)) {
    recovery_by_imputation <- .rbind_or_empty(lapply(seq_along(x$imputations), function(mi) {
      completed <- x$imputations[[mi]]
      .rbind_or_empty(lapply(names(completed), function(nm) {
        idx <- mask[[nm]] %in% TRUE
        if (!any(idx)) return(NULL)
        tr <- truth[[nm]][idx]
        pr <- completed[[nm]][idx]
        keep <- !is.na(tr) & !is.na(pr)
        tr <- tr[keep]; pr <- pr[keep]
        if (!length(tr)) return(NULL)
        if (is.numeric(tr) || is.integer(tr) || inherits(tr, "Date")) {
          err <- as.numeric(pr) - as.numeric(tr)
          data.frame(imputation = mi, variable = nm, type = "numeric",
                     rmse = sqrt(mean(err^2)), mae = mean(abs(err)),
                     bias = mean(err),
                     correlation = suppressWarnings(stats::cor(as.numeric(tr), as.numeric(pr))),
                     accuracy = NA_real_, balanced_accuracy = NA_real_, row.names = NULL)
        } else {
          acc <- mean(as.character(pr) == as.character(tr))
          data.frame(imputation = mi, variable = nm, type = "categorical",
                     rmse = NA_real_, mae = NA_real_, bias = NA_real_,
                     correlation = NA_real_, accuracy = acc,
                     balanced_accuracy = .balanced_accuracy(tr, pr), row.names = NULL)
        }
      }))
    }))
    if (nrow(recovery_by_imputation)) {
      recovery <- stats::aggregate(
        recovery_by_imputation[c("rmse", "mae", "bias", "correlation", "accuracy", "balanced_accuracy")],
        recovery_by_imputation[c("variable", "type")],
        function(z) mean(z, na.rm = TRUE)
      )
    }
  }
  summary <- data.frame(n_imputations = x$m, imputer = x$imputer,
                        total_missing = sum(is.na(original)),
                        evaluated_cells = if (is.null(mask)) 0 else sum(as.matrix(mask)), row.names = NULL)
  out <- list(call = match.call(), summary = summary, distribution = distribution,
              recovery = recovery, recovery_by_imputation = recovery_by_imputation,
              variability = variability, diagnostics = x$diagnostics)
  class(out) <- c("mimar_evaluation", "list")
  out
}

#' @export
evaluate.mimar_amputation <- function(x, ...) {
  imp <- impute(x, m = 1, imputer = "meanmode")
  evaluate(imp, ...)
}
