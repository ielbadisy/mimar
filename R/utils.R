.mimar_stop <- function(..., call. = FALSE) stop(paste0(...), call. = call.)

.mimar_progress <- function(text) {
  message("[mimar] ", text)
}

.plural_s <- function(n) {
  if (identical(as.integer(n), 1L)) "" else "s"
}

.format_imputation_progress <- function(row) {
  details <- sprintf(
    "    %s: %s imputed %s %s cell%s",
    row$variable, row$method, row$n_imputed, row$type, .plural_s(row$n_imputed)
  )
  if (!is.na(row$mean)) {
    details <- paste0(details, sprintf(" (mean %.3f", row$mean))
    if (!is.na(row$sd)) {
      details <- paste0(details, sprintf(", sd %.3f", row$sd))
    }
    details <- paste0(details, ")")
  } else {
    details <- paste0(details, sprintf(" (%s unique value%s)", row$unique, .plural_s(row$unique)))
  }
  paste0(details, ".")
}

.as_tibble <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(x) else x
}

.rbind_or_empty <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (!length(x)) return(.as_tibble(data.frame()))
  .as_tibble(do.call(rbind, x))
}

.check_data_frame <- function(x) {
  if (!is.data.frame(x)) .mimar_stop("`x` must be a data frame.")
  invisible(x)
}

.mode_value <- function(x) {
  y <- x[!is.na(x)]
  if (!length(y)) return(NA)
  tab <- sort(table(y), decreasing = TRUE)
  val <- names(tab)[1]
  if (is.factor(x)) return(factor(val, levels = levels(x), ordered = is.ordered(x))[1])
  if (is.logical(x)) return(as.logical(val))
  if (inherits(x, "Date")) return(as.Date(val))
  val
}

.median_value <- function(x) {
  y <- x[!is.na(x)]
  if (!length(y)) return(NA)
  if (inherits(x, "Date")) return(stats::median(y))
  stats::median(y)
}

.simple_fill_value <- function(x) {
  if (is.numeric(x) || is.integer(x) || inherits(x, "Date")) .median_value(x) else .mode_value(x)
}

.fill_missing_vector <- function(x, value) {
  miss <- is.na(x)
  if (!any(miss)) return(x)
  if (is.factor(x)) {
    if (is.na(value)) return(x)
    value <- as.character(value)
    if (!value %in% levels(x)) levels(x) <- c(levels(x), value)
    x[miss] <- value
  } else {
    x[miss] <- value
  }
  x
}

.missing_pattern <- function(x) {
  if (!nrow(x)) {
    return(.as_tibble(data.frame(pattern = character(), n = integer(), proportion = numeric())))
  }
  pat <- apply(is.na(x), 1, function(z) paste(ifelse(z, "1", "0"), collapse = ""))
  tab <- sort(table(pat), decreasing = TRUE)
  .as_tibble(data.frame(pattern = names(tab), n = as.integer(tab), proportion = as.numeric(tab) / nrow(x), row.names = NULL))
}

.row_summary <- function(x) {
  n_missing <- rowSums(is.na(x))
  .as_tibble(data.frame(row = seq_len(nrow(x)), n_missing = n_missing, prop_missing = n_missing / max(ncol(x), 1)))
}

.variable_type <- function(x) class(x)[1]

.standardize_score <- function(df) {
  parts <- lapply(df, function(v) {
    if (is.numeric(v) || is.integer(v) || inherits(v, "Date")) {
      z <- as.numeric(v)
    } else {
      z <- as.numeric(factor(v))
    }
    z[is.na(z)] <- stats::median(z, na.rm = TRUE)
    if (all(is.na(z))) z <- rep(0, length(v))
    s <- stats::sd(z, na.rm = TRUE)
    if (!is.finite(s) || s == 0) return(rep(0, length(z)))
    (z - mean(z, na.rm = TRUE)) / s
  })
  score <- rowMeans(as.data.frame(parts), na.rm = TRUE)
  score[!is.finite(score)] <- 0
  score
}

.calibrated_prob <- function(score, prop) {
  prop <- min(max(prop, 0), 1)
  if (prop <= 0) return(rep(0, length(score)))
  if (prop >= 1) return(rep(1, length(score)))
  f <- function(alpha) mean(stats::plogis(alpha + score)) - prop
  alpha <- stats::uniroot(f, lower = -30, upper = 30)$root
  stats::plogis(alpha + score)
}

.supported_variable <- function(x) {
  is.numeric(x) || is.integer(x) || is.logical(x) || is.factor(x) ||
    is.character(x) || inherits(x, "Date")
}

.imputed_cells_summary <- function(original, completed) {
  .as_tibble(do.call(rbind, lapply(names(original), function(nm) {
    data.frame(variable = nm, n_imputed = sum(is.na(original[[nm]]) & !is.na(completed[[nm]])), row.names = NULL)
  })))
}

.imputation_variable_summary <- function(object) {
  original <- object$data_original
  first <- object$imputations[[1]]
  methods <- object$variable_methods %||% stats::setNames(rep(NA_character_, length(original)), names(original))
  .as_tibble(do.call(rbind, lapply(names(original), function(nm) {
    miss_before <- is.na(original[[nm]])
    remaining <- is.na(first[[nm]])
    vals <- vapply(object$imputations, function(d) {
      v <- d[[nm]]
      if (is.numeric(v) || is.integer(v) || inherits(v, "Date")) {
        return(mean(as.numeric(v), na.rm = TRUE))
      }
      length(unique(v[!is.na(v)]))
    }, numeric(1))
    data.frame(
      variable = nm,
      type = .variable_type(original[[nm]]),
      method = unname(methods[[nm]] %||% NA_character_),
      n_missing_before = sum(miss_before),
      prop_missing_before = mean(miss_before),
      n_imputed = sum(miss_before & !remaining),
      prop_imputed = if (any(miss_before)) sum(miss_before & !remaining) / sum(miss_before) else 0,
      remaining_missing = sum(remaining),
      between_imputation_sd = if (length(vals) > 1) stats::sd(vals, na.rm = TRUE) else NA_real_,
      row.names = NULL
    )
  })))
}

.imputation_overview <- function(object) {
  original <- object$data_original
  first <- object$imputations[[1]]
  total_missing <- sum(is.na(original))
  total_imputed <- sum(is.na(original) & !is.na(first))
  .as_tibble(data.frame(
    rows = nrow(original),
    columns = ncol(original),
    n_imputations = object$m,
    imputer = object$imputer,
    maxit = object$maxit,
    ncore = object$ncore %||% object$diagnostics$ncore %||% 1L,
    stochastic = object$stochastic,
    total_missing_before = total_missing,
    total_imputed = total_imputed,
    remaining_missing = sum(is.na(first)),
    variables_imputed = sum(colSums(is.na(original)) > 0),
    row.names = NULL
  ))
}

.balanced_accuracy <- function(truth, pred) {
  truth <- factor(truth)
  pred <- factor(pred, levels = levels(truth))
  by_class <- vapply(levels(truth), function(lvl) {
    idx <- truth == lvl
    if (!any(idx, na.rm = TRUE)) return(NA_real_)
    mean(pred[idx] == truth[idx], na.rm = TRUE)
  }, numeric(1))
  mean(by_class, na.rm = TRUE)
}

.as_prediction_vector <- function(pred) {
  if (is.data.frame(pred)) {
    for (nm in c(".pred_class", "prediction", "pred", "response", "fit")) {
      if (nm %in% names(pred)) return(pred[[nm]])
    }
    return(pred[[1]])
  }
  if (is.list(pred) && !is.data.frame(pred)) {
    for (nm in c(".pred_class", "prediction", "pred", "response", "fit")) {
      if (!is.null(pred[[nm]])) return(pred[[nm]])
    }
    return(unlist(pred, use.names = FALSE))
  }
  pred
}
