.mimar_stop <- function(..., call. = FALSE) stop(paste0(...), call. = call.)

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
    return(data.frame(pattern = character(), n = integer(), proportion = numeric()))
  }
  pat <- apply(is.na(x), 1, function(z) paste(ifelse(z, "1", "0"), collapse = ""))
  tab <- sort(table(pat), decreasing = TRUE)
  data.frame(pattern = names(tab), n = as.integer(tab), proportion = as.numeric(tab) / nrow(x), row.names = NULL)
}

.row_summary <- function(x) {
  n_missing <- rowSums(is.na(x))
  data.frame(row = seq_len(nrow(x)), n_missing = n_missing, prop_missing = n_missing / max(ncol(x), 1))
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
  do.call(rbind, lapply(names(original), function(nm) {
    data.frame(variable = nm, n_imputed = sum(is.na(original[[nm]]) & !is.na(completed[[nm]])), row.names = NULL)
  }))
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
