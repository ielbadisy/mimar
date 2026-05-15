#' @export
plot.mimar_missing <- function(x, engine = c("base", "ggplot2"), ...) {
  engine <- match.arg(engine)
  d <- x$variable_summary
  if (engine == "ggplot2" && requireNamespace("ggplot2", quietly = TRUE)) {
    d$variable <- stats::reorder(d$variable, d$prop_missing)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "prop_missing", y = "variable")) +
        ggplot2::geom_col(fill = "#2F6F73", width = 0.72) +
        ggplot2::scale_x_continuous(labels = function(z) paste0(round(100 * z), "%")) +
        ggplot2::labs(x = "Missing proportion", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  graphics::barplot(stats::setNames(d$prop_missing, d$variable), horiz = TRUE, xlab = "Missing proportion", ...)
}

#' @export
plot.mimar_amputation <- function(x, ...) {
  d <- summary(x)
  mat <- t(as.matrix(d[, c("original", "added", "total")]))
  colnames(mat) <- d$variable
  graphics::barplot(mat, beside = TRUE, legend.text = TRUE, ylab = "Missing cells", ...)
}

#' Diagnostic plots for mimar imputations
#'
#' `plot.mimar_imputation()` draws imputation diagnostics. By default it shows
#' imputed cell counts. Other plot types show a cell-status map, observed versus
#' imputed distributions, variable-level imputation methods, or
#' between-imputation variability.
#'
#' @param x A `mimar_imputation` object.
#' @param type Plot type: `"imputed"`, `"missing"`, `"density"`, `"strip"`,
#'   `"methods"`, or `"variability"`.
#' @param variable Optional variable name or names used by distribution plots.
#' @param engine Plotting engine. `"ggplot2"` is used when available; `"base"`
#'   uses base graphics.
#' @param ... Passed to base graphics methods.
#' @return A `ggplot` object when `engine = "ggplot2"` and `ggplot2` is
#'   installed; otherwise invisibly returns `x`.
#' @export
plot.mimar_imputation <- function(x, type = c("imputed", "missing", "density", "strip", "methods", "variability"),
                                  variable = NULL, engine = c("ggplot2", "base"), ...) {
  type <- match.arg(type)
  engine <- match.arg(engine)
  if (engine == "ggplot2" && requireNamespace("ggplot2", quietly = TRUE)) {
    return(.plot_imputation_ggplot(x, type = type, variable = variable))
  }
  .plot_imputation_base(x, type = type, variable = variable, ...)
}

#' @export
plot.mimar_evaluation <- function(x, ...) {
  d <- x$recovery
  if (is.null(d) || !nrow(d)) {
    graphics::plot.new(); graphics::text(.5, .5, "No recovery metrics available"); return(invisible(x))
  }
  val <- ifelse(!is.na(d$rmse), d$rmse, 1 - d$accuracy)
  graphics::barplot(stats::setNames(val, d$variable), horiz = TRUE, xlab = "RMSE or 1 - accuracy", ...)
}

.plot_imputation_base <- function(x, type, variable = NULL, ...) {
  if (identical(type, "imputed")) {
    d <- .imputation_variable_summary(x)
    graphics::barplot(stats::setNames(d$n_imputed, d$variable), horiz = TRUE, xlab = "Imputed cells", ...)
    return(invisible(x))
  }
  if (identical(type, "missing")) {
    return(plot(describe(x$data_original), ...))
  }
  if (identical(type, "methods")) {
    d <- .imputation_variable_summary(x)
    tab <- sort(table(d$method[d$n_missing_before > 0]), decreasing = TRUE)
    graphics::barplot(tab, ylab = "Variables", ...)
    return(invisible(x))
  }
  if (identical(type, "variability")) {
    d <- .imputation_variable_summary(x)
    val <- d$between_imputation_sd
    if (!any(is.finite(val))) {
      graphics::plot.new()
      graphics::text(.5, .5, "No between-imputation variability available")
      return(invisible(x))
    }
    graphics::barplot(stats::setNames(val, d$variable), horiz = TRUE,
                      xlab = "Between-imputation SD", ...)
    return(invisible(x))
  }
  .plot_distribution_base(x, type = type, variable = variable, ...)
}

.plot_distribution_base <- function(x, type, variable = NULL, ...) {
  data <- .observed_imputed_plot_data(x, variable = variable)
  if (!nrow(data)) {
    graphics::plot.new()
    graphics::text(.5, .5, "No plottable imputed variables")
    return(invisible(x))
  }
  variable <- unique(data$variable)[1]
  d <- data[data$variable == variable, , drop = FALSE]
  if (identical(type, "density") && identical(unique(d$value_type), "numeric")) {
    obs <- as.numeric(d$value[d$status == "observed"])
    imp <- as.numeric(d$value[d$status == "imputed"])
    if (length(unique(stats::na.omit(obs))) < 2 || length(unique(stats::na.omit(imp))) < 2) {
      d$value_plot <- as.numeric(d$value)
      graphics::stripchart(value_plot ~ status, data = d, vertical = TRUE, method = "jitter",
                           ylab = variable, xlab = "", ...)
      return(invisible(x))
    }
    graphics::plot(stats::density(obs, na.rm = TRUE), main = variable, xlab = variable, ...)
    graphics::lines(stats::density(imp, na.rm = TRUE), col = "#B84A62")
    graphics::legend("topright", legend = c("observed", "imputed"), lty = 1, col = c("black", "#B84A62"))
    return(invisible(x))
  }
  if (identical(unique(d$value_type), "numeric")) {
    d$value_plot <- as.numeric(d$value)
  } else {
    levels <- sort(unique(d$value))
    d$value_plot <- as.numeric(factor(d$value, levels = levels))
  }
  graphics::stripchart(value_plot ~ status, data = d, vertical = TRUE, method = "jitter",
                       ylab = variable, xlab = "", ...)
  if (!identical(unique(d$value_type), "numeric")) {
    graphics::axis(2, at = seq_along(levels), labels = levels, las = 1)
  }
  invisible(x)
}

.plot_imputation_ggplot <- function(x, type, variable = NULL) {
  if (identical(type, "imputed")) {
    d <- .imputation_variable_summary(x)
    d$variable <- stats::reorder(d$variable, d$n_imputed)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "n_imputed", y = "variable")) +
        ggplot2::geom_col(fill = "#2F6F73", width = 0.72) +
        ggplot2::labs(x = "Imputed cells", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  if (identical(type, "missing")) {
    return(.plot_missing_map_ggplot(x))
  }
  if (identical(type, "methods")) {
    d <- .imputation_variable_summary(x)
    d <- d[d$n_missing_before > 0, , drop = FALSE]
    d$variable <- stats::reorder(d$variable, d$n_missing_before)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "n_missing_before", y = "variable", fill = "method")) +
        ggplot2::geom_col(width = 0.72) +
        ggplot2::scale_fill_manual(values = .mimar_palette(unique(d$method)), na.value = "#8A8F98") +
        ggplot2::labs(x = "Missing cells before imputation", y = NULL, fill = "Method") +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  if (identical(type, "variability")) {
    d <- .imputation_variable_summary(x)
    d <- d[!is.na(d$between_imputation_sd), , drop = FALSE]
    if (!nrow(d)) .mimar_stop("No between-imputation variability is available.")
    d$variable <- stats::reorder(d$variable, d$between_imputation_sd)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "between_imputation_sd", y = "variable")) +
        ggplot2::geom_col(fill = "#7768AE", width = 0.72) +
        ggplot2::labs(x = "Between-imputation SD", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  .plot_distribution_ggplot(x, type = type, variable = variable)
}

.plot_missing_map_ggplot <- function(x) {
  original <- x$data_original
  first <- x$imputations[[1]]
  d <- do.call(rbind, lapply(names(original), function(nm) {
    before <- is.na(original[[nm]])
    after <- is.na(first[[nm]])
    status <- ifelse(before & !after, "imputed", ifelse(before & after, "still missing", "observed"))
    data.frame(row = seq_len(nrow(original)), variable = nm, status = status, row.names = NULL)
  }))
  d$status <- factor(d$status, levels = c("observed", "imputed", "still missing"))
  ggplot2::ggplot(d, .gg_aes(x = "variable", y = "row", fill = "status")) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_manual(values = c(observed = "#E8ECEF", imputed = "#2F6F73", `still missing` = "#B84A62")) +
    ggplot2::labs(x = NULL, y = "Row", fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

.plot_distribution_ggplot <- function(x, type, variable = NULL) {
  d <- .observed_imputed_plot_data(x, variable = variable)
  if (!nrow(d)) .mimar_stop("No plottable imputed variables are available.")
  if (identical(type, "density")) {
    d <- d[d$value_type == "numeric", , drop = FALSE]
    if (!nrow(d)) .mimar_stop("Density diagnostics require at least one numeric imputed variable.")
    d$value <- as.numeric(d$value)
    return(
      ggplot2::ggplot(d, .gg_aes(x = "value", colour = "status", fill = "status")) +
        ggplot2::geom_density(alpha = 0.22, na.rm = TRUE) +
        ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free") +
        ggplot2::scale_colour_manual(values = c(observed = "#4F5D75", imputed = "#B84A62")) +
        ggplot2::scale_fill_manual(values = c(observed = "#4F5D75", imputed = "#B84A62")) +
        ggplot2::labs(x = NULL, y = "Density", colour = NULL, fill = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }
  ggplot2::ggplot(d, .gg_aes(x = "status", y = "value", colour = "status")) +
    ggplot2::geom_jitter(width = 0.18, height = 0, alpha = 0.55, na.rm = TRUE) +
    ggplot2::facet_wrap(stats::as.formula("~ variable"), scales = "free_y") +
    ggplot2::scale_colour_manual(values = c(observed = "#4F5D75", imputed = "#B84A62")) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

.observed_imputed_plot_data <- function(x, variable = NULL) {
  original <- x$data_original
  first <- x$imputations[[1]]
  vars <- names(original)[colSums(is.na(original)) > 0]
  if (!is.null(variable)) vars <- intersect(vars, variable)
  .rbind_or_empty(lapply(vars, function(nm) {
    observed <- original[[nm]][!is.na(original[[nm]])]
    imputed <- first[[nm]][is.na(original[[nm]]) & !is.na(first[[nm]])]
    if (!length(observed) || !length(imputed)) return(NULL)
    numeric_var <- is.numeric(original[[nm]]) || is.integer(original[[nm]]) || inherits(original[[nm]], "Date")
    data.frame(
      variable = nm,
      status = rep(c("observed", "imputed"), c(length(observed), length(imputed))),
      value = if (numeric_var) as.numeric(c(observed, imputed)) else as.character(c(observed, imputed)),
      value_type = if (numeric_var) "numeric" else "categorical",
      row.names = NULL
    )
  }))
}

.mimar_palette <- function(x) {
  pal <- c("#2F6F73", "#B84A62", "#7768AE", "#D99A3D", "#4F5D75", "#5B8E7D")
  stats::setNames(rep(pal, length.out = length(x)), x)
}

.gg_aes <- function(...) {
  args <- list(...)
  do.call(ggplot2::aes, lapply(args, as.name))
}

#' @export
plot.mimar_pool <- function(x, ...) {
  d <- x$pooled
  lab <- if ("term" %in% names(d)) d$term else d$metric
  y <- seq_len(nrow(d))
  graphics::plot(d$estimate, y, xlim = range(c(d$conf.low, d$conf.high), na.rm = TRUE),
                 yaxt = "n", ylab = "", xlab = "Pooled estimate", ...)
  graphics::axis(2, at = y, labels = lab, las = 1)
  graphics::segments(d$conf.low, y, d$conf.high, y)
}

#' @export
plot.mimar_imputers <- function(x, ...) {
  val <- rowSums(x[, c("supports_numeric", "supports_binary", "supports_multiclass")])
  graphics::barplot(stats::setNames(val, x$imputer), ylim = c(0, 3), ylab = "Supported task types", ...)
}
