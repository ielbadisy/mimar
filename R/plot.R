#' @export
plot.mimar_missing <- function(x, engine = c("base", "ggplot2"), ...) {
  engine <- match.arg(engine)
  d <- x$variable_summary
  if (engine == "ggplot2" && requireNamespace("ggplot2", quietly = TRUE)) {
    return(ggplot2::ggplot(d, ggplot2::aes_string("variable", "prop_missing")) + ggplot2::geom_col() + ggplot2::coord_flip())
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

#' @export
plot.mimar_imputation <- function(x, ...) {
  d <- x$imputed_cells
  graphics::barplot(stats::setNames(d$n_imputed, d$variable), horiz = TRUE, xlab = "Imputed cells", ...)
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
