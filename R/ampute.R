#' @rdname ampute
#' @param prop Target marginal proportion of newly introduced missing values.
#' @param mechanism Missingness mechanism: missing completely at random
#'   (`"MCAR"`), missing at random (`"MAR"`), or missing not at random
#'   (`"MNAR"`).
#' @param target Character vector of variables eligible for amputation. Defaults
#'   to all variables.
#' @param by Character vector of fully or partially observed variables used to
#'   drive MAR probabilities. Required for `mechanism = "MAR"`.
#' @param direction For MNAR numeric targets, whether missingness is more likely
#'   in both tails, the left tail, or the right tail.
#' @param seed Optional random seed.
#' @export
ampute.data.frame <- function(x, prop = 0.1, mechanism = c("MCAR", "MAR", "MNAR"),
                              target = NULL, by = NULL,
                              direction = c("both", "left", "right"), seed = NULL, ...) {
  .check_data_frame(x)
  mechanism <- match.arg(mechanism)
  direction <- match.arg(direction)
  if (!is.null(seed)) set.seed(seed)
  prop <- min(max(prop, 0), 1)
  target <- target %||% names(x)
  if (!all(target %in% names(x))) .mimar_stop("All `target` variables must be columns in `x`.")
  if (mechanism == "MAR") {
    if (is.null(by) || !length(by)) .mimar_stop("`by` must be supplied for MAR amputation.")
    if (!all(by %in% names(x))) .mimar_stop("All `by` variables must be columns in `x`.")
    if (any(target %in% by)) .mimar_stop("MAR `by` variables must not include target variables.")
  }
  x_amp <- x
  mask_original <- is.na(x)
  mask_added <- as.data.frame(matrix(FALSE, nrow(x), ncol(x), dimnames = list(NULL, names(x))))
  for (nm in target) {
    observed <- !is.na(x[[nm]])
    if (!any(observed)) next
    if (mechanism == "MCAR") {
      prob <- rep(prop, nrow(x))
    } else if (mechanism == "MAR") {
      prob <- .calibrated_prob(.standardize_score(x[by]), prop)
    } else {
      v <- x[[nm]]
      if (is.numeric(v) || is.integer(v) || inherits(v, "Date")) {
        z <- as.numeric(v)
        z <- (z - mean(z, na.rm = TRUE)) / stats::sd(z, na.rm = TRUE)
        z[!is.finite(z)] <- 0
        score <- switch(direction, right = z, left = -z, both = abs(z))
      } else {
        f <- factor(v)
        freq <- table(f)
        score <- -as.numeric(freq[as.character(f)])
        score <- (score - mean(score, na.rm = TRUE)) / stats::sd(score, na.rm = TRUE)
        score[!is.finite(score)] <- 0
      }
      prob <- .calibrated_prob(score, prop)
    }
    add <- observed & (stats::runif(nrow(x)) < prob)
    x_amp[[nm]][add] <- NA
    mask_added[[nm]] <- add
  }
  out <- list(call = match.call(), data_original = x, data = x_amp,
              mask_original = mask_original, mask_added = mask_added,
              mask_total = is.na(x_amp), prop = prop, mechanism = mechanism,
              target = target, by = by, direction = direction, seed = seed)
  class(out) <- c("mimar_amputation", "list")
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x
