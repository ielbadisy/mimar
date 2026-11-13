test_that("norm and pmm draw (beta, sigma) from the flat-prior posterior", {
  set.seed(11)
  n <- 60
  x <- data.frame(a = rnorm(n), b = rnorm(n))
  y <- 1 + 0.5 * x$a - 0.3 * x$b + rnorm(n)
  ref <- lm(y ~ a + b, data = cbind(y = y, x))
  rss <- sum(residuals(ref)^2)
  df <- n - 3
  xtx_inv <- summary(ref)$cov.unscaled
  for (method in c("norm", "pmm")) {
    draws <- t(replicate(3000, {
      f <- fit(imputer(method), x = x, y = y)
      c(coef(f$fit_draw), sigma2 = f$sigma_draw^2)
    }))
    # E[sigma^2] = RSS / (df - 2) under sigma^2 = RSS / chi2_df
    expect_equal(mean(draws[, "sigma2"]), rss / (df - 2), tolerance = 0.05)
    expect_equal(colMeans(draws[, 1:3]), coef(ref), tolerance = 0.05,
                 ignore_attr = TRUE)
    # Var(beta) = E[sigma^2] (X'X)^{-1}
    expect_equal(diag(var(draws[, 1:3])), rss / (df - 2) * diag(xtx_inv),
                 tolerance = 0.12, ignore_attr = TRUE)
  }
})

test_that("pmm matches observed rows on beta_hat and missing rows on the draw", {
  set.seed(3)
  x <- data.frame(a = rnorm(40))
  y <- 2 * x$a + rnorm(40)
  f <- fit(imputer("pmm"), x = x, y = y, donors = 1)
  expect_equal(unname(f$fitted), unname(fitted(f$fit)))
  newx <- data.frame(a = c(-1, 0, 1))
  pm <- predict(f$fit_draw, newdata = newx)
  nearest <- vapply(pm, function(p) y[which.min(abs(f$fitted - p))], numeric(1))
  expect_equal(predict(f, newx), unname(nearest))
})

test_that("between-imputation variance of norm and pmm matches mice", {
  skip_on_cran()
  skip_if_not_installed("mice")
  # MAR (missingness in x1 depends on y), linear analysis model, m = 20.
  # Compare B for the x1 slope with mice, paired on the same data; the
  # tolerance is 3.5 Monte Carlo SEs of the mean log ratio.
  set.seed(2026)
  n <- 200; m <- 20; reps <- 40
  slope_b <- function(dl) {
    stats::var(vapply(dl, function(d) coef(lm(y ~ x1 + x2, data = d))[["x1"]], numeric(1)))
  }
  lr <- replicate(reps, {
    x2 <- rnorm(n)
    x1 <- 0.5 * x2 + sqrt(0.75) * rnorm(n)
    y <- 1 + 0.5 * x1 + 0.5 * x2 + rnorm(n)
    x1[runif(n) < plogis(-0.5 + 1.2 * as.numeric(scale(y)))] <- NA
    d <- data.frame(y = y, x1 = x1, x2 = x2)
    vapply(c("norm", "pmm"), function(meth) {
      mi <- mice::mice(d, m = m, method = meth, maxit = 1, printFlag = FALSE)
      b_mice <- slope_b(lapply(seq_len(m), function(i) mice::complete(mi, i)))
      im <- impute(d, m = m, imputer = meth, maxit = 1, progress = FALSE)
      b_mimar <- slope_b(lapply(im$imputations, as.data.frame))
      log(b_mimar / b_mice)
    }, numeric(1))
  })
  for (meth in rownames(lr)) {
    mcse <- stats::sd(lr[meth, ]) / sqrt(reps)
    expect_lt(abs(mean(lr[meth, ])), 3.5 * mcse, label = paste("log B ratio,", meth))
  }
})
