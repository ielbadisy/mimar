# Barnard-Rubin df, written out for the checks below.
br_df_ref <- function(dfcom, b, t, m) {
  lambda <- (1 + 1 / m) * b / t
  nu_old <- (m - 1) / lambda^2
  nu_obs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  nu_old * nu_obs / (nu_old + nu_obs)
}

panel_fits <- function(vcov) {
  set.seed(11)
  N <- 30; T <- 4
  d <- data.frame(id = rep(seq_len(N), each = T), time = rep(seq_len(T), N))
  d$x <- rnorm(N)[d$id] + rnorm(N * T)
  d$out <- rnorm(N)[d$id] + d$x + rnorm(N * T)
  d$x[sample(nrow(d), 30)] <- NA
  imp <- impute(d, m = 5, imputer = "norm", seed = 2)
  lapply(complete(imp, "all"), function(cd)
    panglm::panglm(out ~ x, data = cd, index = c("id", "time"), model = "within", vcov = vcov))
}

test_that("pool_panglm uses G - 1 complete-data df for cluster-robust fits", {
  skip_if_not_installed("panglm", minimum_version = "1.2.0")
  fits <- panel_fits("cluster")
  p <- pool_panglm(fits)
  expect_equal(p$dfcom, 29)
  r <- p$pooled[p$pooled$term == "x", ]
  q <- vapply(fits, function(f) coef(f)[["x"]], numeric(1))
  u <- vapply(fits, function(f) vcov(f)["x", "x"], numeric(1))
  b <- var(q); t <- mean(u) + (1 + 1 / 5) * b
  expect_equal(r$estimate, mean(q))
  expect_equal(r$std.error, sqrt(t))
  expect_equal(r$df, br_df_ref(29, b, t, 5))
  expect_equal(pool_panglm(fits, dfcom = 50)$dfcom, 50)
})

test_that("pool_panglm uses df.residual() for classical fits", {
  skip_if_not_installed("panglm", minimum_version = "1.2.0")
  fits <- panel_fits("classical")
  expect_equal(pool_panglm(fits)$dfcom, stats::df.residual(fits[[1]]))
})

test_that("pool_panglm warns when fits mix covariance types", {
  skip_if_not_installed("panglm", minimum_version = "1.2.0")
  fits <- c(panel_fits("cluster")[1:2], panel_fits("classical")[3:5])
  expect_warning(pool_panglm(fits), "vcov")
})

test_that("pool_test D1 defaults to the panglm complete-data df", {
  skip_if_not_installed("panglm", minimum_version = "1.2.0")
  fits <- panel_fits("cluster")
  expect_equal(pool_test(fits, terms = "x", method = "D1")$df2,
               pool_test(fits, terms = "x", method = "D1", dfcom = 29)$df2)
})

test_that("a predictor named y is used by the imputation model", {
  set.seed(3)
  d <- data.frame(x = rnorm(200), y = rnorm(200))
  d$x <- d$y + rnorm(200, sd = 0.1)
  miss <- 1:60
  d$x[miss] <- NA
  for (im in c("norm", "pmm")) {
    cd <- complete(impute(d, m = 1, imputer = im, seed = 1), "all")[[1]]
    expect_gt(cor(cd$x[miss], cd$y[miss]), 0.9)
  }
})
