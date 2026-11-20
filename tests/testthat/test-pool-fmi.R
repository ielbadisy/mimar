test_that("pool and model pooling report fmi and lambda matching mice::pool()", {
  skip_if_not_installed("mice"); skip_if_not_installed("survival")
  set.seed(3)
  n <- 150
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), g = factor(sample(c("a", "b"), n, TRUE)))
  d$y <- 1 + d$x1 - 0.5 * d$x2 + rnorm(n)
  d$time <- rexp(n, exp(0.3 * d$x1)); d$status <- rbinom(n, 1, 0.7)
  d$x1[sample(n, 40)] <- NA; d$x2[sample(n, 30)] <- NA
  cd <- complete(impute(d, m = 5, imputer = "pmm", seed = 1), "all")
  mp <- function(f, ...) mice::pool(mice::as.mira(f), ...)$pooled
  chk <- function(a, b) {
    expect_equal(a$fmi, b$fmi, tolerance = 1e-10)
    expect_equal(a$lambda, b$lambda, tolerance = 1e-10)
  }
  fl <- lapply(cd, function(x) lm(y ~ x1 + x2 + g, x))
  chk(poollm(fl)$pooled, mp(fl))
  fg <- lapply(cd, function(x) glm(status ~ x1 + x2, binomial, x))
  chk(poolglm(fg)$pooled, mp(fg))
  fc <- lapply(cd, function(x) survival::coxph(survival::Surv(time, status) ~ x1 + x2, x))
  chk(poolcoxph(fc)$pooled, mp(fc, dfcom = fc[[1]]$nevent - 2))
  q <- vapply(fl, function(f) coef(f)[["x1"]], 0); s <- vapply(fl, function(f) sqrt(vcov(f)[2, 2]), 0)
  chk(pool(q, std.error = s, dfcom = fl[[1]]$df.residual)$pooled, mp(fl)[2, ])
  chk(suppressWarnings(pool(q, std.error = s)$pooled), suppressWarnings(mp(fl, dfcom = Inf))[2, ])
})

test_that("fmi follows the textbook formula and equals lambda for infinite df", {
  p <- pool(c(0.10, 0.12, 0.09, 0.11), std.error = c(0.04, 0.05, 0.04, 0.045))$pooled
  r <- p$relative_increase_variance
  expect_equal(p$fmi, (r + 2 / (p$df + 3)) / (1 + r))
  expect_equal(p$lambda, r / (1 + r))
  expect_equal(pool(c(1, 1, 1), std.error = c(0.1, 0.1, 0.1))$pooled$fmi, 0)
})

test_that("nelsonaalen() is the exact Nelson-Aalen and differs from mice only at ties", {
  skip_if_not_installed("survival")
  set.seed(2)
  d <- data.frame(time = sample(1:30, 200, TRUE), ev = rbinom(200, 1, 0.6))
  h <- vapply(d$time, function(t) {
    u <- sort(unique(d$time[d$ev == 1 & d$time <= t]))
    sum(vapply(u, function(s) sum(d$time == s & d$ev == 1) / sum(d$time >= s), 0))
  }, 0)
  expect_equal(nelsonaalen(d, time, ev), h, tolerance = 1e-12)
  skip_if_not_installed("mice")
  d$time <- d$time + runif(200) / 10
  expect_equal(nelsonaalen(d, time, ev), mice::nelsonaalen(d, time, ev), tolerance = 1e-10)
})
