pool_test_fixture <- function() {
  set.seed(11)
  n <- 60
  base <- data.frame(x = rnorm(n), g = factor(sample(c("a", "b", "c"), n, TRUE)))
  sets <- lapply(1:5, function(i) {
    d <- base
    d$x <- d$x + rnorm(n, sd = 0.3)
    d$y <- 1 + 0.5 * d$x + 0.4 * (d$g == "b") + rnorm(n)
    d
  })
  list(
    sets = sets,
    f1 = lapply(sets, function(d) lm(y ~ x + g, data = d)),
    f0 = lapply(sets, function(d) lm(y ~ x, data = d))
  )
}

test_that("pool_test reproduces mitml::testModels() for D1, D2 and D3", {
  fx <- pool_test_fixture()
  # Reference values from mitml::testModels() on the same fits.
  ref <- list(
    list(args = list(method = "D1"), stat = 1.160706574, df2 = 14.43680336),
    list(args = list(method = "D1", dfcom = 50), stat = 1.160706574, df2 = 8.212667262),
    list(args = list(method = "D2"), stat = 1.543914758, df2 = 19.56479578),
    list(args = list(method = "D2", use = "likelihood"), stat = 1.608866348, df2 = 20.85900745),
    list(args = list(method = "D3"), stat = 0.8987983394, df2 = 12.80783324)
  )
  for (r in ref) {
    res <- do.call(pool_test, c(list(fx$f1, fx$f0), r$args))
    expect_s3_class(res, "mimar_pool_test")
    expect_equal(res$test$statistic, r$stat, tolerance = 1e-8)
    expect_equal(res$test$df2, r$df2, tolerance = 1e-8)
    expect_equal(res$test$df1, 2)
    expect_equal(res$test$p.value, stats::pf(r$stat, 2, r$df2, lower.tail = FALSE), tolerance = 1e-8)
    expect_identical(res$terms, c("gb", "gc"))
  }
})

test_that("pool_test with terms matches the nested-model form", {
  fx <- pool_test_fixture()
  a <- pool_test(fx$f1, fx$f0, method = "D1")
  b <- pool_test(fx$f1, terms = c("gb", "gc"), method = "D1")
  expect_equal(a$test$statistic, b$test$statistic)
  # a single coefficient D1 equals the squared pooled Wald t statistic
  one <- pool_test(fx$f1, terms = "x", method = "D1")
  q <- vapply(fx$f1, function(f) coef(f)[["x"]], numeric(1))
  se <- vapply(fx$f1, function(f) sqrt(vcov(f)["x", "x"]), numeric(1))
  p <- pool(q, std.error = se)$pooled
  expect_equal(one$test$statistic, p$statistic^2)
  expect_output(print(one), "D1 \\(multivariate Wald\\)")
})

test_that("pool_test D3 evaluates glm likelihoods at the pooled estimates", {
  fx <- pool_test_fixture()
  g1 <- lapply(fx$sets, function(d) glm(I(y > 1) ~ x + g, family = binomial, data = d))
  g0 <- lapply(fx$sets, function(d) glm(I(y > 1) ~ x, family = binomial, data = d))
  for (f in g1) expect_equal(mimar:::.pool_test_loglik(f, coef(f)), as.numeric(logLik(f)))
  res <- pool_test(g1, g0, method = "D3")
  expect_equal(res$test$df1, 2)
  expect_true(is.finite(res$test$statistic) && is.finite(res$test$df2))
})

test_that("pool_test validates its inputs", {
  fx <- pool_test_fixture()
  expect_error(pool_test(fx$f1[1], fx$f0[1]), "at least two")
  expect_error(pool_test(fx$f1, fx$f0[1:3]), "same length")
  expect_error(pool_test(fx$f0, fx$f1), "nested")
  expect_error(pool_test(fx$f1, method = "D3"), "needs nested models")
  expect_error(pool_test(fx$f1, method = "D2", use = "likelihood"), "needs nested models")
  expect_error(pool_test(fx$f1), "Supply `terms`")
  expect_error(pool_test(fx$f1, terms = "zz"), "not found")
  expect_error(pool_test(fx$f1, fx$f0, terms = "x"), "either `terms` or `fits0`")
  if (requireNamespace("survival", quietly = TRUE)) {
    cx <- lapply(fx$sets, function(d) survival::coxph(survival::Surv(exp(y)) ~ x + g, data = d))
    cx0 <- lapply(fx$sets, function(d) survival::coxph(survival::Surv(exp(y)) ~ x, data = d))
    expect_error(pool_test(cx, cx0, method = "D3"), "use D1 or D2")
    expect_s3_class(pool_test(cx, cx0, method = "D2", use = "likelihood"), "mimar_pool_test")
  }
})
