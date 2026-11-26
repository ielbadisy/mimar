set.seed(1)
n <- 300
d <- data.frame(a = rnorm(n), b = rnorm(n), f = factor(sample(c("u", "v"), n, TRUE)))
d$y <- d$a + 0.5 * d$b + rnorm(n)
d$y[sample(n, 90)] <- NA
mis <- is.na(d$y)

test_that("knn, hotdeck and famd give different imputations", {
  v <- lapply(c(knn = "knn", hotdeck = "hotdeck", famd = "famd"),
              function(m) complete(impute(d, m = 1, imputer = m, seed = 1))$y[mis])
  expect_false(identical(v$knn, v$hotdeck))
  expect_false(identical(v$knn, v$famd))
})

test_that("rf draws observed values and differs from ranger", {
  rf <- complete(impute(d, m = 1, imputer = "rf", seed = 1))$y[mis]
  expect_true(all(rf %in% d$y[!mis]))
  expect_false(identical(rf, complete(impute(d, m = 1, imputer = "ranger", seed = 1))$y[mis]))
})

test_that("learner imputers keep the residual spread in impute()", {
  obs_sd <- sd(d$y, na.rm = TRUE)
  for (m in c("ranger", "rpart", "glmnet")) {
    v <- complete(impute(d, m = 1, imputer = m, seed = 1))$y[mis]
    expect_true(all(v %in% d$y[!mis]))
    expect_gt(sd(v), 0.7 * obs_sd)
  }
})

test_that("fitimputer() on a learner still predicts the conditional mean", {
  o <- !mis
  f <- fitimputer("glmnet", x = d[o, c("a", "b")], y = d$y[o])
  expect_null(f$match)
  expect_lt(sd(predict(f, d[mis, c("a", "b")])), sd(d$y, na.rm = TRUE))
})
