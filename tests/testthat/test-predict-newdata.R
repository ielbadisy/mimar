test_that("predict() imputes new rows from the training imputation only", {
  train <- airquality[1:120, ]
  test <- airquality[121:153, ]
  imp <- impute(train, m = 3, seed = 1, progress = FALSE)
  before <- complete(imp, "all")
  pt <- predict(imp, test, seed = 2)
  expect_s3_class(pt, "mimar_imputation")
  expect_equal(pt$m, 3L)
  for (k in 1:3) {
    d <- as.data.frame(complete(pt, k))
    expect_equal(nrow(d), nrow(test))
    expect_false(anyNA(d))
    obs <- !is.na(test$Ozone)
    expect_equal(d$Ozone[obs], test$Ozone[obs])
    # PMM draws observed training values
    expect_true(all(d$Ozone[!obs] %in% train$Ozone))
  }
  # training imputations untouched
  expect_equal(complete(imp, "all"), before)
  # downstream fit/pool works
  expect_s3_class(poollm(with(pt, lm(Ozone ~ Wind))), "mimar_pool_lm")
  expect_error(predict(imp, test[, -1]), "lacks training column")
})

test_that("predict() with the mean imputer returns a bootstrap mean of training rows", {
  train <- airquality[1:120, ]
  test <- airquality[121:153, ]
  imp <- impute(train, m = 2, imputer = "mean", seed = 1, progress = FALSE)
  pt <- complete(predict(imp, test), 1)
  v <- unique(pt$Ozone[is.na(test$Ozone)])
  o <- train$Ozone[!is.na(train$Ozone)]
  expect_length(v, 1L)
  # parameter uncertainty: mean of a bootstrap resample of observed rows
  expect_lt(abs(v - mean(o)), 4 * sd(o) / sqrt(length(o)))
})

test_that("predict() for a variable complete in training but missing in new rows", {
  set.seed(3)
  n <- 400
  x <- rnorm(n)
  d <- data.frame(x = x, y = 2 * x + rnorm(n, sd = 0.5))
  d$y[sample(n, 60)] <- NA
  test <- data.frame(x = rnorm(200), y = NA_real_)
  test$y <- NA_real_
  test_x <- test$x
  test$x[1:20] <- NA
  test$y[21:200] <- 2 * test_x[21:200] + rnorm(180, sd = 0.5)
  test$y[1:20] <- 2 * test_x[1:20] + rnorm(20, sd = 0.5)
  test$y[150:200] <- NA
  test$x[150:200] <- test_x[150:200]
  imp <- impute(d, m = 5, imputer = "norm", seed = 4, progress = FALSE)
  pt <- predict(imp, test, seed = 5)
  # x was complete in training: method chosen for it, no NA left
  expect_false(anyNA(as.data.frame(complete(pt, 1))))
  # imputed y follow the training regression (slope near 2)
  long <- as.data.frame(complete(pt, "long"))
  mis <- rep(is.na(test$y), 5)
  slope <- unname(coef(lm(y ~ x, data = long[mis, ]))[2])
  expect_lt(abs(slope - 2), 0.3)
})

test_that("predict() agrees with mice ignore = TRUE in distribution", {
  skip_if_not_installed("mice")
  set.seed(10)
  n <- 600
  x <- rnorm(n)
  z <- 0.5 * x + rnorm(n)
  y <- 1 + 1.5 * x - z + rnorm(n)
  all <- data.frame(x = x, z = z, y = y)
  all$y[sample(n, 150)] <- NA
  tr <- all[1:400, ]
  te <- all[401:600, ]
  imp <- impute(tr, m = 20, imputer = "norm", seed = 1, progress = FALSE)
  pt <- predict(imp, te, seed = 2)
  ours <- as.data.frame(complete(pt, "long"))
  mis <- rep(is.na(te$y), 20)
  ours_mean <- mean(ours$y[mis])
  mi <- mice::mice(all, m = 20, method = "norm", ignore = c(rep(FALSE, 400), rep(TRUE, 200)),
                   printFlag = FALSE, seed = 3)
  ml <- mice::complete(mi, "long")
  mice_mis <- ml$.id > 400 & rep(is.na(all$y), 20)
  mice_mean <- mean(ml$y[mice_mis])
  expect_lt(abs(ours_mean - mice_mean), 0.15)
  expect_lt(abs(sd(ours$y[mis]) - sd(ml$y[mice_mis])), 0.15)
})
