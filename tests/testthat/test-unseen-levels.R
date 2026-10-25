test_that("regression imputers handle factor levels absent from fitting rows", {
  set.seed(1)
  n <- 120
  x <- data.frame(g = factor(sample(c("a", "b"), n, TRUE), levels = c("a", "b", "c")),
                  z = rnorm(n))
  x$g[1:3] <- "c"
  x$y <- x$z + rnorm(n)
  x$y[1:3] <- NA
  for (im in c("pmm", "norm")) {
    r <- impute(x, m = 2, imputer = im, seed = 1)
    expect_false(anyNA(as.data.frame(r$imputations[[1]])$y))
  }
})

test_that("regression imputers drop predictors that are constant in fitting rows", {
  set.seed(2)
  n <- 120
  x <- data.frame(g = factor(c(rep("a", 100), rep("b", 20))), z = rnorm(n))
  x$y <- x$z + rnorm(n)
  x$y[101:120] <- NA   # all rows with g == "b" are missing y
  for (im in c("pmm", "norm")) {
    r <- impute(x, m = 2, imputer = im, seed = 1)
    expect_false(anyNA(as.data.frame(r$imputations[[1]])$y))
  }
})
