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
