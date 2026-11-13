eval_data <- function() {
  set.seed(1)
  dat <- data.frame(a = rnorm(80), b = rnorm(80),
                    g = factor(sample(c("x", "y"), 80, TRUE)))
  list(dat = dat, amp = ampute(dat, prop = .2, mechanism = "MCAR", seed = 1))
}

test_that("evaluate(imp, truth = amp) reports NRMSE and PFC", {
  e0 <- eval_data()
  imp <- impute(as.data.frame(e0$amp$data), m = 2, imputer = "pmm", seed = 1)
  e <- evaluate(imp, truth = e0$amp)
  rec <- as.data.frame(e$recovery)
  expect_true(nrow(rec) > 0)
  expect_true(all(c("nrmse", "pfc") %in% names(rec)))
  expect_true(all(is.finite(rec$nrmse[rec$type == "numeric"])))
  expect_true(all(rec$pfc[rec$type == "categorical"] >= 0 &
                  rec$pfc[rec$type == "categorical"] <= 1))
  expect_equal(e$summary$evaluated_cells, sum(as.matrix(e0$amp$mask_added)))
})

test_that("evaluate accepts the complete data frame as truth", {
  e0 <- eval_data()
  imp <- impute(as.data.frame(e0$amp$data), m = 2, imputer = "pmm", seed = 1)
  a <- as.data.frame(evaluate(imp, truth = e0$amp)$recovery)
  b <- as.data.frame(evaluate(imp, truth = e0$dat)$recovery)
  expect_equal(b$nrmse, a$nrmse)
  expect_equal(b$pfc, a$pfc)
})

test_that("evaluate accepts a list of completed data frames (missknn MI output)", {
  e0 <- eval_data()
  imp <- impute(e0$amp, m = 2, imputer = "pmm", seed = 1)
  lst <- structure(lapply(1:2, function(l) as.data.frame(complete(imp, l))),
                   class = c("mimar_mi", "list"))
  ref <- as.data.frame(evaluate(imp)$recovery)
  e1 <- as.data.frame(evaluate(lst, truth = e0$amp)$recovery)
  e2 <- as.data.frame(evaluate(lst, truth = e0$dat, data = e0$amp$data)$recovery)
  expect_equal(e1$nrmse, ref$nrmse)
  expect_equal(e2$pfc, ref$pfc)
  e3 <- evaluate(lst[[1]], truth = e0$amp)
  expect_true(nrow(e3$recovery) > 0)
  expect_error(evaluate(lst, truth = e0$dat), "incomplete data")
  skip_if_not_installed("missknn")
  mk <- missknn::missknn(as.data.frame(e0$amp$data), m = 2, seed = 1, parallel_cores = 1L)
  e4 <- as.data.frame(evaluate(mk, truth = e0$amp)$recovery)
  expect_setequal(e4$variable, c("a", "b", "g"))
})
