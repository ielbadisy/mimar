test_that("describe data frames and imputers", {
  dat <- data.frame(a = c(1, NA, 3), b = factor(c("x", "y", NA)))
  d <- describe(dat)
  expect_s3_class(d, "mimar_missing")
  expect_equal(d$n, 3)
  expect_equal(d$p, 2)
  expect_s3_class(d$variable_summary, "tbl_df")
  expect_s3_class(d$row_summary, "tbl_df")
  expect_s3_class(d$pattern, "tbl_df")
  expect_equal(d$variable_summary$n_missing, c(1, 1))
  expect_equal(nrow(d$row_summary), 3)
  expect_true(nrow(d$pattern) >= 1)
  expect_s3_class(describe("imputers"), "mimar_imputers")
})

test_that("ampute mechanisms work", {
  set.seed(1)
  dat <- data.frame(a = rnorm(100), b = rnorm(100), g = factor(sample(letters[1:3], 100, TRUE)))
  dat$a[1] <- NA
  mcar <- ampute(dat, prop = .2, mechanism = "MCAR", target = "b", seed = 1)
  expect_s3_class(mcar, "mimar_amputation")
  expect_s3_class(mcar$data, "tbl_df")
  expect_s3_class(mcar$mask_added, "tbl_df")
  expect_s3_class(summary(mcar), "tbl_df")
  expect_true(sum(mcar$mask_added$b) > 5)
  expect_true(is.na(mcar$data$a[1]))
  expect_false(mcar$mask_added$a[1])
  expect_error(ampute(dat, mechanism = "MAR", target = "b"), "`by`")
  expect_error(ampute(dat, mechanism = "MAR", target = "b", by = "b"), "must not include")
  expect_s3_class(ampute(dat, prop = .2, mechanism = "MAR", target = "b", by = c("a", "g"), seed = 2), "mimar_amputation")
  expect_s3_class(ampute(dat, prop = .2, mechanism = "MNAR", target = "b", direction = "right", seed = 3), "mimar_amputation")
})

test_that("naive imputation works", {
  dat <- data.frame(a = c(1, NA, 3), b = factor(c("x", "y", NA)), c = c(TRUE, NA, FALSE))
  i1 <- impute(dat, m = 1, imputer = "naive", seed = 1)
  i3 <- impute(dat, m = 3, imputer = "naive", seed = 1)
  expect_s3_class(i1, "mimar_imputation")
  expect_s3_class(i1$data, "tbl_df")
  expect_s3_class(i1$imputations[[1]], "tbl_df")
  expect_s3_class(i1$imputed_cells, "tbl_df")
  expect_length(i1$imputations, 1)
  expect_length(i3$imputations, 3)
  expect_equal(names(i1)[1:2], c("imputations", "data"))
  expect_s3_class(complete(i1), "data.frame")
  expect_s3_class(complete(i1), "tbl_df")
  expect_s3_class(complete(i3, "long"), "tbl_df")
  expect_true(all(vapply(complete(i3, "all"), inherits, logical(1), what = "tbl_df")))
  expect_length(complete(i3, "all"), 3)
  expect_equal(nrow(complete(i3, "long")), nrow(dat) * 3)
  expect_false(anyNA(i1$imputations[[1]]))
  expect_true(i1$stochastic)
  expect_error(impute(dat, imputer = "not_real"), "Unknown imputer")
  expect_error(impute(dat, imputer = "meanmode"), "Unknown imputer")
})

test_that("internal chained equation imputation uses fit/predict steps", {
  dat <- data.frame(
    a = c(1, 2, NA, 4, 5, NA, 7, 8),
    b = factor(c("x", "x", "y", NA, "y", "x", NA, "y")),
    c = c(TRUE, FALSE, TRUE, TRUE, NA, FALSE, TRUE, NA)
  )
  imp <- impute(dat, m = 2, imputer = "naive", maxit = 2, seed = 10)
  expect_s3_class(imp, "mimar_imputation")
  s <- summary(imp)
  expect_s3_class(s$overview, "tbl_df")
  expect_s3_class(s$variables, "tbl_df")
  expect_length(imp$imputations, 2)
  expect_false(anyNA(imp$imputations[[1]]))
  expect_equal(imp$diagnostics$strategy, "chained_equations")
  expect_s3_class(imp$diagnostics$trace, "tbl_df")
  expect_true(all(c("imputation", "iteration", "variable", "mean", "sd") %in% names(imp$diagnostics$trace)))
  expect_equal(imp$diagnostics$ncore, 1)
  expect_true(all(imp$variable_methods[c("a", "b", "c")] %in% "naive"))
})

test_that("parallel imputations expose ncore and remain reproducible", {
  skip_on_cran()
  dat <- data.frame(
    a = c(1, 2, NA, 4, 5, NA, 7, 8),
    b = c(2, NA, 4, 5, NA, 7, 8, 9),
    c = factor(c("x", "x", "y", NA, "y", "x", NA, "y"))
  )
  serial <- impute(dat, m = 2, imputer = "naive", maxit = 2, seed = 20, ncore = 1)
  parallel <- impute(dat, m = 2, imputer = "naive", maxit = 2, seed = 20, ncore = 2)
  expect_equal(parallel$ncore, 2)
  expect_equal(parallel$diagnostics$ncore, 2)
  expect_length(parallel$imputations, 2)
  expect_equal(parallel$imputations, serial$imputations)
  expect_equal(parallel$diagnostics$trace, serial$diagnostics$trace)
  expect_error(impute(dat, ncore = 0), "`ncore`")
})

test_that("imputer learners expose a standard fit/predict contract", {
  dat <- data.frame(x = c(1, 2, 3, 4), z = factor(c("a", "b", "a", "b")))
  learner <- imputer("pmm")
  fitted <- fit(learner, x = dat["z"], y = dat$x, variable = "x")
  pred <- predict(fitted, dat["z"])
  expect_s3_class(learner, "mimar_imputer")
  expect_s3_class(fitted, "mimar_imputer_fit")
  expect_length(pred, 4)
  expect_equal(imputer("knn")$implementation, "mimar")
  expect_equal(imputer("rpart")$package, "rpart")
  expect_equal(imputer("glmnet")$package, "glmnet")
  expect_error(imputer("missforest"), "Unknown imputer")
  expect_error(imputer("mixgb"), "Unknown imputer")
  expect_error(imputer("naive_bayes"), "Unknown imputer")
  expect_equal(imputer("nbayes")$package, "naivebayes")
  expect_s3_class(imputer_registry(), "tbl_df")
  expect_equal(imputer_registry()$package[imputer_registry()$imputer == "knn"], "internal")
  expect_error(imputer("mice"), "Unknown imputer")
})

test_that("imputation accepts explicit hyperparameters and donor tuning", {
  dat <- data.frame(
    a = c(1, 2, NA, 4, 5, NA, 7, 8),
    b = c(2, NA, 4, 5, NA, 7, 8, 9),
    c = factor(c("x", "x", "y", NA, "y", "x", NA, "y"))
  )

  spec <- imputer("rf", num.trees = 5)
  i_spec <- impute(dat, m = 1, imputer = spec, maxit = 1, seed = 1)
  expect_s3_class(i_spec$imputer_spec, "mimar_imputer")
  expect_equal(i_spec$imputer, "rf")
  expect_equal(i_spec$imputer_spec$args$num.trees, 5)

  i_dots <- impute(dat, m = 1, imputer = "xgboost", maxit = 1, seed = 1,
                   nrounds = 1, verbose = 0)
  expect_s3_class(i_dots$imputer_spec, "mimar_imputer")
  expect_equal(i_dots$imputer_spec$args$nrounds, 1)
  expect_equal(i_dots$imputer_spec$args$verbose, 0)

  i_donors <- impute(dat, m = 1, imputer = "knn", maxit = 1, seed = 1, donors = 3)
  expect_equal(i_donors$donors, 3)
  expect_error(impute(dat, imputer = "pmm", maxit = 1, seed = 1, nrounds = 1),
               "does not accept additional hyperparameters")
})

test_that("all registered imputers run on numeric, categorical, and mixed data frames", {
  skip_on_cran()

  numeric_dat <- data.frame(
    x1 = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10, 11, 12),
    x2 = c(2, 1, 3, NA, 5, 4, 7, 8, NA, 10, 12, 11),
    x3 = c(10, 9, 8, 7, NA, 5, 4, 3, 2, NA, 1, 0)
  )
  categorical_dat <- data.frame(
    c1 = factor(c("a", "b", NA, "a", "b", "a", NA, "b", "a", "b", "a", "b")),
    c2 = factor(c("u", "v", "w", NA, "u", "v", "w", "u", NA, "v", "w", "u")),
    c3 = c(TRUE, FALSE, TRUE, NA, FALSE, TRUE, FALSE, TRUE, NA, FALSE, TRUE, FALSE)
  )
  mixed_dat <- data.frame(
    x1 = numeric_dat$x1,
    x2 = numeric_dat$x2,
    c1 = categorical_dat$c1,
    c2 = categorical_dat$c2,
    c3 = categorical_dat$c3
  )

  run_imputer <- function(method, dat) {
    args <- list(x = dat, m = 1, imputer = method, maxit = 1, seed = 1)
    if (identical(method, "bart")) args <- c(args, list(ntree = 5, ndpost = 5, nskip = 5))
    if (identical(method, "xgboost")) args <- c(args, list(nrounds = 5))
    if (identical(method, "gbm")) args <- c(args, list(n.trees = 10))
    do.call(impute, args)
  }

  available <- imputer_registry()
  methods <- available$imputer[available$available]
  learner_methods <- c("rf", "ranger", "rpart", "nbayes", "svm", "bart", "glmnet", "gbm", "xgboost", "famd")
  for (method in methods) {
    for (dat in list(numeric_dat, categorical_dat, mixed_dat)) {
      imp <- run_imputer(method, dat)
      expect_s3_class(imp, "mimar_imputation")
      expect_false(anyNA(complete(imp)))
      if (method %in% learner_methods) {
        missing_vars <- names(dat)[colSums(is.na(dat)) > 0]
        expect_true(all(imp$variable_methods[missing_vars] == method))
      }
    }
  }
})

test_that("evaluate uses amputation truth", {
  dat <- data.frame(a = rnorm(50), b = factor(sample(c("x", "y"), 50, TRUE)))
  a <- ampute(dat, prop = .2, mechanism = "MCAR", seed = 1)
  i <- impute(a, m = 2, imputer = "naive")
  e <- evaluate(i)
  expect_s3_class(e, "mimar_evaluation")
  expect_s3_class(e$summary, "tbl_df")
  expect_s3_class(e$distribution, "tbl_df")
  expect_s3_class(e$recovery, "tbl_df")
  expect_s3_class(e$recovery_by_imputation, "tbl_df")
  expect_s3_class(e$variability, "tbl_df")
  expect_true(all(c("summary", "distribution", "recovery", "recovery_by_imputation", "variability") %in% names(e)))
  expect_true(e$summary$evaluated_cells > 0)
  expect_true(all(e$recovery_by_imputation$imputation %in% 1:2))
})

test_that("pool estimates and metrics", {
  ps <- pool(c(.10, .11, .09), std.error = c(.04, .05, .04), name = "age")
  expect_s3_class(ps, "mimar_pool")
  expect_equal(ps$type, "scalar")
  expect_equal(ps$estimate, .10)
  expect_s3_class(ps$pooled, "tbl_df")

  betas <- list(c(age = .10, bmi = .30), c(age = .11, bmi = .32), c(age = .09, bmi = .29))
  covs <- list(diag(c(.04, .08)^2), diag(c(.05, .09)^2), diag(c(.04, .08)^2))
  pv <- pool(betas, covariance = covs)
  expect_equal(pv$type, "vector")
  expect_equal(names(pv$estimate), c("age", "bmi"))
  expect_equal(dim(pv$variance), c(2L, 2L))

  surv <- list(matrix(c(.90, .80, .70, .60), 2, 2),
               matrix(c(.91, .79, .72, .61), 2, 2),
               matrix(c(.89, .81, .71, .59), 2, 2))
  psurv <- pool(surv)
  expect_equal(psurv$type, "array_elementwise")
  expect_equal(dim(psurv$estimate), c(2L, 2L))
  expect_true(all(psurv$pooled$rule == "robust"))

  res <- data.frame(term = rep(c("a", "b"), each = 3), estimate = 1:6 / 10,
                    std.error = .1, imputation = rep(1:3, 2))
  p <- pool(res)
  expect_s3_class(p, "mimar_pool")
  expect_s3_class(p$pooled, "tbl_df")
  expect_s3_class(p$data, "tbl_df")
  expect_equal(nrow(p$pooled), 2)
  expect_true(all(c("df", "p.value", "relative_increase_variance") %in% names(p$pooled)))
  met <- data.frame(metric = rep("rmse", 3), value = c(1, 1.2, .9), imputation = 1:3)
  pm <- pool(met)
  expect_equal(pm$type, "tidy_metric")
  expect_s3_class(pm$pooled, "tbl_df")
  expect_true(all(pm$pooled$rule == "robust"))
  expect_error(pool(data.frame(x = 1)), "Tabular pooling requires")
})

test_that("plot methods run", {
  dat <- data.frame(
    a = c(1, NA, 3, 4, NA, 6, 7, 8, 9, 10),
    b = c(2, 3, NA, 5, 6, NA, 8, 9, 10, 11),
    c = factor(c("x", "y", NA, "x", "y", "x", NA, "y", "x", "y"))
  )
  d <- describe(dat)
  a <- ampute(data.frame(a = 1:20, b = rnorm(20)), prop = .1, seed = 1)
  i <- impute(dat, m = 2, imputer = "naive", maxit = 2, seed = 1)
  e <- evaluate(i)
  p <- pool(data.frame(term = rep("a", 2), estimate = c(1, 2), std.error = c(.1, .1), imputation = 1:2))
  expect_silent(plot(d))
  expect_silent(plot(a))
  expect_silent(plot(i))
  expect_silent(plot(i, type = "missing"))
  expect_silent(plot(i, type = "density"))
  expect_silent(plot(i, type = "strip"))
  expect_silent(plot(i, type = "boxplot"))
  expect_silent(plot(i, type = "xy", formula = a ~ b | c))
  expect_silent(plot(i, type = "proportion"))
  expect_silent(plot(i, type = "proportion", formula = c ~ a))
  expect_silent(plot(i, type = "trace"))
  expect_silent(plot(i, type = "methods"))
  expect_silent(plot(i, type = "variability"))
  expect_silent(plot(e))
  expect_silent(plot(p))
  expect_silent(plot(describe("imputers")))
})

test_that("formula diagnostics parse and generate ggplots", {
  dat <- data.frame(
    a = c(1, NA, 3, 4, NA, 6, 7, 8, 9, 10),
    b = c(2, 3, NA, 5, 6, NA, 8, 9, 10, 11),
    c = factor(c("x", "y", NA, "x", "y", "x", NA, "y", "x", "y"))
  )
  i <- impute(dat, m = 2, imputer = "knn", maxit = 2, seed = 1)

  p_xy <- plot(i, type = "xy", formula = a ~ b | c)
  p_prop <- plot(i, type = "proportion", formula = c ~ a)
  p_density <- plot(i, type = "density", variable = c("a", "b"))

  expect_s3_class(p_xy, "ggplot")
  expect_s3_class(p_prop, "ggplot")
  expect_s3_class(p_density, "ggplot")
  expect_equal(p_xy$labels$x, "b")
  expect_equal(p_xy$labels$y, "a")
  expect_error(plot(i, type = "xy"), "`formula` is required")
  expect_error(plot(i, type = "xy", formula = a ~ b + c), "one y variable, one x variable")
  expect_error(plot(i, type = "proportion", formula = c), "variable ~ strata")
})
