test_that("fit()/evaluate() forward unknown objects to a masked function", {
  fake <- list(
    fit = function(formula, data, model, ...) structure(list(kind = "other-fit", model = model, n = nrow(data)), class = "otherfit"),
    evaluate = function(object, ...) list(kind = "other-evaluate", from = object$kind)
  )
  attach(fake, name = "fakeothers", pos = length(search()), warn.conflicts = FALSE)
  on.exit(detach("fakeothers"), add = TRUE)
  d <- data.frame(y = 1:5, x = 5:1)
  r <- mimar::fit(y ~ x, data = d, model = "m1")
  expect_equal(r$kind, "other-fit")
  expect_equal(r$n, 5L)
  # arguments evaluated in the caller's frame
  g <- function(dd, mm) mimar::fit(y ~ x, data = dd, model = mm)
  expect_equal(g(d[1:3, ], "m2")$model, "m2")
  expect_equal(mimar::evaluate(r)$from, "other-fit")
  # mimar's own methods still dispatch
  imp <- impute(airquality, m = 2, seed = 1, progress = FALSE)
  expect_length(suppressWarnings(mimar::fit(imp, Ozone ~ Wind)), 2L)
})

test_that("fit() without another attached fit() gives an informative error", {
  skip_if(any(vapply(setdiff(search(), "package:mimar"), function(p)
    exists("fit", envir = as.environment(p), inherits = FALSE), logical(1))))
  expect_error(mimar::fit(y ~ x, data = data.frame(y = 1, x = 1)), "no method for this object")
})

test_that("survalis fit()/evaluate() keep working when mimar masks them", {
  skip_if_not_installed("survalis")
  skip_if_not_installed("survival")
  if (!"package:survalis" %in% search()) {
    suppressPackageStartupMessages(
      attachNamespace("survalis", pos = length(search()))
    )
    on.exit(detach("package:survalis"), add = TRUE)
  }
  lung <- survival::lung
  r <- mimar::fit(survival::Surv(time, status) ~ age + sex, data = lung, model = "coxph")
  expect_s3_class(r, "survalis_fit")
  expect_s3_class(mimar::evaluate(r, data = lung), "survalis_eval")
})
