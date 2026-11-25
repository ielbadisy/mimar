test_that("with() on an imputation fits one model per completed data set", {
  imp <- impute(airquality, m = 3, imputer = "pmm", seed = 1)
  ref <- lapply(complete(imp, "all"), function(d) lm(Ozone ~ Wind + Temp, data = d))

  fits <- with(imp, lm(Ozone ~ Wind + Temp))
  expect_length(fits, 3L)
  expect_true(all(vapply(fits, inherits, logical(1), "lm")))
  expect_equal(lapply(fits, coef), lapply(ref, coef))
  expect_equal(poollm(fits)$coefficients, poollm(ref)$coefficients)

  # arguments are found in the calling environment
  g <- function(im, fam) with(im, glm(I(Ozone > 50) ~ Wind, family = fam))
  expect_true(all(vapply(g(imp, stats::binomial), inherits, logical(1), "glm")))
})

test_that(".fitall() fits a function-style model on each completed data set", {
  imp <- impute(airquality, m = 3, imputer = "pmm", seed = 1)
  f <- mimar:::.fitall(imp, function(d) lm(Ozone ~ Wind, data = d))
  expect_equal(coef(f[[2]]), coef(lm(Ozone ~ Wind, data = complete(imp, 2))))
  expect_error(mimar:::.fitall(imp, "Ozone ~ Wind"), "formula or a function")
})

test_that("fitimputer() accepts an imputer object or a name", {
  d <- airquality[!is.na(airquality$Ozone), ]; x <- d[c("Wind", "Temp")]
  set.seed(1); a <- fitimputer("norm", x = x, y = d$Ozone)
  set.seed(1); b <- fitimputer(imputer("norm"), x = x, y = d$Ozone)
  set.seed(2); pa <- predict(a, x[1:5, ]); set.seed(2); pb <- predict(b, x[1:5, ])
  expect_equal(pa, pb)
})

test_that("mimar no longer exports fit() or evaluate()", {
  expect_false(any(c("fit", "evaluate") %in% getNamespaceExports("mimar")))
})
