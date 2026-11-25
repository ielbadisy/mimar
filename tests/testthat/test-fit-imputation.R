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

test_that("fit() on an imputation is deprecated and matches with()", {
  imp <- impute(airquality, m = 3, imputer = "pmm", seed = 1)
  expect_warning(fits <- fit(imp, Ozone ~ Wind + Temp), "with\\(imp")
  expect_equal(lapply(fits, coef), lapply(with(imp, lm(Ozone ~ Wind + Temp)), coef))
  fits_fun <- suppressWarnings(fit(imp, function(d) lm(Ozone ~ Wind, data = d)))
  expect_equal(coef(fits_fun[[2]]), coef(lm(Ozone ~ Wind, data = complete(imp, 2))))
  expect_error(suppressWarnings(fit(imp, "Ozone ~ Wind")), "formula or a function")
})
