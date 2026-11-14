test_that("fit() on an imputation fits one model per completed data set", {
  imp <- impute(airquality, m = 3, imputer = "pmm", seed = 1)
  ref <- lapply(complete(imp, "all"), function(d) lm(Ozone ~ Wind + Temp, data = d))

  fits <- fit(imp, Ozone ~ Wind + Temp)
  expect_length(fits, 3L)
  expect_true(all(vapply(fits, inherits, logical(1), "lm")))
  expect_equal(lapply(fits, coef), lapply(ref, coef))
  expect_equal(poollm(fits)$coefficients, poollm(ref)$coefficients)

  fits_glm <- fit(imp, I(Ozone > 50) ~ Wind, fun = stats::glm, family = stats::binomial)
  expect_true(all(vapply(fits_glm, inherits, logical(1), "glm")))

  fits_fun <- fit(imp, function(d) lm(Ozone ~ Wind, data = d))
  expect_equal(coef(fits_fun[[2]]), coef(lm(Ozone ~ Wind, data = complete(imp, 2))))

  expect_error(fit(imp, "Ozone ~ Wind"), "formula or a function")
})
