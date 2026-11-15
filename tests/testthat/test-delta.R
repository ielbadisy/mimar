test_that("deltaadjust() shifts imputed cells only, by hand", {
  imp <- impute(airquality, m = 3, seed = 1, progress = FALSE)
  adj <- deltaadjust(imp, delta = c(Ozone = -10, Solar.R = 5))
  mis_o <- is.na(airquality$Ozone)
  mis_s <- is.na(airquality$Solar.R)
  for (k in 1:3) {
    a <- complete(adj, k)
    b <- complete(imp, k)
    expect_equal(a$Ozone[mis_o], b$Ozone[mis_o] - 10)
    expect_equal(a$Ozone[!mis_o], airquality$Ozone[!mis_o])
    expect_equal(a$Solar.R[mis_s], b$Solar.R[mis_s] + 5)
    expect_equal(a$Wind, b$Wind)
  }
  expect_equal(adj$delta, c(Ozone = -10, Solar.R = 5))

  # subgroup only
  grp <- airquality$Month == 5
  adj2 <- deltaadjust(imp, delta = 7, variables = "Ozone", rows = grp)
  d <- complete(adj2, 2)$Ozone - complete(imp, 2)$Ozone
  expect_equal(d, ifelse(mis_o & grp, 7, 0))

  expect_error(deltaadjust(imp, 1), "Name the variables")
  expect_error(deltaadjust(imp, 1, "nope"), "Unknown")
})

test_that("tippingpoint() pooled mean moves by delta * fraction missing", {
  imp <- impute(airquality, m = 4, seed = 2, progress = FALSE)
  deltas <- c(0, -5, -20)
  tp <- tippingpoint(imp, deltas, variables = "Ozone", model = Ozone ~ 1)
  frac <- mean(is.na(airquality$Ozone))
  expect_equal(tp$estimate - tp$estimate[1], deltas * frac)
  # Rubin pooled mean computed by hand at delta = -20
  ests <- vapply(complete(deltaadjust(imp, -20, "Ozone"), "all"),
                 function(d) mean(d$Ozone), numeric(1))
  expect_equal(tp$estimate[3], mean(ests))

  tp2 <- tippingpoint(imp, c(0, -10), "Ozone", Ozone ~ Wind, term = "Wind",
                      pooler = poollm)
  expect_equal(nrow(tp2), 2L)
  expect_true(all(tp2$term == "Wind"))
  ref <- poollm(fit(deltaadjust(imp, -10, "Ozone"), Ozone ~ Wind))$pooled
  expect_equal(tp2$estimate[2], ref$estimate[ref$term == "Wind"])
})
