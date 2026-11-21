dat <- data.frame(a = c(1, NA, 3, 4, NA, 6, 7, 8, 9, 10, 2, 5), b = c(2, 3, NA, 5, 6, NA, 8, 9, 10, 11, 4, 7),
                  c = factor(c("x", "y", NA, "x", "y", "x", NA, "y", "x", "y", "x", "y")))
i <- impute(dat, m = 3, imputer = "pmm", maxit = 3, seed = 1, progress = FALSE)
cols <- function(p, k = 1) unique(ggplot2::ggplot_build(p)$data[[k]]$colour)

test_that("density and trace use the sober defaults", {
  p <- plot(i, type = "density")
  expect_setequal(c(cols(p, 1), cols(p, 2)), c("#2166AC", "grey20"))
  expect_true(all(cols(plot(i, type = "trace")) %in% .grey_shades(1:3)))
})

test_that("palette sets colours without a second scale", {
  p <- plot(i, type = "density", palette = c(observed = "black", imputed = "red"))
  expect_silent(ggplot2::ggplot_build(p))
  expect_equal(cols(p, 1), "red"); expect_equal(cols(p, 2), "black")
  expect_equal(cols(plot(i, type = "density", palette = c(imputed = "red")), 2), "grey20")
  expect_equal(cols(plot(i, type = "boxplot", palette = c("black", "red"))), c("black", "red"))
  expect_equal(cols(plot(i, type = "trace", palette = "grey50")), "grey50")
  expect_setequal(cols(plot(i, type = "trace", palette = c("red", "blue", "green"))), c("red", "blue", "green"))
  p <- plot(i, type = "trace", average = TRUE, palette = c(average = "red"))
  expect_equal(length(p$layers), 3); expect_equal(cols(p, 3), "red")
  f <- unique(ggplot2::ggplot_build(plot(i, type = "missing", palette = c(imputed = "orange")))$data[[1]]$fill)
  expect_true("orange" %in% f)
  for (t in c("imputed", "strip", "proportion", "methods", "variability"))
    expect_s3_class(ggplot2::ggplot_build(plot(i, type = t, palette = "grey40")), "ggplot_built")
  expect_equal(unique(ggplot2::ggplot_build(plot(describe(dat), palette = "red"))$data[[1]]$fill), "red")
})
