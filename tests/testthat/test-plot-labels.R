aq <- airquality; aq$grp <- rep(c("a", "b"), length.out = nrow(aq))
imp <- impute(airquality, m = 5, maxit = 3, seed = 1, progress = FALSE)
b <- function(p, k = 1) ggplot2::ggplot_build(p)$data[[k]]

test_that("pooled model fits dispatch to the forest plot", {
  skip_if_not_installed("survival")
  fits <- lapply(complete(imp, "all"), function(d) survival::coxph(survival::Surv(Ozone, rep(1, nrow(d))) ~
                                                                     Wind + Temp, d))
  pc <- poolcoxph(fits)
  expect_s3_class(pc, "mimar_pool")
  p <- plot(pc, labels = c(Wind = "Wind (mph)"))
  expect_equal(p$labels$x, "Hazard ratio")
  expect_equal(levels(p$data$label), c("Temp", "Wind (mph)"))
  expect_equal(p$data$estimate, exp(pc$pooled$estimate))
  pl <- poollm(lapply(complete(imp, "all"), function(d) lm(Ozone ~ Wind + Temp, d)))
  expect_s3_class(pl, "mimar_pool")
  expect_false("(Intercept)" %in% plot(pl)$data$label)
  expect_true("(Intercept)" %in% plot(pl, intercept = TRUE)$data$label)
  expect_equal(plot(pl)$labels$x, "Pooled estimate")
  expect_output(print(pl), "Pooled")
})

test_that("trace legend is dropped with average, density has x title and line keys", {
  p <- plot(imp, type = "trace", average = TRUE)
  expect_equal(p$guides$guides$colour %||% p$guides$colour, "none")
  expect_equal(p$labels$y, "Mean of imputed values")
  d <- plot(imp, type = "density", labels = c(Ozone = "Ozone (ppb)"))
  expect_equal(d$labels$x, "Value")
  expect_true("Ozone (ppb)" %in% d$data$variable)
  expect_s3_class(d$layers[[1]]$geom$draw_key(list(colour = "black"), list(), NULL), "segments")
  expect_true("Ozone (ppb)" %in% plot(imp, type = "trace", labels = c(Ozone = "Ozone (ppb)"))$data$variable)
})

test_that("describe plot: percent labels, complete variables, by and pattern", {
  x <- describe(aq)
  p <- plot(x)
  expect_setequal(as.character(p$data$variable), c("Ozone", "Solar.R"))
  expect_equal(nrow(plot(x, complete = TRUE)$data), ncol(aq))
  expect_equal(.pct(c(0, 0.025, 0.05, 0.075)), c("0%", "2.5%", "5%", "7.5%"))
  g <- plot(x, by = "grp")
  expect_equal(nrow(g$data), 4)
  expect_equal(g$data$prop[g$data$variable == "Ozone" & g$data$group == "a"],
               mean(is.na(aq$Ozone[aq$grp == "a"])))
  pt <- plot(x, type = "pattern")
  skip_if_not_installed("mice")
  mp <- mice::md.pattern(airquality, plot = FALSE)
  cnt <- as.integer(rownames(mp)[-nrow(mp)])
  expect_setequal(as.integer(ggplot2::ggplot_build(pt)$layout$panel_params[[1]]$y$get_labels()), cnt)
  expect_equal(sum(pt$data$status == "missing"), sum(mp[nrow(mp), ][mp[nrow(mp), ] > 0] > 0) + 1)
})

test_that("amputation plot keeps only amputed columns; evaluation uses NRMSE", {
  a <- ampute(airquality[complete.cases(airquality), ], prop = 0.3, mechanism = "MAR", target = "Ozone",
              by = "Temp", seed = 1)
  expect_equal(unique(as.character(plot(a)$data$variable)), "Ozone")
  e <- evaluate(impute(a, m = 3, seed = 1, progress = FALSE))
  p <- plot(e)
  expect_equal(p$data$value, e$recovery$nrmse)
  expect_equal(plot(e, metric = "rmse")$data$value, e$recovery$rmse)
  e2 <- evaluate(impute(a, m = 3, imputer = "mean", seed = 1, progress = FALSE))
  pc <- plot(e, compare = list(mean = e2))
  expect_equal(nrow(pc$data), 2)
  expect_s3_class(ggplot2::ggplot_build(pc), "ggplot_built")
})

test_that("tippingpoint plot marks the interpolated tipping point", {
  tp <- tippingpoint(imp, deltas = seq(0, -40, by = -5), variables = "Ozone", model = Ozone ~ Wind,
                     term = "Wind", pooler = poollm)
  expect_s3_class(tp, "mimar_tippingpoint")
  p <- plot(tp)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_equal(attr(p, "tipping")$delta, approx(tp$conf.high, tp$delta, 0)$y)
  expect_true(is.na(attr(plot(tp, null = -100), "tipping")$delta))
})
