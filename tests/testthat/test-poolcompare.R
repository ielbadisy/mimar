aq <- function() {
  set.seed(2); d <- airquality; d$high <- as.integer(d$Ozone > 40)
  d$high[is.na(d$high)] <- NA; d
}

test_that("poolcompare matches confint for lm, confint.default for glm, and the pooled table", {
  d <- aq(); imp <- impute(d, m = 5, seed = 1, progress = FALSE)
  cl <- lm(Ozone ~ Wind + Temp, d); cg <- glm(high ~ Wind + Temp, binomial, d)
  mi <- poollm(lapply(complete(imp, "all"), function(x) lm(Ozone ~ Wind + Temp, x)))
  pc <- poolcompare(list(CC = cl, MI = mi), term = "Wind", scale = 5)
  expect_s3_class(pc, "mimar_poolcompare")
  expect_equal(unlist(pc[1, c("conf.low", "conf.high")]), 5 * confint(cl)["Wind", ], ignore_attr = TRUE)
  p <- as.data.frame(mi$pooled); p <- p[p$term == "Wind", ]
  expect_equal(unlist(pc[2, c("estimate", "std.error", "conf.low", "conf.high", "fmi")]),
               c(5, 5, 5, 5, 1) * unlist(p[c("estimate", "std.error", "conf.low", "conf.high", "fmi")]),
               ignore_attr = TRUE)
  g <- poolcompare(list(cg), term = "Wind", scale = -1)
  expect_equal(g$analysis, factor("Analysis 1"))
  expect_equal(unlist(g[c("conf.low", "conf.high")]), rev(-confint.default(cg)["Wind", ]), ignore_attr = TRUE)
  expect_equal(attr(g, "model"), "logistic")
  expect_error(poolcompare(list(cl), term = "nope"), "not found")
})

test_that("poolcompare on Cox fits agrees with coxph and mice::pool; plot draws one row per analysis", {
  skip_if_not_installed("survival"); skip_if_not_installed("mice")
  d <- survival::lung[c("time", "status", "age", "ph.karno", "meal.cal")]
  d$lcal <- log(d$meal.cal); d$meal.cal <- NULL
  cx <- function(x) survival::coxph(survival::Surv(time, status) ~ age + ph.karno + lcal, x)
  imp <- impute(d, m = 5, seed = 1, progress = FALSE); fits <- lapply(complete(imp, "all"), cx)
  cc <- cx(d); pc <- poolcompare(list(CC = cc, MI = poolcoxph(fits)), term = "lcal", scale = log(2))
  expect_equal(unlist(pc[1, c("conf.low", "conf.high")]), log(2) * confint(cc)["lcal", ], ignore_attr = TRUE)
  mp <- summary(mice::pool(mice::as.mira(fits)), conf.int = TRUE); mp <- mp[mp$term == "lcal", ]
  expect_equal(unlist(pc[2, c("estimate", "conf.low", "conf.high")]) / log(2),
               unlist(mp[c("estimate", "2.5 %", "97.5 %")]), ignore_attr = TRUE, tolerance = 1e-6)
  p <- plot(pc, labels = c(CC = "Complete cases"), ref = "MI", palette = c(CC = "black", MI = "#0072B2"))
  b <- ggplot2::ggplot_build(p)
  expect_equal(levels(b$plot$data$label), c("MI", "Complete cases"))
  expect_equal(sort(b$plot$data$estimate), sort(exp(pc$estimate)))
  expect_false(is.null(p$scales$get_scales("x")))
  expect_match(b$plot$labels$x, "Hazard ratio for lcal \\(coefficient x 0.6931\\)")
  expect_equal(nrow(b$data[[2]]), 1L)
})

test_that("plot(describe(), by =) leaves room on the right for the last axis label", {
  m <- ggplot2::ggplot_build(plot(describe(airquality), by = "Month"))$plot$theme$plot.margin
  expect_gt(as.numeric(m[2]), 5.5)
})
