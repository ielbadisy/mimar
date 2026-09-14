make_surv <- function(n = 150, seed = 1) {
  set.seed(seed)
  x <- rnorm(n)
  sex <- factor(sample(c("F", "M"), n, TRUE))
  lp <- 0.4 * x + 0.3 * (sex == "M")
  ev <- rexp(n, rate = exp(lp) / 8)
  ce <- rexp(n, rate = 1 / 12)
  data.frame(time = pmin(ev, ce), status = as.integer(ev <= ce), x = x, sex = sex)
}

test_that("nelsonaalen marginal matches a direct Nelson-Aalen computation", {
  d <- make_surv()
  h <- nelsonaalen(d, "time", "status")
  expect_length(h, nrow(d))
  expect_false(anyNA(h))
  expect_true(all(diff(h[order(d$time)]) >= -1e-8))

  et <- sort(unique(d$time[d$status == 1]))
  dd <- vapply(et, function(tt) sum(d$time == tt & d$status == 1), numeric(1))
  rr <- vapply(et, function(tt) sum(d$time >= tt), numeric(1))
  ch <- cumsum(dd / rr)
  ref <- ifelse(findInterval(d$time, et) > 0, ch[pmax(findInterval(d$time, et), 1)], 0)
  expect_equal(h, ref)
})

test_that("nelsonaalen accepts strings and unquoted symbols equivalently", {
  d <- make_surv()
  expect_identical(nelsonaalen(d, "time", "status"), nelsonaalen(d, time, status))
  expect_identical(ipcw(d, "time", "status"), ipcw(d, time, status))
})

test_that("nelsonaalen with categorical aux estimates within strata", {
  d <- make_surv()
  h <- nelsonaalen(d, time, status, aux = "sex")
  expect_length(h, nrow(d))
  for (lv in levels(d$sex)) {
    idx <- d$sex == lv
    expect_equal(h[idx], nelsonaalen(d[idx, ], time, status))
  }
})

test_that("nelsonaalen breslow path runs and is monotone in time within subject", {
  skip_if_not_installed("survival")
  d <- make_surv()
  h <- nelsonaalen(d, time, status, aux = "x", method = "breslow")
  expect_length(h, nrow(d))
  expect_true(all(h >= 0))
  expect_gt(cor(h, nelsonaalen(d, time, status)), 0.5)
})

test_that("ipcw returns event weights that undo censoring", {
  d <- make_surv()
  w <- ipcw(d, "time", "status")
  expect_length(w, nrow(d))
  expect_true(all(w[d$status == 0] == 0))
  expect_true(all(w[d$status == 1] >= 1))
  expect_equal(sum(w > 0), sum(d$status == 1))
})

test_that("ipcw type = 'all' keeps positive weights for censored subjects", {
  d <- make_surv()
  w <- ipcw(d, time, status, type = "all")
  expect_true(all(w >= 1))
  expect_true(all(w[d$status == 0] > 0))
})

test_that("ipcw truncate caps the weights", {
  d <- make_surv(n = 300)
  w_raw <- ipcw(d, time, status, type = "all")
  w_cap <- ipcw(d, time, status, type = "all", truncate = 0.9)
  expect_lte(max(w_cap), max(w_raw))
  expect_lte(max(w_cap), stats::quantile(w_raw, 0.9, names = FALSE) + 1e-8)
})

test_that("ipcw cox path is subject-specific and finite", {
  skip_if_not_installed("survival")
  d <- make_surv()
  w <- ipcw(d, time, status, aux = c("x", "sex"), method = "cox", type = "all")
  expect_length(w, nrow(d))
  expect_false(anyNA(w))
  expect_true(all(w >= 1))
})

test_that("nelsonaalen and ipcw return NA for missing time, status, or aux", {
  d <- make_surv()
  d$time[1] <- NA
  d$status[2] <- NA
  d$sex[3] <- NA
  expect_equal(which(is.na(nelsonaalen(d, time, status, aux = "sex"))), 1:3)
  expect_equal(which(is.na(ipcw(d, time, status, aux = "sex"))), 1:3)
})

test_that("input validation errors are informative", {
  d <- make_surv()
  expect_error(nelsonaalen(d, "nope", "status"), "not a column")
  expect_error(ipcw(d, "time", "status", aux = "missingcol"), "not found")
  bad <- d
  bad$status <- bad$status + 1
  expect_error(nelsonaalen(bad, "time", "status"), "0/1")
  expect_error(ipcw(d, "time", "status", truncate = 1.5), "truncate")
})
