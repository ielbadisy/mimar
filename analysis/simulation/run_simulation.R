#!/usr/bin/env Rscript

root <- getwd()
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  root <- normalizePath(file.path(root, "..", ".."), mustWork = FALSE)
}
setwd(root)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to run the simulation from source.")
}

pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)

out_dir <- file.path("analysis", "simulation", "results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

seed_base <- as.integer(Sys.getenv("MIMAR_SIM_SEED", "20240601"))
n_rep <- as.integer(Sys.getenv("MIMAR_SIM_REP", "5"))
prop_grid <- seq(0.1, 0.7, by = 0.1)
n <- as.integer(Sys.getenv("MIMAR_SIM_N", "180"))
maxit <- as.integer(Sys.getenv("MIMAR_SIM_MAXIT", "5"))
m <- as.integer(Sys.getenv("MIMAR_SIM_M", "5"))
ncore <- as.integer(Sys.getenv("MIMAR_SIM_NCORE", "1"))

all_methods <- c("mean", "median", "mode", "naive", "norm", "pmm", "spmm",
                 "logreg", "polyreg", "rf", "ranger", "rpart", "nbayes",
                 "svm", "bart", "glmnet", "gbm", "xgboost", "knn", "hotdeck",
                 "famd")

method_groups <- list(
  native = c("mean", "median", "mode", "naive", "norm", "pmm", "spmm", "logreg", "polyreg", "knn", "hotdeck"),
  learner = c("rf", "ranger", "rpart", "nbayes", "svm", "bart", "glmnet", "gbm", "xgboost", "famd")
)

available <- mimar::imputer_registry()
available_methods <- available$imputer[available$available]
method_env <- Sys.getenv("MIMAR_SIM_METHODS", "")
if (nzchar(method_env)) {
  requested_methods <- trimws(strsplit(method_env, ",")[[1]])
  methods <- intersect(requested_methods, available_methods)
} else {
  methods <- intersect(all_methods, available_methods)
}
if (!length(methods)) stop("No imputers are available in the current installation.")

dgp_specs <- list(
  linear = function(seed, n) {
    set.seed(seed)
    z <- matrix(stats::rnorm(n * 6), ncol = 6)
    L <- matrix(c(
      1.0, 0.6, 0.3, 0.2, 0.1, 0.0,
      0.6, 1.0, 0.5, 0.3, 0.1, 0.0,
      0.3, 0.5, 1.0, 0.4, 0.2, 0.1,
      0.2, 0.3, 0.4, 1.0, 0.5, 0.2,
      0.1, 0.1, 0.2, 0.5, 1.0, 0.4,
      0.0, 0.0, 0.1, 0.2, 0.4, 1.0
    ), nrow = 6, byrow = TRUE)
    x <- z %*% chol(L)
    data.frame(
      x1 = x[, 1],
      x2 = 0.7 * x[, 2] - 0.2 * x[, 1] + stats::rnorm(n, 0, 0.2),
      x3 = 0.5 * x[, 3] + 0.4 * x[, 2] + stats::rnorm(n, 0, 0.2),
      x4 = 0.6 * x[, 4] - 0.3 * x[, 2] + stats::rnorm(n, 0, 0.2),
      x5 = 0.4 * x[, 5] + 0.2 * x[, 3] + stats::rnorm(n, 0, 0.2),
      x6 = 0.5 * x[, 6] + 0.3 * x[, 4] + stats::rnorm(n, 0, 0.2)
    )
  },
  nonlinear = function(seed, n) {
    set.seed(seed)
    x1 <- stats::rnorm(n)
    x2 <- stats::rnorm(n)
    x3 <- stats::rnorm(n)
    x4 <- sin(x1) + 0.6 * x2^2 + stats::rnorm(n, 0, 0.15)
    x5 <- x1 * x2 + 0.3 * x3 + stats::rnorm(n, 0, 0.15)
    x6 <- exp(0.2 * x1 - 0.1 * x2 + 0.2 * x3) + stats::rnorm(n, 0, 0.1)
    data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6)
  },
  factor = function(seed, n) {
    set.seed(seed)
    f1 <- stats::rnorm(n)
    f2 <- stats::rnorm(n)
    f3 <- stats::rnorm(n)
    data.frame(
      x1 = 0.8 * f1 + 0.2 * f2 + stats::rnorm(n, 0, 0.2),
      x2 = -0.4 * f1 + 0.7 * f2 + stats::rnorm(n, 0, 0.2),
      x3 = 0.5 * f1 + 0.2 * f3 + stats::rnorm(n, 0, 0.2),
      x4 = f1 * f2 + stats::rnorm(n, 0, 0.2),
      x5 = tanh(f2) + 0.3 * f3 + stats::rnorm(n, 0, 0.2),
      x6 = 0.6 * f1 - 0.5 * f2 + 0.2 * f3 + stats::rnorm(n, 0, 0.2)
    )
  }
)

dgp_env <- Sys.getenv("MIMAR_SIM_DGPS", "")
if (nzchar(dgp_env)) {
  requested_dgps <- trimws(strsplit(dgp_env, ",")[[1]])
  requested_dgps <- intersect(requested_dgps, names(dgp_specs))
  if (length(requested_dgps)) {
    dgp_specs <- dgp_specs[requested_dgps]
  }
}

method_groups <- lapply(method_groups, intersect, methods)

metric_row <- function(truth, completed, mask) {
  out <- lapply(names(truth), function(nm) {
    idx <- as.logical(mask[[nm]])
    if (!any(idx)) return(NULL)
    tr <- truth[[nm]][idx]
    pr <- completed[[nm]][idx]
    keep <- is.finite(tr) & is.finite(pr)
    tr <- tr[keep]
    pr <- pr[keep]
    if (!length(tr)) return(NULL)
    s <- stats::sd(tr)
    if (!is.finite(s) || s == 0) s <- 1
    err <- pr - tr
    data.frame(
      variable = nm,
      rmse = sqrt(mean(err^2)),
      nrmse = sqrt(mean(err^2)) / s,
      mae = mean(abs(err)),
      bias = mean(err),
      row.names = NULL
    )
  })
  out <- do.call(rbind, out)
  if (is.null(out)) return(data.frame(rmse = NA_real_, nrmse = NA_real_, mae = NA_real_, bias = NA_real_))
  data.frame(
    rmse = mean(out$rmse, na.rm = TRUE),
    nrmse = mean(out$nrmse, na.rm = TRUE),
    mae = mean(out$mae, na.rm = TRUE),
    bias = mean(out$bias, na.rm = TRUE)
  )
}

run_one <- function(method, dgp, prop, rep_id) {
  data_seed <- seed_base + rep_id * 1000 + as.integer(prop * 100) * 10 + match(dgp, names(dgp_specs)) * 100000
  amp_seed <- data_seed + 1
  imp_seed <- data_seed + 2
  truth <- dgp_specs[[dgp]](data_seed, n)
  amp <- mimar::ampute(truth, prop = prop, mechanism = "MNAR", seed = amp_seed)
  start <- proc.time()[["elapsed"]]
  res <- tryCatch(
    mimar::impute(amp, m = m, imputer = method, maxit = maxit, seed = imp_seed, ncore = ncore),
    error = function(e) e
  )
  elapsed <- proc.time()[["elapsed"]] - start
  if (inherits(res, "error")) {
    return(data.frame(
      dgp = dgp, prop = prop, replicate = rep_id, method = method,
      status = "error", elapsed = elapsed,
      rmse = NA_real_, nrmse = NA_real_, mae = NA_real_, bias = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  metrics <- lapply(res$imputations, function(comp) metric_row(truth, comp, amp$mask_added))
  metrics <- do.call(rbind, metrics)
  data.frame(
    dgp = dgp, prop = prop, replicate = rep_id, method = method,
    status = "ok", elapsed = elapsed,
    rmse = mean(metrics$rmse, na.rm = TRUE),
    nrmse = mean(metrics$nrmse, na.rm = TRUE),
    mae = mean(metrics$mae, na.rm = TRUE),
    bias = mean(metrics$bias, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

grid <- expand.grid(
  method = methods,
  dgp = names(dgp_specs),
  prop = prop_grid,
  replicate = seq_len(n_rep),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

if (interactive()) {
  message("Running in interactive mode with ", nrow(grid), " simulation cells.")
}

jobs <- split(grid, seq_len(nrow(grid)))
workers <- as.integer(Sys.getenv("MIMAR_SIM_WORKERS", max(1, parallel::detectCores(logical = FALSE) - 1)))
if (workers > 1L) {
  res_list <- parallel::mclapply(jobs, function(row) run_one(row$method, row$dgp, row$prop, row$replicate),
                                 mc.cores = workers, mc.set.seed = TRUE)
} else {
  res_list <- lapply(jobs, function(row) run_one(row$method, row$dgp, row$prop, row$replicate))
}
results <- do.call(rbind, res_list)

summary_stats <- aggregate(
  cbind(nrmse, rmse, mae, bias, elapsed) ~ method + dgp + prop + status,
  data = results,
  FUN = mean
)

ranking <- aggregate(
  cbind(nrmse, elapsed) ~ method + status,
  data = results,
  FUN = mean
)
ranking <- ranking[order(ranking$nrmse, ranking$elapsed), ]

failure_rate <- aggregate(status ~ method + dgp + prop, data = results, FUN = function(x) mean(x != "ok"))
names(failure_rate)[4] <- "failure_rate"

write.csv(results, file.path(out_dir, "raw_results.csv"), row.names = FALSE)
write.csv(summary_stats, file.path(out_dir, "summary_stats.csv"), row.names = FALSE)
write.csv(ranking, file.path(out_dir, "ranking.csv"), row.names = FALSE)
write.csv(failure_rate, file.path(out_dir, "failure_rate.csv"), row.names = FALSE)
  saveRDS(
  list(
    results = results,
    summary = summary_stats,
    ranking = ranking,
    failure_rate = failure_rate,
    params = list(seed_base = seed_base, n_rep = n_rep, prop_grid = prop_grid,
                  n = n, maxit = maxit, m = m, ncore = ncore,
                  methods = methods, method_groups = method_groups,
                  dgp_names = names(dgp_specs))
  ),
  file.path(out_dir, "simulation_results.rds")
)

message("Simulation finished. Results written to ", out_dir)
