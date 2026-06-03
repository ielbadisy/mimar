.impute_chained <- function(x, m = 5, maxit = 5, seed = NULL, method = NULL,
                            imputer_spec = NULL, donors = 5, ncore = 1,
                            verbose = FALSE) {
  method <- method %||% "pmm"
  if (!is.numeric(maxit) || length(maxit) != 1 || maxit < 0) {
    .mimar_stop("`maxit` must be a non-negative integer.")
  }
  maxit <- as.integer(maxit)
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    .mimar_stop("`verbose` must be `TRUE` or `FALSE`.")
  }
  if (!is.numeric(ncore) || length(ncore) != 1 || is.na(ncore) || ncore < 1) {
    .mimar_stop("`ncore` must be a positive integer.")
  }
  ncore <- as.integer(ncore)
  if (!is.numeric(donors) || length(donors) != 1 || is.na(donors) || donors < 1) {
    .mimar_stop("`donors` must be a positive integer.")
  }
  donors <- as.integer(donors)
  init <- .impute_meanmode(x, m = 1, seed = seed)$imputations[[1]]
  missing_vars <- names(x)[colSums(is.na(x)) > 0]
  variable_methods <- stats::setNames(rep("none", ncol(x)), names(x))
  if (length(missing_vars)) {
    variable_methods[missing_vars] <- vapply(missing_vars, function(nm) {
      .chained_method_for(x[[nm]], method = method, variable = nm)
    }, character(1))
  }
  if (is.null(imputer_spec)) {
    imputer_spec <- do.call(imputer, list(method))
  }
  missing_cells <- sum(is.na(x))
  if (verbose) {
    .mimar_progress("Starting chained imputation.")
    .mimar_progress(sprintf(
      "Data: %s rows x %s columns; %s missing cell%s in %s variable%s.",
      nrow(x), ncol(x), missing_cells, .plural_s(missing_cells),
      length(missing_vars), .plural_s(length(missing_vars))
    ))
    .mimar_progress(sprintf(
      "Plan: m = %s completed dataset%s, maxit = %s iteration%s, imputer = %s, ncore = %s.",
      m, .plural_s(m), maxit, .plural_s(maxit), method, ncore
    ))
    if (length(missing_vars)) {
      .mimar_progress(sprintf(
        "Variable methods: %s.",
        paste(sprintf("%s=%s", missing_vars, variable_methods[missing_vars]), collapse = ", ")
      ))
    } else {
      .mimar_progress("No missing variables detected; returning initialized completed datasets.")
    }
    if (ncore > 1) {
      .mimar_progress("Parallel execution is active; chain messages may arrive out of order.")
    }
  }

  chains <- functionals::fmap(
    seq_len(m),
    .impute_chained_one,
    ncores = if (ncore > 1) ncore else NULL,
    x = x,
    init = init,
    missing_vars = missing_vars,
    variable_methods = variable_methods,
    maxit = maxit,
    seed = seed,
    imputer_spec = imputer_spec,
    donors = donors,
    verbose = verbose,
    m_total = m
  )
  imputations <- lapply(chains, `[[`, "data")
  trace <- .rbind_or_empty(lapply(chains, `[[`, "trace"))
  if (verbose) {
    .mimar_progress(sprintf(
      "Finished chained imputation; generated %s completed dataset%s with %s trace row%s.",
      length(imputations), .plural_s(length(imputations)),
      nrow(trace), .plural_s(nrow(trace))
    ))
  }

  list(imputations = imputations, variable_methods = variable_methods,
       diagnostics = list(strategy = "chained_equations",
                          imputer = method,
                          engine = "internal_fit_predict",
                          donor_count = donors,
                          ncore = ncore,
                          trace = trace),
       stochastic = TRUE)
}

.impute_chained_one <- function(mi, x, init, missing_vars, variable_methods,
                                maxit, seed, imputer_spec, donors,
                                verbose = FALSE, m_total = NULL) {
  if (!is.null(seed)) set.seed(seed + mi - 1)
  completed <- init
  trace <- list()
  trace_i <- 0L
  if (verbose) {
    seed_note <- if (is.null(seed)) "no fixed seed" else sprintf("seed %s", seed + mi - 1)
    .mimar_progress(sprintf("Imputation %s/%s started (%s).", mi, m_total %||% mi, seed_note))
  }
  for (iter in seq_len(maxit)) {
    if (verbose) {
      .mimar_progress(sprintf("  Iteration %s/%s.", iter, maxit))
    }
    for (nm in missing_vars) {
      obs <- !is.na(x[[nm]])
      mis <- is.na(x[[nm]])
      method_nm <- variable_methods[[nm]]
      if (any(obs) && any(mis) && !(identical(method_nm, "spmm") && iter > 1)) {
        predictors <- setdiff(names(x), nm)
        boot <- sample(which(obs), replace = TRUE)
        learner <- .clone_imputer_spec(imputer_spec, method_nm)
        fitted <- fit(
          learner,
          x = completed[boot, predictors, drop = FALSE],
          y = x[[nm]][boot],
          target = x[[nm]],
          variable = nm,
          donors = donors
        )
        pred <- stats::predict(fitted, completed[mis, predictors, drop = FALSE])
        completed[[nm]][mis] <- .restore_imputed_values(x[[nm]], pred, n = sum(mis))
      }
      if (any(mis)) {
        trace_i <- trace_i + 1L
        trace[[trace_i]] <- .imputation_trace_row(
          imputation = mi,
          iteration = iter,
          variable = nm,
          method = method_nm,
          target = x[[nm]],
          values = completed[[nm]][mis]
        )
        if (verbose) {
          .mimar_progress(.format_imputation_progress(trace[[trace_i]]))
        }
      }
    }
  }
  if (verbose) {
    .mimar_progress(sprintf("Imputation %s/%s finished.", mi, m_total %||% mi))
  }
  list(data = completed, trace = .rbind_or_empty(trace))
}

.clone_imputer_spec <- function(template, method) {
  if (is.null(template)) {
    return(do.call(imputer, list(method)))
  }
  args <- template$args %||% list()
  do.call(imputer, c(list(method, spec = template$spec), args))
}

.imputation_trace_row <- function(imputation, iteration, variable, method, target, values) {
  numeric_var <- is.numeric(target) || is.integer(target) || inherits(target, "Date")
  values <- values[!is.na(values)]
  data.frame(
    imputation = imputation,
    iteration = iteration,
    variable = variable,
    method = method,
    type = .variable_type(target),
    n_imputed = length(values),
    mean = if (numeric_var && length(values)) mean(as.numeric(values), na.rm = TRUE) else NA_real_,
    sd = if (numeric_var && length(values) > 1) stats::sd(as.numeric(values), na.rm = TRUE) else NA_real_,
    unique = length(unique(values)),
    row.names = NULL
  )
}

.chained_method_for <- function(x, method = NULL, variable = NULL) {
  if (!is.null(method)) {
    if (length(method) == 1 && is.null(names(method))) {
      return(.compatible_imputer_method(as.character(method), x))
    }
    if (!is.null(names(method)) && variable %in% names(method)) {
      return(.compatible_imputer_method(as.character(method[[variable]]), x))
    }
  }
  if (is.numeric(x) || is.integer(x) || inherits(x, "Date")) return("pmm")
  if (is.logical(x)) return("logreg")
  nlev <- length(unique(x[!is.na(x)]))
  if (nlev <= 2) "logreg" else "polyreg"
}

.compatible_imputer_method <- function(method, x) {
  imputer(method)
  task <- .target_task(x)
  if (method %in% c("rf", "ranger", "rpart", "nbayes", "svm", "bart", "glmnet",
                    "gbm", "xgboost", "knn", "hotdeck", "famd", "naive",
                    "superlearner", "sl")) {
    return(method)
  }
  if (method %in% c("mean", "median", "norm", "pmm", "spmm")) {
    if (task == "numeric") return(method)
    return(if (task == "binary") "logreg" else "polyreg")
  }
  if (method == "mode") {
    if (task == "numeric") return("median")
    return("mode")
  }
  if (method == "logreg") {
    if (task == "numeric") return("pmm")
    if (task == "multiclass") return("polyreg")
    return("logreg")
  }
  if (method == "polyreg") {
    if (task == "numeric") return("pmm")
    if (task == "binary") return("logreg")
    return("polyreg")
  }
  method
}
