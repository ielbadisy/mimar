# mimar 1.2.0

* Migrated the internal `.as_dt()`/`.rbind_or_empty()` helpers (the shared
  row-binding/coercion choke point behind every public return object) from
  `data.table` to `basetable`. Public return objects (`describe()`,
  `impute()`, `complete()`, `evaluate()`, `pool()`, `imputer_registry()`)
  are now `basetable`s instead of `data.table`s. `typeconflict = "coerce"`
  is used at the one row-bind site that legitimately mixes column types
  across inputs (plot-data assembly). Also fixed a latent infinite-recursion
  bug this surfaced in `print.mimar_imputers()`: `basetable::as_basetable()`
  is a no-op on anything already carrying class `"basetable"`, so calling
  it on a `describe("imputers")` result (tagged `"mimar_imputers"` on top of
  `"basetable"`) returned the object unchanged instead of a clean table,
  causing `print()` to recurse into itself. `.as_dt()` now always strips to
  a plain `data.frame` first.

* The `gbm` imputer is now `fastgbm`: `imputer = "gbm"` is replaced by
  `imputer = "fastgbm"`, backed by the `fastgbm` package (compiled
  gradient boosting, native regression/binary/multiclass objective
  inference) instead of `gbm`. `gbm` dropped from `Imports` in favor of
  `fastgbm`. Hyperparameters passed through `imputer()`/`...` change
  accordingly (`ntrees`, `learning_rate`, `max_depth` in place of
  `n.trees`, `shrinkage`, `interaction.depth`).

* Added `nelsonaalen()` and `ipcw()`, preprocessing helpers for imputation
  with time-to-event data. `nelsonaalen()` returns the Nelson-Aalen
  cumulative hazard at each subject's time, the predictor to use in place of
  the raw survival time when imputing other covariates (White and Royston,
  2009); it matches `mice::nelsonaalen()` with no auxiliary variables and,
  when `aux` is supplied, estimates the cumulative hazard within strata
  (categorical `aux`) or from a Cox model (`method = "breslow"`).
  `ipcw()` returns subject-level inverse-probability-of-censoring weights
  `Delta_i / G(T_i-)` from a Kaplan-Meier censoring model, with `aux` support
  for covariate-dependent censoring (stratified Kaplan-Meier or `method =
  "cox"`), plus `type = "all"`, `stabilized`, and `truncate` options. Both
  accept the time and status columns either as strings or as unquoted
  symbols. The Cox paths use `survival` (already in `Suggests`).

# mimar 1.1.0

* The `densemlp` imputer now runs on `densemlp`'s native
  C++/RcppArmadillo dense MLP (no `torch`/`libtorch` dependency, faster to
  fit). `imputer = "densemlp"` fits/predicts through the standard chained
  loop as before, but `task` detection is finer-grained: `"regression"`,
  `"binary"`, and `"multiclass"` are distinguished natively, matching
  mimar's own per-variable task detection exactly, instead of the previous
  single `"classification"`.

# mimar 1.0.1

* Added `pool_panglm()`, pooling a list of `panglm` panel-data model fits
  (one per completed data set) across imputations using Rubin's rules,
  following the design of the existing `pool_glm()`/`pool_lm()` (printed
  coefficient table mirroring `summary.glm()`, `t value`/`z value` labelled
  by family, Barnard-Rubin corrected degrees of freedom from
  `df.residual()` when the `panglm` estimator provides one - `model =
  "pooling"`/`"within"` do; `model = "random"` does not compute a full
  likelihood and falls back to a normal reference distribution). Warns
  (does not error) if the fits being pooled disagree on `panglm`'s
  `model`/`effect`/`family` settings. `panglm` added to `Suggests` for the
  example/tests. `devtools::test()`: all pass, including a new
  `test-pool-panglm` case. `R CMD check --as-cran`: Status OK, 0 errors/0
  warnings.

# mimar 1.0.0

* Added `pool_coxph()`, `pool_glm()`, `pool_lm()`, `pool_survreg()`, and
  `pool_clogit()`, convenience helpers that pool a list of fitted models
  (one per completed data set) using Rubin's rules and print a coefficient
  table formatted like the corresponding base-R `summary()` output
  (`summary.coxph()`, `summary.glm()`, `summary.lm()`, `summary.survreg()`;
  `pool_clogit()` matches `pool_coxph()` since `clogit()` fits a stratified
  Cox model internally). Degrees of freedom use the Barnard and Rubin (1999)
  correction rather than the classic Rubin (1987) formula, which can diverge
  to implausibly large values when between-imputation variance is small
  relative to within-imputation variance; all five were cross-validated
  against `mice::pool()` to numerical precision on matched examples.

# mimar 0.9.0

* Added `densemlp` imputer, wrapping the `densemlp` package's dense
  multilayer perceptron as a standard fit/predict learner inside the chained
  imputation loop (numeric targets predict a point estimate, categorical
  targets draw from predicted class probabilities).
* Added `missknn` imputer, wrapping the `missknn` package's whole-table
  masked k-nearest-neighbor engine. Because `missknn` imputes all variables
  jointly in one pass rather than per-variable, selecting it bypasses the
  chained-equations loop entirely as a distinct single-shot strategy.
* Added `progress` argument to `impute()`, showing an elapsed/ETA progress
  bar over completed datasets via `functionals::fmap(pb = TRUE)`. Defaults to
  `TRUE` in interactive sessions when `verbose = FALSE`, and has no effect
  when `imputer = "missknn"`.
* Dropped the `tibble` dependency package-wide. All tabular results
  previously returned as tibbles (`describe()`, `impute()`, `complete()`,
  `evaluate()`, `pool()`, `imputer_registry()`, and friends) are now
  `data.table`s instead. Internal row-binding of trace/diagnostic/pooling
  data frames also moved from base `do.call(rbind, ...)` to
  `data.table::rbindlist()`, the shared backend behind `.rbind_or_empty()`.
  The engine internals (chained-equations loop, amputation) still operate on
  plain `data.frame`s to keep base-R subsetting semantics intact; only the
  user-facing return objects changed class.

# mimar 0.8.0

* Added `superlearner` and `sl` imputers. These construct a Super
  Learner-style ensemble by cross-validating candidate imputers on observed
  cells, assigning non-negative loss-based weights, and combining predictions
  inside the existing chained-imputation loop.
* Added `library`, `folds`, and `metalearner` hyperparameters for
  `superlearner`.
* Updated CRAN preparation files and vignette examples for the new release.

# mimar 0.7

First public release candidate.

* Added `ncore` to `impute()` for completed-dataset-level parallel imputation
  through `functionals::fmap()`.
* Added lightweight iteration traces to `mimar_imputation` diagnostics for
  convergence screening.
* Added diagnostic plot types for boxplots, bivariate observed/imputed
  comparisons, categorical proportions, and trace summaries.
* Updated density diagnostics to draw line-only overlays across imputations so
  multiple completed datasets remain visible.
* Refreshed the diagnostic plotting palette to give `mimar` a distinct visual
  identity while retaining the existing plot themes.
* Expanded the vignette with KNN-based diagnostic examples, parallel imputation
  notes, and interpretation guidance.

# mimar 0.0.1

* Initial compact missing-data grammar.
* Added description, amputation, imputation, evaluation, pooling, and plotting.
* Added chained native and optional learner-backed imputation adapters without
  a `funcml` dependency.
