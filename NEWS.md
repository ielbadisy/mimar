# mimar 0.10.0

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
