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
