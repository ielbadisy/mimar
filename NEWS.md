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
