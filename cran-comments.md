## Test environments

* Local Ubuntu 24.04.3 LTS, R 4.5.1

## Submission notes

This is an update from the CRAN-published 1.0.0 to 1.2.0.

* Replaced the `gbm` imputer with `fastgbm` (compiled GBM, native
  regression/binary/multiclass objective inference); `gbm` dropped from
  `Imports`.
* Added `nelsonaalen()` and `ipcw()` (`R/survival_prep.R`) — predictors to
  use in place of raw survival time/censoring when imputing covariates
  (White & Royston 2009); verified against `mice::nelsonaalen()`.
* Added `pool_panglm()`, in the style of the existing `pool_*()` family,
  for pooling `panglm` panel-data model fits across imputations.
* Migrated the internal `.as_dt()`/`.rbind_or_empty()` row-binding helpers
  from `data.table` to `basetable::rbindfill()`; `data.table` dropped from
  `Imports` entirely, `basetable (>= 1.4.0)` added (needs `as_basetable()`,
  now on CRAN).
* `densemlp` and `fastgbm`, both hard `Imports`, are now themselves on
  CRAN (they were GitHub-only when 1.1.0/1.2.0 were developed, which is
  why this resubmission is only happening now).

## R CMD check results

0 errors | 0 warnings | 1 note

* "Suggests or Enhances not in mainstream repositories: panglm" —
  `pool_panglm()` is the only function that uses `panglm` (GitHub-only),
  guarded by `requireNamespace("panglm")`; it is `Suggests`, not
  `Imports`, for exactly this reason.
