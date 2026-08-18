## Test environments

* Local Ubuntu 24.04.3 LTS, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission notes

This is an update from the CRAN-published 0.8.0 to 0.9.0.

* Added `densemlp` and `missknn` imputers, and two new hard dependencies
  (`densemlp`, `missknn`) alongside the existing learner backend packages,
  kept in `Imports` for the same reason as before: every imputer listed by
  `imputer_registry()` is available immediately after installation.
* Added a `progress` argument to `impute()`.
* Dropped the `tibble` dependency; public return objects are now
  `data.table`s.
