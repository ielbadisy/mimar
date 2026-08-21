## Test environments

* Local Ubuntu 24.04.3 LTS, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 2 notes

* "unable to verify current time" — local clock-check note, unrelated to
  the package.
* "New maintainer" — the Maintainer field's family-name casing changed
  from "EL BADISY" to "El Badisy" (title case); same person, same email
  address, no change in maintainership.

## Submission notes

This is an update from the CRAN-published 0.8.0 to 1.0.0.

* Added `pool_coxph()`/`pool_glm()`/`pool_lm()`/`pool_survreg()`/
  `pool_clogit()` — pool a list of fitted models (one per completed data
  set) via Rubin's rules (Barnard-Rubin 1999 correction for `coxph`/`glm`),
  cross-validated against `mice::pool()` to numerical precision.
* Added `densemlp` and `missknn` imputers, and two new hard dependencies
  (`densemlp`, `missknn`) alongside the existing learner backend packages,
  kept in `Imports` for the same reason as before: every imputer listed by
  `imputer_registry()` is available immediately after installation.
* Added a `progress` argument to `impute()`.
* Dropped the `tibble` dependency; public return objects are now
  `data.table`s.
