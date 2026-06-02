## Test environments

* Local Ubuntu 24.04.3 LTS, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.
* Local check could not verify the current time in this environment.

## Submission notes

This is the first CRAN submission of `mimar`.

Learner backend packages are kept in `Imports` intentionally so every imputer
listed by `imputer_registry()` is available immediately after installation.
