## Update submission (1.0.0 -> 1.4.2)

This update adds model poolers (`pool_lm()`, `pool_glm()`, `pool_coxph()`,
`pool_survreg()`, `pool_clogit()`, `pool_panglm()`), pooled multi-parameter
tests (`pool_test()`, D1/D2/D3), named pooling scales and a `dfcom` argument
in `pool()`, the `nelsonaalen()` and `ipcw()` helpers, and replaces the `gbm`
imputer with `fastgbm`. Tabular results now use `basetable`. See NEWS.md.

The `missknn` imputer works with both the current CRAN missknn (1.1.2) and
the upcoming missknn 1.2.0, which changes its return value. missknn 1.2.0
will be submitted after this version is on CRAN, so that the CRAN version of
mimar keeps working.

## Test environments

* Local: Ubuntu 24.04, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

missknn (Suggests). Its checks pass with this version.
