#' Deprecated functions in mimar
#'
#' These functions were renamed to glued lowercase names and will be removed
#' in a future release. Use the new names.
#'
#' | Old name | New name |
#' |---|---|
#' | `imputer_registry()` | [imputerregistry()] |
#' | `pool_clogit()` | [poolclogit()] |
#' | `pool_coxph()` | [poolcoxph()] |
#' | `pool_glm()` | [poolglm()] |
#' | `pool_lm()` | [poollm()] |
#' | `pool_panglm()` | [poolpanglm()] |
#' | `pool_survmat()` | [poolsurvmat()] |
#' | `pool_survreg()` | [poolsurvreg()] |
#' | `pool_test()` | [pooltest()] |
#'
#' @param ... Arguments passed to the new function.
#' @name mimar-deprecated
#' @keywords internal
NULL

#' @rdname mimar-deprecated
#' @export
imputer_registry <- function(...) { .Deprecated("imputerregistry"); imputerregistry(...) }

#' @rdname mimar-deprecated
#' @export
pool_clogit <- function(...) { .Deprecated("poolclogit"); poolclogit(...) }

#' @rdname mimar-deprecated
#' @export
pool_coxph <- function(...) { .Deprecated("poolcoxph"); poolcoxph(...) }

#' @rdname mimar-deprecated
#' @export
pool_glm <- function(...) { .Deprecated("poolglm"); poolglm(...) }

#' @rdname mimar-deprecated
#' @export
pool_lm <- function(...) { .Deprecated("poollm"); poollm(...) }

#' @rdname mimar-deprecated
#' @export
pool_panglm <- function(...) { .Deprecated("poolpanglm"); poolpanglm(...) }

#' @rdname mimar-deprecated
#' @export
pool_survmat <- function(...) { .Deprecated("poolsurvmat"); poolsurvmat(...) }

#' @rdname mimar-deprecated
#' @export
pool_survreg <- function(...) { .Deprecated("poolsurvreg"); poolsurvreg(...) }

#' @rdname mimar-deprecated
#' @export
pool_test <- function(...) { .Deprecated("pooltest"); pooltest(...) }
