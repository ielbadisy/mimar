#' Forward unknown objects to another attached `fit()` or `evaluate()`
#'
#' `mimar` exports the S3 generics `fit()` and `evaluate()`. Other attached
#' packages, such as survalis, funcml or survfusion, export functions with the
#' same names, and whichever package is attached last masks the other. The
#' default methods make the mimar generics safe to mask with: when `fit()` or
#' `evaluate()` is called on an object that mimar has no method for (for
#' example a formula, as in `fit(Surv(time, status) ~ ., data, model)`), the
#' call is forwarded unchanged to the next function of the same name on the
#' search path. If no such function is attached, an informative error is
#' raised. Calling the other package explicitly, as in `survalis::fit()`,
#' always works too.
#'
#' @param object,x The object passed to the generic.
#' @param ... Passed on unchanged.
#' @return Whatever the forwarded function returns.
#' @name mimar-forwarding
#' @keywords internal
NULL

.mimar_find_masked <- function(name) {
  mine <- tryCatch(getExportedValue("mimar", name), error = function(e) NULL)
  for (pos in search()) {
    if (identical(pos, "package:mimar")) next
    env <- as.environment(pos)
    if (exists(name, envir = env, mode = "function", inherits = FALSE)) {
      f <- get(name, envir = env, mode = "function", inherits = FALSE)
      if (!identical(f, mine)) return(f)
    }
  }
  NULL
}

.mimar_forward <- function(name, call, env) {
  target <- .mimar_find_masked(name)
  if (is.null(target)) {
    .mimar_stop("mimar::", name, "() has no method for this object. ",
                "If you meant another package's ", name, "() (for example survalis or funcml), ",
                "attach it or call it as pkg::", name, "().")
  }
  call[[1L]] <- target
  eval(call, env)
}

#' @rdname mimar-forwarding
#' @export
fit.default <- function(object, ...) {
  .mimar_forward("fit", sys.call(), parent.frame())
}

#' @rdname mimar-forwarding
#' @export
evaluate.default <- function(x, ...) {
  .mimar_forward("evaluate", sys.call(), parent.frame())
}
