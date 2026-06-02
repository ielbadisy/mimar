#' Describe missing data and mimar objects
#'
#' `describe()` summarizes missingness for data frames and returns compact
#' summaries for `mimar` objects.
#'
#' For a data matrix \eqn{X \in R^{n \times p}}, missingness is represented by
#' the indicator
#' \deqn{R_{ij} = I(X_{ij}\ \text{is missing}),}
#' where \eqn{i} indexes rows and \eqn{j} indexes variables. The variable-level
#' missingness proportion reported by `describe()` is
#' \deqn{\hat{\pi}_j = n^{-1}\sum_{i=1}^n R_{ij}.}
#' Row summaries use \eqn{p^{-1}\sum_{j=1}^p R_{ij}}, and missingness patterns
#' are the unique row vectors of \eqn{R}.
#'
#' @param x An object to describe.
#' @param ... Passed to methods.
#' @return A mimar S3 object.
#' @export
describe <- function(x, ...) UseMethod("describe")

#' Artificially introduce missing data
#'
#' `ampute()` creates benchmark data by adding missing values under MCAR, MAR,
#' or MNAR mechanisms. The returned object keeps the original data, amputated
#' data, and masks for original, added, and total missingness.
#'
#' For each target variable \eqn{X_j}, observed cells are removed by drawing
#' \deqn{A_{ij} \sim \mathrm{Bernoulli}(p_i)}
#' and setting \eqn{X_{ij}} to missing when \eqn{A_{ij}=1}. For MCAR,
#' \eqn{p_i = \pi}, where \eqn{\pi} is `prop`. For MAR, probabilities are based
#' on observed `by` variables through a standardized score \eqn{s_i}:
#' \deqn{p_i = \mathrm{logit}^{-1}(\alpha + s_i),}
#' with \eqn{\alpha} calibrated so that the average probability is `prop`. For
#' MNAR, \eqn{s_i} is derived from the target variable itself; `direction`
#' selects high values, low values, or both tails for numeric targets.
#'
#' @param x A data frame.
#' @param ... Passed to methods.
#' @return A `mimar_amputation` object.
#' @export
ampute <- function(x, ...) UseMethod("ampute")

#' Impute missing data
#'
#' `impute()` performs single or multiple imputation through a chained update
#' procedure owned by `mimar`. The default `imputer = "pmm"` uses predictive
#' mean matching. A named imputer such as `"naive"`, `"rf"`, `"xgboost"`,
#' `"knn"`, or `"glmnet"` can also be supplied to use that learner for every
#' incomplete variable it supports. The returned
#' object keeps completed datasets first; use `complete()` to extract one
#' completed dataset, all completed datasets, or a stacked long data frame.
#'
#' Hyperparameters for learner-backed imputers can be supplied through the
#' `imputer()` specification or directly through `...` when calling `impute()`.
#' For donor-based imputers, `donors` controls the donor pool used by `"pmm"`,
#' `"spmm"`, `"knn"`, and `"hotdeck"`.
#'
#' Let \eqn{X_j} be an incomplete variable and \eqn{X_{-j}} all remaining
#' variables. At each iteration, `mimar` fits an imputer learner on observed rows
#' \deqn{\hat f_j: X_{-j}^{obs} \rightarrow X_j^{obs}}
#' and predicts missing cells \eqn{X_j^{mis}} from \eqn{X_{-j}^{mis}}. This is
#' repeated across incomplete variables for `maxit` iterations and across
#' \eqn{m} independent completed data sets. The algorithm is intentionally
#' learner-agnostic: each imputer is constructed with `imputer()`, trained with
#' `fit()`, and used through `predict()`.
#'
#' Each requested imputer is applied to all incomplete variables it supports.
#' Use `imputer_registry()` to inspect target-type compatibility.
#' Learner-backed methods are supervised stochastic update rules inside the
#' chained workflow; inspect diagnostics and downstream sensitivity rather than
#' treating any single learner as a guarantee of proper uncertainty
#' quantification.
#'
#' @param x A data frame or `mimar_amputation` object.
#' @param m Number of completed data sets to generate.
#' @param imputer Imputer name or a `mimar_imputer` specification.
#' @param maxit Number of chained-equation iterations.
#' @param seed Optional random seed.
#' @param donors Number of candidate donors used by donor-based imputers.
#' @param ncore Number of CPU cores used to run completed datasets in
#'   parallel. The default, `1`, runs sequentially. Values greater than one are
#'   passed to `functionals::fmap()` as `ncores`.
#' @param ... Passed to methods.
#' @return A `mimar_imputation` object.
#' @export
impute <- function(x, m = 5, imputer = "pmm", maxit = 5, seed = NULL,
                   donors = 5, ncore = 1, ...) UseMethod("impute")

#' Extract completed imputed data
#'
#' `complete()` extracts completed data sets from imputation objects. For
#' `mimar_imputation` objects, use `complete(x)` or `complete(x, 1)` for one
#' completed data set, `complete(x, "all")` for all completed data sets, and
#' `complete(x, "long")` for a stacked long data frame.
#'
#' @param x An object containing completed data.
#' @param ... Passed to methods.
#' @return A data frame or list of data frames.
#' @export
complete <- function(x, ...) UseMethod("complete")

#' Build an imputer learner
#'
#' `imputer()` constructs a standalone learner descriptor used by the chained
#' imputation engine. All imputers expose the same standard lifecycle:
#'
#' \enumerate{
#'   \item construct with `imputer(method)`;
#'   \item fit on observed rows with `fit(object, x, y)`;
#'   \item impute new rows with `predict(fitted, newdata)`.
#' }
#'
#' Native learners implemented in `mimar` include `"mean"`, `"median"`,
#' `"mode"`, `"naive"`, `"norm"`, `"pmm"`, `"spmm"`, `"logreg"`, `"polyreg"`,
#' `"knn"`, and `"hotdeck"`. Learner-backed imputers such as `"rf"`,
#' `"xgboost"`, `"svm"`, `"bart"`, `"nbayes"`, `"rpart"`, `"glmnet"`,
#' `"gbm"`, and `"famd"` are called directly through their original packages
#' installed with `mimar`. Additional arguments supplied to `imputer()` are
#' retained as hyperparameters and used by `impute()` and `fit()`.
#' The `"superlearner"` imputer, also available as `"sl"`, cross-validates a
#' candidate imputer library on observed cells and combines candidates using
#' non-negative loss-based weights.
#'
#' Compatibility with target types is explicit. If an imputer does not support a
#' numeric, binary, or multiclass target, `mimar` stops with an error rather than
#' silently falling back to another method.
#'
#' @param method Imputer method name.
#' @param ... Passed to the imputer constructor.
#' @return A `mimar_imputer` object.
#' @export
imputer <- function(method, ...) UseMethod("imputer")

#' Evaluate imputation quality
#'
#' `evaluate()` compares imputed values with known truth when an amputation
#' object is available. Numeric targets are summarized with errors such as
#' \deqn{e_{ij} = \tilde X_{ij} - X_{ij},}
#' while categorical targets are summarized with agreement and balanced
#' accuracy across classes. When no truth is available, `evaluate()` reports
#' diagnostics that can be computed from the imputed data alone. Distribution,
#' variability, and recovery summaries are computed across all completed data
#' sets; per-imputation recovery metrics are kept in `recovery_by_imputation`.
#'
#' @param x A `mimar_imputation` or `mimar_amputation` object.
#' @param ... Passed to methods.
#' @return A `mimar_evaluation` object.
#' @export
evaluate <- function(x, ...) UseMethod("evaluate")

#' Pool post-fit quantities across imputations
#'
#' `pool()` combines post-fit quantities estimated separately on each completed
#' data set. The object being pooled is a quantity: a scalar, vector, matrix,
#' array, model coefficient, survival probability, metric, or other estimate of
#' interest. A data frame is only a convenient tabular adapter for tidy scalar
#' estimates; it is not the statistical target being pooled.
#'
#' For a scalar quantity with estimates \eqn{Q_1,\ldots,Q_m} and complete-data
#' variances \eqn{U_1,\ldots,U_m}, Rubin rules are
#' \deqn{\bar Q = m^{-1}\sum_{k=1}^m Q_k,}
#' \deqn{\bar U = m^{-1}\sum_{k=1}^m U_k,}
#' \deqn{B = (m-1)^{-1}\sum_{k=1}^m (Q_k-\bar Q)^2,}
#' and total variance is
#' \deqn{T = \bar U + (1 + m^{-1})B.}
#' The reported standard error is \eqn{\sqrt{T}}. Confidence intervals use a
#' \eqn{t} reference distribution with
#' \deqn{\nu = (m-1)\left(1 + r^{-1}\right)^2,\quad
#' r = \frac{(1 + m^{-1})B}{\bar U}.}
#'
#' For a vector quantity, pass `x` as a list of numeric vectors and `covariance`
#' as a list of complete-data covariance matrices. Rubin's matrix form is then
#' used: \eqn{\bar Q} is a vector and \eqn{T = \bar U + (1 + m^{-1})B} is the
#' pooled covariance matrix. For matrices or arrays, pass a list of same-shaped
#' quantities. Unless a joint covariance structure is supplied through a vector
#' input, these are pooled element by element, which is appropriate for grids of
#' scalar estimands such as survival probabilities at several times and
#' covariate profiles.
#'
#' Some metrics do not have reliable complete-data variance estimates or do not
#' satisfy approximate normality. Following Marshall et al. (2009), `pool()`
#' reports robust summaries by default when no variance is supplied: median,
#' interquartile range, and range across imputations. Use `rule = "mean"` to
#' request a mean and between-imputation standard error for such metrics.
#'
#' @examples
#' pool(c(0.10, 0.11, 0.09), std.error = c(0.04, 0.05, 0.04), name = "age")
#'
#' betas <- list(c(age = 0.10, bmi = 0.30),
#'               c(age = 0.11, bmi = 0.32),
#'               c(age = 0.09, bmi = 0.29))
#' covs <- list(diag(c(0.04, 0.08)^2),
#'              diag(c(0.05, 0.09)^2),
#'              diag(c(0.04, 0.08)^2))
#' pool(betas, covariance = covs)
#'
#' @references Marshall A, Altman DG, Holder RL, Royston P. Combining estimates
#'   of interest in prognostic modelling studies after multiple imputation:
#'   current practice and guidelines. BMC Medical Research Methodology. 2009;9:57.
#'
#' @param x Quantity estimates across imputations. Use a numeric vector for one
#'   scalar quantity, a list for scalar/vector/matrix/array quantities, a matrix
#'   with imputations in rows and quantities in columns, or a data frame as a
#'   tabular adapter for tidy scalar estimates.
#' @param ... Passed to methods.
#' @return A `mimar_pool` object.
#' @export
pool <- function(x, ...) UseMethod("pool")
