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
#' `impute()` performs single or multiple imputation. The default
#' `imputer = "mi"` runs a chained-equation algorithm. A named imputer such as
#' `"pmm"`, `"logreg"`, `"randomForest"`, or `"glmnet"` can also be supplied to
#' use that learner for every incomplete variable it supports.
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
#' Default method selection inside `imputer = "mi"` is type-aware: predictive
#' mean matching (`"pmm"`) for numeric, integer, and Date targets, logistic
#' regression (`"logreg"`) for binary targets, and multinomial one-vs-rest
#' logistic regression (`"polyreg"`) for multiclass targets.
#'
#' @param x A data frame or `mimar_amputation` object.
#' @param ... Passed to methods.
#' @return A `mimar_imputation` object.
#' @export
impute <- function(x, ...) UseMethod("impute")

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
#' Classical learners implemented in `mimar` are `"mean"`, `"median"`, `"mode"`,
#' `"norm"`, `"pmm"`, `"logreg"`, and `"polyreg"`. Machine-learning learners are
#' treated as ordinary `mimar` imputers and are fitted through the required
#' `funcml` fit/predict API: `"randomForest"`, `"knn"`, `"xgboost"`, `"svm"`,
#' `"bart"`, `"naive_bayes"`, `"rpart"`, and `"glmnet"`.
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
#' diagnostics that can be computed from the imputed data alone.
#'
#' @param x A `mimar_imputation` or `mimar_amputation` object.
#' @param ... Passed to methods.
#' @return A `mimar_evaluation` object.
#' @export
evaluate <- function(x, ...) UseMethod("evaluate")

#' Pool external estimates or metrics
#'
#' `pool()` combines analysis results computed on multiple imputed data sets. If
#' `x` contains columns `term`, `estimate`, `std.error`, and `imputation`, Rubin
#' rules are applied using the notation in Marshall et al. (2009). For estimates
#' \eqn{Q_1,\ldots,Q_m} and complete-data variances \eqn{U_1,\ldots,U_m},
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
#' If `x` instead contains `metric`, `value`, and `imputation`, metrics are
#' pooled by their mean and between-imputation standard error.
#'
#' @references Marshall A, Altman DG, Holder RL, Royston P. Combining estimates
#'   of interest in prognostic modelling studies after multiple imputation:
#'   current practice and guidelines. BMC Medical Research Methodology. 2009;9:57.
#'
#' @param x A data frame or list of data frames containing external results.
#' @param ... Passed to methods.
#' @return A `mimar_pool` object.
#' @export
pool <- function(x, ...) UseMethod("pool")
