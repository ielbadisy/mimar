# mimar 1.4.14

* New `assess()` scores imputation quality, with the arguments and output of the former `evaluate()`
  (imputations, amputations, lists of completed data frames and single data frames).
* `evaluate()` is deprecated in favour of `assess()`; it still works and warns. It will be removed in a later
  release with `fit()` and the forwarding to funcml and survalis, so mimar no longer masks their verbs.

# mimar 1.4.13

* New `fitimputer(object, x, y, ...)` trains one imputer on the observed rows of one variable (the step `impute()`
  repeats for each variable and iteration). `object` can be a `mimar_imputer` or an imputer name such as `"rf"`.
* `fit()` on a `mimar_imputer` is deprecated in favour of `fitimputer()`; it still works and warns. With
  `with()` (1.4.12), `fit()` no longer has a non-deprecated use in mimar and will be removed in a later release,
  ending the name clash with funcml and survalis.

# mimar 1.4.12

* New `with()` method for imputations: `with(imp, lm(y ~ x))` evaluates the model call on each completed data set
  and returns the list of fits for pooling, as in `mice`. Any model call works, so no `fun` argument is needed.
* `fit()` on a `mimar_imputation` is deprecated in favour of `with()`; it still works and warns. `fit()` stays for
  training a `mimar_imputer`. This prepares removing the `fit()` name clash with funcml and survalis.

# mimar 1.4.11

* New `poolcompare(fits, term, scale = 1)` collects one term from several analyses (a named list of mimar pooled
  results and/or single `coxph`, `glm`, `lm`, `survreg` fits), with the coefficient, standard error and limits
  multiplied by `scale` (for example `log(2)` for a per-doubling effect of a log-transformed covariate). Pooled rows
  keep their Rubin's-rules interval; single fits get the Wald interval of `confint()` (`confint.default()` for
  `glm`, the t interval for `lm`). Its `plot()` method draws one row per analysis, exponentiated on a log axis for
  Cox, logistic and log-link models, with `labels`, `ref` (dotted line at one analysis), `xlab` and per-analysis
  `palette`. A `plot()` method for bare lists is not provided, since it would capture `plot()` on any list.
* `plot(describe(x))` (bar type): the right plot margin is wider so the last percent label of the x axis
  (for example "12.5%") is no longer clipped (bug fix).

# mimar 1.4.10

Plot fixes. Only plots change; no numeric result changes.

* `plot()` now dispatches on the model pooling results: `poollm()`, `poolglm()`, `poolcoxph()`, `poolclogit()`, `poolsurvreg()` and `poolpanglm()` objects also inherit `mimar_pool` (previously `plot()` fell back to `plot.default()` and errored). The forest plot gains `exponentiate` (default `TRUE` for Cox and conditional logistic fits, drawn on a log axis with a reference line at 1 and the axis titled "Hazard ratio" or "Odds ratio"), `labels` for term names, `intercept` (the intercept is now left out by default) and `xlab`; terms are shown in model order.
* New `plot()` method for `tippingpoint()` results (now of class `mimar_tippingpoint`, still a basetable): the pooled estimate and its confidence band against the shift, a reference line at `null`, and a dotted line at the tipping point (the linearly interpolated shift at which the confidence limit nearest the null reaches it), returned in the `tipping` attribute.
* `plot()` on `describe()` output gains `type = "pattern"` (missing-data pattern matrix, rows labelled by their counts, as `mice::md.pattern()`), `by` (missing share per group), `complete` (variables without missing values are now left out by default) and `labels`. Percent axis labels are no longer rounded to whole numbers (2.5% printed as "2%").
* `plot()` on an imputation gains `labels` (display names for facet strips and axes). Trace plots drop the per-chain legend when `average = TRUE` or with more than 8 chains, and the y axis reads "Mean (or SD) of imputed values". Density plots have an x axis title, line legend keys and the legend at the bottom.
* `plot()` on `ampute()` output shows only the amputed columns, as stacked originally missing and added cells, horizontally.
* `plot()` on `evaluate()` output shows the normalized RMSE by default (`metric = "nrmse"`, comparable across variables; `metric = "rmse"` gives the raw RMSE) and the proportion of falsely classified cells for categorical variables, and gains `compare` (a named list of further evaluations drawn side by side) and `labels`.
* Default accent colours follow the shared palette: blue `#0072B2` and vermillion `#D55E00`; groups use blue, vermillion, then Okabe-Ito.

# mimar 1.4.9

* `plot()` on a `mimar_imputation` gains `palette` (colours for observed and imputed values, one per chain for `type = "trace"`, or per category for the other types) and `average` (adds the across-chain average to trace plots as a thick line). Colours can now be changed without adding a second ggplot2 scale, which triggered "Scale for colour is already present" messages. `plot()` on `describe()`, `ampute()`, `evaluate()`, `pool()` and `describe("imputers")` output also gains `palette`.
* Default plot colours are now sober and colour-blind safe: observed values dark grey, imputed values blue, trace chains in grey shades, methods in the Okabe-Ito palette. In density plots the observed curve is drawn last and thicker so the imputed curves no longer hide it, and trace plots use integer iteration breaks. Only colours change, no numeric result.

# mimar 1.4.8

* Rubin pooling (`pool()` and the model helpers `poollm()`, `poolglm()`, `poolcoxph()`, `poolclogit()`, `poolsurvreg()`, `poolpanglm()`) now reports, in `$pooled`, the proportion of variance due to missingness `lambda` and the fraction of missing information `fmi` = (r + 2/(df + 3))/(1 + r), computed with the reported (Rubin or Barnard-Rubin) df. Both match `mice::pool()`.
* `?nelsonaalen` now states that it computes the exact Nelson-Aalen estimator (as `basehaz()` of a null `coxph(ties = "breslow")` fit) and that `mice::nelsonaalen()` uses Efron tie handling, so the two differ slightly when event times are tied. No numeric change.

# mimar 1.4.7

* New `deltaadjust()` shifts the imputed values of chosen numeric variables by a fixed delta in every completed data set (observed values untouched, optional subgroup `rows`), for MNAR sensitivity analysis. New `tippingpoint()` repeats the shift over a grid of deltas, fits and pools the analysis model at each, and returns the pooled estimates.
* New `predict()` method for a `mimar_imputation` imputes the missing cells of new data (such as a test set) from the training imputation only: for each completed training data set, every imputation model is fitted on the training rows and applied to the new rows by chained updates, without refitting on or leaking from the new data (as `mice(ignore = )`). It returns a `mimar_imputation` of the new data, ready for `complete()`, `fit()` and pooling.
* `fit()` and `evaluate()` gain default methods that forward a call mimar has no method for (such as `fit(Surv(time, status) ~ ., data, model = "coxph")`) to the next attached function of the same name, for example from survalis or funcml. Loading mimar after those packages no longer breaks their `fit()` and `evaluate()`. No export is removed or renamed.

# mimar 1.4.7

* New `deltaadjust()` shifts the imputed values of chosen numeric variables by a fixed delta in every completed data set (observed values untouched, optional subgroup `rows`), for MNAR sensitivity analysis. New `tippingpoint()` repeats the shift over a grid of deltas, fits and pools the analysis model at each, and returns the pooled estimates.

# mimar 1.4.6

* `fit()` now accepts an imputation: `fit(imp, Ozone ~ Wind)` fits the model (with `lm()` by default, or any `fun` such as `glm`) on every completed data set and returns the list of fits that `poollm()`, `poolglm()`, `poolcoxph()` and the other pooling functions take. `model` may also be a function of one completed data frame.

# mimar 1.4.5

* `evaluate()` now scores imputations against known truth in all the documented ways: `truth` may be a `mimar_amputation` object (previously ignored, so `evaluate(imp, truth = amp)` returned an empty recovery table) or the complete data frame, and the recovery tables report the normalized RMSE (`nrmse`) for numeric and the proportion of falsely classified cells (`pfc`) for categorical variables. `evaluate()` also accepts completed data from other imputers: a list of completed data frames (such as `missknn::missknn()` multiple imputation output, class `mimar_mi`) or a single completed data frame, with the incomplete data given as `data` or taken from an amputation `truth`.

# mimar 1.4.4

* Exported functions are renamed to glued lowercase names (no underscores).
  The old names still work as deprecated wrappers for one release and will be
  removed afterwards.

| Old name | New name |
|---|---|
| `imputer_registry()` | `imputerregistry()` |
| `pool_clogit()` | `poolclogit()` |
| `pool_coxph()` | `poolcoxph()` |
| `pool_glm()` | `poolglm()` |
| `pool_lm()` | `poollm()` |
| `pool_panglm()` | `poolpanglm()` |
| `pool_survmat()` | `poolsurvmat()` |
| `pool_survreg()` | `poolsurvreg()` |
| `pool_test()` | `pooltest()` |

* Requires basetable >= 1.4.5, which renamed `as_basetable()` to
  `asbasetable()`.

# mimar 1.4.3

* Bug fix: `imputer = "norm"` was improper. It refit on a bootstrap sample and
  plugged in the residual SD, so between-imputation variance was too small.
  It now draws `sigma^2 = RSS / chi2(n - p)` and
  `beta ~ N(beta_hat, sigma^2 (X'X)^-1)` (the flat-prior posterior, as in
  `mice`) and imputes from those draws.
* Bug fix: `imputer = "pmm"` (and `"spmm"`) used the same bootstrap coefficients
  to score both observed and missing rows (type-0 matching) and drew donors
  from the bootstrap sample, so between-imputation variance did not match
  `mice` (up to 60% above it under MAR in our checks). It now uses type-1
  matching: the posterior draw scores the missing rows, `beta_hat` scores the
  observed rows, and a donor is sampled at random among the `donors` nearest,
  with ties broken at random. Coverage and between-imputation variance now
  match `mice` within Monte Carlo error.
* Vignette: the response indicator now follows Rubin's convention
  (`R = 1` observed) and the relation to the `ampute()` masks is stated; the
  claim that complete-case analysis is unbiased only under MCAR is corrected
  (for regression it is also unbiased when missingness depends on the
  covariates only).

# mimar 1.4.2

* `impute(imputer = "missknn")` works with missknn 1.2.0, which returns the
  completed data directly instead of a `missknn` object; earlier missknn
  versions keep working.
* Grouped summaries and joins now use `basetable` (>= 1.4.2): the
  `evaluate()` recovery table (`basetable::aggregate()`), the proportion plot
  data (`basetable::count()` and `basetable::merge()`), and tabular
  `pool()` (`basetable::split()`). Pooled terms and metrics now keep their
  input order instead of being sorted alphabetically, and the recovery table
  is ordered by variable.

# mimar 1.4.1

* Bug fix: `impute(imputer = "polyreg")` failed with "'x' must be an array
  of at least two dimensions" when a multiclass factor had exactly one
  missing value.

# mimar 1.4.0

* Bug fix: `pool_panglm()` used `df.residual()` as the complete-data df
  whatever covariance the fits carried. For cluster-robust fits
  (`panglm(..., vcov = "cluster")`) that is the classical NT - N - K,
  which overstates the Barnard-Rubin df and narrows intervals when the
  number of panel individuals G is small. The default is now
  G - 1 for cluster-robust fits and `df.residual()` otherwise, and the
  new `dfcom` argument overrides it. The value used is returned as `dfcom`.
* `pool_test()` with `method = "D1"` on `panglm` fits uses the same default
  complete-data df when `dfcom` is `NULL`.
* `pool_panglm()` also warns when the fits differ in covariance type, and
  its documentation states that a single-level imputation model is not
  valid for within or random-effects analyses.
* Bug fix: a predictor named `y` collided with the internal response name of
  the regression-based imputers (`norm`, `pmm`, `logreg`, `polyreg`,
  `ranger`, `rpart`, `naive_bayes`, `svm`) and was silently dropped from the
  imputation model. The internal response is now `.mimar_y`.

# mimar 1.3.0

* New `pool_test()` for pooled multi-parameter tests after multiple
  imputation, e.g. all levels of a factor or a model against a nested one:
  D1 (multivariate Wald, Li, Raghunathan and Rubin, 1991, with the Reiter
  2007 small-sample df when `dfcom` is given), D2 (combined Wald or
  likelihood-ratio chi-square statistics, Li, Meng, Raghunathan and Rubin,
  1991), and D3 (Meng and Rubin, 1992, likelihood ratio for `lm` and
  gaussian/binomial/poisson `glm`), each with an F reference distribution.
  Results match `mitml::testModels()`.

* `pool()` accepts a named pooling scale, `transform = "log"`, `"logit"`,
  `"cloglog"`, or `"fisherz"`, so that bounded or skewed quantities (ratios,
  probabilities, AUC, C-index, correlations) are pooled where the normal
  approximation behind Rubin's rules holds (Marshall et al., 2009). Variances
  are given on the original scale and carried over with the delta method;
  estimates, standard errors, and intervals are reported on the original
  scale. Tidy metric rows (`metric`, `value`, `imputation`) that also carry a
  `std.error` column are now pooled with Rubin's rules instead of the robust
  summary, and honour `transform`. Intervals are now ordered correctly when
  a function `transform` has a decreasing inverse.

* `pool()` and `pool_survmat()` gain a `dfcom` argument (complete-data
  degrees of freedom). When supplied, Rubin pooling uses the Barnard and
  Rubin (1999) small-sample degrees of freedom, as the model poolers
  (`pool_lm()`, `pool_glm()`, ...) already did. Without it, the classic Rubin
  (1987) degrees of freedom are kept; they assume a normal complete-data
  reference and can be far too large in small samples. Results match
  `mice::pool()` with the same `dfcom`.

* `pool(rule = "mean")` now honours `conf.level`; it previously always
  returned a 95% interval. Its documentation now states that it uses only the
  between-imputation variance.

# mimar 1.2.0

* Migrated the internal `.as_dt()`/`.rbind_or_empty()` helpers (the shared
  row-binding/coercion choke point behind every public return object) from
  `data.table` to `basetable`. Public return objects (`describe()`,
  `impute()`, `complete()`, `evaluate()`, `pool()`, `imputer_registry()`)
  are now `basetable`s instead of `data.table`s. `typeconflict = "coerce"`
  is used at the one row-bind site that legitimately mixes column types
  across inputs (plot-data assembly). Also fixed a latent infinite-recursion
  bug this surfaced in `print.mimar_imputers()`: `basetable::as_basetable()`
  is a no-op on anything already carrying class `"basetable"`, so calling
  it on a `describe("imputers")` result (tagged `"mimar_imputers"` on top of
  `"basetable"`) returned the object unchanged instead of a clean table,
  causing `print()` to recurse into itself. `.as_dt()` now always strips to
  a plain `data.frame` first.

* The `gbm` imputer is now `fastgbm`: `imputer = "gbm"` is replaced by
  `imputer = "fastgbm"`, backed by the `fastgbm` package (compiled
  gradient boosting, native regression/binary/multiclass objective
  inference) instead of `gbm`. `gbm` dropped from `Imports` in favor of
  `fastgbm`. Hyperparameters passed through `imputer()`/`...` change
  accordingly (`ntrees`, `learning_rate`, `max_depth` in place of
  `n.trees`, `shrinkage`, `interaction.depth`).

* Added `nelsonaalen()` and `ipcw()`, preprocessing helpers for imputation
  with time-to-event data. `nelsonaalen()` returns the Nelson-Aalen
  cumulative hazard at each subject's time, the predictor to use in place of
  the raw survival time when imputing other covariates (White and Royston,
  2009); it matches `mice::nelsonaalen()` with no auxiliary variables and,
  when `aux` is supplied, estimates the cumulative hazard within strata
  (categorical `aux`) or from a Cox model (`method = "breslow"`).
  `ipcw()` returns subject-level inverse-probability-of-censoring weights
  `Delta_i / G(T_i-)` from a Kaplan-Meier censoring model, with `aux` support
  for covariate-dependent censoring (stratified Kaplan-Meier or `method =
  "cox"`), plus `type = "all"`, `stabilized`, and `truncate` options. Both
  accept the time and status columns either as strings or as unquoted
  symbols. The Cox paths use `survival` (already in `Suggests`).

# mimar 1.1.0

* The `densemlp` imputer now runs on `densemlp`'s native
  C++/RcppArmadillo dense MLP (no `torch`/`libtorch` dependency, faster to
  fit). `imputer = "densemlp"` fits/predicts through the standard chained
  loop as before, but `task` detection is finer-grained: `"regression"`,
  `"binary"`, and `"multiclass"` are distinguished natively, matching
  mimar's own per-variable task detection exactly, instead of the previous
  single `"classification"`.

# mimar 1.0.1

* Added `pool_panglm()`, pooling a list of `panglm` panel-data model fits
  (one per completed data set) across imputations using Rubin's rules,
  following the design of the existing `pool_glm()`/`pool_lm()` (printed
  coefficient table mirroring `summary.glm()`, `t value`/`z value` labelled
  by family, Barnard-Rubin corrected degrees of freedom from
  `df.residual()` when the `panglm` estimator provides one - `model =
  "pooling"`/`"within"` do; `model = "random"` does not compute a full
  likelihood and falls back to a normal reference distribution). Warns
  (does not error) if the fits being pooled disagree on `panglm`'s
  `model`/`effect`/`family` settings. `panglm` added to `Suggests` for the
  example/tests. `devtools::test()`: all pass, including a new
  `test-pool-panglm` case. `R CMD check --as-cran`: Status OK, 0 errors/0
  warnings.

# mimar 1.0.0

* Added `pool_coxph()`, `pool_glm()`, `pool_lm()`, `pool_survreg()`, and
  `pool_clogit()`, convenience helpers that pool a list of fitted models
  (one per completed data set) using Rubin's rules and print a coefficient
  table formatted like the corresponding base-R `summary()` output
  (`summary.coxph()`, `summary.glm()`, `summary.lm()`, `summary.survreg()`;
  `pool_clogit()` matches `pool_coxph()` since `clogit()` fits a stratified
  Cox model internally). Degrees of freedom use the Barnard and Rubin (1999)
  correction rather than the classic Rubin (1987) formula, which can diverge
  to implausibly large values when between-imputation variance is small
  relative to within-imputation variance; all five were cross-validated
  against `mice::pool()` to numerical precision on matched examples.

# mimar 0.9.0

* Added `densemlp` imputer, wrapping the `densemlp` package's dense
  multilayer perceptron as a standard fit/predict learner inside the chained
  imputation loop (numeric targets predict a point estimate, categorical
  targets draw from predicted class probabilities).
* Added `missknn` imputer, wrapping the `missknn` package's whole-table
  masked k-nearest-neighbor engine. Because `missknn` imputes all variables
  jointly in one pass rather than per-variable, selecting it bypasses the
  chained-equations loop entirely as a distinct single-shot strategy.
* Added `progress` argument to `impute()`, showing an elapsed/ETA progress
  bar over completed datasets via `functionals::fmap(pb = TRUE)`. Defaults to
  `TRUE` in interactive sessions when `verbose = FALSE`, and has no effect
  when `imputer = "missknn"`.
* Dropped the `tibble` dependency package-wide. All tabular results
  previously returned as tibbles (`describe()`, `impute()`, `complete()`,
  `evaluate()`, `pool()`, `imputer_registry()`, and friends) are now
  `data.table`s instead. Internal row-binding of trace/diagnostic/pooling
  data frames also moved from base `do.call(rbind, ...)` to
  `data.table::rbindlist()`, the shared backend behind `.rbind_or_empty()`.
  The engine internals (chained-equations loop, amputation) still operate on
  plain `data.frame`s to keep base-R subsetting semantics intact; only the
  user-facing return objects changed class.

# mimar 0.8.0

* Added `superlearner` and `sl` imputers. These construct a Super
  Learner-style ensemble by cross-validating candidate imputers on observed
  cells, assigning non-negative loss-based weights, and combining predictions
  inside the existing chained-imputation loop.
* Added `library`, `folds`, and `metalearner` hyperparameters for
  `superlearner`.
* Updated CRAN preparation files and vignette examples for the new release.

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
