
# mimar

`mimar` implements a compact chained-imputation workflow in R for
missing-data analysis, artificial amputation, native and learner-backed
single and multiple imputation, diagnostic evaluation, and
post-imputation pooling.

The package is built around a complete missing-data workflow: describe
the missingness, create benchmark amputations when needed, impute with
native or learner-backed update rules, inspect diagnostics, evaluate
recovered cells when truth is available, and pool post-fit quantities.
The goal is a concise grammar for the whole workflow, not a replacement
for every specialist feature in larger imputation systems.

The package owns the imputation loop. Every imputer, whether implemented
natively or backed by a learner package, is called the same way:

``` r
impute(data, imputer = "pmm", m = 5, maxit = 5, seed = 1)
impute(data, imputer = "rf", m = 5, seed = 1)
impute(data, imputer = "xgboost", m = 5, seed = 1)
```

There is no dependency on `funcml`. Learner-backed imputers call their
original packages directly, and those backend packages are hard
dependencies so users can run any registered imputer without manually
resolving learner installations.

## Installation

Install the released version from CRAN:

``` r
install.packages("mimar")
```

The CRAN package page is <https://CRAN.R-project.org/package=mimar>.

You can install the development version from GitHub:

``` r
install.packages("remotes")
remotes::install_github("ielbadisy/mimar")
```

Then load the package:

``` r
library(mimar)
```

## Quick use

For normal use, `impute()` is the only function you need. The input data
can contain `NA`, and the completed outputs returned by `complete()` do
not. Set `verbose = TRUE` when you want a concise progress log for the
chained imputation workflow.

``` r
i <- impute(a, imputer = "knn", m = 5, maxit = 5, seed = 1)
complete(i, 1)
complete(i, "all")
```

## Grammar

``` r
describe()
ampute()
imputer_registry()
imputer()
impute()
complete()
evaluate()
pool()
plot()
```

## Short example

``` r
library(mimar)

set.seed(1)
dat <- data.frame(
  age = rnorm(120, 50, 10),
  bmi = rnorm(120, 25, 4),
  sex = factor(sample(c("F", "M"), 120, TRUE)),
  group = factor(sample(c("A", "B", "C"), 120, TRUE)),
  smoker = sample(c(TRUE, FALSE), 120, TRUE)
)

a <- ampute(
  dat,
  prop = 0.25,
  mechanism = "MAR",
  target = c("bmi", "group"),
  by = c("age", "sex"),
  seed = 1
)

i <- impute(a, imputer = "knn", m = 5, maxit = 5, seed = 1, ncore = 2)
complete(i, 1)
```

    ##           age      bmi    sex  group smoker
    ##         <num>    <num> <fctr> <fctr> <lgcl>
    ##   1: 43.73546 22.97617      M      C  FALSE
    ##   2: 51.83643 30.37216      F      A  FALSE
    ##   3: 41.64371 24.14168      F      B   TRUE
    ##   4: 65.95281 24.28177      F      C   TRUE
    ##   5: 53.29508 24.59924      F      A  FALSE
    ##  ---                                       
    ## 116: 46.07192 13.44432      M      C   TRUE
    ## 117: 46.80007 27.24328      F      C   TRUE
    ## 118: 47.20887 24.76111      F      A  FALSE
    ## 119: 54.94188 29.38711      M      C  FALSE
    ## 120: 48.22670 24.97862      M      B  FALSE

``` r
summary(i)
```

    ## mimar imputation summary
    ##     rows columns n_imputations imputer maxit ncore stochastic
    ##    <int>   <int>         <int>  <char> <num> <int>     <lgcl>
    ## 1:   120       5             5     knn     5     2       TRUE
    ##    total_missing_before total_imputed remaining_missing variables_imputed
    ##                   <int>         <int>             <int>             <int>
    ## 1:                   53            53                 0                 2
    ## 
    ## Variables:
    ##    variable    type method n_missing_before prop_missing_before n_imputed
    ##      <char>  <char> <char>            <int>               <num>     <int>
    ## 1:      age numeric   none                0           0.0000000         0
    ## 2:      bmi numeric    knn               26           0.2166667        26
    ## 3:      sex  factor   none                0           0.0000000         0
    ## 4:    group  factor    knn               27           0.2250000        27
    ## 5:   smoker logical   none                0           0.0000000         0
    ##    prop_imputed remaining_missing between_imputation_sd
    ##           <num>             <int>                 <num>
    ## 1:            0                 0              0.000000
    ## 2:            1                 0              0.341076
    ## 3:            0                 0              0.000000
    ## 4:            1                 0              0.000000
    ## 5:            0                 0              0.000000

``` r
evaluate(i)
```

    ## mimar imputation evaluation
    ##    n_imputations imputer total_missing evaluated_cells
    ##            <int>  <char>         <int>           <int>
    ## 1:             5     knn            53              53

``` r
plot(i, type = "density")
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Imputers

Inspect available imputers with:

``` r
imputer_registry()
```

    ##          imputer implementation    package supports_numeric supports_binary
    ##           <char>         <char>     <char>           <lgcl>          <lgcl>
    ##  1:         mean          mimar   internal             TRUE            TRUE
    ##  2:       median          mimar   internal             TRUE            TRUE
    ##  3:         mode          mimar   internal             TRUE            TRUE
    ##  4:        naive          mimar   internal             TRUE            TRUE
    ##  5:         norm          mimar   internal             TRUE            TRUE
    ##  6:          pmm          mimar   internal             TRUE            TRUE
    ##  7:         spmm          mimar   internal             TRUE            TRUE
    ##  8:       logreg          mimar   internal             TRUE            TRUE
    ##  9:      polyreg          mimar   internal             TRUE            TRUE
    ## 10:           rf        wrapped     ranger             TRUE            TRUE
    ## 11:       ranger        wrapped     ranger             TRUE            TRUE
    ## 12:        rpart        wrapped      rpart             TRUE            TRUE
    ## 13:       nbayes        wrapped naivebayes             TRUE            TRUE
    ## 14:          svm        wrapped      e1071             TRUE            TRUE
    ## 15:         bart        wrapped       BART             TRUE            TRUE
    ## 16:       glmnet        wrapped     glmnet             TRUE            TRUE
    ## 17:          gbm        wrapped        gbm             TRUE            TRUE
    ## 18:      xgboost        wrapped    xgboost             TRUE            TRUE
    ## 19:          knn          mimar   internal             TRUE            TRUE
    ## 20:      hotdeck          mimar   internal             TRUE            TRUE
    ## 21:         famd        wrapped    missMDA             TRUE            TRUE
    ## 22: superlearner          mimar   internal             TRUE            TRUE
    ## 23:           sl          mimar   internal             TRUE            TRUE
    ## 24:     densemlp        wrapped   densemlp             TRUE            TRUE
    ## 25:      missknn        wrapped    missknn             TRUE            TRUE
    ##          imputer implementation    package supports_numeric supports_binary
    ##           <char>         <char>     <char>           <lgcl>          <lgcl>
    ##     supports_multiclass stochastic
    ##                  <lgcl>     <lgcl>
    ##  1:                TRUE      FALSE
    ##  2:                TRUE      FALSE
    ##  3:                TRUE      FALSE
    ##  4:                TRUE      FALSE
    ##  5:                TRUE       TRUE
    ##  6:                TRUE       TRUE
    ##  7:                TRUE       TRUE
    ##  8:                TRUE       TRUE
    ##  9:                TRUE       TRUE
    ## 10:                TRUE       TRUE
    ## 11:                TRUE       TRUE
    ## 12:                TRUE       TRUE
    ## 13:                TRUE       TRUE
    ## 14:                TRUE       TRUE
    ## 15:                TRUE       TRUE
    ## 16:                TRUE       TRUE
    ## 17:                TRUE       TRUE
    ## 18:                TRUE       TRUE
    ## 19:                TRUE       TRUE
    ## 20:                TRUE       TRUE
    ## 21:                TRUE       TRUE
    ## 22:                TRUE       TRUE
    ## 23:                TRUE       TRUE
    ## 24:                TRUE       TRUE
    ## 25:                TRUE       TRUE
    ##     supports_multiclass stochastic
    ##                  <lgcl>     <lgcl>
    ##                                                                              description
    ##                                                                                   <char>
    ##  1:                                                  Mean imputation for numeric targets
    ##  2:                                                Median imputation for numeric targets
    ##  3:                                              Mode imputation for categorical targets
    ##  4:                                                         Median/mode chained baseline
    ##  5:                                         Bayesian normal-style linear regression draw
    ##  6:                                                             Predictive mean matching
    ##  7:                                                 Single-step predictive mean matching
    ##  8:                                                      Binary logistic regression draw
    ##  9:                                     One-vs-rest multinomial logistic regression draw
    ## 10:                                               MissForest-style random forest imputer
    ## 11:                                                 Random forest imputer through ranger
    ## 12:                                                           Tree imputer through rpart
    ## 13:                                                                  Naive Bayes imputer
    ## 14:                                                       Support vector machine imputer
    ## 15:                                                                         BART imputer
    ## 16:                                          Penalized regression imputer through glmnet
    ## 17:                                                Gradient boosting imputer through gbm
    ## 18:                                        Gradient boosted tree imputer through xgboost
    ## 19:                                                       Nearest-neighbor donor imputer
    ## 20:                                                               Hot-deck donor imputer
    ## 21:                                                          FAMD-assisted donor imputer
    ## 22:                                 Cross-validated Super Learner-style ensemble imputer
    ## 23:                                                         Short alias for superlearner
    ## 24:                                 Dense multilayer perceptron imputer through densemlp
    ## 25: Whole-table masked k-nearest-neighbor imputer through missknn (single-shot strategy)
    ##                                                                              description
    ##                                                                                   <char>
    ##     available    status
    ##        <lgcl>    <char>
    ##  1:      TRUE available
    ##  2:      TRUE available
    ##  3:      TRUE available
    ##  4:      TRUE available
    ##  5:      TRUE available
    ##  6:      TRUE available
    ##  7:      TRUE available
    ##  8:      TRUE available
    ##  9:      TRUE available
    ## 10:      TRUE available
    ## 11:      TRUE available
    ## 12:      TRUE available
    ## 13:      TRUE available
    ## 14:      TRUE available
    ## 15:      TRUE available
    ## 16:      TRUE available
    ## 17:      TRUE available
    ## 18:      TRUE available
    ## 19:      TRUE available
    ## 20:      TRUE available
    ## 21:      TRUE available
    ## 22:      TRUE available
    ## 23:      TRUE available
    ## 24:      TRUE available
    ## 25:      TRUE available
    ##     available    status
    ##        <lgcl>    <char>

``` r
describe("imputers")
```

    ## mimar available imputers
    ##          imputer implementation    package supports_numeric supports_binary
    ##           <char>         <char>     <char>           <lgcl>          <lgcl>
    ##  1:         mean          mimar   internal             TRUE            TRUE
    ##  2:       median          mimar   internal             TRUE            TRUE
    ##  3:         mode          mimar   internal             TRUE            TRUE
    ##  4:        naive          mimar   internal             TRUE            TRUE
    ##  5:         norm          mimar   internal             TRUE            TRUE
    ##  6:          pmm          mimar   internal             TRUE            TRUE
    ##  7:         spmm          mimar   internal             TRUE            TRUE
    ##  8:       logreg          mimar   internal             TRUE            TRUE
    ##  9:      polyreg          mimar   internal             TRUE            TRUE
    ## 10:           rf        wrapped     ranger             TRUE            TRUE
    ## 11:       ranger        wrapped     ranger             TRUE            TRUE
    ## 12:        rpart        wrapped      rpart             TRUE            TRUE
    ## 13:       nbayes        wrapped naivebayes             TRUE            TRUE
    ## 14:          svm        wrapped      e1071             TRUE            TRUE
    ## 15:         bart        wrapped       BART             TRUE            TRUE
    ## 16:       glmnet        wrapped     glmnet             TRUE            TRUE
    ## 17:          gbm        wrapped        gbm             TRUE            TRUE
    ## 18:      xgboost        wrapped    xgboost             TRUE            TRUE
    ## 19:          knn          mimar   internal             TRUE            TRUE
    ## 20:      hotdeck          mimar   internal             TRUE            TRUE
    ## 21:         famd        wrapped    missMDA             TRUE            TRUE
    ## 22: superlearner          mimar   internal             TRUE            TRUE
    ## 23:           sl          mimar   internal             TRUE            TRUE
    ## 24:     densemlp        wrapped   densemlp             TRUE            TRUE
    ## 25:      missknn        wrapped    missknn             TRUE            TRUE
    ##          imputer implementation    package supports_numeric supports_binary
    ##           <char>         <char>     <char>           <lgcl>          <lgcl>
    ##     supports_multiclass stochastic
    ##                  <lgcl>     <lgcl>
    ##  1:                TRUE      FALSE
    ##  2:                TRUE      FALSE
    ##  3:                TRUE      FALSE
    ##  4:                TRUE      FALSE
    ##  5:                TRUE       TRUE
    ##  6:                TRUE       TRUE
    ##  7:                TRUE       TRUE
    ##  8:                TRUE       TRUE
    ##  9:                TRUE       TRUE
    ## 10:                TRUE       TRUE
    ## 11:                TRUE       TRUE
    ## 12:                TRUE       TRUE
    ## 13:                TRUE       TRUE
    ## 14:                TRUE       TRUE
    ## 15:                TRUE       TRUE
    ## 16:                TRUE       TRUE
    ## 17:                TRUE       TRUE
    ## 18:                TRUE       TRUE
    ## 19:                TRUE       TRUE
    ## 20:                TRUE       TRUE
    ## 21:                TRUE       TRUE
    ## 22:                TRUE       TRUE
    ## 23:                TRUE       TRUE
    ## 24:                TRUE       TRUE
    ## 25:                TRUE       TRUE
    ##     supports_multiclass stochastic
    ##                  <lgcl>     <lgcl>
    ##                                                                              description
    ##                                                                                   <char>
    ##  1:                                                  Mean imputation for numeric targets
    ##  2:                                                Median imputation for numeric targets
    ##  3:                                              Mode imputation for categorical targets
    ##  4:                                                         Median/mode chained baseline
    ##  5:                                         Bayesian normal-style linear regression draw
    ##  6:                                                             Predictive mean matching
    ##  7:                                                 Single-step predictive mean matching
    ##  8:                                                      Binary logistic regression draw
    ##  9:                                     One-vs-rest multinomial logistic regression draw
    ## 10:                                               MissForest-style random forest imputer
    ## 11:                                                 Random forest imputer through ranger
    ## 12:                                                           Tree imputer through rpart
    ## 13:                                                                  Naive Bayes imputer
    ## 14:                                                       Support vector machine imputer
    ## 15:                                                                         BART imputer
    ## 16:                                          Penalized regression imputer through glmnet
    ## 17:                                                Gradient boosting imputer through gbm
    ## 18:                                        Gradient boosted tree imputer through xgboost
    ## 19:                                                       Nearest-neighbor donor imputer
    ## 20:                                                               Hot-deck donor imputer
    ## 21:                                                          FAMD-assisted donor imputer
    ## 22:                                 Cross-validated Super Learner-style ensemble imputer
    ## 23:                                                         Short alias for superlearner
    ## 24:                                 Dense multilayer perceptron imputer through densemlp
    ## 25: Whole-table masked k-nearest-neighbor imputer through missknn (single-shot strategy)
    ##                                                                              description
    ##                                                                                   <char>
    ##     available    status
    ##        <lgcl>    <char>
    ##  1:      TRUE available
    ##  2:      TRUE available
    ##  3:      TRUE available
    ##  4:      TRUE available
    ##  5:      TRUE available
    ##  6:      TRUE available
    ##  7:      TRUE available
    ##  8:      TRUE available
    ##  9:      TRUE available
    ## 10:      TRUE available
    ## 11:      TRUE available
    ## 12:      TRUE available
    ## 13:      TRUE available
    ## 14:      TRUE available
    ## 15:      TRUE available
    ## 16:      TRUE available
    ## 17:      TRUE available
    ## 18:      TRUE available
    ## 19:      TRUE available
    ## 20:      TRUE available
    ## 21:      TRUE available
    ## 22:      TRUE available
    ## 23:      TRUE available
    ## 24:      TRUE available
    ## 25:      TRUE available
    ##     available    status
    ##        <lgcl>    <char>

Core native imputers:

- `mean`, `median`, `mode`
- `naive`: median/mode chained baseline
- `norm`: linear normal draw
- `pmm`, `spmm`: predictive mean matching
- `logreg`: binary logistic regression draw
- `polyreg`: one-vs-rest multinomial draw
- `knn`: nearest-neighbor donor imputation
- `hotdeck`: stochastic donor imputation

Learner-backed imputers:

- `rf`: MissForest-style chained random forest imputer through `ranger`
- `ranger`: random forest through `ranger`
- `rpart`: tree imputer through `rpart`
- `nbayes`: naive Bayes through `naivebayes`
- `svm`: support vector machine through `e1071`
- `bart`: Bayesian additive regression trees through `BART`
- `glmnet`: penalized regression through `glmnet`
- `gbm`: gradient boosting through `gbm`
- `xgboost`: gradient boosted trees through `xgboost`
- `famd`: FAMD-assisted donor imputation through `missMDA`
- `superlearner`, `sl`: cross-validated Super Learner-style ensemble
  imputer

Imputer names are strict: use the names shown by `imputer_registry()`.
Learner-backed imputers are applied as requested to numeric, binary, and
multiclass targets; `mimar` does not silently swap them for another
imputer inside benchmark runs.

## Parallel imputation

The `ncore` argument runs independent completed datasets in parallel.
The parallel boundary is the outer imputation index: each completed
dataset gets a deterministic seed offset, so a fixed `seed`, `m`,
`maxit`, and imputer remain reproducible.

``` r
i <- impute(a, imputer = "knn", m = 5, maxit = 5, seed = 1, ncore = 2)
```

Use `ncore = 1` for sequential execution, small examples, and the most
conservative behavior in constrained environments.

## Tuning imputers

Learner-backed imputers expose their hyperparameters through `imputer()`
or directly through `...` in `impute()`. Donor-based imputers use the
explicit `donors` argument.

``` r
rf_spec <- imputer("rf", num.trees = 500)
xgb_spec <- imputer("xgboost", nrounds = 100, max_depth = 3)

i1 <- impute(a, imputer = rf_spec, m = 5, maxit = 5, seed = 1)
i2 <- impute(a, imputer = "xgboost", m = 5, maxit = 5, seed = 1,
             nrounds = 100, max_depth = 3)
i3 <- impute(a, imputer = "knn", m = 5, maxit = 5, seed = 1, donors = 10)
```

The same hyperparameter set is reused across all incomplete variables
that a given imputer supports, which keeps the full chained-imputation
pipeline reproducible and easy to tune.

## Super Learner imputation

`superlearner` combines candidate imputers by cross-validating them on
observed cells, assigning non-negative loss-based weights, and using the
weighted ensemble inside the chained-imputation loop.

``` r
sl <- imputer(
  "superlearner",
  library = c("pmm", "knn", "rpart"),
  folds = 5,
  metalearner = "inverse_loss"
)

i_sl <- impute(a, imputer = sl, m = 5, maxit = 5, seed = 1)
```

The short alias `imputer = "sl"` is equivalent to
`imputer = "superlearner"`.

## Diagnostic Plots

`plot()` methods return `ggplot` objects. For `mimar_imputation`
objects, the main diagnostic types are:

``` r
plot(i)                                      # imputed cell counts
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
plot(i, type = "missing")                   # observed/imputed cell map
```

![](README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
plot(i, type = "trace", statistic = "mean") # convergence-screening trace
```

![](README_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
plot(i, type = "density", variable = "bmi") # line-only density overlays
```

![](README_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
plot(i, type = "boxplot", variable = "bmi") # observed vs imputation 1:m
```

![](README_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->

``` r
plot(i, type = "strip", variable = "bmi")   # individual values by imputation
```

![](README_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->

Formula diagnostics are available for bivariate and categorical checks:

``` r
plot(i, type = "xy", formula = bmi ~ age | sex)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
plot(i, type = "proportion", formula = group ~ sex)
```

![](README_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

For `type = "xy"`, formulas use `y ~ x` or `y ~ x | group`. For
`type = "proportion"`, formulas use `categorical_variable ~ strata`.
Density diagnostics use line-only overlays so several imputations remain
visible rather than obscuring each other with filled areas.

## Chained Imputation Model

Let `X` be an `n x p` data frame and let `R_ij = 1` when cell `(i, j)`
is missing. For each incomplete variable `X_j`:

- `O_j = {i : R_ij = 0}` are the observed rows
- `M_j = {i : R_ij = 1}` are the missing rows

At each chained update, `mimar` fits an imputer-specific model from the
observed rows and then predicts the missing rows from the current
completed data. In compact form:

``` text
fit model on X_-j, O_j -> X_j, O_j
update X_j, M_j using the fitted model
```

Multiple imputation repeats the same chained procedure `m` times with
controlled seeds, bootstrap samples of observed rows, and stochastic
prediction where supported.

Learner-backed imputers are practical stochastic update rules inside
this chained workflow. They can improve predictive recovery, but users
should still inspect trace, distribution, categorical-proportion, and
downstream sensitivity diagnostics rather than assuming every learner
automatically supplies proper multiple-imputation uncertainty for every
analysis.

## Algorithm

``` text
Input: X, R, h, m, T
Initialize: X~(0) <- init(X)
For k = 1,...,m:
  X~_k(0) <- X~(0)
  For t = 1,...,T:
    For each incomplete variable j:
      B_j <- bootstrap sample of O_j
      fit h on X~_k, B_j, -j and X_Bj,j
      update missing rows M_j using the fitted model
      restore observed rows O_j to their original values
Return: {X~_1(T), ..., X~_m(T)}
```

## Evaluation

When imputation is run on an `ampute()` object, `evaluate()` uses the
retained truth and scores only artificially removed cells. Numeric
recovery reports RMSE, MAE, bias, and correlation. Categorical recovery
reports accuracy and balanced accuracy.

## Pooling

`pool()` combines post-fit quantities estimated separately in each
completed dataset. The statistical target is the quantity itself, not a
data frame. A quantity can be a scalar, coefficient vector,
covariance-aware parameter vector, matrix of survival probabilities, or
a scalar metric. Data frames are accepted only as a tidy adapter for
scalar model output.

For survival-probability matrices, `pool_survmat()` applies the
complementary log-log transform internally, pools on that scale, and
back-transforms the result.

For scalar quantities with complete-data variance estimates, `pool()`
applies Rubin-style pooling:

``` text
Q_bar = mean(Q_k)
U_bar = mean(U_k)
B     = sample variance of Q_k
T     = U_bar + (1 + 1/m) * B
```

``` r
results <- data.frame(
  term = rep(c("age", "bmi"), each = 3),
  estimate = c(0.10, 0.11, 0.09, 0.30, 0.32, 0.29),
  std.error = c(0.04, 0.05, 0.04, 0.08, 0.09, 0.08),
  imputation = rep(1:3, times = 2)
)

pool(results)
```

    ## mimar pooled results
    ##      term  estimate  std.error statistic       df     p.value   conf.low
    ##    <char>     <num>      <num>     <num>    <num>       <num>      <num>
    ## 1:    age 0.1000000 0.04509250  2.217664  465.125 0.027060256 0.01138975
    ## 2:    bmi 0.3033333 0.08530989  3.555664 1094.452 0.000393134 0.13594390
    ##    conf.high     m within_variance between_variance total_variance
    ##        <num> <int>           <num>            <num>          <num>
    ## 1: 0.1886102     3     0.001900000     0.0001000000    0.002033333
    ## 2: 0.4707228     3     0.006966667     0.0002333333    0.007277778
    ##    relative_increase_variance   rule
    ##                         <num> <char>
    ## 1:                 0.07017544  rubin
    ## 2:                 0.04465710  rubin

Direct quantity inputs are preferred when available:

``` r
pool(c(0.10, 0.11, 0.09), std.error = c(0.04, 0.05, 0.04), name = "age")
```

    ## mimar pooled results
    ##      term estimate std.error statistic      df    p.value   conf.low conf.high
    ##    <char>    <num>     <num>     <num>   <num>      <num>      <num>     <num>
    ## 1:    age      0.1 0.0450925  2.217664 465.125 0.02706026 0.01138975 0.1886102
    ##        m within_variance between_variance total_variance
    ##    <int>           <num>            <num>          <num>
    ## 1:     3          0.0019            1e-04    0.002033333
    ##    relative_increase_variance   rule
    ##                         <num> <char>
    ## 1:                 0.07017544  rubin

``` r
betas <- list(
  c(age = 0.10, bmi = 0.30),
  c(age = 0.11, bmi = 0.32),
  c(age = 0.09, bmi = 0.29)
)
covariances <- list(
  diag(c(0.04, 0.08)^2),
  diag(c(0.05, 0.09)^2),
  diag(c(0.04, 0.08)^2)
)
pool(betas, covariance = covariances)
```

    ## mimar pooled results
    ##      term  estimate  std.error statistic       df     p.value   conf.low
    ##    <char>     <num>      <num>     <num>    <num>       <num>      <num>
    ## 1:    age 0.1000000 0.04509250  2.217664  465.125 0.027060256 0.01138975
    ## 2:    bmi 0.3033333 0.08530989  3.555664 1094.452 0.000393134 0.13594390
    ##    conf.high     m within_variance between_variance total_variance
    ##        <num> <int>           <num>            <num>          <num>
    ## 1: 0.1886102     3     0.001900000     0.0001000000    0.002033333
    ## 2: 0.4707228     3     0.006966667     0.0002333333    0.007277778
    ##    relative_increase_variance   rule
    ##                         <num> <char>
    ## 1:                 0.07017544  rubin
    ## 2:                 0.04465710  rubin

When no reliable complete-data variance is supplied, as is common for
some performance metrics, `pool()` reports robust summaries by default:
median, interquartile range, and range across imputations.

## Installation notes

Learner backends are hard dependencies. Installing `mimar` installs the
packages needed by the registered learner-backed imputers, including
`ranger`, `rpart`, `naivebayes`, `e1071`, `BART`, `glmnet`, `gbm`,
`xgboost`, and `missMDA`.
