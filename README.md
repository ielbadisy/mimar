# mimar

`mimar` provides a compact grammar for missing-data workflows in R:

```r
describe()
ampute()
impute()
evaluate()
pool()
plot()
```

The public imputation API has one engine selector: `imputer`. Modeling verbs are not exported.

## Examples

```r
library(mimar)

set.seed(1)
dat <- data.frame(
  age = rnorm(100, 50, 10),
  bmi = rnorm(100, 25, 4),
  sex = factor(sample(c("F", "M"), 100, TRUE)),
  group = factor(sample(c("A", "B", "C"), 100, TRUE))
)

d <- describe(dat)
plot(d)

a <- ampute(dat, prop = 0.25, mechanism = "MAR",
            target = c("bmi", "group"), by = c("age", "sex"), seed = 1)
plot(a)

i1 <- impute(a, m = 1, imputer = "meanmode", seed = 1)
i5 <- impute(a, m = 5, imputer = "meanmode", seed = 1)

e <- evaluate(i5)
plot(e)
```

Use MCAR, MAR, or MNAR artificial missingness:

```r
ampute(dat, prop = 0.10, mechanism = "MCAR", seed = 1)
ampute(dat, prop = 0.20, mechanism = "MAR", target = "bmi", by = c("age", "sex"))
ampute(dat, prop = 0.20, mechanism = "MNAR", target = "bmi", direction = "right")
```

Multiple imputation uses the same compact form:

```r
impute(dat, m = 5, imputer = "meanmode")
```

Chained-equation imputation uses standalone imputer learners:

```r
impute(dat, m = 5, imputer = "mi")
```

ML learners use the same imputer contract and are provided through the required `funcml` dependency:

```r
describe("imputers")
impute(dat, m = 5, imputer = "randomForest")
```

External model results can be pooled without `mimar` fitting models:

```r
external_results <- data.frame(
  term = rep(c("age", "bmi"), each = 5),
  estimate = c(0.10, 0.11, 0.09, 0.12, 0.10, 0.30, 0.32, 0.29, 0.31, 0.33),
  std.error = c(0.04, 0.05, 0.04, 0.05, 0.04, 0.08, 0.09, 0.08, 0.09, 0.08),
  imputation = rep(1:5, times = 2)
)

p <- pool(external_results)
plot(p)
```

## Imputer learners

`imputer()` constructs a standalone learner. `fit()` trains it on observed rows and `predict()` imputes missing rows. Classical learners are implemented in `mimar`; ML learners such as `randomForest`, `knn`, `xgboost`, `svm`, `bart`, `naive_bayes`, `rpart`, and `glmnet` use the same contract through `funcml`.

## Limitations

The baseline `meanmode` imputer supports numeric, integer, logical, factor, character, and Date variables. Other variable types are left unchanged with a warning. Real-world missingness mechanisms cannot be proven from observed data alone; `ampute()` is for simulation, benchmarking, teaching, and sensitivity analysis.
