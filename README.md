# mimar

`mimar` is a compact R package for missing-data description, artificial
missingness, chained single and multiple imputation, imputation evaluation,
pooling, and diagnostic plotting.

The package owns the imputation loop. Every imputer, whether implemented
natively or backed by an optional learner package, is called the same way:

```r
impute(data, imputer = "pmm", m = 5, maxit = 5, seed = 1)
impute(data, imputer = "rf", m = 5, seed = 1)
impute(data, imputer = "xgboost", m = 5, seed = 1)
```

There is no dependency on `funcml`. Optional learner packages are used directly
only when the corresponding imputer is requested.

## Grammar

```r
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

## Quick Example

```r
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

i <- impute(a, imputer = "naive", m = 5, maxit = 5, seed = 1)
complete(i, 1)
summary(i)
evaluate(i)
plot(i, type = "density")
```

## Imputers

Inspect available imputers with:

```r
imputer_registry()
describe("imputers")
```

Core native imputers:

- `mean`, `median`, `mode`
- `naive`: median/mode chained baseline
- `norm`: linear normal draw
- `pmm`, `spmm`: predictive mean matching
- `logreg`: binary logistic regression draw
- `polyreg`: one-vs-rest multinomial draw
- `knn`: nearest-neighbor donor imputation
- `hotdeck`: stochastic donor imputation

Optional learner-backed imputers:

- `rf`: MissForest-style chained random forest imputer through `ranger`
- `ranger`: random forest through `ranger`
- `rpart`: tree imputer through `rpart`
- `naive_bayes`: naive Bayes through `naivebayes`
- `svm`: support vector machine through `e1071`
- `bart`: Bayesian additive regression trees through `BART`
- `glmnet`: penalized regression through `glmnet`
- `gbm`: gradient boosting through `gbm`
- `xgboost`: gradient boosted trees through `xgboost`
- `famd`: FAMD-assisted donor imputation through `missMDA`

Imputer names are strict: use the names shown by `imputer_registry()`.

## Chained Imputation Model

Let \(X\) be an \(n \times p\) data frame and let \(R_{ij}=1\) if cell
\((i,j)\) is missing. For an incomplete variable \(X_j\), define observed and
missing row sets:

\[
O_j = \{i: R_{ij}=0\}, \qquad M_j = \{i: R_{ij}=1\}.
\]

At each chained update, `mimar` fits an imputer-specific model

\[
\hat f_j: X_{-j,O_j} \rightarrow X_{j,O_j}
\]

and then updates

\[
\tilde X_{j,M_j} \leftarrow \hat f_j(\tilde X_{-j,M_j}).
\]

Multiple imputation is produced by repeating the same chained procedure
\(m\) times with controlled seeds, bootstrap samples of observed rows, and
stochastic prediction where supported.

## Pseudo Algorithm

```text
Input: data X with missingness mask R, imputer name h, imputations m, iterations T

Initialize missing cells with a median/mode baseline.

For k = 1,...,m:
  set seed
  start from initialized data
  For t = 1,...,T:
    For each incomplete variable j:
      choose method h_j from the requested imputer name
      bootstrap observed rows O_j
      fit h_j on completed predictors X_-j and observed target X_j
      predict rows M_j
      restore predicted values to the original storage type
      update X_j[M_j]
  store completed dataset k

Return a mimar_imputation object.
```

## Evaluation and Pooling

When imputation is run on an `ampute()` object, `evaluate()` uses the retained
truth and scores only artificially removed cells. Numeric recovery reports RMSE,
MAE, bias, and correlation. Categorical recovery reports accuracy and balanced
accuracy.

`pool()` combines external estimates or metrics across completed datasets. For
model estimates it applies Rubin-style pooling:

\[
\bar Q = \frac{1}{m}\sum_{k=1}^m Q_k,\quad
\bar U = \frac{1}{m}\sum_{k=1}^m U_k,\quad
B = \frac{1}{m-1}\sum_{k=1}^m (Q_k-\bar Q)^2,
\]

\[
T = \bar U + \left(1 + \frac{1}{m}\right)B.
\]

## Installation Notes

The base package installs without learner backends beyond base R dependencies.
Optional packages are needed only for their imputers. For example:

```r
install.packages("ranger")   # for imputer = "rf" or "ranger"
install.packages("glmnet")   # for imputer = "glmnet"
install.packages("xgboost")  # for imputer = "xgboost"
```

If an optional package is absent, `mimar` gives a direct message naming the
required package.
