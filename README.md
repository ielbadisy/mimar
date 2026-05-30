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

## Algorithm

\[
\begin{array}{ll}
\textbf{Input:} & X,\ R,\ h,\ m,\ T.\\
\textbf{Initialize:} & \tilde X^{(0)} \leftarrow \operatorname{init}(X).\\
\textbf{For } k=1,\ldots,m: &
  \tilde X_k^{(0)} \leftarrow \tilde X^{(0)}.\\
& \textbf{For } t=1,\ldots,T: \\
& \quad \textbf{For each incomplete variable } j: \\
& \quad\quad B_j \leftarrow \mathcal{B}(O_j).\\
& \quad\quad \hat f_{jk}^{(t)} \leftarrow
  \operatorname{fit}_h(\tilde X_{k,B_j,-j}^{(t-1)}, X_{B_j,j}).\\
& \quad\quad \tilde X_{k,M_j,j}^{(t)} \leftarrow
  \operatorname{restore}_j\{g_h(\hat f_{jk}^{(t)}, \tilde X_{k,M_j,-j}^{(t-1)})\}.\\
\textbf{Return:} & \{\tilde X_1^{(T)},\ldots,\tilde X_m^{(T)}\}.
\end{array}
\]

## Evaluation and Pooling

When imputation is run on an `ampute()` object, `evaluate()` uses the retained
truth and scores only artificially removed cells. Numeric recovery reports RMSE,
MAE, bias, and correlation. Categorical recovery reports accuracy and balanced
accuracy.

`pool()` combines post-fit quantities estimated separately in each completed
dataset. The statistical target is the quantity itself, not a data frame. A
quantity can be a scalar, coefficient vector, covariance-aware parameter vector,
matrix of survival probabilities, or a scalar metric. Data frames are accepted
only as a tidy adapter for scalar model output.

For a scalar quantity with complete-data variance estimates, `pool()` applies
Rubin-style pooling:

\[
\bar Q = \frac{1}{m}\sum_{k=1}^m Q_k,\quad
\bar U = \frac{1}{m}\sum_{k=1}^m U_k,\quad
B = \frac{1}{m-1}\sum_{k=1}^m (Q_k-\bar Q)^2,
\]

\[
T = \bar U + \left(1 + \frac{1}{m}\right)B.
\]

```r
results <- data.frame(
  term = rep(c("age", "bmi"), each = 3),
  estimate = c(0.10, 0.11, 0.09, 0.30, 0.32, 0.29),
  std.error = c(0.04, 0.05, 0.04, 0.08, 0.09, 0.08),
  imputation = rep(1:3, times = 2)
)

pool(results)
```

Direct quantity inputs are preferred when available:

```r
pool(c(0.10, 0.11, 0.09), std.error = c(0.04, 0.05, 0.04), name = "age")

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

When no reliable complete-data variance is supplied, as is common for some
performance metrics, `pool()` reports robust summaries by default: median,
interquartile range, and range across imputations.

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
