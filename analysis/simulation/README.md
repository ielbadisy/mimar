# Simulation launcher

Run the JSS benchmark from the repository root:

```bash
Rscript analysis/simulation/run_simulation.R
```

Defaults:
- all available imputers
- MNAR only
- 5 Monte Carlo replicates
- `m = 5`
- `maxit = 5`

Optional environment overrides:

```bash
MIMAR_SIM_REP=5 MIMAR_SIM_N=140 MIMAR_SIM_WORKERS=2 Rscript analysis/simulation/run_simulation.R
MIMAR_SIM_METHODS='mean,median,naive,pmm,rf,knn' Rscript analysis/simulation/run_simulation.R
MIMAR_SIM_DGPS='linear,nonlinear' Rscript analysis/simulation/run_simulation.R
```

The script writes results to `analysis/simulation/results/` and the report
template is in `analysis/simulation/report.Rmd`.
