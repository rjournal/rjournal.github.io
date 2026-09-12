# Metafrontier Replication Package

Replicates all quantitative claims from:

> Enstad (2026). "metafrontier: Unified metafrontier analysis for efficiency and productivity in R." *The R Journal*.

## Quick start

### Option A: From R/RStudio

Open R with the `replication/` folder as the working directory (or `setwd()` to it), then:

```r
source("00-setup-renv.R")   # once — creates isolated renv library
source("01-replicate.R")    # runs all 10 targets (~30 min)
```

### Option B: From command line
```
cd <path-to>/replication
Rscript 00-setup-renv.R   # once - creates isolated renv library
Rscript 01-replicate.R    # runs all 10 targets (~30 min)
```

Requires R (>= 4.0) with `Rscript` on the PATH.

## What is replicated

| Target | Paper section | Description | Oracle |
|--------|--------------|-------------|--------|
| 1 | §4.1–4.5 | All code examples (simulate, det/stoch/DEA MF, identity, poolability) | Exact numeric match (tol 0.005) |
| 2 | §4.6 | Real-data example (utility from sfaR) | TGR means within 0.005 |
| 3 | §6.1–6.3 | Monte Carlo Table 5 (500 reps, baseline) | Bias/SD within MC tolerance |
| 4 | §6.4 | Small-sample MC (30 firms/group) | TGR rank corr ≈ 0.869 ± 0.03 |
| 5 | Table 4 | Murphy–Topel SE ratios | Ratios within 0.05–0.10 |
| 6 | Table 3 | Bootstrap CIs for TGR | Group mean TGRs within 0.01 |
| 7 | Monte Carlo evidence: efficiency recovery | Scenario A efficiency recovery (500 reps, 3 estimators: mean TE/TE*/TGR + Spearman rho) | Printed to log (oracle values to be frozen after first verified run) |
| 8 | Monte Carlo evidence: efficiency recovery | Scenario B efficiency recovery (group-specific slopes, correlated inputs) | Printed to log (oracle values to be frozen after first verified run) |
| 9 | Monte Carlo evidence: parameter recovery | LP vs QP objective: max abs TGR difference (50 reps) | Printed to log (oracle values to be frozen after first verified run) |
| 10 | Monte Carlo evidence: robustness | Unbalanced groups (200 vs 20 firms) | Printed to log (oracle values to be frozen after first verified run) |

## Outputs

- `replication-log.txt` — full console log with [PASS]/[FAIL] for each check
- `replication-results.rds` — structured R object with all results
- `renv/` — isolated package library (created by `00-setup-renv.R`)
- `renv.lock` — reproducible snapshot of package versions

## Requirements

- R ≥ 4.0.0
- Internet connection (for initial package installation)
- ~30 minutes runtime
