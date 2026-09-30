# `xplainfi` Feature Importance Methods Benchmark

Online supplement and reproducibility materials for the paper introducing the [`xplainfi`](https://github.com/mlr-org/xplainfi) R package.
This repository contains all code to reproduce the empirical benchmarks presented in the paper, using the [`batchtools`](https://github.com/mlr-org/batchtools) framework for managing the experiment registries.

There are two separate benchmarks, each with their own configuration, setup, execution, and analysis scripts:

1. **Importance benchmark** — compares feature importance scores from `xplainfi` against reference implementations across multiple data-generating processes and a real-world dataset.
2. **Runtime benchmark** — measures computational cost of each method across varying sample sizes, feature dimensions, and algorithm parameters.

## Prerequisites

- **R >= 4.5** (see `rproject.toml`)
- **Python == 3.12** (see `pyproject.toml` `.python-version`)
- **[rv](https://a2-ai.github.io/rv-docs/)** — R dependency manager (handles all R packages including `xplainfi`)
- **[uv](https://docs.astral.sh/uv/)** — Python dependency manager (handles Python implementations)

## Getting Started

### 1. Install R packages

All R dependencies are declared in `rproject.toml` and managed by `rv`.
This includes `xplainfi` itself (pinned to a specific version via git tag), as well as all learners, samplers, and reference implementation packages.

```bash
rv sync
```

This creates a project-local R library under `rv/` with all required packages.

The `torch` R package additionally requires a one-time installation of libtorch.
After `rv sync`, open an R session and run:

```r
library(torch)
# Follow the on-screen prompt to install libtorch
```

This is required for the MLP learner (`mlr3torch`). Without it, jobs using `learner_type = "mlp"` will fail.

### 2. Install Python packages

Python dependencies are declared in `pyproject.toml` and managed by `uv`.
These are only needed for the Python-based reference implementations (`fippy`, `sage-importance`).

```bash
uv sync
```

This creates a `.venv/` directory with exact package versions locked in `uv.lock`.
The `.Rprofile` configures `reticulate` to automatically use this virtual environment when R calls Python code.

### 3. Set up the batchtools registries

Each benchmark has a setup script that creates a batchtools experiment registry, registers all problems and algorithms, and defines the full factorial experiment design:

```bash
# Importance benchmark
Rscript setup-batchtools-importance.R

# Runtime benchmark
Rscript setup-batchtools-runtime.R
```

Each script creates a registry directory under `registries/` (e.g., `registries/importance/`, `registries/runtime/`).
If a registry already exists at the target path, the script loads it instead of creating a new one.

### 4. Configure batchtools for your compute environment

batchtools uses a `batchtools.conf.R` file in the project root to configure how jobs are executed.
Create this file to match your compute environment (Slurm, SGE, SSH, local multicore, etc.).
See the [batchtools documentation](https://mllg.github.io/batchtools/articles/batchtools.html) for available cluster functions.

Example for Slurm:

```r
# batchtools.conf.R
cluster.functions <- makeClusterFunctionsSlurm(template = "slurm-simple")
```

If no `batchtools.conf.R` exists, the run scripts fall back to `makeClusterFunctionsSSH()` on localhost, which is another option for multicore execution on the current machine.
Adjust the number of parallel jobs according to your hardware.
Each job will consume one thread.

### 5. Run experiments

The `run-experiment-*.R` scripts load the registry for the specific benchmark and submit jobs.
These are designed as interactive scripts — review and adapt them to your needs.

**Important**: R jobs and Python jobs must be submitted in **separate R sessions**.
Loading R `torch` and Python PyTorch in the same session causes conflicts due to incompatible libtorch versions.

The scripts start by submitting only the first replication (`repl == 1`) as a smoke test.
After verifying those results, uncomment the remaining submission calls for the full benchmark.

```r
# In an R session — R jobs only:
source("run-experiment-importance.R")

# In a separate R session — Python jobs only:
# (uncomment the Python submission block in the script)
source("run-experiment-importance.R")
```

### 6. Collect results

After jobs complete, the collect scripts read the batchtools registries, aggregate raw results, and produce the processed RDS files that the analysis Rmd documents expect:

```bash
Rscript collect-results-importance.R
Rscript collect-results-runtime.R
```

This produces:
- `results/importance/importances.rds` — processed importance scores with metadata
- `results/runtime/runtime.rds` — processed runtime measurements with metadata

### 7. Render analysis documents

The `A01-results-importance.Rmd` and `A02-results-runtime.Rmd` documents read the collected results and produce the figures and tables for the paper:

```bash
Rscript -e 'rmarkdown::render("A01-results-importance.Rmd")'
Rscript -e 'rmarkdown::render("A02-results-runtime.Rmd")'
```

Figures are saved to `figures/importance/` and `figures/runtime/` respectively, while other HTML assets (JS, CSS) go to the default `*_files/` directories.

## Project Structure

```
.
├── batchtools.conf.R              # batchtools cluster configuration (user-created, gitignored)
├── config-importance.R            # Experiment parameters for importance benchmark
├── config-runtime.R               # Experiment parameters for runtime benchmark
├── setup-common.R                 # Shared setup: dependency checks, library loading
├── setup-batchtools-importance.R  # Creates importance registry, registers problems/algorithms
├── setup-batchtools-runtime.R     # Creates runtime registry, registers problems/algorithms
├── run-experiment-importance.R    # Submit importance benchmark jobs
├── run-experiment-runtime.R       # Submit runtime benchmark jobs
├── collect-results-importance.R  # Aggregate registry results into RDS files
├── collect-results-runtime.R     # Aggregate registry results into RDS files
├── A01-results-importance.Rmd    # Analysis and figures for importance benchmark
├── A02-results-runtime.Rmd       # Analysis and figures for runtime benchmark
├── R/
│   ├── helpers.R                  # Shared utilities (create_learner, create_sampler, etc.)
│   ├── helpers-python.R           # Python/reticulate integration helpers
│   ├── problems.R                 # batchtools problem definitions (DGPs + real data)
│   ├── algorithms.R               # batchtools algorithm definitions (xplainfi + references)
│   └── clean-results.R            # Result post-processing functions
├── rproject.toml                  # R dependency declarations (managed by rv)
├── pyproject.toml                 # Python dependency declarations (managed by uv)
├── .Rprofile                      # R session configuration (rv activation, reticulate setup)
├── .python-version                # Python version pin (3.12)
├── figures/
│   ├── importance/                # Plots generated by A01-results-importance.Rmd
│   └── runtime/                   # Plots generated by A02-results-runtime.Rmd
├── registries/                    # batchtools experiment registries (created by setup scripts)
└── results/                       # Aggregated benchmark results
```

## Configuration

Each benchmark has its own configuration file (`config-importance.R`, `config-runtime.R`) that defines a `conf` list with all experiment parameters (sample sizes, learner types, number of replications, SAGE settings, etc.).
The setup scripts source the corresponding config file and use it to build the factorial experiment design.

## Algorithms

### xplainfi methods

| Algorithm           | Description                                                |
| ------------------- | ---------------------------------------------------------- |
| **PFI**             | Permutation Feature Importance (marginal sampling)         |
| **CFI**             | Conditional Feature Importance (with conditional samplers) |
| **MarginalSAGE**    | SAGE with marginal sampling                                |
| **ConditionalSAGE** | SAGE with conditional sampling                             |

### Reference implementations

| Algorithm                 | Source                                        | Description                                 |
| ------------------------- | --------------------------------------------- | ------------------------------------------- |
| **PFI_iml**               | [iml](https://cran.r-project.org/package=iml) | PFI via `iml::FeatureImp`                   |
| **PFI_vip**               | [vip](https://cran.r-project.org/package=vip) | PFI via `vip::vi_permute`                   |
| **PFI_fippy**             | [fippy](https://github.com/gcskoenig/fippy)   | PFI (Python, simple sampler)                |
| **CFI_fippy**             | [fippy](https://github.com/gcskoenig/fippy)   | CFI (Python, Gaussian sampler)              |
| **MarginalSAGE_fippy**    | [fippy](https://github.com/gcskoenig/fippy)   | Marginal SAGE (Python)                      |
| **ConditionalSAGE_fippy** | [fippy](https://github.com/gcskoenig/fippy)   | Conditional SAGE (Python, Gaussian sampler) |
| **MarginalSAGE_sage**     | [sage](https://github.com/iancovert/sage)     | KernelSAGE estimator (Python)               |

## Problems (Tasks / Data-Generating Processes)

### Importance benchmark

The importance benchmark uses multiple synthetic DGPs and one real-world dataset:

- **friedman1** — Friedman's nonlinear regression (10 features, 5 relevant)
- **ewald** — Ewald DGP with known feature importance structure
- **interactions** — DGP with feature interaction effects
- **correlated** — DGP with correlated features (varying correlation strength)
- **independent** — Independent features baseline
- **confounded** — Confounded feature relationships
- **mediated** — Mediated feature effects
- **bike_sharing** — Real-world bike sharing dataset

### Runtime benchmark

- **peak** — Peak DGP with varying `n_samples` and `n_features` for systematic runtime scaling analysis. 
- Importance scores are ignored.

## Troubleshooting

### Python environment conflicts

The `.Rprofile` forces `reticulate` to use the project-local `.venv`.
On HPC systems, tools like **spack** can set `PYTHONPATH` to incompatible Python packages.
The `.Rprofile` unsets `PYTHONPATH` and explicitly sets `RETICULATE_PYTHON` to prevent this.

If you encounter import errors (e.g., numpy), check for conflicting environment variables:

```bash
env | grep -i python
```

### Threading

The `.Rprofile` sets `OMP_NUM_THREADS=1`, `MKL_NUM_THREADS=1`, and `ranger.num.threads=1` to prevent nested parallelization issues when running many jobs concurrently via batchtools.
