# zfbehavior

<!-- badges: start -->
[![R-CMD-check](https://github.com/moggces/zfbehavior/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/moggces/zfbehavior/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

zfbehavior analyzes zebrafish larval **light-dark transition test (LDTT)**
tracking data and **developmental toxicity**, from the vendor's tracking
files to hit calls and benchmark concentrations (BMC). It ships **Agent
Skills**: step-by-step guides that a coding agent (or a person) follows to
run one analysis step, asking you for every scientific choice and rendering
a Quarto report with a record of the run.

## Modules and skills

| # | Module | Skills |
|---|---|---|
| 1 | Data import and consistency checks | `dataset-import-qc` |
| 2 | Raw-response visualization and inter-plate VC QC | `raw-response-plots`, `interplate-vc-qc` |
| 3 | Endpoint calculation, normalization, and intra-plate VC QC | `behavior-endpoints`, `intraplate-vc-qc`, `devtox-endpoints` |
| 4 | ANOVA/repeated-measures ANOVA hit calling | `behavior-endpoints-anova` |
| 5 | Curvep-based BMC modeling of behavioral and developmental toxicity endpoints | `behavior-endpoints-bmc`, `devtox-endpoints-bmc` |

VC = vehicle control. Behavioral endpoints: Total Distance, Distance Change,
Distance Shift and Movement Similarity, per light/dark phase.

## Installation

```r
# install.packages("remotes")
remotes::install_github("moggces/zfbehavior")
```

This installs the dependencies from CRAN, including
[Rcurvep](https://cran.r-project.org/package=Rcurvep). The skills render
their reports with [Quarto](https://quarto.org) (>= 1.4), which is installed
separately.

## Getting started

With an agent: the skills are installed with the package.

```r
system.file("skills", package = "zfbehavior")
```

Point your agent at a skill's `SKILL.md` (start with
`dataset-import-qc/SKILL.md` and the shared `CONVENTIONS.md`), or link the
folder into your agent's skill directory. Each run gets its own folder,
`analysis/<skill>/<run>/`, with a parameter file, a report and a manifest.

In R: the functions can be used directly.

```r
library(zfbehavior)
raw <- readRDS(system.file("extdata", "zf_behavior_example.rds", package = "zfbehavior"))
?module1   # overview of module 1; also ?module2 ... ?module5
```

Read more:

- `vignette("getting-started", package = "zfbehavior")`: using the Agent
  Skills.
- `vignette("methods", package = "zfbehavior")`: the methods of each module,
  with a worked example on the shipped data.

## Citation

If you use zfbehavior, please cite the methods it implements:

- Hsieh J-H et al. (2019). Application of benchmark concentration (BMC)
  analysis on zebrafish data: a new perspective for quantifying toxicity in
  alternative animal models. *Toxicological Sciences* 167(1):92-104.
  <https://doi.org/10.1093/toxsci/kfy258>
- Konrad KS et al. (2026). Power calculations for larval zebrafish in
  light-dark transition test for developmental neurotoxicity.
  *NeuroToxicology* 112:103371. <https://doi.org/10.1016/j.neuro.2025.103371>
- Truong L et al. (2026). Zebrafish behavioral assessment as a first-tier
  whole-organism NAM for developmental neurotoxicity: a multi-laboratory
  evaluation. *NeuroToxicology* 116:103504.
  <https://doi.org/10.1016/j.neuro.2026.103504>

## License

MIT (see `LICENSE.md`).
