---
name: devtox-endpoints-bmc
description: Calculate benchmark concentrations (BMC, Curvep) of the zebrafish developmental toxicity endpoint (percent of larvae dead or malformed) with the zfbehavior R package from a complete devtox-endpoints run, per plate or pooled per chemical, using a user-given BMR or a reviewed Rcurvep knee-point BMR estimate, and plot dose-response curves with BMC labels, in a dedicated run folder.
---

# devtox-endpoints-bmc

Module 5 (Curvep-based BMC modeling of behavioral and developmental toxicity
endpoints) skill of the zfbehavior workflow. Developmental toxicity (devtox)
BMC: it reads the endpoint table of one `complete` `devtox-endpoints` run,
fits Curvep curves to the dose-group counts (`n_affected` of `N`), and writes
the BMC results, the full Curvep result, and a new dose-response PDF with BMC
labels.

Read `../CONVENTIONS.md` (run folders, naming, manifest, upstream rules)
before starting. Locate the installed skill directory with:

```r
system.file("skills", "devtox-endpoints-bmc", package = "zfbehavior")
```

## Inputs and outputs

- Upstream: exactly one `analysis/devtox-endpoints/<run>/` with manifest
  `status: complete`.
- Run folder: `<project>/analysis/devtox-endpoints-bmc/<run_label>/`
- Outputs (`outputs/`):
  - `devtox-endpoints-bmc__<run>__bmc-results.rds` -- one row per curve
    (plate, or chemical when pooled): id/title columns, the Curvep summary
    (`POD_med`, `POD_cil`, `POD_ciu`, `hit_confidence`, ...), `hit`
    (`hit_confidence > hit_threshold`), and `BMC`/`BMCL`/`BMCU` in the
    dose group's unit (`concentration_unit`, uM or ug/mL).
  - `devtox-endpoints-bmc__<run>__curvep-raw.rds` -- the full
    `run_bmc_curvep()` result (`$bmc`: raw Rcurvep objects + act summary)
    with the settings key it was computed with (`$key`).
  - `devtox-endpoints-bmc__<run>__dose-response-bmc.pdf` -- one panel per
    curve, VC labeled `VC`, BMC/score labels on hits only.
  - With `bmr_source: estimate`: `bmr-sweep.rds` (Curvep run across the BMR
    range, with its key), `bmr-estimate.rds` (knee-point estimate,
    `$outcome` has `bmr_ori`/`bmr_exp`/`qc`), `bmr-estimate.pdf`.
  - `devtox-endpoints-bmc__<run>__manifest.yml`
- Report: `devtox-endpoints-bmc.html`.

Concentrations are fitted as `log10(concentration / 1e6)` (log10 molar for
uM; the same formula is applied to ug/mL plates), and the BMC is
back-transformed with `10^POD * 1e6`, so it is in the plate's own unit.

**Saved Curvep runs are reused.** `curvep-raw.rds` and `bmr-sweep.rds` are
kept between re-renders and reused when the input table (md5) and the
Curvep settings (BMR or BMR range, `rnge`, sample count, seed) are
unchanged; otherwise they are recomputed. Plot or threshold changes
re-render in seconds. The manifest's `curvep_runs` says `reused` or
`computed`.

## Package functions used (exported only)

`estimate_bmr_curvep()`, `run_bmc_curvep()`,
`plot_devtox_dose_response_pages()` (plus Rcurvep's own
`estimate_dataset_bmr()` / `plot()` on the saved estimate).

## Workflow

### Step 1 -- Check the upstream run (STOP if not complete)

Read `analysis/devtox-endpoints/<run>/outputs/*__manifest.yml`. If `status` is
not `complete`, stop and tell the user. For `table: pooled` the upstream run
must have `layout: by_chemical_pooled` (it then has `endpoints-pooled.rds`).

### Step 2 -- Gather parameters (STOP: ask the user)

Ask for, and do not guess:

1. Run label (e.g. `260416-bmr30`).
2. Table: `per_plate` (one BMC per plate) or `pooled` (one per chemical).
3. BMR source:
   - `user`: the BMR in percent affected and the reason. This project used
     **BMR 30** for this endpoint in phases 1 and 2 -- offer it, but the user
     confirms it.
   - `estimate`: run Rcurvep's knee-point estimate first; the user chooses
     the BMR after reviewing it (Step 5).
4. Optional (defaults in parentheses): `n_samples` (1000), `seed` (2022),
   `hit_threshold` (0.5; BMC labels for `hit_confidence` above it),
   `workers` (1; >1 runs Curvep in parallel via `future`, ~15 s per curve
   sequentially), estimate range (TRSH 5-95 by 5, 100 samples), panel
   titles (`title_cols`), `title_wrap_width` (40), page grid (1 x 5).

If `analysis/devtox-endpoints-bmc/<run_label>/` already exists, ask before
reusing it.

### Step 3 -- Create the run folder

```r
project_root <- normalizePath("<path/to/project>")
run_label <- "<run_label>"
skill_dir <- system.file("skills", "devtox-endpoints-bmc", package = "zfbehavior")
run_dir <- file.path(project_root, "analysis", "devtox-endpoints-bmc", run_label)
stopifnot(!dir.exists(run_dir))   # never overwrite an existing run without asking
dir.create(run_dir, recursive = TRUE)
file.copy(file.path(skill_dir, "templates", c("devtox-endpoints-bmc.qmd", "devtox-endpoints-bmc.yml")), run_dir)
```

Fill in the yml with the confirmed values (`run_label` = folder name;
`project_root: "../../.."`; `upstream` relative to the project root). Do not
edit the `.qmd`.

### Step 4 -- Render

```bash
cd <project>/analysis/devtox-endpoints-bmc/<run_label>
quarto render devtox-endpoints-bmc.qmd --execute-params devtox-endpoints-bmc.yml
```

A render error means a blocked upstream or an invalid parameter (the message
says which). An md5 drift warning means the upstream table changed after its
run completed: stop and ask the user.

### Step 5 -- BMR review (only for `bmr_source: estimate`; STOP: user decides)

The first render stops at `needs_review`. Show the user the knee-point
estimate (report section "BMR", `bmr-estimate.pdf`, `bmr-estimate.rds`
`$outcome`: `bmr_ori`, `bmr_exp`, `qc`) and ask which BMR to use and why.
Never pick it yourself. Put the user's `bmr` and `bmr_reason` in the yml and
re-render: the saved sweep is reused and the BMC is calculated.

### Step 6 -- Report (STOP: user inspects the PDF)

Tell the user: status, BMR and reason, curves and hits, the most potent hits
(lowest BMC, with unit), whether Curvep was reused or computed, and the
paths of `bmc-results.rds` and the PDF.

## Guardrails (never do)

- Never run on an upstream run that is not `complete`.
- Never choose or change the BMR without the user; never use a knee-point
  estimate as the BMR unreviewed.
- Never re-curate wells or drop dose groups or plates here.
- Never compare or pool BMCs across concentration units.
- Never edit upstream outputs, saved Curvep objects, or another run folder;
  never write analysis code that bypasses the template.
