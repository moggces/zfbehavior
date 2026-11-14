---
name: behavior-endpoints-bmc
description: Calculate benchmark concentrations (BMC, Curvep) of zebrafish behavioral endpoints (total distance in both directions, distance change/shift, movement similarity) with the zfbehavior R package from a complete behavior-endpoints run (optionally dropping plates excluded by intraplate-vc-qc runs), per plate or pooled per chemical, with a BMR per endpoint type from the user, a reviewed Rcurvep knee-point estimate, or the vehicle-control median + n x MAD, and plot dose-response curves with BMC labels and a BMC activity overview, in a dedicated run folder.
---

# behavior-endpoints-bmc

Module 5 (Curvep-based BMC modeling of behavioral and developmental toxicity
endpoints) skill of the zfbehavior workflow. Behavioral BMC: it reads the
endpoint table of one `complete` `behavior-endpoints` run (plus optional
`intraplate-vc-qc` runs on it, whose excluded plates are dropped), fits Curvep
curves to the normalized responses (`endpoint_value_norm`), and writes the BMR
table, the BMC results, four dose-response PDFs with BMC labels, and an
activity PDF.

Read `../CONVENTIONS.md` (run folders, naming, manifest, upstream rules)
before starting. Locate the installed skill directory with:

```r
system.file("skills", "behavior-endpoints-bmc", package = "zfbehavior")
```

## Inputs and outputs

- Upstream: exactly one `analysis/behavior-endpoints/<run>/` with manifest
  `status: complete`, plus zero or more `complete`
  `analysis/intraplate-vc-qc/<run>/` made on that same endpoints run. For
  `table: pooled` the endpoints run should use `layout: by_chemical_pooled`
  (it keeps the chemical columns; its `chemical_col` is the default).
- Run folder: `<project>/analysis/behavior-endpoints-bmc/<run_label>/`
- Outputs (`outputs/`, prefix `behavior-endpoints-bmc__<run>__`):
  - `bmr-table.rds` -- one row per endpoint x direction (x curve for scope
    plate/chemical): `bmr`, `source`, `scope`, `plate_medians`, `n_mad`,
    `n_values`, `vc_median`, `vc_mad`, `reason`, `RNGE`.
  - `bmc-results.rds` -- one row per curve x endpoint x direction: labels,
    the Curvep summary (`POD_med`, `hit_confidence`, ...), `hit`
    (`hit_confidence > hit_threshold`), `plotted` (the direction shown in the
    plots: the higher hit confidence), `BMC`/`BMCL`/`BMCU` in the curve's
    unit.
  - `curvep-raw-<type>.rds` -- the full `run_bmc_curvep()` result per
    endpoint type, with its settings key (reused on re-render).
  - With `source: estimate`: `bmr-sweep-<type>.rds` (reused),
    `bmr-estimate-<type>.rds` (`$outcome`: `bmr_ori`, `bmr_exp`, `qc` per
    endpoint x direction), `bmr-estimate-<type>.pdf`.
  - `dose-response-{total-distance,distance-change,distance-shift,movement-similarity}.pdf`
    -- one panel per curve, BMC/score labels on hits (total distance: arrow
    for the direction).
  - `activity.rds`, `activity.pdf` -- BMC activity overview (same layout as
    the ANOVA activity plot); hits only, labelled with the BMC.
    `activity_exclude_endpoints` (default `L1@down_auc`, `L2@down_auc`,
    historically very noisy) are fitted and kept in `bmc-results.rds`,
    `activity.rds` (`in_activity_pdf = FALSE`) and the dose-response PDFs,
    but not drawn in the activity PDF; `[]` adds them back.
  - `manifest.yml`
- Report: `behavior-endpoints-bmc.html`.

## BMR per endpoint type (yml `bmr:`)

| type | default source | direction (`rnge`) | estimate range (`trsh`) |
|---|---|---|---|
| total_distance | estimate (published, Hsieh et al. 2018) | both | 5-95 by 5 |
| distance_change_shift | control_mad, scope global, n_mad 3 (historical) | up | from the data |
| movement_similarity | estimate (historical: reviewed to 30 pearson / 25 spearman) | down | 5-95 by 5 |

Every type accepts every source:

- `user`: `bmr` = one number, or a map `endpoint: number`; for `rnge: both`
  a map `{up: ..., down: ...}` of those. `reason` required.
- `estimate`: the first render runs `estimate_bmr_curvep()` and stops at
  `needs_review`; the user reviews it and fills in `bmr`/`reason`.
  `trsh: null` derives each endpoint's range from the data (0 to the 95th
  percentile of the absolute treated response, 19 steps) -- use it for
  responses not on a percent scale (e.g. distance change/shift AUC).
- `control_mad`: `estimate_bmr_from_control()`, BMR = median + `n_mad` x MAD
  of the vehicle-control values chosen by `scope`:
  - `global` -- all VC wells of all plates; one BMR per endpoint (historical)
  - `plate` -- each plate's VC wells (`table: per_plate`)
  - `chemical` -- VC wells of the chemical's plates (`table: pooled`)
  - `plate_medians: true` -- each plate's VC wells reduced to their median
    first (the plate denominator); distance_change_shift only, >= 3 plates
    per BMR, not with scope plate. Not historical: for comparison runs.

## Package functions used (exported only)

`estimate_bmr_from_control()`, `estimate_bmr_curvep()`, `run_bmc_curvep()`,
`plot_behavior_dose_response_pages()`, `plot_behavior_bmc_activity_pages()`
(plus Rcurvep's own `plot()` on the saved estimate).

## Workflow

### Step 1 -- Check the upstream runs (STOP if not complete)

Read the manifests of the behavior-endpoints run and of any intraplate-vc-qc
runs on it. If a `status` is not `complete`, stop and tell the user. Ask
whether intraplate-vc-qc runs should be applied.

### Step 2 -- Gather parameters (STOP: ask the user)

Ask for, and do not guess:

1. Run label (e.g. `260416-hist`).
2. Table: `per_plate` (one curve per plate) or `pooled` (one per chemical).
3. Endpoint types to fit and the similarity metric(s).
4. Per endpoint type: BMR source and its settings (table above). Offer the
   defaults; for `user`, the value(s) and the reason.
5. Endpoints left out of the activity PDF (default L1@down_auc and
   L2@down_auc, reason: historically very noisy) -- confirm or set `[]`.
6. Optional (defaults in parentheses): `n_samples` (1000), `seed` (2026),
   estimate samples/seed (10 / 2022), `hit_threshold` (0.5), `workers` (1;
   about 15 s per curve sequentially at 1000 samples), label columns, page
   settings.

If `analysis/behavior-endpoints-bmc/<run_label>/` already exists, ask before
reusing it.

### Step 3 -- Create the run folder

```r
project_root <- normalizePath("<path/to/project>")
run_label <- "<run_label>"
skill_dir <- system.file("skills", "behavior-endpoints-bmc", package = "zfbehavior")
run_dir <- file.path(project_root, "analysis", "behavior-endpoints-bmc", run_label)
stopifnot(!dir.exists(run_dir))   # never overwrite an existing run without asking
dir.create(run_dir, recursive = TRUE)
file.copy(file.path(skill_dir, "templates", c("behavior-endpoints-bmc.qmd", "behavior-endpoints-bmc.yml")), run_dir)
```

Fill in the yml with the confirmed values (`run_label` = folder name;
`project_root: "../../.."`; `upstream` relative to the project root). Do not
edit the `.qmd`.

### Step 4 -- Render

```bash
cd <project>/analysis/behavior-endpoints-bmc/<run_label>
quarto render behavior-endpoints-bmc.qmd --execute-params behavior-endpoints-bmc.yml
```

A render error means a blocked upstream or an invalid parameter (the message
says which). An md5 drift warning means an upstream table changed after its
run completed: stop and ask the user.

### Step 5 -- BMR review (only for `source: estimate`; STOP: user decides)

The first render stops at `needs_review` (no BMC for the pending types).
Show the user the estimate per type (report section "BMR",
`bmr-estimate-<type>.pdf`, `bmr-estimate-<type>.rds` `$outcome`: `bmr_ori`,
`bmr_exp`, `qc`, per endpoint and direction) and ask which BMR to use and
why. Never pick it yourself. Put the user's `bmr` and `reason` in the yml
and re-render: the saved sweep is reused.

### Step 6 -- Report (STOP: user inspects the PDFs)

Tell the user: status, the BMR per type and its source, curves and hits per
endpoint and direction, whether Curvep was reused or computed, labels that
do not fit the activity panels (if any), and the paths of `bmc-results.rds`
and the PDFs.

## Guardrails (never do)

- Never run on an upstream run that is not `complete`.
- Never choose or change a BMR without the user; never use a knee-point
  estimate unreviewed.
- Never change `activity_exclude_endpoints` without the user; it only hides
  results from the activity PDF, never from the results tables.
- Never use `plate_medians` for total distance or movement similarity
  (their plate medians are 0 by normalization).
- Never re-curate wells or drop dose groups or plates here (plates only via
  intraplate-vc-qc runs).
- Never compare or pool BMCs across concentration units.
- Never edit upstream outputs, saved Curvep objects, or another run folder;
  never write analysis code that bypasses the template.
