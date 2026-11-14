---
name: interplate-vc-qc
description: Inter-plate vehicle-control QC with the zfbehavior R package -- reduce each plate to a vehicle-control reference movement pattern, compare plates by PCA with normal and t confidence ellipses, flag outlier plates, and record the user's keep/exclude decision for each flagged plate (interactive HTML report plus a QC PDF), from a complete dataset-import-qc run, in a dedicated run folder. Use after import QC and before endpoint calculation.
---

# interplate-vc-qc

Module 2 (Raw-response visualization and inter-plate VC QC) skill of the
zfbehavior workflow (Truong et al. 2026, Section 2.6.2.3). Inter-plate
vehicle-control QC: it reads the combined dataset of one `complete`
`dataset-import-qc` run, flags plates whose vehicle-control movement pattern
is an outlier among all plates, and records a human keep/exclude decision for
each flagged plate. It changes no data; its output is a plate exclusion list
for downstream skills.

Read `../CONVENTIONS.md` (run folders, naming, manifest, upstream rules)
before starting. Locate the installed skill directory with:

```r
system.file("skills", "interplate-vc-qc", package = "zfbehavior")
```

## Inputs and outputs

- Upstream: exactly one `analysis/dataset-import-qc/<run>/` with manifest
  `status: complete` and `well_contract: single_larva_wells_only` (so every
  plate has kept vehicle-control wells).
- Run folder: `<project>/analysis/interplate-vc-qc/<run_label>/`
- Outputs (`outputs/`):
  - `interplate-vc-qc__<run>__qc.pdf` -- page 1 PCA scatter with normal
    (dashed) and t (solid) ellipses, page 2 reference-pattern overlay, then
    vehicle-control well traces of each flagged plate (2 x 2 per page,
    alphabetical).
  - `interplate-vc-qc__<run>__reference-pattern.rds`, `__pca.rds`,
    `__outliers.rds` (both methods, conf level, flag method).
  - `interplate-vc-qc__<run>__plate-decisions.rds` -- one row per plate:
    PCs, distances, outlier flags (both methods), `flagged`, `decision`,
    `reason`, `excluded`. **Written only when status is `complete`.**
  - `interplate-vc-qc__<run>__manifest.yml` -- incl. `cohort_note`,
    `method`, `flagged_plates`, `excluded_plates` (with reasons),
    `kept_flagged_plates`.
- Report: `interplate-vc-qc.html` -- interactive (plotly) PCA and overlay
  plots with plate identity on hover, interactive (reactable) tables; static
  fallback if those packages are not installed.

## Package functions used (exported only)

`interplate_qc_input_cols()`, `raw_plot_input_cols()`,
`calculate_plate_reference_pattern()`, `fit_plate_pca()`,
`detect_plate_pca_outliers()`, `plot_plate_pca_ellipse()`,
`plot_plate_reference_overlay()`, `count_wells_by_dose()`,
`plot_raw_responses_pages()`.

## Workflow

### Step 1 -- Check the upstream run (STOP if not complete)

Read `analysis/dataset-import-qc/<run>/outputs/*__manifest.yml`. If `status`
is not `complete` or `well_contract` is not `single_larva_wells_only`, stop
and tell the user which run blocks and why. The template enforces the same.

### Step 2 -- Gather parameters (STOP: ask the user)

Ask for, and do not guess:

1. Run label (usually the batch ID).
2. `cohort_note`: all plates of the upstream run are compared with each
   other; confirm they form one comparable cohort (same lab, assay, batch)
   and record why. If not, the plates must be split into separate import
   runs first.
3. Assay phase windows (`segments`, shading only) and response axis label.
4. Plate identity columns (`title_cols`), e.g. chemical name + chemical ID +
   plate ID.
5. Optional (defaults in parentheses): `flag_method` (ellipse_normal, the
   published method; ellipse_t is robust and flags more plates),
   `conf_level` (0.95), `summary_type` (median, published).

If `analysis/interplate-vc-qc/<run_label>/` already exists, ask before
reusing it.

### Step 3 -- Create the run folder

```r
project_root <- normalizePath("<path/to/project>")
run_label <- "<run_label>"
skill_dir <- system.file("skills", "interplate-vc-qc", package = "zfbehavior")
run_dir <- file.path(project_root, "analysis", "interplate-vc-qc", run_label)
stopifnot(!dir.exists(run_dir))   # never overwrite an existing run without asking
dir.create(run_dir, recursive = TRUE)
file.copy(file.path(skill_dir, "templates", c("interplate-vc-qc.qmd", "interplate-vc-qc.yml")), run_dir)
```

Fill in `interplate-vc-qc.yml` with the confirmed values (`run_label` must
equal the folder name; keep `project_root: "../../.."`). Do not edit the
`.qmd`.

### Step 4 -- First render

```bash
cd <project>/analysis/interplate-vc-qc/<run_label>
quarto render interplate-vc-qc.qmd --execute-params interplate-vc-qc.yml
```

Read `outputs/interplate-vc-qc__<run>__manifest.yml`:
`status: complete` (no plate flagged) -> Step 6; `needs_review` -> Step 5.
A render error means a blocked upstream or an invalid parameter (the message
says which).

### Step 5 -- Decide each flagged plate (STOP: user decides)

Show the user, for each flagged plate: its identity, distances under both
methods, and where to look -- the interactive PCA/overlay plots in the HTML
report and the plate's vehicle-control traces in the QC PDF. Also mention
plates flagged only by the other method (shown in the table) as context.
For each flagged plate the user decides:

- `exclude_plates` -- with the reason (e.g. "VC traces erratic, D1 response
  absent"); the plate is dropped by downstream skills,
- `keep_plates` -- with the reason (e.g. "VC pattern plausible on
  inspection").

The user may also exclude unflagged plates (with a reason). Record the
decisions in the yml and re-render (Step 4) until `complete`.

### Step 6 -- Report

Tell the user: status, PC1/PC2 variance, plates flagged by each method,
excluded plates with reasons, and the paths of the QC PDF and
`plate-decisions.rds`. Downstream skills (e.g. endpoints) list both the
import run and this run under `upstream:`.

## Guardrails (never do)

- Never run on an upstream run that is not `complete` or lacks the
  single-larva well contract.
- Never decide a flagged plate yourself; never exclude a plate without the
  user's decision and reason in the yml.
- Never offer or use `detect_plate_pca_outliers(method = "mahalanobis_npc")`
  (unvalidated; it did not reproduce the published result).
- Never filter wells or plates, or edit upstream outputs; this skill only
  records decisions.
- Never change the published defaults (median reference, PC1/PC2, 95%
  ellipses) without the user's explicit choice in the yml.
- Never write analysis code that bypasses the template; if something the
  template cannot express is needed, tell the user.
