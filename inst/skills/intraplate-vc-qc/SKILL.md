---
name: intraplate-vc-qc
description: Per-plate vehicle-control QC of zebrafish behavioral endpoints with the zfbehavior R package -- compare each plate's vehicle-control baseline (median per endpoint) with all other plates by a Tukey fence (degree 3), flag plates with low vehicle-control movement similarity (< 0.4), show a per-plate consensus of flagged endpoints, interactive distributions and a QC PDF with the flagged plates' vehicle-control traces, and record the user's keep/exclude decision per flagged plate, from a complete behavior-endpoints run, in a dedicated run folder. Use after behavior-endpoints and before behavioral statistics or BMC.
---

# intraplate-vc-qc

Module 3 (Endpoint calculation, normalization, and intra-plate VC QC) skill of
the zfbehavior workflow (Truong et al. 2026, Section 2.6.2.2). Intra-plate
(per-plate) vehicle-control QC: it reads the endpoints of one `complete`
`behavior-endpoints` run, flags plates whose vehicle-control larvae behaved
unusually, and records a human keep/exclude decision for each flagged plate.
It changes no data; its output is a plate exclusion list for downstream
skills.

Read `../CONVENTIONS.md` (run folders, naming, manifest, upstream rules)
before starting. Locate the installed skill directory with:

```r
system.file("skills", "intraplate-vc-qc", package = "zfbehavior")
```

## What is checked

Each plate's **baseline** for an endpoint is `denom` in the endpoints table:
the median of the plate's vehicle-control wells, against which the plate's
endpoints are normalized (log10(distance + 1) for total distance, AUC for
distance change/shift, raw 0-1 similarity for movement similarity).

1. **Baseline outliers** -- per endpoint, a Tukey fence over all plates'
   baselines, `[Q1 - k*IQR, Q3 + k*IQR]`, k = `outlier_degree` (default 3,
   far outliers). A plate outside it is flagged for that endpoint (`low` or
   `high`).
2. **Low movement similarity** (empirical rule, Truong et al. 2026) -- a plate
   is flagged when its vehicle-control median movement similarity is below
   `similarity_threshold` (default 0.4; similarity is on a 0-1 scale and
   below 0.4 the plate's vehicle-control larvae move very inconsistently).

A plate is **flagged** if either check flags it for any endpoint. Decisions
are per **whole plate**. The **consensus table** counts, per plate, how many
endpoints flagged it (and of which type), so a plate flagged by many or
related endpoints stands out from a plate flagged by one.

## Inputs and outputs

- Upstream: exactly one `analysis/behavior-endpoints/<run>/` with
  `status: complete`. Its import run (for the vehicle-control traces) and the
  plates already excluded by its own upstream QC runs (e.g.
  `interplate-vc-qc`; listed as context, not re-checked) come from its
  manifest. Whether to use an `interplate-vc-qc` run is therefore decided
  when the behavior-endpoints run is made.
- Run folder: `<project>/analysis/intraplate-vc-qc/<run_label>/`
- Outputs (`outputs/`):
  - `intraplate-vc-qc__<run>__baseline-outliers.rds` -- one row per plate x
    endpoint: baseline, quartiles, fences, `direction`, `is_outlier`,
    `below_similarity_threshold`, plate label.
  - `__distribution.rds` -- per endpoint: distribution of plate baselines
    (n, mean, median, sd, mad, q1, q3, fences, n_outliers, min, max,
    n_below_similarity_threshold).
  - `__plate-flags.rds` -- the consensus table: one row per flagged plate.
  - `__qc.pdf` -- one baseline-distribution page per endpoint type (flagged
    plates red and labelled), then the vehicle-control well traces of each
    flagged plate (2 x 2 per page, consensus order).
  - `__plate-decisions.rds` -- one row per plate: `flagged`,
    `n_flagged_endpoints`, `low_similarity`, `decision`, `reason`,
    `excluded`. **Written only when status is `complete`.**
  - `__manifest.yml` -- incl. `method`, `flagged_plates`, `excluded_plates`
    (with reasons), `kept_flagged_plates`, `plates_excluded_upstream`.
- Report: `intraplate-vc-qc.html` -- interactive (plotly) distributions with
  plate identity on hover and interactive (reactable) tables; static
  fallback if those packages are not installed.

## Package functions used (exported only)

`detect_behavior_vc_baseline_outliers()`,
`summarize_behavior_vc_distribution()`, `plot_behavior_vc_baseline()`,
`count_wells_by_dose()`, `plot_raw_responses_pages()`.

## Workflow

### Step 1 -- Check the upstream run (STOP if not complete)

Read `analysis/behavior-endpoints/<run>/outputs/*__manifest.yml`. If `status`
is not `complete`, stop and tell the user which run blocks and why. Tell the
user which QC runs (if any) that endpoints run already applied. The template
enforces the same.

### Step 2 -- Gather parameters (STOP: ask the user)

Ask for, and do not guess:

1. Run label (usually the same as the behavior-endpoints run).
2. `response_label` for the trace pages (e.g. "distance moved").
3. Plate identity columns (`title_cols`), e.g.
   `[CHEMICAL_NAME, chemical_id, plate_id]`.
4. Optional (defaults in parentheses): `outlier_degree` (3; 1.5 flags many
   more plates), `similarity_threshold` (0.4; `null` to skip the rule).

If `analysis/intraplate-vc-qc/<run_label>/` already exists, ask before
reusing it.

### Step 3 -- Create the run folder

```r
project_root <- normalizePath("<path/to/project>")
run_label <- "<run_label>"
skill_dir <- system.file("skills", "intraplate-vc-qc", package = "zfbehavior")
run_dir <- file.path(project_root, "analysis", "intraplate-vc-qc", run_label)
stopifnot(!dir.exists(run_dir))   # never overwrite an existing run without asking
dir.create(run_dir, recursive = TRUE)
file.copy(file.path(skill_dir, "templates", c("intraplate-vc-qc.qmd", "intraplate-vc-qc.yml")), run_dir)
```

Fill in `intraplate-vc-qc.yml` with the confirmed values (`run_label` must
equal the folder name; keep `project_root: "../../.."`). Do not edit the
`.qmd`.

### Step 4 -- First render

```bash
cd <project>/analysis/intraplate-vc-qc/<run_label>
quarto render intraplate-vc-qc.qmd --execute-params intraplate-vc-qc.yml
```

Read `outputs/intraplate-vc-qc__<run>__manifest.yml`:
`status: complete` (no plate flagged) -> Step 6; `needs_review` -> Step 5.
A render error means a blocked upstream or an invalid parameter (the message
says which).

### Step 5 -- Decide each flagged plate (STOP: user decides)

Show the user the consensus table (`plate-flags.rds`): for each flagged
plate, its identity, how many endpoints flagged it and which (with
direction), and any low-similarity value. Point to the interactive
distributions in the HTML report and the plate's vehicle-control traces in
the QC PDF. For each flagged plate the user decides:

- `exclude_plates` -- with the reason (e.g. "VC larvae barely move in all
  phases; 6/22 endpoints flagged"); the plate is dropped by downstream
  skills,
- `keep_plates` -- with the reason (e.g. "only D1 flagged; VC traces
  plausible").

The user may also exclude unflagged plates (with a reason). Record the
decisions in the yml and re-render (Step 4) until `complete`.

### Step 6 -- Report

Tell the user: status, number of plates and endpoints checked, flagged
plates (by baseline outlier and by low similarity) with their consensus
counts, excluded plates with reasons, and the paths of the QC PDF and
`plate-decisions.rds`. Downstream skills (behavioral statistics, BMC) list
both the behavior-endpoints run and this run under `upstream:`.

## Guardrails (never do)

- Never run on an upstream run that is not `complete`.
- Never decide a flagged plate yourself; never exclude a plate without the
  user's decision and reason in the yml.
- Never change `outlier_degree` or `similarity_threshold` from the defaults
  without the user's explicit choice in the yml.
- Never filter wells or plates, recompute endpoints, or edit upstream
  outputs; this skill only records decisions.
- Never write analysis code that bypasses the template; if something the
  template cannot express is needed, tell the user.
