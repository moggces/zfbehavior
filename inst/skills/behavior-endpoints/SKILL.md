---
name: behavior-endpoints
description: Calculate zebrafish behavioral endpoints (total distance, distance change, distance shift per assay phase; movement similarity across phases), normalized per plate against the vehicle control, with the zfbehavior R package from a complete dataset-import-qc run (optionally dropping plates excluded by QC runs), and plot them as four dose-response PDFs per plate or per chemical pooled across plates, in a dedicated run folder. Use before behavioral statistics or BMC.
---

# behavior-endpoints

Module 3 (Endpoint calculation, normalization, and intra-plate VC QC) skill of
the zfbehavior workflow. Behavioral endpoints: it reads the combined dataset
of one `complete` `dataset-import-qc` run and writes one long endpoint table
plus four dose-response PDFs. Vehicle-control comparisons between plates/wells
are not part of it (`intraplate-vc-qc`).

Read `../CONVENTIONS.md` (run folders, naming, manifest, upstream rules)
before starting. Locate the installed skill directory with:

```r
system.file("skills", "behavior-endpoints", package = "zfbehavior")
```

## Endpoints

Calculated per kept well (`keep_well == TRUE`; missing distance values set
to 0), then normalized per plate against the plate's vehicle-control median:

| Group (PDF) | Endpoint names | Per | Default normalization |
|---|---|---|---|
| total distance | `L1`, `D1`, ... | phase segment | `log_diff`: (log10(x+1) - VC) x 100 |
| distance change | `<seg>@both_auc` | phase segment | `none` |
| distance shift | `<seg>@down_auc`, `<seg>@up_auc` | phase segment | `none` |
| movement similarity | `<window>_<metric>`, e.g. `LD_spearman` | similarity window | `ratio`: x / VC x 100 - 100 |

Distance change/shift are areas between the well's trace and the plate's
vehicle-control median trace (both = |difference|; up = above; down =
below).

## Inputs and outputs

- Upstream: exactly one `analysis/dataset-import-qc/<run>/` (`complete`,
  `well_contract: single_larva_wells_only`), plus optional `complete` QC runs
  of that import (e.g. `interplate-vc-qc`) whose `excluded_plates` are
  dropped.
- Run folder: `<project>/analysis/behavior-endpoints/<run_label>/`
- Outputs (`outputs/`):
  - `behavior-endpoints__<run>__endpoints.rds` -- one row per plate x well x
    endpoint: identifiers, `endpoint_type`, `segment`, `component`,
    `metric`, `normalization_method`, `is_default_normalization`,
    `endpoint`, `endpoint_value_raw`, `endpoint_value_transformed`, `denom`
    (plate VC median), `endpoint_value_norm`. Always per plate (pooling only
    changes the plots).
  - `behavior-endpoints__<run>__endpoint-validation.rds` --
    `validate_behavior_endpoints()` result (structural checks: columns,
    types, unique keys, endpoint coverage, every kept well present,
    normalization provenance).
  - Four PDFs: `total-distance.pdf`, `distance-change.pdf`,
    `distance-shift.pdf` (down and up of each segment adjacent),
    `movement-similarity.pdf`; one panel per plate (or per chemical, points
    coloured by plate), one row per endpoint, `ncol` panels per page,
    normalized values, VC first.
  - `behavior-endpoints__<run>__manifest.yml`
- Report: `behavior-endpoints.html`.

## Package functions used (exported only)

`behavior_endpoint_input_cols()`, `replace_na_response_zero()`,
`generate_behavior_endpoints()`, `validate_behavior_endpoints()`,
`plot_behavior_dose_response_pages()`.

## Workflow

### Step 1 -- Check the upstream runs (STOP if not complete)

The import run must be `complete` with the single-larva well contract; any
QC run listed must be `complete` and made on that same import. Ask whether
QC runs should drop plates. The template enforces the same checks.

### Step 2 -- Gather parameters (STOP: ask the user)

Ask for, and do not guess:

1. Run label (e.g. the batch ID; `-pooled` for the pooled layout).
2. Phase segments (`segments`, inclusive time_value ranges), e.g.:
   L1 1-10, D1 11-20, L2 21-30, D2 31-40, D2-ext 41-50. **Ask whether the
   second dark phase should be split (D2 + D2-ext) or merged (D2 31-50).**
3. Movement similarity window(s) (e.g. LD 1-50) and metric(s) (spearman;
   pearson can be added).
4. Normalization per endpoint type; keep the defaults (log_diff / none /
   ratio) unless the user asks otherwise (non-default choices are flagged).
5. Layout: `per_plate` or `by_chemical_pooled` with `chemical_col` (default
   `DTXSID`; another column if DTXSID is missing).
6. Panel titles (`title_cols`, e.g. `[CHEMICAL_NAME, chemical_id, plate_id]`
   per plate or `[CHEMICAL_NAME, DTXSID]` pooled), `title_wrap_width` (40),
   panels per page (`ncol`, 4), PDF width (15) and height per endpoint row
   (2.2 in).

Use the same segments as `raw-response-plots`. If
`analysis/behavior-endpoints/<run_label>/` already exists, ask before reusing
it.

### Step 3 -- Create the run folder

```r
project_root <- normalizePath("<path/to/project>")
run_label <- "<run_label>"
skill_dir <- system.file("skills", "behavior-endpoints", package = "zfbehavior")
run_dir <- file.path(project_root, "analysis", "behavior-endpoints", run_label)
stopifnot(!dir.exists(run_dir))   # never overwrite an existing run without asking
dir.create(run_dir, recursive = TRUE)
file.copy(file.path(skill_dir, "templates", c("behavior-endpoints.qmd", "behavior-endpoints.yml")), run_dir)
```

Fill in the yml with the confirmed values. Do not edit the `.qmd`.

### Step 4 -- Render

```bash
cd <project>/analysis/behavior-endpoints/<run_label>
quarto render behavior-endpoints.qmd --execute-params behavior-endpoints.yml
```

A render error means a blocked upstream or an invalid parameter (e.g.
overlapping segments, a time value not in the data, a missing column). Fix
the yml with the user. Status `failed` means the structural validation
failed: report the failed checks; do not use the endpoints.

### Step 5 -- Report (STOP: user inspects the PDFs)

Tell the user: status, plates/wells used, endpoints, normalization (and any
non-default choice), warnings (e.g. zero-variance wells in movement
similarity), and the paths of `endpoints.rds` and the four PDFs. Ask them to
inspect the PDFs. Downstream skills use
`upstream: [analysis/behavior-endpoints/<run_label>]`.

## Guardrails (never do)

- Never run on an upstream run that is not `complete`, lacks the single-larva
  well contract, or (for QC runs) belongs to another import.
- Never invent segments, windows, metrics, or normalization; ask.
- Never re-curate wells or change `keep_well`; never exclude plates here.
- Never use endpoints from a `failed` run.
- Never edit upstream outputs or overwrite another run folder; never write
  analysis code that bypasses the template.
