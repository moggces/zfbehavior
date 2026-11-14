---
name: devtox-endpoints
description: Calculate the zebrafish developmental toxicity endpoint (percent of larvae dead or malformed per dose group, vehicle control included) with the zfbehavior R package from a complete dataset-import-qc run (optionally dropping plates excluded by QC runs), and plot dose-response curves per plate, per chemical with one line per plate, or per chemical pooled across plates, in a dedicated run folder. Use before devtox BMC analysis.
---

# devtox-endpoints

Module 3 (Endpoint calculation, normalization, and intra-plate VC QC) skill of
the zfbehavior workflow. Developmental toxicity (devtox) endpoints: it reads
the combined dataset of one `complete` `dataset-import-qc` run, counts per
dose group the larvae (`N`) and the affected larvae (`n_affected`: dead or
malformed, `keep_well = FALSE`), and plots percent affected against
concentration. It ends with the plots; BMR/BMC is the later
`devtox-endpoints-bmc` skill, which reads this run's outputs.

Read `../CONVENTIONS.md` (run folders, naming, manifest, upstream rules)
before starting. Locate the installed skill directory with:

```r
system.file("skills", "devtox-endpoints", package = "zfbehavior")
```

## Inputs and outputs

- Upstream:
  - exactly one `analysis/dataset-import-qc/<run>/` with manifest
    `status: complete` and `well_contract: single_larva_wells_only`;
  - optionally, `complete` QC runs of that same import (e.g.
    `analysis/interplate-vc-qc/<run>/`): their `excluded_plates` are dropped.
- Run folder: `<project>/analysis/devtox-endpoints/<run_label>/`
- Outputs (`outputs/`):
  - `devtox-endpoints__<run>__endpoints.rds` -- always per plate: one row
    per plate x dose group (vehicle control included, `is_vehicle_control`),
    `concentration`, `concentration_unit`, `N`, `n_affected`,
    `pct_affected`, `endpoint` ("percent of affected"), plus the id/title
    columns.
  - `devtox-endpoints__<run>__endpoints-pooled.rds` -- only for
    `layout: by_chemical_pooled`: one row per chemical x concentration,
    wells of all plates of the chemical counted together.

  The two tables differ only in grain. Example, a chemical on 3 plates:
  `endpoints.rds` has 3 VC rows (16 larvae each) and each plate's dose
  groups separately; `endpoints-pooled.rds` has 1 VC row (48 larvae) and one
  row per distinct concentration, summing `N` and `n_affected` over the
  plates tested at that concentration (plates with a different dose set add
  their own concentrations). The pooled table is what the pooled PDF shows
  and the input for a pooled BMC (one curve per chemical); `endpoints.rds`
  is the input for per-plate BMC. Both are always tibbles.
  - `devtox-endpoints__<run>__dose-response.pdf` -- one panel per plate or
    chemical, VC labeled `VC`, y axis 0-100 %, no BMC results.
  - `devtox-endpoints__<run>__manifest.yml`
- Report: `devtox-endpoints.html` (upstream, plates used/excluded, endpoint
  and unit summaries, vehicle-control percent affected per plate, all dose
  groups as a searchable table, first PDF page).

## Package functions used (exported only)

`devtox_input_cols()`, `calculate_devtox_endpoints()`,
`plot_devtox_dose_response_pages()` (one panel per group).

## Workflow

### Step 1 -- Check the upstream runs (STOP if not complete)

Read the manifest(s) under `analysis/<skill>/<run>/outputs/`. The import run
must be `complete` with `well_contract: single_larva_wells_only`; any QC run
listed must be `complete` and made on that same import run. Otherwise stop
and tell the user which run blocks and why. Ask whether QC runs exist that
should drop plates (e.g. `interplate-vc-qc`); the user decides which to list.
The template enforces the same checks.

### Step 2 -- Gather parameters (STOP: ask the user)

Ask for, and do not guess:

1. Run label (e.g. the batch ID).
2. Upstream runs: the import run, plus the QC runs whose exclusions apply.
3. Layout of the plots:
   - `per_plate` (default): one panel per plate.
   - `by_chemical_plates`: one panel per chemical, one colored line per
     plate (compare replicate plates).
   - `by_chemical_pooled`: one panel per chemical, one line; larvae of all
     plates are counted together per concentration (also writes
     `endpoints-pooled.rds`, which the BMC skill can use).
4. For the `by_chemical_*` layouts: the column that identifies the same
   chemical across plates (`chemical_col`, default `DTXSID`; e.g.
   `CHEMICAL_NAME` when DTXSID is missing). Plate IDs such as
   `chemical_id` are usually different per plate and cannot be used.
5. Panel title columns (`title_cols`), constant within a panel, e.g.
   `[CHEMICAL_NAME, chemical_id, plate_id]` (per plate) or
   `[CHEMICAL_NAME, DTXSID]` (per chemical). Default: the panel key.
   Long titles wrap at `title_wrap_width` characters (default 40).
6. Optional: `panel_order` (alphabetical by title; or `group_key`), panels
   per page (`nrow` x `ncol`, default 1 x 5), PDF size.

Point out the concentration units from the import manifest
(`concentration_units`): a chemical tested in different units on different
plates cannot use a `by_chemical_*` layout (the template stops).

To help the user, you may show the upstream columns and how many plates
each chemical has (e.g. `table(table(unique(d[c("plate_id", "DTXSID")])$DTXSID))`),
but the choices are the user's.

If `analysis/devtox-endpoints/<run_label>/` already exists, ask before
reusing it.

### Step 3 -- Create the run folder

```r
project_root <- normalizePath("<path/to/project>")
run_label <- "<run_label>"
skill_dir <- system.file("skills", "devtox-endpoints", package = "zfbehavior")
run_dir <- file.path(project_root, "analysis", "devtox-endpoints", run_label)
stopifnot(!dir.exists(run_dir))   # never overwrite an existing run without asking
dir.create(run_dir, recursive = TRUE)
file.copy(file.path(skill_dir, "templates", c("devtox-endpoints.qmd", "devtox-endpoints.yml")), run_dir)
```

Fill in `devtox-endpoints.yml` with the confirmed values (`run_label` must
equal the folder name; keep `project_root: "../../.."`; `upstream` paths are
relative to the project root). Do not edit the `.qmd`.

### Step 4 -- Render

```bash
cd <project>/analysis/devtox-endpoints/<run_label>
quarto render devtox-endpoints.qmd --execute-params devtox-endpoints.yml
```

A render error means a blocked upstream or an invalid parameter (the message
says which: e.g. a column not in the data, a chemical column with missing
values, titles not constant per panel, mixed concentration units when
pooling). Fix the yml with the user and re-render. A warning about upstream
md5 drift means the import's `combined.rds` changed after its run completed:
stop and ask the user.

### Step 5 -- Report (STOP: user inspects the PDF)

Tell the user: plates used / excluded by QC, dose groups, the vehicle-control
percent affected (highest plates), and the PDF and endpoint paths. Ask them
to inspect the dose-response PDF. The next skill (`devtox-endpoints-bmc`)
uses `upstream: [analysis/devtox-endpoints/<run_label>]`.

## Guardrails (never do)

- Never run on an upstream run that is not `complete`, lacks the single-larva
  well contract, or (for QC runs) belongs to another import run.
- Never re-curate wells: no filtering by comments or defect codes, no extra
  well drops, no change of `keep_well`. Affected = `keep_well == FALSE` as
  delivered by the import (one larva, dead or malformed).
- Never exclude plates here; plate exclusions come only from the import run
  and the listed QC runs.
- Never choose the chemical column, layout, or titles for the user, and never
  pool plates whose concentrations are in different units.
- Never compute BMR/BMC here.
- Never edit upstream outputs or overwrite another run folder; never write
  analysis code that bypasses the template.
