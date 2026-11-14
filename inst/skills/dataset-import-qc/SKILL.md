---
name: dataset-import-qc
description: Import new zebrafish behavioral plate data (one file per plate, one combined file, or one sheet per plate) with the zfbehavior R package, run per-plate and dataset-level QC, curate wells so that every output well holds exactly one larva (trustworthy keep_well and denominators), record human plate/well decisions, and save one combined canonical .rds in a dedicated run folder. Use when new behavioral tracking data arrives, before any raw-response plotting, endpoint, statistics, or BMC analysis.
---

# dataset-import-qc

Module 1 (Data import and consistency checks) skill of the zfbehavior
workflow. It produces the combined canonical dataset that every downstream
skill reads.

**Output contract (single-larva wells):** every well in `combined.rds` holds
exactly one larva. `keep_well = TRUE` means analyze it; `keep_well = FALSE`
means "one larva, dead or malformed" (still counted in denominators). Wells
with no larva, multiple larvae, or a technical failure are NOT in the output.
Downstream skills rely on this and never re-curate wells (see
`../CONVENTIONS.md`, section 6).

Read `../CONVENTIONS.md` (shared run-folder, naming, manifest rules) before
starting. Locate the installed skill directory with:

```r
system.file("skills", "dataset-import-qc", package = "zfbehavior")
```

## Inputs and outputs

- Inputs: provider files under `<project>/data/raw/...` (never modified).
- Run folder: `<project>/analysis/dataset-import-qc/<run_label>/`
- Outputs (`outputs/`):
  - `dataset-import-qc__<run>__combined.rds` -- canonical tibble, all kept
    plates, vendor columns preserved, plus `time_value`, `source_file`.
    **Written only when status is `complete`.**
  - `dataset-import-qc__<run>__plate-qc.rds` -- import log, per-plate QC
    table, full `validate_behavior_data()` results (with diagnostics).
  - `dataset-import-qc__<run>__dataset-qc.rds` -- `validate_combined_dataset()`
    result (or `NULL` if not reached).
  - `dataset-import-qc__<run>__well-curation.rds` -- keep_well source,
    removed / provider-removed / flagged wells with reasons and comments, and
    per-plate well counts (delivered, removed, output, keep_well FALSE),
    and kept vehicle-control wells per plate (`vc_usability`).
  - `dataset-import-qc__<run>__manifest.yml` -- status, inputs + md5,
    outputs + md5, parameters, package version.
- Report: `dataset-import-qc.html`.

## Package functions used (exported only)

`canonical_behavior_roles()`, `required_canonical_roles()`,
`read_behavior_data()`, `parse_binning_to_time()`,
`validate_behavior_data()`, `validate_combined_dataset()`.

## Workflow

### Step 1 -- Gather study parameters (STOP: ask the user)

Ask for, and do not guess:

1. Project root and where the raw files are; run label (e.g. batch ID).
2. Input layout: one file per plate, a single file with all plates, or one
   Excel sheet per plate.
3. Plate design: wells per plate x timepoints (expected rows per plate),
   vehicle-control wells per plate, wells per dose group.
4. Identifier columns that must be 1:1 with plates (e.g. chemical ID,
   blinded ID) and the expected number of distinct chemicals (replicated
   chemicals make this smaller than the plate count).
5. Well curation -- which situation applies (the package cannot verify
   this; see `?"canonical-columns"`, section "Single-larva well contract"):
   a. **Provider already removed** no-larva / multi-larva / technical-failure
      wells from the files. Plates are then short of the expected counts;
      get the list of removed wells (plate, well, reason) for
      `provider_removed_wells`.
   b. **Problem wells are still in the data** (typically `keep_well = FALSE`
      with a comment such as "no larva", "2 larva", "missing", "empty").
      Ask which comment column/pattern identifies candidates
      (`flag_comment_column`, `flag_comment_pattern`); the wells to drop go
      to `remove_wells` after review (Step 5b).
   c. **No usable keep_well flag**: `keep_well$source: derived`; the user
      supplies the wells to set FALSE (`keep_well$false_wells`, with
      reasons); all others are TRUE. Do not map keep_well in
      `column_mapping`; add it to `optional_roles`.
   Several situations can apply to one batch (e.g. a + b).
6. Minimum fraction of usable vehicle-control embryos per plate
   (`min_kept_vc_pct`, e.g. 75 = flag plates with fewer than 75% of their
   vehicle-control wells kept). Plates with no kept vehicle-control well
   are always flagged.

If the run folder `analysis/dataset-import-qc/<run_label>/` already exists,
ask before reusing it.

### Step 2 -- Inspect inputs and propose the mapping (STOP: confirm)

Read only the headers and a few rows of one or two files, e.g.
`readxl::read_excel(f, n_max = 5)` or `readr::read_csv(f, n_max = 5)`, and
`readxl::excel_sheets(f)` for workbooks. Then propose:

- `column_mapping` for every role in `required_canonical_roles()`
  (plus `concentration_unit` if present). If time is only a text bin column
  (e.g. `BINNING` like `"0:01:00-0:02:00"`), put `time_value` in
  `optional_roles` and set `derive_time_value_from_binning`.
- `col_types` for **every** column in file order. Type mostly-empty text
  columns (comments, defect annotations) as `text` explicitly.

If the headers match `references/biobide-vendor-schema.md`, use it as a
starting point but still confirm with the user. Show the proposed mapping
and types and wait for confirmation.

### Step 3 -- Create the run folder

```r
project_root <- normalizePath("<path/to/project>")   # must already contain data/raw/...
run_label <- "<run_label>"
skill_dir <- system.file("skills", "dataset-import-qc", package = "zfbehavior")
run_dir <- file.path(project_root, "analysis", "dataset-import-qc", run_label)
stopifnot(!dir.exists(run_dir))   # never overwrite an existing run without asking
dir.create(run_dir, recursive = TRUE)
file.copy(file.path(skill_dir, "templates", c("dataset-import-qc.qmd", "dataset-import-qc.yml")), run_dir)
```

Fill in `dataset-import-qc.yml` with the confirmed values from Steps 1-2
(including `run_label`, which must equal the run folder name; keep
`project_root: "../../.."`; input paths are relative to the project root,
and symlinked raw-data folders are fine). Do not edit the analysis code in
the `.qmd`; all choices go in the yml.

### Step 4 -- First render

```bash
cd <project>/analysis/dataset-import-qc/<run_label>
quarto render dataset-import-qc.qmd --execute-params dataset-import-qc.yml
```

Then read `outputs/dataset-import-qc__<run>__manifest.yml`:

- `status: complete` -> go to Step 6.
- `status: needs_review` -> Step 5.
- `status: failed` or a render error -> report the error to the user (a
  parse/coercion error from `read_behavior_data()` means the file does not
  match the declared types/mapping, or the file itself is bad).

### Step 5a -- Review failed plates (STOP: user decides)

Summarize for the user, from the report and `plate-qc.rds`
(`readRDS(...)$plate_results[[plate_id]]$checks` / `$diagnostics`):
which plates failed, which checks, how many rows/wells are affected, and
import errors per file. Offer the options:

- fix or re-export the source data (preferred when it is a provider error),
- correct an expected value in the yml if the user confirms it was wrong,
- if the shortfall is wells the provider deliberately removed (no larva,
  multiple larvae, technical failure): declare them in
  `provider_removed_wells`. The plate is accepted only if its sole failures
  are the row/VC/dose count checks and the shortfall matches the
  declaration exactly; otherwise it stays failed,
- exclude the plate: add it to `exclude_plates` with the user's reason.

Record the user's decision in the yml, then re-render (Step 4). Repeat
until `complete`. If dataset-level QC fails (identifier not 1:1, wrong
distinct count), handle it the same way.

### Step 5b -- Review flagged wells (STOP: user decides)

If the status reason lists unresolved flagged wells, show the "Wells flagged
by the comment pattern" table (also in `well-curation.rds` `$flagged`) with
plate, well, VC/dose, keep_well, and comment. For each well the user decides:

- `remove_wells` -- no larva, multiple larvae, or technical failure
  (reason required; ambiguous comments such as "missing" need the user's
  or provider's confirmation),
- `keep_flagged_wells` -- the well does hold exactly one larva (reason
  required; e.g. the pattern matched an unrelated word).

A well can also be removed without being flagged (add it to `remove_wells`
directly). Re-render until `complete`.

### Step 5c -- Review plates with too few usable vehicle controls (STOP: user decides)

If the status reason lists plates with too few kept vehicle-control wells,
show the "Plates flagged for too few kept vehicle-control wells" table
(`well-curation.rds` `$vc_usability`). For each plate the user decides:
`exclude_plates` (reason) or `accept_low_vc_plates` (reason). A plate with
no kept vehicle-control well can only be excluded. Re-render until
`complete`.

### Step 6 -- Report

Tell the user: status, plates combined / excluded (with reasons), wells
removed / provider-removed (with counts per plate), row count, the
concentration units of treated wells (report table "Concentration units";
manifest `concentration_units` / `plates_mixed_concentration_units`; point
out mixed units, which matter for pooling plates downstream), and the
paths of `combined.rds` and the manifest. The next skill (e.g.
raw-response plots or endpoints) uses
`upstream: [analysis/dataset-import-qc/<run_label>]`.

## Guardrails (never do)

- Never guess a column role or a scientific expectation; ask.
- Never edit, rename, merge, or re-key plates/wells to make QC pass, and never
  build composite plate IDs without the user's explicit decision.
- Never exclude or accept a plate without the user's decision and reason in
  the yml.
- Never infer `keep_well` or decide a well's larva status yourself. Wells
  are removed only via `remove_wells`, declared absent only via
  `provider_removed_wells`, and keep_well is derived only from
  `keep_well$false_wells` -- every entry with the user's confirmed reason.
  A comment-pattern match is a candidate, never a decision.
- Never impute missing `response_value` (that is
  `replace_na_response_zero()`, an endpoint-step decision).
- Never disable or work around strict parsing in `read_behavior_data()`;
  fix types or the source file instead.
- Never modify files in `data/raw/` or overwrite another run folder.
- Never write analysis code that bypasses the template; if something the
  template cannot express is needed, tell the user.
