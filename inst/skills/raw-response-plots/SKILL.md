---
name: raw-response-plots
description: Plot raw zebrafish behavioral response traces with the zfbehavior R package -- per-plate dose-group summary curves and vehicle-control well traces as multi-page PDFs, labeled with kept/total well counts, plus a vehicle-control well-usage table -- from a complete dataset-import-qc run, in a dedicated run folder. Use after import QC, for visual QC of movement patterns before endpoint, statistics, or BMC analysis.
---

# raw-response-plots

Module 2 (Raw-response visualization and inter-plate VC QC) skill of the
zfbehavior workflow. Raw-response plots: it reads the combined dataset of one
`complete` `dataset-import-qc` run and writes visual QC PDFs and well-usage
tables. It changes no data. Inter-plate vehicle-control PCA QC is the separate
`interplate-vc-qc` skill.

Read `../CONVENTIONS.md` (run folders, naming, manifest, upstream rules)
before starting. Locate the installed skill directory with:

```r
system.file("skills", "raw-response-plots", package = "zfbehavior")
```

## Inputs and outputs

- Upstream: exactly one `analysis/dataset-import-qc/<run>/` with manifest
  `status: complete` and `well_contract: single_larva_wells_only`.
- Run folder: `<project>/analysis/raw-response-plots/<run_label>/`
- Outputs (`outputs/`):
  - `raw-response-plots__<run>__summary.pdf` -- one panel per group (default
    per plate): median (or mean) curve per dose group with Q25-Q75 (or 95%
    CI) band, segment shading, legend `VC(n_kept/n_total)`, `<conc>(n/N)`;
    3 x 2 panels per page, panels in `panel_order` (default alphabetical
    by panel title).
  - `raw-response-plots__<run>__well.pdf` -- one panel per group: individual
    vehicle-control well traces (or all wells), faceted by dose group;
    2 x 2 panels per page.
  - `raw-response-plots__<run>__well-counts.rds` -- `count_wells_by_dose()`
    per plate x dose group (`n_kept_wells`, `n_total_wells`).
  - `raw-response-plots__<run>__vc-usage.rds` -- vehicle-control usage per
    plate (`pct_used`), lowest first.
  - `raw-response-plots__<run>__manifest.yml`
- Report: `raw-response-plots.html` (inputs, usage tables, first page of
  each PDF).

## Package functions used (exported only)

`raw_plot_input_cols()`, `count_wells_by_dose()`,
`plot_raw_responses_pages()` (builds one `plot_raw_responses()` panel per
group via an internal helper).

## Workflow

### Step 1 -- Check the upstream run (STOP if not complete)

Read `analysis/dataset-import-qc/<run>/outputs/*__manifest.yml`. If `status`
is not `complete`, or `well_contract` is not `single_larva_wells_only`, stop
and tell the user which run blocks and why (finish or re-run
`dataset-import-qc` first). The template enforces the same checks.

### Step 2 -- Gather plot parameters (STOP: ask the user)

Ask for, and do not guess:

1. Run label (e.g. the batch ID; usually the same as the upstream run).
2. Assay phase windows (`segments`): name and inclusive `time_value` range of
   each phase, e.g. L1 1-10, D1 11-20, ... Use the same windows as the
   endpoint step. Light phases should be named starting with `L` to get the
   default yellow shading (or set `segment_style`).
3. Response axis label, e.g. "distance moved (mm)".
4. Panel title columns (`title_cols`), e.g. chemical name + chemical ID +
   plate ID; must be constant within a plate. Default: plate ID. Long
   titles wrap at `title_wrap_width` characters (default 60).
5. Optional choices (defaults in parentheses): `summary_type` (median),
   `missing_value_policy` (impute_zero -- matches how endpoints are computed),
   `wells` for the well-level PDF (vehicle_control; `all` is slow and large),
   `group_cols` (plate_id), `panel_order` (alphabetical by panel title,
   case-insensitive; or `group_key`).

To help the user, you may show the available columns and time values of the
upstream data (e.g. `names(readRDS(<combined.rds>))`,
`sort(unique(d$time_value))`), but the windows are the user's decision.

If `analysis/raw-response-plots/<run_label>/` already exists, ask before
reusing it.

### Step 3 -- Create the run folder

```r
project_root <- normalizePath("<path/to/project>")
run_label <- "<run_label>"
skill_dir <- system.file("skills", "raw-response-plots", package = "zfbehavior")
run_dir <- file.path(project_root, "analysis", "raw-response-plots", run_label)
stopifnot(!dir.exists(run_dir))   # never overwrite an existing run without asking
dir.create(run_dir, recursive = TRUE)
file.copy(file.path(skill_dir, "templates", c("raw-response-plots.qmd", "raw-response-plots.yml")), run_dir)
```

Fill in `raw-response-plots.yml` with the confirmed values (`run_label` must
equal the folder name; keep `project_root: "../../.."`; `upstream` is the
dataset-import-qc run folder relative to the project root). Do not edit the
`.qmd`.

### Step 4 -- Render

```bash
cd <project>/analysis/raw-response-plots/<run_label>
quarto render raw-response-plots.qmd --execute-params raw-response-plots.yml
```

A render error means a blocked upstream or an invalid parameter (the message
says which: e.g. a segment time not present in the data, a missing title
column, titles not constant per plate). Fix the yml with the user and
re-render. A warning about upstream md5 drift means the upstream
`combined.rds` changed after its run completed: stop and ask the user.

### Step 5 -- Report (STOP: user inspects the PDFs)

Tell the user: status, panels and pages per PDF, the vehicle-control usage
summary (lowest plates), and the PDF paths. Ask them to inspect the PDFs
(movement patterns, light/dark responses, outlying wells/plates). This skill
does not exclude anything; if the inspection shows a data problem (e.g. a
plate or well that must go), that is a new `dataset-import-qc` run, not an
edit here.

## Guardrails (never do)

- Never run on an upstream run that is not `complete` or lacks the
  single-larva well contract.
- Never invent phase windows, labels, or titles; ask.
- Never filter, drop, relabel, or re-curate wells or plates here (no comment
  regex, no extra exclusions); well curation belongs to `dataset-import-qc`.
- Never change `keep_well` or impute data outside the template's
  `missing_value_policy` parameter.
- Never edit upstream outputs or overwrite another run folder.
- Never write plotting code that bypasses the template; if something the
  template cannot express is needed, tell the user.
