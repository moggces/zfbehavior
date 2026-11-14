---
name: behavior-endpoints-anova
description: Call behavioral hits per plate with the zfbehavior R package -- one-way ANOVA with Dunnett post-hoc tests on distance change per segment and on movement similarity (one or more correlation metrics, tested separately), plus repeated-measures ANOVA over the distance-change segments -- from a complete behavior-endpoints run (optionally dropping plates excluded by intraplate-vc-qc), and plot all hits as a sorted, paginated activity overview (p-value classes as point sizes), in a dedicated run folder. Use after behavior-endpoints and intraplate-vc-qc.
---

# behavior-endpoints-anova

Module 4 (ANOVA/repeated-measures ANOVA hit calling) skill of the zfbehavior
workflow (Konrad et al. 2026). It reads the endpoints of one `complete`
`behavior-endpoints` run, tests each plate's concentrations against its
vehicle control, and writes the decisions plus an activity PDF. It changes no
data and has no review stop: the decisions are fixed by the method; the PDF is
for visual inspection.

Read `../CONVENTIONS.md` (run folders, naming, manifest, upstream rules)
before starting. Locate the installed skill directory with:

```r
system.file("skills", "behavior-endpoints-anova", package = "zfbehavior")
```

## Method (fixed; as in the original analysis)

On the raw endpoint values (`endpoint_value_raw`), one model per plate:

| Test | Endpoints | Transform | Hit rule |
|---|---|---|---|
| ANOVA + Dunnett | distance change (`<seg>@both_auc`), per segment | log10 | omnibus p < alpha and >= 1 Dunnett p < alpha |
| ANOVA + Dunnett | movement similarity, per window and selected metric | Fisher z | same |
| RM-ANOVA (GLS, AR(1)) + Dunnett | distance change, all segments together | log10 | interaction (concentration x segment) p < alpha with a per-segment Dunnett hit; else reduced-model concentration p < alpha with a pooled Dunnett hit |

A missing p-value that would decide the outcome gives `indeterminate`.
RM-ANOVA needs >= 2 distance-change segments; with one it is skipped (noted).
No multiple-testing correction across endpoints or metrics (historical);
testing two similarity metrics roughly doubles the chance of a false
movement-similarity hit. Distance shift and total distance are not tested.

## Inputs and outputs

- Upstream: exactly one `analysis/behavior-endpoints/<run>/` (`complete`),
  plus optional `complete` `analysis/intraplate-vc-qc/<run>/` runs made on that
  same endpoints run; their `excluded_plates` are dropped. Labels come from
  the endpoints run's import run.
- Run folder: `<project>/analysis/behavior-endpoints-anova/<run_label>/`
- Outputs (`outputs/`):
  - `behavior-endpoints-anova__<run>__anova.rds` -- `run_anova()` result
    (`anova` omnibus table, `dunn` post-hoc table).
  - `__rmanova.rds` -- `run_rmanova()` result (if run).
  - `__hits.rds` -- one row per plate x endpoint (ANOVA) and per plate
    (RM-ANOVA, `endpoint = "distance change (all segments)"`): labels,
    `test`, endpoint provenance (`endpoint_type`, `segment`, `component`,
    `metric`), `decision_state` (hit / not_hit / indeterminate),
    `decision_source`, `hit_p_value`, raw p-values and post-hoc counts.
  - `__activity.rds` -- the activity table (a subset of `hits` columns);
    `in_activity_pdf` is FALSE for results left out of the PDF by
    `activity_exclude_tests` (ANOVA and/or RMANOVA) or
    `activity_exclude_endpoints` (default: nothing left out).
  - `__activity.pdf` -- `plot_behavior_anova_activity_pages()`: chemicals as
    facet rows (sorted), plates (the tested units) as rows within a chemical,
    `n_per_page` plate rows per page (whole chemicals; pages padded so a
    plate row has the same height on every page),
    columns = distance change and one movement-similarity column per metric,
    x = segments (RM-ANOVA hits at "all phases (RM)"), point size = p-value
    class (legend p < 0.05 ... p < 1e-10).
  - `__manifest.yml` -- incl. `method`, hit counts, `plates_excluded_by_qc`.
- Report: `behavior-endpoints-anova.html` -- decision counts, metric
  agreement (if >= 2 metrics), interactive decision table.

To summarize across metrics yourself (e.g. "hit by spearman or pearson"),
build a table from `hits.rds` with the activity-table columns (see
`?plot_behavior_anova_activity_pages`) and plot it with that function; the
skill itself never pools metrics.

## Package functions used (exported only)

`run_anova()`, `run_rmanova()`, `run_anova_rmanova_hit_calling()`,
`plot_behavior_anova_activity_pages()`.

## Workflow

### Step 1 -- Check the upstream runs (STOP if not complete)

The behavior-endpoints run must be `complete`; any intraplate-vc-qc run listed
must be `complete` and made on that endpoints run. Ask whether intraplate QC
runs should drop plates. The template enforces the same.

### Step 2 -- Gather parameters (STOP: ask the user)

Ask for, and do not guess:

1. Run label (e.g. the batch ID; a suffix for variants, e.g. `-sp-pe`).
2. Similarity metric(s) to test (`spearman`, historical; `pearson` too to
   compare; each must exist in the endpoints run).
3. Optional (defaults in parentheses): `alpha` (0.05), `rmanova` (auto),
   `seed` (20260806).
4. Plot labels: `chemical_label_cols` (e.g. `[CHEMICAL_NAME, chemical_id]`),
   `plate_label_cols` (`[plate_id]`), `n_per_page` (20 plate rows), `order`
   (alphabetical), `chemical_wrap_width` (50), PDF size (17 x 16 in).
5. Results to leave out of the activity PDF only (default none):
   `activity_exclude_tests` (e.g. `[RMANOVA]`), `activity_exclude_endpoints`,
   with `activity_exclude_reason`. They stay in `hits.rds`.

If `analysis/behavior-endpoints-anova/<run_label>/` already exists, ask before
reusing it.

### Step 3 -- Create the run folder

```r
project_root <- normalizePath("<path/to/project>")
run_label <- "<run_label>"
skill_dir <- system.file("skills", "behavior-endpoints-anova", package = "zfbehavior")
run_dir <- file.path(project_root, "analysis", "behavior-endpoints-anova", run_label)
stopifnot(!dir.exists(run_dir))   # never overwrite an existing run without asking
dir.create(run_dir, recursive = TRUE)
file.copy(file.path(skill_dir, "templates",
                    c("behavior-endpoints-anova.qmd", "behavior-endpoints-anova.yml")), run_dir)
```

Fill in the yml with the confirmed values. Do not edit the `.qmd`.

### Step 4 -- Render

```bash
cd <project>/analysis/behavior-endpoints-anova/<run_label>
quarto render behavior-endpoints-anova.qmd --execute-params behavior-endpoints-anova.yml
```

A render error means a blocked upstream or an invalid parameter (the message
says which), or a model problem on some plate (the error names the plate and
endpoint; report it to the user, do not work around it).

### Step 5 -- Report (STOP: user inspects the PDF)

If the report warns that chemical labels do not fit their panels
(`n_labels_not_fitting` in the manifest), suggest a larger `pdf_height` (or
`chemical_wrap_width`) and re-render.

Tell the user: status, plates tested (and excluded by QC), hit counts per
test and endpoint, metric agreement (if several metrics), indeterminate
decisions, RM-ANOVA note, and the paths of `hits.rds` and the activity PDF.
Ask them to inspect the PDF.

## Guardrails (never do)

- Never run on an upstream run that is not `complete`, or with an
  intraplate-vc-qc run made on another endpoints run.
- Never change the transforms, the value column, or the hit rule; never add
  endpoints (distance shift, total distance) to the tests without a package
  change agreed with the user.
- Never change the activity exclusions without the user; they only hide
  results from the activity PDF, never from `hits.rds`.
- Never pool metrics or endpoints inside the skill; never drop plates other
  than the QC-excluded ones.
- Never edit upstream outputs or overwrite another run folder; never write
  analysis code that bypasses the template.
