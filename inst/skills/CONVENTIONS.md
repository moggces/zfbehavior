# zfbehavior skill conventions (shared by all module skills)

Every module skill in `inst/skills/` follows these rules so that runs are
reproducible and traceable. Each skill's `SKILL.md` refers back to this file
instead of repeating it.

## Modules and skills

The workflow has five scientific modules. A module is a method step; a skill
is the unit you run (one run folder per skill run). A module can have several
skills.

| # | Module | Skills |
|---|---|---|
| 1 | Data import and consistency checks | `dataset-import-qc` |
| 2 | Raw-response visualization and inter-plate VC QC | `raw-response-plots`, `interplate-vc-qc` |
| 3 | Endpoint calculation, normalization, and intra-plate VC QC | `behavior-endpoints`, `intraplate-vc-qc`, `devtox-endpoints` |
| 4 | ANOVA/repeated-measures ANOVA hit calling | `behavior-endpoints-anova` |
| 5 | Curvep-based BMC modeling of behavioral and developmental toxicity endpoints | `behavior-endpoints-bmc`, `devtox-endpoints-bmc` |

Skill dependencies (arrow = "reads the outputs of"; `[QC]` runs are
optional upstreams whose `excluded_plates` the downstream skill drops, see
section 6):

```
dataset-import-qc  (M1)
 |-- raw-response-plots  (M2)
 |-- [QC] interplate-vc-qc  (M2)
 |-- behavior-endpoints  (M3)      <- optional [QC] interplate-vc-qc
 |    |-- [QC] intraplate-vc-qc  (M3)
 |    |-- behavior-endpoints-anova  (M4)  <- optional [QC] intraplate-vc-qc
 |    `-- behavior-endpoints-bmc  (M5)    <- optional [QC] intraplate-vc-qc
 `-- devtox-endpoints  (M3)        <- optional [QC] interplate-vc-qc
      `-- devtox-endpoints-bmc  (M5)
```

A typical behavior run order is `dataset-import-qc` -> `raw-response-plots`
-> `interplate-vc-qc` -> `behavior-endpoints` -> `intraplate-vc-qc` ->
`behavior-endpoints-anova` and/or `behavior-endpoints-bmc`; the devtox branch
is `devtox-endpoints` -> `devtox-endpoints-bmc`.

## 1. Where results go

All results live in the **user's analysis project**, never inside the
installed package:

```text
<project>/
|-- data/raw/                         # provider files; never modified by a skill
`-- analysis/
    `-- <skill-name>/
        `-- <run-label>/              # one folder per run
            |-- <skill-name>.qmd      # copied from the skill's templates/
            |-- <skill-name>.yml      # this run's parameters
            |-- <skill-name>.html     # rendered report
            `-- outputs/
                |-- <skill-name>__<run-label>__<object>.rds
                |-- <skill-name>__<run-label>__<object>.pdf
                `-- <skill-name>__<run-label>__manifest.yml
```

- `<run-label>` is **required and chosen by the user** (e.g. a batch ID such
  as `260416`, or a setting name such as `bmr20`). Allowed characters:
  letters, digits, `-`, `_`.
- Several runs of the same skill (e.g. BMC with different settings) are
  sibling folders under `analysis/<skill-name>/`.
- **Never overwrite an existing run folder** without explicit user
  confirmation. Re-rendering the same run (e.g. the second pass of a
  two-pass review) is allowed; it replaces that run's outputs.

## 2. File naming

`<skill-name>__<run-label>__<object>.<ext>` (double underscores). Every output
file is unique across the project and traceable to the `.qmd` and run that
produced it.

**Tables are tibbles.** Every table saved to `outputs/` as `.rds` is a
tibble (`tibble::as_tibble()`), so `readRDS()` gives a tibble. This includes
tables one level inside a saved list or result object (e.g.
`well-curation.rds$vc_usability`). Non-table objects (lists, result classes,
model objects) keep their class. Templates save through a small `save_rds()`
helper that applies this rule.

## 3. Parameters

Each run's parameters live in `<skill-name>.yml` next to the `.qmd` and are
passed at render time:

```bash
cd analysis/<skill-name>/<run-label>
quarto render <skill-name>.qmd --execute-params <skill-name>.yml
```

The yml is the record of every scientific choice made for that run (column
mappings, expected counts, windows, BMR settings, exclusions with reasons).
Agents fill it in **with** the user; they never invent scientific settings.

## 4. Upstream references and ordering

- A run's yml lists its inputs under `upstream:` as paths to upstream run
  folders (relative to the project root), e.g.
  `upstream: [analysis/dataset-import-qc/260416]`.
- Order comes from these references, **not** from folder numbering, so
  parallel branches (e.g. devtox BMC before behavioral endpoints) and
  repeated runs are allowed.
- Root steps (e.g. `dataset-import-qc`) have no `upstream:`; they list raw
  input files instead.

## 5. Manifest

Each render writes `outputs/<skill-name>__<run-label>__manifest.yml`:

```yaml
skill: dataset-import-qc
run_label: "260416"
status: complete            # needs_review | complete | failed
status_reason: "..."
rendered_at: "2026-08-12T14:03:00"
zfbehavior_version: "0.0.0.9000"
upstream: []                # upstream manifest paths
inputs:                     # every file read, with md5
  - path: data/raw/plate_01.xlsx
    md5: 1b2c...
outputs:
  - path: outputs/dataset-import-qc__260416__combined.rds
    md5: 9f8e...
parameters: { ... }         # the yml as used
```

Status meanings:

- `needs_review` -- the run stopped at a human decision point (e.g. failed
  plates must be excluded or fixed). Downstream-facing outputs (e.g. the
  combined dataset) are **not** written.
- `complete` -- all downstream-facing outputs written; safe to use.
- `failed` -- the run could not complete (e.g. unreadable inputs).

## 6. Rules for downstream skills

- Refuse to start if any upstream manifest's `status` is not `complete`;
  explain which upstream run blocks and why.
- Before use, recompute md5 of each upstream output listed in the upstream
  manifest; if it differs, warn that the upstream run was modified after it
  completed and ask the user how to proceed.
- Read upstream data **only** from upstream `outputs/` files by explicit path.
  Never rely on objects from a previous R session.
- **Single-larva well contract.** A `complete` `dataset-import-qc` run
  guarantees (manifest `well_contract: single_larva_wells_only`) that every
  well in its `combined.rds` holds exactly one larva: `keep_well = TRUE`
  means analyze, `keep_well = FALSE` means "one larva, dead or malformed".
  No-larva, multi-larva, and technical-failure wells were removed (or
  declared removed by the provider) at import. Downstream skills therefore
  use the wells present as the denominator (e.g. `count_wells_by_dose()`,
  devtox percent-affected) and **never re-curate wells** (no comment regex,
  no extra well drops); a well-curation problem found later is fixed by a
  new import run, not downstream. The same run also guarantees every plate
  has at least one kept vehicle-control well (and at least
  `min_kept_vc_pct` percent unless the plate is listed in the manifest's
  `accepted_low_vc_plates`).
- **Plate exclusions from QC skills.** A QC skill that decides about plates
  (e.g. `interplate-vc-qc`) does not write a filtered dataset; its
  `complete` manifest lists `excluded_plates` (plate_id + reason). A
  downstream analysis skill lists the import run **and** such QC runs under
  `upstream:`, reads the import run's `combined.rds`, and drops exactly the
  plates in those `excluded_plates` lists.
- **Plate exclusions from endpoint-level QC.** A QC skill may also sit on an
  endpoint run (e.g. `intraplate-vc-qc` on a `behavior-endpoints` run). A
  downstream skill using those endpoints lists the endpoint run **and** such
  QC runs under `upstream:`, checks that each QC run's upstream is that same
  endpoint run, and drops the plates in their `excluded_plates` from the
  endpoints. (Plates excluded before the endpoint run are already absent.)

## 7. Possible later extensions

Not part of the package now. The run folders are designed so that each run
could later map to one `targets` target (the yml and upstream outputs as file
dependencies, `outputs/` as file outputs, the manifest md5 hashes as what
`targets` would track), or each skill step could be exposed as a tool by an
MCP server reading and writing the same folders.
