# AGENTS.md -- zfbehavior

R package for zebrafish larval light-dark transition test (LDTT) behavior and
developmental toxicity (devtox) analysis, plus **Agent Skills shipped in
`inst/skills/`** (one or more per module; workflow guides that call exported
functions and render a Quarto report per run folder).

**If `AGENTS.local.md` exists in this folder, read it too** (maintainer's
private setup: notes, private data, validation folders).

- Run-folder / manifest / upstream rules for skills:
  `inst/skills/CONVENTIONS.md` (authoritative).
- Developer workflow, versions and releases: `CONTRIBUTING.md`.
- Methods and module overview: `vignette("methods")`, `?module1` ...
  `?module5`; using the skills: `vignette("getting-started")`.

## Commands

- `Rscript -e 'devtools::document()'` after any roxygen change, then
  `devtools::test()` (single file: `devtools::test(filter = "bmc")`), then
  `devtools::check(cran = FALSE)` (add `args = "--no-tests"` to skip tests).
  Target: 0 errors / 0 warnings / 0 notes, 0 test warnings.
- Format only the files you change (`air format R/<file>.R`); never
  `air format .` on the whole repo.
- Delete stray `Rplots.pdf` (root and `tests/testthat/`) before check -- it
  causes a NOTE.
- Long jobs (full test suite ~5 min, full check ~10 min, Curvep BMC on real
  data ~15-20 min): run in ONE foreground call with a long tool timeout.
  Background `nohup`/`&` processes are killed when the shell call ends.

## Skill tests and skill runs use the INSTALLED package

- Templates run `library(zfbehavior)` in a fresh Quarto R process.
  `test-skills.R` render tests are **silently skipped** unless quarto is on
  PATH and the installed version equals the source version. Reinstall first:
  `R CMD INSTALL --no-test-load .`
- `test-skills.R` fails if any `SKILL.md`/template calls a non-exported
  function. Skills/templates may only use exported functions; promote risky
  glue to a tested exported function instead.
- Render a run: `cd analysis/<skill>/<run>; quarto render <skill>.qmd --execute-params <skill>.yml`.
- Quarto params gotchas: nested defaults in the qmd header must be `null`;
  a yml list of maps arrives in R as a data.frame; `yaml::write_yaml(TRUE)`
  writes `yes`, which arrives as the string `"yes"`.

## Rules that are easy to break

- Never invent scientific settings (windows, BMR, exclusions); stop and ask.
  Every human decision is recorded in the run yml with a reason.
- Downstream skills **never re-curate wells**; the import run guarantees the
  single-larva well contract. QC skills output `excluded_plates` lists, not
  filtered datasets.
- Every table saved to `outputs/*.rds` is a tibble (incl. nested tables),
  via the template's `save_rds()` helper.
- String ordering: dplyr and base R collate differently. Sort panel/chemical
  labels with `order(tolower(x), x, method = "radix")`.
- Zero-variance wells get movement similarity **0** (a change to 1 was
  reverted). Negative similarity -> 0.
- Statistics use `endpoint_value_raw`; BMR/BMC use `endpoint_value_norm`.
- Dunnett (`DescTools`/`multcomp`) p-values and Curvep BMC are Monte-Carlo;
  keep tolerance-based regression tests. Curvep parallelism is set by the
  caller (`future::plan(multisession, workers = n)`), never inside the package.
- Exported-API rule: a function stays exported only if a skill or clear user
  need calls it; single-panel and `_by_group()` plot helpers are internal
  (use `_pages()`).
- Roxygen: refer to `@noRd` functions in backticks, not markdown links
  (Rd cross-reference WARNING). Avoid `:=` (check NOTE). `@param a,b` has no
  space after the comma.
- `readxl` type guessing silently drops strings; imports must pass
  explicit `excel_col_types`.
- Large wide PDFs: activity/dose-response pages need `cairo_pdf` (non-ASCII
  chemical names) and generous sizes (dose-response: 1.5 + 2.2 in per
  endpoint row); render and inspect, do not trust layout.
- No `setwd()`, no `library()` in package code, no hard-coded absolute paths;
  user data/results never go into the installed package.
- Tests needing private data use `zf_private_file()`
  (`tests/testthat/helper-private-data.R`) and skip without it.
- Commit only when asked; regenerate `man/` with `document()` before
  committing R changes; add a `NEWS.md` line for user-visible changes.
