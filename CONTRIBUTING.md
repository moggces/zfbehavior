# Contributing to zfbehavior

## Setup

- R (>= 4.1) with `devtools`, and the package dependencies:
  `devtools::install_deps(dependencies = TRUE)` (Rcurvep is on CRAN).
- [Quarto](https://quarto.org) (>= 1.4), needed by the Agent Skill render
  tests.

## Workflow

- Branch from `main`: `git switch -c <short-name>`; open a pull request to
  `main`. The GitHub check (Linux, macOS, Windows) must pass.
- After any roxygen change: `devtools::document()`.
- Tests: `devtools::test()` (one file: `devtools::test(filter = "bmc")`).
- Full check: `devtools::check(cran = FALSE)`. Target 0 errors, 0 warnings,
  0 notes. Delete stray `Rplots.pdf` (root and `tests/testthat/`) first.
- Add a line to `NEWS.md` for every user-visible change.
- Format only the files you change (`air format R/<file>.R`).

## Agent Skills (`inst/skills/`)

- Skill reports run `library(zfbehavior)` in a fresh R process, so they use
  the **installed** package. Before skill tests or renders, reinstall:
  `R CMD INSTALL --no-test-load .`. The render tests in
  `tests/testthat/test-skills.R` skip unless Quarto is on the PATH and the
  installed version equals the source version.
- Skills and templates may call **exported** functions only (checked by
  `test-skills.R`). Shared rules for all skills: `inst/skills/CONVENTIONS.md`.
- Never put scientific defaults into a skill without a reason recorded in
  its yml template; the agent asks the user for every scientific choice.

## Design rules

- Statistics use `endpoint_value_raw`; BMR/BMC use `endpoint_value_norm`.
- Dunnett p-values and Curvep BMC are Monte Carlo: regression tests use
  tolerances. The package never sets a `future` plan.
- A function is exported only if a skill or a clear user need calls it;
  single-panel plot helpers are internal (`*_pages()` is exported).
- Refer to non-exported functions in roxygen with backticks, not links.
- No `setwd()`, no `library()` in package code, no absolute paths.

## Tests that need private data

A few regression tests compare with outputs of the original analysis that
are not public. They skip unless `ZFBEHAVIOR_PRIVATE_DIR` points to that
data (see `tests/testthat/helper-private-data.R`).

## Versions and releases

- Between releases the version is `x.y.z.9000`.
- Patch release (`0.1.1`) for fixes, minor release (`0.2.0`) for features.
- Release: set the version in `DESCRIPTION`, finish `NEWS.md`, check, tag
  `vX.Y.Z`, create a GitHub release.
