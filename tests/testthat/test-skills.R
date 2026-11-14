# Agent Skills shipped in inst/skills/ are agent-facing workflow guides (the
# counterpart of vignettes). These tests keep them from silently drifting
# away from the package: valid frontmatter, only exported functions
# referenced, and (where possible) the template actually rendered on a fixture.

skills_root <- function() {
  path <- system.file("skills", package = "zfbehavior")
  testthat::skip_if(!nzchar(path), "inst/skills not found")
  path
}

skill_dirs <- function() {
  root <- skills_root()
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  dirs[file.exists(file.path(dirs, "SKILL.md"))]
}

read_frontmatter <- function(skill_md) {
  lines <- readLines(skill_md, warn = FALSE)
  delim <- which(lines == "---")
  testthat::expect_true(length(delim) >= 2 && delim[1] == 1)
  yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
}

test_that("shared CONVENTIONS.md exists", {
  expect_true(file.exists(file.path(skills_root(), "CONVENTIONS.md")))
})

test_that("every skill has valid frontmatter with name matching its directory", {
  dirs <- skill_dirs()
  expect_gt(length(dirs), 0)
  for (d in dirs) {
    fm <- read_frontmatter(file.path(d, "SKILL.md"))
    expect_identical(fm$name, basename(d))
    expect_match(fm$name, "^[a-z0-9]+(-[a-z0-9]+)*$")
    expect_true(is.character(fm$description) && nchar(fm$description) > 20)
    expect_lte(nchar(fm$description), 1024)
  }
})

test_that("skills and templates only reference exported zfbehavior functions", {
  exported <- getNamespaceExports("zfbehavior")
  internal <- setdiff(ls(asNamespace("zfbehavior"), all.names = TRUE), exported)
  for (d in skill_dirs()) {
    files <- list.files(d, pattern = "\\.(md|qmd|R)$", recursive = TRUE, full.names = TRUE)
    text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
    called <- unique(regmatches(text, gregexpr("[A-Za-z_][A-Za-z0-9._]*(?=\\()", text, perl = TRUE))[[1]])
    used_internal <- intersect(called, internal)
    expect_identical(used_internal, character(0), info = basename(d))
    ns_called <- unique(sub(
      "^zfbehavior::", "",
      regmatches(text, gregexpr("zfbehavior::[A-Za-z_][A-Za-z0-9._]*", text))[[1]]
    ))
    expect_identical(setdiff(ns_called, exported), character(0), info = basename(d))
  }
})

test_that("dataset-import-qc lists the exported functions it relies on", {
  d <- file.path(skills_root(), "dataset-import-qc")
  qmd <- paste(readLines(file.path(d, "templates", "dataset-import-qc.qmd")), collapse = "\n")
  for (fn in c("read_behavior_data", "parse_binning_to_time",
               "validate_behavior_data", "validate_combined_dataset")) {
    expect_true(fn %in% getNamespaceExports("zfbehavior"))
    expect_match(qmd, paste0(fn, "("), fixed = TRUE)
  }
})

test_that("dataset-import-qc template code chunks parse and yml template is valid", {
  d <- file.path(skills_root(), "dataset-import-qc", "templates")
  lines <- readLines(file.path(d, "dataset-import-qc.qmd"))
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  expect_gt(length(starts), 0)
  for (s in starts) {
    e <- min(ends[ends > s])
    expect_no_error(parse(text = lines[(s + 1):(e - 1)]))
  }
  y <- yaml::read_yaml(file.path(d, "dataset-import-qc.yml"))
  expect_true(all(c("run_label", "input", "column_mapping", "plate_qc",
                    "dataset_qc", "exclude_plates") %in% names(y)))
})

test_that("dataset-import-qc yml template and qmd params declare the same keys", {
  d <- file.path(skills_root(), "dataset-import-qc", "templates")
  y <- yaml::read_yaml(file.path(d, "dataset-import-qc.yml"))
  lines <- readLines(file.path(d, "dataset-import-qc.qmd"))
  delim <- which(lines == "---")
  front <- yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  expect_setequal(names(y), names(front$params))
})

# ---- execution test (like a vignette): render the template on a fixture ----

# CONVENTIONS.md section 2: every table in outputs/*.rds (top level or one
# level inside a list) is a tibble.
expect_rds_tables_are_tibbles <- function(run_dir) {
  files <- list.files(file.path(run_dir, "outputs"), pattern = "\\.rds$", full.names = TRUE)
  expect_gt(length(files), 0)
  for (f in files) {
    x <- readRDS(f)
    if (is.data.frame(x)) {
      expect_s3_class(x, "tbl_df")
    } else if (is.list(x)) {
      for (nm in names(x)) {
        if (is.data.frame(x[[nm]])) expect_true(inherits(x[[nm]], "tbl_df"), info = paste(basename(f), nm))
      }
    }
  }
}


skip_unless_render_possible <- function() {
  skip_on_cran()
  skip_if(Sys.which("quarto") == "", "quarto CLI not available")
  skip_if_not_installed("yaml")
  # The rendered document runs library(zfbehavior) in a fresh R process, so it
  # uses the INSTALLED package; skip unless it is the same version under test.
  installed <- tryCatch(as.character(utils::packageVersion("zfbehavior", lib.loc = .libPaths())),
                        error = function(e) NA_character_)
  skip_if(is.na(installed) || installed != as.character(utils::packageVersion("zfbehavior")),
          "installed zfbehavior differs from version under test")
}

# Write the 3-plate fixture as one CSV per plate under <project>/data/raw/fx
# (optionally dropping wells: drop = list(<plate> = c(<wells>))) and return a
# base yml for it. `drop_keep_well = TRUE` removes the KEEP_WELL_DATA column.
setup_import_fixture <- function(project, drop = list(), drop_keep_well = FALSE) {
  raw_dir <- file.path(project, "data", "raw", "fx")
  dir.create(raw_dir, recursive = TRUE)
  fx <- readRDS(zf_example_path())
  fx$time_end <- NULL
  if (drop_keep_well) fx$KEEP_WELL_DATA <- NULL
  col_types <- vapply(fx, function(x) {
    if (is.logical(x)) "logical" else if (is.numeric(x)) "numeric" else "text"
  }, "")
  for (pl in unique(fx$PLATE_NAME)) {
    d <- fx[fx$PLATE_NAME == pl, ]
    if (!is.null(drop[[pl]])) d <- d[!d$WELL_POSITION %in% drop[[pl]], ]
    utils::write.csv(d, file.path(raw_dir, paste0(pl, ".csv")), row.names = FALSE, na = "")
  }
  column_mapping <- list(
    plate_id = "PLATE_NAME", well_id = "WELL_POSITION", chemical_id = "BIOBIDE_ID",
    is_vehicle_control = "IS_VEHICLE", concentration = "CONCENTRATION",
    concentration_unit = "CONCENTRATION_UNIT", response_value = "DISTANCE_MOVED",
    keep_well = "KEEP_WELL_DATA"
  )
  if (drop_keep_well) column_mapping$keep_well <- NULL
  list(
    run_label = "fx",
    project_root = "../../..",
    input = list(files = list(), pattern = "data/raw/fx/*.csv",
                 format = "one_file_per_plate", sheets = list()),
    column_mapping = column_mapping,
    optional_roles = if (drop_keep_well) list("time_value", "keep_well") else list("time_value"),
    derive_time_value_from_binning = TRUE,
    col_types = as.list(unname(col_types)),
    plate_qc = list(expected_row_count = 4800L, expected_vc_wells = 16L,
                    expected_dose_wells = 16L),
    dataset_qc = list(unique_id_cols = list("chemical_id", "NIH_BLINDED_ID"),
                      expected_n_distinct = list(CHEMICAL_NAME = 2L)),
    exclude_plates = list()
  )
}

well_entry <- function(plate_id, well_id, reason = "test") {
  list(plate_id = plate_id, well_id = well_id, reason = reason)
}

render_import_qc <- function(project, run, yml) {
  run_dir <- file.path(project, "analysis", "dataset-import-qc", run)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(
    file.path(skills_root(), "dataset-import-qc", "templates", "dataset-import-qc.qmd"),
    run_dir, overwrite = TRUE
  )
  yaml::write_yaml(yml, file.path(run_dir, "dataset-import-qc.yml"))
  res <- suppressWarnings(withr::with_dir(run_dir, system2(
    "quarto",
    c("render", "dataset-import-qc.qmd", "--execute-params", "dataset-import-qc.yml"),
    stdout = TRUE, stderr = TRUE
  )))
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    stop("quarto render failed:\n", paste(utils::tail(res, 30), collapse = "\n"))
  }
  out <- file.path(run_dir, "outputs")
  list(
    run_dir = run_dir,
    manifest = yaml::read_yaml(file.path(out, paste0("dataset-import-qc__", run, "__manifest.yml"))),
    combined = file.path(out, paste0("dataset-import-qc__", run, "__combined.rds"))
  )
}

test_that("dataset-import-qc template renders end-to-end on the fixture (two-pass review)", {
  skip_unless_render_possible()

  project <- withr::local_tempdir()
  yml <- setup_import_fixture(project, drop = list(Plate_47 = "A1"))

  # Pass 1: Plate_47 fails per-plate QC -> needs_review, no combined dataset.
  r1 <- render_import_qc(project, "fx", yml)
  expect_identical(r1$manifest$status, "needs_review")
  expect_match(r1$manifest$status_reason, "Plate_47")
  expect_false(file.exists(r1$combined))
  expect_length(r1$manifest$inputs, 3)

  # Pass 2: user records the exclusion with a reason -> complete.
  yml$exclude_plates <- list(list(plate_id = "Plate_47", reason = "test: well A1 missing"))
  r2 <- render_import_qc(project, "fx", yml)
  expect_identical(r2$manifest$status, "complete")
  expect_true(file.exists(r2$combined))
  combined <- readRDS(r2$combined)
  expect_identical(nrow(combined), 9600L)
  expect_setequal(unique(combined$plate_id), c("Plate_01", "Plate_62"))
  expect_true(all(required_canonical_roles() %in% names(combined)))
  expect_identical(r2$manifest$excluded_plates[[1]]$plate_id, "Plate_47")
  expect_identical(r2$manifest$well_contract, "single_larva_wells_only")
  expect_rds_tables_are_tibbles(r2$run_dir)
  expect_identical(r2$manifest$concentration_units[[1]]$unit, "uM")
  expect_identical(r2$manifest$concentration_units[[1]]$n_plates, 2L)
  expect_length(r2$manifest$plates_mixed_concentration_units, 0)
  expect_true(file.exists(file.path(r2$run_dir, "outputs", "dataset-import-qc__fx__well-curation.rds")))
  out_md5 <- vapply(r2$manifest$outputs, `[[`, "", "md5")
  out_paths <- file.path(r2$run_dir, vapply(r2$manifest$outputs, `[[`, "", "path"))
  expect_identical(unname(tools::md5sum(out_paths)), out_md5)
})

test_that("dataset-import-qc accepts plates short only by declared provider-removed wells", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  # Plate_47: provider removed VC well A1 and dose well A3; Plate_01: B4
  # missing but NOT declared -> must still need review.
  yml <- setup_import_fixture(project, drop = list(Plate_47 = c("A1", "A3"), Plate_01 = "B4"))
  yml$provider_removed_wells <- list(
    well_entry("Plate_47", "A1", "no larva (provider)"),
    well_entry("Plate_47", "A3", "2 larvae (provider)")
  )
  r1 <- render_import_qc(project, "fx", yml)
  expect_identical(r1$manifest$status, "needs_review")
  expect_match(r1$manifest$status_reason, "Plate_01")
  expect_no_match(r1$manifest$status_reason, "Plate_47")

  # Wrong declaration (wrong well count) must not be accepted either.
  yml$provider_removed_wells <- list(well_entry("Plate_47", "A1"))
  yml$exclude_plates <- list(list(plate_id = "Plate_01", reason = "test: undeclared shortfall"))
  r2 <- render_import_qc(project, "fx", yml)
  expect_identical(r2$manifest$status, "needs_review")
  expect_match(r2$manifest$status_reason, "Plate_47")

  yml$provider_removed_wells <- list(
    well_entry("Plate_47", "A1", "no larva (provider)"),
    well_entry("Plate_47", "A3", "2 larvae (provider)")
  )
  r3 <- render_import_qc(project, "fx", yml)
  expect_identical(r3$manifest$status, "complete")
  combined <- readRDS(r3$combined)
  expect_identical(nrow(combined), 4800L + 4700L)
  expect_identical(r3$manifest$summary$n_plates_accepted_with_provider_removed_wells, 1L)
  cur <- readRDS(file.path(r3$run_dir, "outputs", "dataset-import-qc__fx__well-curation.rds"))
  expect_true(all(cur$provider_removed_check$accepted))
})

test_that("dataset-import-qc comment flags require a decision and remove_wells drops wells", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  yml <- setup_import_fixture(project)
  yml$dataset_qc$expected_n_distinct <- list(CHEMICAL_NAME = 3L)
  yml$flag_comment_column <- "COMMENTS"
  yml$flag_comment_pattern <- "larva|empty"

  r1 <- render_import_qc(project, "fx", yml)
  expect_identical(r1$manifest$status, "needs_review")
  expect_match(r1$manifest$status_reason, "Plate_62/B2")
  expect_false(file.exists(r1$combined))

  # B2 "2 larvae" and three "empty" wells removed; B4 "empty" confirmed kept.
  yml$remove_wells <- list(
    well_entry("Plate_62", "B2", "2 larvae"),
    well_entry("Plate_62", "E12", "empty"),
    well_entry("Plate_62", "F11", "empty"),
    well_entry("Plate_62", "H12", "empty")
  )
  yml$keep_flagged_wells <- list(well_entry("Plate_62", "B4", "test: confirmed one larva"))
  r2 <- render_import_qc(project, "fx", yml)
  expect_identical(r2$manifest$status, "complete")
  combined <- readRDS(r2$combined)
  expect_identical(nrow(combined), 3L * 4800L - 4L * 50L)
  expect_false(any(combined$plate_id == "Plate_62" &
                     combined$well_id %in% c("B2", "E12", "F11", "H12")))
  expect_true(any(combined$plate_id == "Plate_62" & combined$well_id == "B4"))
  expect_identical(r2$manifest$summary$n_wells_removed, 4L)
  cur <- readRDS(file.path(r2$run_dir, "outputs", "dataset-import-qc__fx__well-curation.rds"))
  p62 <- cur$well_counts_per_plate[cur$well_counts_per_plate$plate_id == "Plate_62", ]
  expect_identical(p62$n_wells_delivered, 96L)
  expect_identical(p62$n_wells_output, 92L)

  # A remove_wells entry for a well that does not exist is a hard error.
  yml$remove_wells <- c(yml$remove_wells, list(well_entry("Plate_62", "Z99")))
  expect_error(render_import_qc(project, "fx", yml), "not in the data: Plate_62/Z99")
})

test_that("dataset-import-qc derives keep_well from an explicit false_wells list", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  yml <- setup_import_fixture(project, drop_keep_well = TRUE)
  yml$dataset_qc$expected_n_distinct <- list(CHEMICAL_NAME = 3L)
  yml$keep_well <- list(
    source = "derived",
    false_wells = list(
      well_entry("Plate_01", "A3", "dead"),
      well_entry("Plate_62", "C5", "edema")
    )
  )
  r <- render_import_qc(project, "fx", yml)
  expect_identical(r$manifest$status, "complete")
  expect_identical(r$manifest$keep_well_source, "derived")
  combined <- readRDS(r$combined)
  wl <- unique(combined[, c("plate_id", "well_id", "keep_well")])
  expect_identical(sum(!wl$keep_well), 2L)
  expect_false(wl$keep_well[wl$plate_id == "Plate_01" & wl$well_id == "A3"])
})

# ---- raw-response-plots ------------------------------------------------------

test_that("raw-response-plots template chunks parse and yml/qmd params match", {
  d <- file.path(skills_root(), "raw-response-plots", "templates")
  lines <- readLines(file.path(d, "raw-response-plots.qmd"))
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  expect_gt(length(starts), 0)
  for (s in starts) {
    e <- min(ends[ends > s])
    expect_no_error(parse(text = lines[(s + 1):(e - 1)]))
  }
  y <- yaml::read_yaml(file.path(d, "raw-response-plots.yml"))
  delim <- which(lines == "---")
  front <- yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  expect_setequal(names(y), names(front$params))
  qmd <- paste(lines, collapse = "\n")
  for (fn in c("count_wells_by_dose", "plot_raw_responses_pages", "raw_plot_input_cols")) {
    expect_true(fn %in% getNamespaceExports("zfbehavior"))
    expect_match(qmd, paste0(fn, "("), fixed = TRUE)
  }
})

# Text runs drawn in a PDF written by R's pdf() device: one row per text run
# with page (content-stream order), x, y (text matrix), and text. Handles
# both "(..) Tj" and kerned "[(..) n (..)] TJ" runs.
pdf_text_runs <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  find_all <- function(pattern) {
    pat <- charToRaw(pattern)
    n <- length(pat)
    cand <- which(raw == pat[1])
    cand[vapply(cand, function(i) i + n - 1 <= length(raw) && all(raw[i:(i + n - 1)] == pat), TRUE)]
  }
  ends <- find_all("endstream")
  starts <- find_all("stream\n")
  starts <- starts[!(starts - 3L) %in% ends] + 7L
  unescape <- function(x) gsub("\\\\(.)", "\\1", x)
  rows <- list()
  page <- 0L
  for (s0 in starts) {
    e0 <- ends[ends > s0][1] - 1L
    if (is.na(e0)) next
    dec <- tryCatch(memDecompress(raw[s0:e0], "gzip"), error = function(e) NULL)
    if (is.null(dec)) next
    t <- rawToChar(dec[dec != as.raw(0)])
    Encoding(t) <- "bytes"
    page <- page + 1L
    str <- "\\((?:[^()\\\\]|\\\\.)*\\)"
    re <- paste0("Tm (", str, " Tj|\\[(?:", str, "|[^]()])*\\] TJ)")
    pos <- gregexpr(paste0("([-0-9.]+) ([-0-9.]+) ", re), t, perl = TRUE, useBytes = TRUE)[[1]]
    if (pos[1] == -1) next
    m <- regmatches(t, list(pos))[[1]]
    for (x in m) {
      xy <- as.numeric(strsplit(x, " ", fixed = TRUE)[[1]][1:2])
      body <- sub("^[-0-9.]+ [-0-9.]+ Tm ", "", x, useBytes = TRUE)
      parts <- regmatches(body, gregexpr("\\((?:[^()\\\\]|\\\\.)*\\)", body, perl = TRUE, useBytes = TRUE))[[1]]
      txt <- paste(unescape(substr(parts, 2, nchar(parts) - 1)), collapse = "")
      rows[[length(rows) + 1]] <- data.frame(page = page, x = xy[1], y = xy[2], text = txt,
                                             stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

# Panel titles in marrangeGrob() order: page, then column-major within the
# page grid (gridExtra fills panels down each column first: 1 3 5 / 2 4 6).
# Titles may be wrapped onto several lines (title_wrap_width); locate each by
# its first wrapped line and return the full titles in panel order.
pdf_panel_titles <- function(path, titles, wrap_width = NULL) {
  first_line <- if (is.null(wrap_width)) titles else
    vapply(titles, function(t) stringr::str_wrap(t, wrap_width)[1] |>
      strsplit("\n", fixed = TRUE) |> (\(x) x[[1]][1])(), "")
  r <- pdf_text_runs(path)
  r <- r[r$text %in% first_line, , drop = FALSE]
  r <- r[order(r$page, round(r$x), -round(r$y)), , drop = FALSE]
  unname(titles[match(unique(r$text), first_line)])
}

render_skill <- function(project, skill, run, yml) {
  run_dir <- file.path(project, "analysis", skill, run)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(skills_root(), skill, "templates", paste0(skill, ".qmd")),
            run_dir, overwrite = TRUE)
  yaml::write_yaml(yml, file.path(run_dir, paste0(skill, ".yml")))
  res <- suppressWarnings(withr::with_dir(run_dir, system2(
    "quarto",
    c("render", paste0(skill, ".qmd"), "--execute-params", paste0(skill, ".yml")),
    stdout = TRUE, stderr = TRUE
  )))
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    stop("quarto render failed:\n", paste(utils::tail(res, 30), collapse = "\n"))
  }
  out <- file.path(run_dir, "outputs")
  list(
    run_dir = run_dir,
    log = res,
    manifest = yaml::read_yaml(file.path(out, paste0(skill, "__", run, "__manifest.yml"))),
    out = function(object, ext) file.path(out, paste0(skill, "__", run, "__", object, ".", ext))
  )
}

test_that("raw-response-plots renders on a complete fixture import and blocks otherwise", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  imp_yml <- setup_import_fixture(project, drop = list(Plate_47 = "A1"))
  imp_yml$dataset_qc$expected_n_distinct <- list(CHEMICAL_NAME = 3L)
  imp_yml$flag_comment_column <- "COMMENTS"
  imp_yml$flag_comment_pattern <- "larva|empty"
  # Pass 1 of the import is needs_review (Plate_47 short; B2 flagged).
  imp1 <- render_import_qc(project, "fx", imp_yml)
  expect_identical(imp1$manifest$status, "needs_review")

  plot_yml <- list(
    run_label = "fx",
    project_root = "../../..",
    upstream = list("analysis/dataset-import-qc/fx"),
    segments = list(L1 = list(from = 1, to = 10), D1 = list(from = 11, to = 20),
                    L2 = list(from = 21, to = 30), D2 = list(from = 31, to = 50)),
    segment_style = NULL,
    response_label = "distance moved (mm)",
    group_cols = list("plate_id"),
    title_cols = list("CHEMICAL_NAME", "chemical_id", "plate_id"),
    summary_type = "median",
    missing_value_policy = "impute_zero",
    wells = "vehicle_control",
    panel_order = "alphabetical",
    pdf_width = 15,
    pdf_height = 10
  )
  # Blocked: upstream not complete.
  expect_error(render_skill(project, "raw-response-plots", "fx", plot_yml), "not `complete`")

  # Complete the import: provider removed Plate_47/A1; B2 (2 larvae) removed,
  # the "empty" wells kept as one-larva wells for this test.
  imp_yml$provider_removed_wells <- list(well_entry("Plate_47", "A1", "no larva (provider)"))
  imp_yml$remove_wells <- list(well_entry("Plate_62", "B2", "2 larvae"))
  imp_yml$keep_flagged_wells <- lapply(c("B4", "E12", "F11", "H12"), function(w) {
    well_entry("Plate_62", w, "test: treat as one larva")
  })
  imp2 <- render_import_qc(project, "fx", imp_yml)
  expect_identical(imp2$manifest$status, "complete")

  r <- render_skill(project, "raw-response-plots", "fx", plot_yml)
  expect_identical(r$manifest$status, "complete")
  expect_rds_tables_are_tibbles(r$run_dir)
  expect_identical(r$manifest$summary$n_panels, 3L)
  expect_identical(r$manifest$summary$n_pages_summary, 1L)
  expect_identical(r$manifest$summary$n_pages_well, 1L)
  for (obj in c("summary", "well")) expect_true(file.exists(r$out(obj, "pdf")))

  # Default panel_order = alphabetical (case-insensitive) by panel title.
  titles <- c(
    "2-Ethylhexyl 2,3,4,5- tetrabromobenzoate (TBB)@PE-4190@Plate_62",
    "Pyrasulfotole (AE 0317309)@PE-4189@Plate_47",
    "Sulfisoxazole@PE-4188@Plate_01"
  )
  for (obj in c("summary", "well")) {
    expect_identical(pdf_panel_titles(r$out(obj, "pdf"), titles, 60), titles, info = obj)
  }

  wc <- readRDS(r$out("well-counts", "rds"))
  vc <- wc[wc$is_vehicle_control, ]
  expect_identical(vc$n_total_wells[vc$plate_id == "Plate_47"], 15L)
  expect_identical(vc$n_total_wells[vc$plate_id == "Plate_62"], 15L)
  expect_identical(vc$n_total_wells[vc$plate_id == "Plate_01"], 16L)
  vu <- readRDS(r$out("vc-usage", "rds"))
  expect_true(all(c("CHEMICAL_NAME", "chemical_id", "pct_used") %in% names(vu)))
  expect_false(is.unsorted(vu$pct_used))

  out_md5 <- vapply(r$manifest$outputs, `[[`, "", "md5")
  out_paths <- file.path(r$run_dir, vapply(r$manifest$outputs, `[[`, "", "path"))
  expect_identical(unname(tools::md5sum(out_paths)), out_md5)
  expect_true(r$manifest$inputs[[1]]$md5_matches_upstream_manifest)

  # panel_order = group_key keeps the group-key (plate_id text) order.
  gk <- plot_yml
  gk$run_label <- "gk"
  gk$panel_order <- "group_key"
  r_gk <- render_skill(project, "raw-response-plots", "gk", gk)
  expect_identical(pdf_panel_titles(r_gk$out("summary", "pdf"), titles, 60), titles[c(3, 2, 1)])

  # A segment time outside the data is a parameter error.
  bad <- plot_yml
  bad$run_label <- "bad"
  bad$segments$D2$to <- 60
  expect_error(render_skill(project, "raw-response-plots", "bad", bad), "not present in the data")
})

test_that("dataset-import-qc flags plates with too few kept vehicle-control wells", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  yml <- setup_import_fixture(project)
  yml$dataset_qc$expected_n_distinct <- list(CHEMICAL_NAME = 3L)

  # Threshold 90%: Plate_47 and Plate_62 have 14/16 kept VC (87.5%) -> flagged;
  # Plate_01 has 15/16 (93.75%) -> passes.
  yml$min_kept_vc_pct <- 90
  r1 <- render_import_qc(project, "fx", yml)
  expect_identical(r1$manifest$status, "needs_review")
  expect_match(r1$manifest$status_reason, "Plate_47, Plate_62")
  expect_false(file.exists(r1$combined))

  yml$accept_low_vc_plates <- list(list(plate_id = "Plate_47", reason = "test: accepted"))
  yml$exclude_plates <- list(list(plate_id = "Plate_62", reason = "test: low VC"))
  yml$dataset_qc$expected_n_distinct <- list(CHEMICAL_NAME = 2L)
  r2 <- render_import_qc(project, "fx", yml)
  expect_identical(r2$manifest$status, "complete")
  expect_equal(r2$manifest$min_kept_vc_pct, 90)
  expect_identical(r2$manifest$accepted_low_vc_plates[[1]]$plate_id, "Plate_47")
  cur <- readRDS(file.path(r2$run_dir, "outputs", "dataset-import-qc__fx__well-curation.rds"))
  expect_equal(cur$vc_usability$pct_vc_kept[cur$vc_usability$plate_id == "Plate_47"], 87.5)
})

test_that("dataset-import-qc always flags a plate with no kept vehicle-control well", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  yml <- setup_import_fixture(project, drop_keep_well = TRUE)
  yml$dataset_qc$expected_n_distinct <- list(CHEMICAL_NAME = 3L)
  vc_wells <- c(paste0(LETTERS[1:8], 1), paste0(LETTERS[1:8], 2))
  yml$keep_well <- list(
    source = "derived",
    false_wells = lapply(vc_wells, function(w) well_entry("Plate_01", w, "test: all VC dead"))
  )
  r1 <- render_import_qc(project, "fx", yml) # min_kept_vc_pct is null
  expect_identical(r1$manifest$status, "needs_review")
  expect_match(r1$manifest$status_reason, "Plate_01")

  # Cannot be accepted, only excluded.
  yml$accept_low_vc_plates <- list(list(plate_id = "Plate_01", reason = "test"))
  expect_error(render_import_qc(project, "fx", yml), "cannot be accepted")

  yml$accept_low_vc_plates <- list()
  yml$exclude_plates <- list(list(plate_id = "Plate_01", reason = "test: no usable VC"))
  yml$dataset_qc$expected_n_distinct <- list(CHEMICAL_NAME = 2L)
  r2 <- render_import_qc(project, "fx", yml)
  expect_identical(r2$manifest$status, "complete")
})

# ---- interplate-vc-qc --------------------------------------------------------

test_that("interplate-vc-qc template chunks parse and yml/qmd params match", {
  d <- file.path(skills_root(), "interplate-vc-qc", "templates")
  lines <- readLines(file.path(d, "interplate-vc-qc.qmd"))
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  expect_gt(length(starts), 0)
  for (s in starts) {
    e <- min(ends[ends > s])
    expect_no_error(parse(text = lines[(s + 1):(e - 1)]))
  }
  y <- yaml::read_yaml(file.path(d, "interplate-vc-qc.yml"))
  delim <- which(lines == "---")
  front <- yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  expect_setequal(names(y), names(front$params))
  qmd <- paste(lines, collapse = "\n")
  for (fn in c("calculate_plate_reference_pattern", "fit_plate_pca", "detect_plate_pca_outliers",
               "plot_plate_pca_ellipse", "plot_plate_reference_overlay", "plot_raw_responses_pages")) {
    expect_true(fn %in% getNamespaceExports("zfbehavior"))
    expect_match(qmd, paste0(fn, "("), fixed = TRUE)
  }
  expect_no_match(qmd, "mahalanobis_npc")
})

# 12 synthetic plates (4 copies of each fixture plate with multiplicative
# noise); plate S12's responses are scaled 6x so it is a clear outlier.
setup_interplate_fixture <- function(project) {
  raw_dir <- file.path(project, "data", "raw", "ip")
  dir.create(raw_dir, recursive = TRUE)
  fx <- readRDS(zf_example_path())
  fx$time_end <- NULL
  col_types <- vapply(fx, function(x) {
    if (is.logical(x)) "logical" else if (is.numeric(x)) "numeric" else "text"
  }, "")
  src <- unique(fx$PLATE_NAME)
  set.seed(1)
  for (i in 1:12) {
    d <- fx[fx$PLATE_NAME == src[(i - 1) %% 3 + 1], ]
    pid <- sprintf("S%02d", i)
    d$PLATE_NAME <- pid
    d$BIOBIDE_ID <- paste0("PE-", pid)
    d$NIH_BLINDED_ID <- paste0("B", pid)
    d$CHEMICAL_NAME <- paste("Chem", pid)
    d$DISTANCE_MOVED <- d$DISTANCE_MOVED * stats::runif(nrow(d), 0.8, 1.2)
    if (pid == "S12") d$DISTANCE_MOVED <- d$DISTANCE_MOVED * 6
    utils::write.csv(d, file.path(raw_dir, paste0(pid, ".csv")), row.names = FALSE, na = "")
  }
  list(
    run_label = "ip",
    project_root = "../../..",
    input = list(files = list(), pattern = "data/raw/ip/*.csv",
                 format = "one_file_per_plate", sheets = list()),
    column_mapping = list(
      plate_id = "PLATE_NAME", well_id = "WELL_POSITION", chemical_id = "BIOBIDE_ID",
      is_vehicle_control = "IS_VEHICLE", concentration = "CONCENTRATION",
      concentration_unit = "CONCENTRATION_UNIT", response_value = "DISTANCE_MOVED",
      keep_well = "KEEP_WELL_DATA"
    ),
    optional_roles = list("time_value"),
    derive_time_value_from_binning = TRUE,
    col_types = as.list(unname(col_types)),
    plate_qc = list(expected_row_count = 4800L, expected_vc_wells = 16L, expected_dose_wells = 16L),
    dataset_qc = list(unique_id_cols = list("chemical_id", "NIH_BLINDED_ID"),
                      expected_n_distinct = list(CHEMICAL_NAME = 12L)),
    exclude_plates = list()
  )
}

test_that("interplate-vc-qc flags outlier plates and requires a decision for each", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  imp <- render_import_qc(project, "ip", setup_interplate_fixture(project))
  expect_identical(imp$manifest$status, "complete")

  yml <- list(
    run_label = "ip",
    project_root = "../../..",
    upstream = list("analysis/dataset-import-qc/ip"),
    cohort_note = "test: 12 synthetic plates",
    segments = list(L1 = list(from = 1, to = 10), D1 = list(from = 11, to = 20),
                    L2 = list(from = 21, to = 30), D2 = list(from = 31, to = 50)),
    segment_style = NULL,
    response_label = "distance moved (mm)",
    title_cols = list("CHEMICAL_NAME", "chemical_id", "plate_id"),
    summary_type = "median",
    conf_level = 0.95,
    flag_method = "ellipse_normal",
    exclude_plates = list(),
    keep_plates = list(),
    pdf_width = 15,
    pdf_height = 10
  )
  r1 <- render_skill(project, "interplate-vc-qc", "ip", yml)
  expect_identical(r1$manifest$status, "needs_review")
  flagged <- unlist(r1$manifest$flagged_plates)
  expect_true("S12" %in% flagged)
  expect_match(r1$manifest$status_reason, "S12")
  expect_false(file.exists(r1$out("plate-decisions", "rds")))
  expect_identical(r1$manifest$summary$n_qc_pdf_pages, 2L + as.integer(ceiling(length(flagged) / 4)))
  expect_true(file.exists(r1$out("qc", "pdf")))

  # Same results as calling the package functions directly.
  comb <- readRDS(imp$combined)
  ref <- calculate_plate_reference_pattern(comb[comb$keep_well, ])
  pca <- fit_plate_pca(ref, n_pc = 2)
  out_n <- detect_plate_pca_outliers(pca, method = "ellipse_normal")
  expect_setequal(flagged, out_n$plate_id[out_n$is_outlier])
  expect_equal(readRDS(r1$out("pca", "rds"))$scores, pca$scores)

  # Decide: exclude S12, keep any other flagged plate.
  yml$exclude_plates <- list(list(plate_id = "S12", reason = "test: VC traces 6x"))
  yml$keep_plates <- lapply(setdiff(flagged, "S12"), function(x) list(plate_id = x, reason = "test: keep"))
  r2 <- render_skill(project, "interplate-vc-qc", "ip", yml)
  expect_identical(r2$manifest$status, "complete")
  dec <- readRDS(r2$out("plate-decisions", "rds"))
  expect_identical(dec$plate_id[dec$excluded], "S12")
  expect_identical(r2$manifest$excluded_plates[[1]]$plate_id, "S12")
  expect_identical(r2$manifest$method$flag_method, "ellipse_normal")
  expect_rds_tables_are_tibbles(r2$run_dir)
  out_md5 <- vapply(r2$manifest$outputs, `[[`, "", "md5")
  out_paths <- file.path(r2$run_dir, vapply(r2$manifest$outputs, `[[`, "", "path"))
  expect_identical(unname(tools::md5sum(out_paths)), out_md5)

  # Decision pages show the flagged plates' titles.
  titles <- dec$plate_label[dec$flagged]
  expect_setequal(pdf_panel_titles(r2$out("qc", "pdf"), titles), titles)

  # Unknown plate in a decision list is a parameter error.
  bad <- yml
  bad$run_label <- "bad"
  bad$keep_plates <- c(bad$keep_plates, list(list(plate_id = "NOPE", reason = "x")))
  expect_error(render_skill(project, "interplate-vc-qc", "bad", bad), "unknown plate_id")
})

# ---- devtox-endpoints --------------------------------------------------------

test_that("devtox-endpoints template chunks parse and yml/qmd params match", {
  d <- file.path(skills_root(), "devtox-endpoints", "templates")
  lines <- readLines(file.path(d, "devtox-endpoints.qmd"))
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  expect_gt(length(starts), 0)
  for (s in starts) {
    e <- min(ends[ends > s])
    expect_no_error(parse(text = lines[(s + 1):(e - 1)]))
  }
  y <- yaml::read_yaml(file.path(d, "devtox-endpoints.yml"))
  delim <- which(lines == "---")
  front <- yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  expect_setequal(names(y), names(front$params))
  qmd <- paste(lines, collapse = "\n")
  for (fn in c("devtox_input_cols", "calculate_devtox_endpoints", "plot_devtox_dose_response_pages")) {
    expect_true(fn %in% getNamespaceExports("zfbehavior"))
    expect_match(qmd, paste0(fn, "("), fixed = TRUE)
  }
})

# 4 plates: Plate_01 copied as P1/P2 (same DTXSID = pooled chemical, P2 with
# extra affected larvae), Plate_47 as P3, Plate_62 as P4 (in ug/mL).
setup_devtox_fixture <- function(project) {
  raw_dir <- file.path(project, "data", "raw", "dt")
  dir.create(raw_dir, recursive = TRUE)
  fx <- readRDS(zf_example_path())
  fx$time_end <- NULL
  col_types <- vapply(fx, function(x) {
    if (is.logical(x)) "logical" else if (is.numeric(x)) "numeric" else "text"
  }, "")
  src <- c(P1 = "Plate_01", P2 = "Plate_01", P3 = "Plate_47", P4 = "Plate_62")
  for (pid in names(src)) {
    d <- fx[fx$PLATE_NAME == src[[pid]], ]
    d$PLATE_NAME <- pid
    d$BIOBIDE_ID <- paste0("PE-", pid)
    d$NIH_BLINDED_ID <- paste0("B", pid)
    if (pid == "P2") {
      # 3 more dead larvae at the top concentration (wells kept on P1).
      top <- max(d$CONCENTRATION)
      wells <- utils::head(sort(unique(d$WELL_POSITION[d$CONCENTRATION == top & d$KEEP_WELL_DATA])), 3)
      d$KEEP_WELL_DATA[d$WELL_POSITION %in% wells] <- FALSE
    }
    if (pid == "P4") d$CONCENTRATION_UNIT <- "ug/mL"
    utils::write.csv(d, file.path(raw_dir, paste0(pid, ".csv")), row.names = FALSE, na = "")
  }
  list(
    run_label = "dt",
    project_root = "../../..",
    input = list(files = list(), pattern = "data/raw/dt/*.csv",
                 format = "one_file_per_plate", sheets = list()),
    column_mapping = list(
      plate_id = "PLATE_NAME", well_id = "WELL_POSITION", chemical_id = "BIOBIDE_ID",
      is_vehicle_control = "IS_VEHICLE", concentration = "CONCENTRATION",
      concentration_unit = "CONCENTRATION_UNIT", response_value = "DISTANCE_MOVED",
      keep_well = "KEEP_WELL_DATA"
    ),
    optional_roles = list("time_value"),
    derive_time_value_from_binning = TRUE,
    col_types = as.list(unname(col_types)),
    plate_qc = list(expected_row_count = 4800L, expected_vc_wells = 16L, expected_dose_wells = 16L),
    dataset_qc = list(unique_id_cols = list("chemical_id", "NIH_BLINDED_ID"),
                      expected_n_distinct = list(CHEMICAL_NAME = 3L)),
    exclude_plates = list()
  )
}

devtox_yml <- function(run, layout = "per_plate", upstream = list("analysis/dataset-import-qc/dt"),
                       title_cols = list()) {
  list(
    run_label = run, project_root = "../../..", upstream = upstream,
    layout = layout, chemical_col = "DTXSID", title_cols = title_cols,
    panel_order = "alphabetical", nrow = 1L, ncol = 5L, pdf_width = 15, pdf_height = 5
  )
}

test_that("devtox-endpoints renders all layouts and matches calculate_devtox_endpoints()", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  imp <- render_import_qc(project, "dt", setup_devtox_fixture(project))
  expect_identical(imp$manifest$status, "complete")
  comb <- readRDS(imp$combined)

  # per_plate: one panel per plate, endpoints identical to the package call.
  r1 <- render_skill(project, "devtox-endpoints", "pp",
                     devtox_yml("pp", title_cols = list("CHEMICAL_NAME", "plate_id")))
  expect_identical(r1$manifest$status, "complete")
  e1 <- readRDS(r1$out("endpoints", "rds"))
  direct <- calculate_devtox_endpoints(comb, group_cols = c("plate_id", "CHEMICAL_NAME"))
  expect_identical(as.data.frame(e1), as.data.frame(direct))
  expect_identical(r1$manifest$summary$n_panels, 4L)
  expect_identical(r1$manifest$summary$n_pdf_pages, 1L)
  expect_false(file.exists(r1$out("endpoints-pooled", "rds")))
  expect_setequal(unlist(r1$manifest$summary$concentration_units), c("uM", "ug/mL"))
  # VC dose group per plate included.
  expect_identical(sum(e1$is_vehicle_control), 4L)
  # Panel titles in alphabetical order.
  titles <- unique(paste(e1$CHEMICAL_NAME, e1$plate_id, sep = " | "))
  expect_identical(pdf_panel_titles(r1$out("dose-response", "pdf"), titles, 40), titles[order(tolower(titles))])
  out_md5 <- vapply(r1$manifest$outputs, `[[`, "", "md5")
  out_paths <- file.path(r1$run_dir, vapply(r1$manifest$outputs, `[[`, "", "path"))
  expect_identical(unname(tools::md5sum(out_paths)), out_md5)

  # by_chemical_plates: one panel per DTXSID (3), endpoints still per plate.
  r2 <- render_skill(project, "devtox-endpoints", "bcp", devtox_yml("bcp", "by_chemical_plates"))
  expect_identical(r2$manifest$summary$n_panels, 3L)
  expect_true(all(c("plate_id", "DTXSID") %in% names(readRDS(r2$out("endpoints", "rds")))))

  # by_chemical_pooled: P1 + P2 counted together.
  r3 <- render_skill(project, "devtox-endpoints", "pool", devtox_yml("pool", "by_chemical_pooled"))
  expect_identical(r3$manifest$status, "complete")
  expect_rds_tables_are_tibbles(r3$run_dir)
  pooled <- readRDS(r3$out("endpoints-pooled", "rds"))
  e3 <- readRDS(r3$out("endpoints", "rds"))
  dtx <- unique(comb$DTXSID[comb$plate_id == "P1"])
  sum_pp <- stats::aggregate(cbind(N, n_affected) ~ concentration,
                             data = e3[e3$DTXSID == dtx, ], FUN = sum)
  pl <- pooled[pooled$DTXSID == dtx, ]
  expect_identical(pl$N, sum_pp$N[match(pl$concentration, sum_pp$concentration)])
  expect_identical(pl$n_affected, sum_pp$n_affected[match(pl$concentration, sum_pp$concentration)])
  top <- max(pl$concentration)
  p1 <- e3[e3$plate_id == "P1" & e3$concentration == top, ]
  p2 <- e3[e3$plate_id == "P2" & e3$concentration == top, ]
  expect_identical(p2$n_affected - p1$n_affected, 3L)
})

test_that("devtox-endpoints drops QC-excluded plates and blocks invalid upstream", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  imp <- render_import_qc(project, "dt", setup_devtox_fixture(project))

  # A complete QC run of this import that excludes P3 (manifest written by hand:
  # only status/skill/upstream/excluded_plates are read).
  qc_dir <- file.path(project, "analysis", "interplate-vc-qc", "dt", "outputs")
  dir.create(qc_dir, recursive = TRUE)
  qc_manifest <- list(
    skill = "interplate-vc-qc", run_label = "dt", status = "complete", status_reason = "",
    rendered_at = "2026-01-01T00:00:00",
    upstream = list(list(run = "analysis/dataset-import-qc/dt")),
    excluded_plates = list(list(plate_id = "P3", reason = "test"))
  )
  yaml::write_yaml(qc_manifest, file.path(qc_dir, "interplate-vc-qc__dt__manifest.yml"))

  up <- list("analysis/dataset-import-qc/dt", "analysis/interplate-vc-qc/dt")
  r <- render_skill(project, "devtox-endpoints", "qc", devtox_yml("qc", upstream = up))
  expect_identical(r$manifest$status, "complete")
  e <- readRDS(r$out("endpoints", "rds"))
  expect_setequal(unique(e$plate_id), c("P1", "P2", "P4"))
  expect_identical(r$manifest$plates_excluded_by_qc[[1]]$plate_id, "P3")
  expect_length(r$manifest$upstream, 2)

  # QC run not complete -> blocked.
  qc_manifest$status <- "needs_review"
  yaml::write_yaml(qc_manifest, file.path(qc_dir, "interplate-vc-qc__dt__manifest.yml"))
  expect_error(render_skill(project, "devtox-endpoints", "qc2", devtox_yml("qc2", upstream = up)),
               "not `complete`")

  # QC run of another import -> blocked.
  qc_manifest$status <- "complete"
  qc_manifest$upstream <- list(list(run = "analysis/dataset-import-qc/other"))
  yaml::write_yaml(qc_manifest, file.path(qc_dir, "interplate-vc-qc__dt__manifest.yml"))
  expect_error(render_skill(project, "devtox-endpoints", "qc3", devtox_yml("qc3", upstream = up)),
               "not on the import run")

  # Pooling a chemical whose plates use different units is refused
  # (P4 is in ug/mL; pool by VEHICLE, shared by all plates).
  mixed <- devtox_yml("mixed", "by_chemical_pooled")
  mixed$chemical_col <- "VEHICLE"
  expect_error(render_skill(project, "devtox-endpoints", "mixed", mixed), "different concentration units")

  # Missing chemical column -> parameter error.
  bad <- devtox_yml("bad", "by_chemical_pooled")
  bad$chemical_col <- "NOPE"
  expect_error(render_skill(project, "devtox-endpoints", "bad", bad), "not in the upstream data")
})

# ---- devtox-endpoints-bmc ---------------------------------------------------

test_that("devtox-endpoints-bmc template chunks parse and yml/qmd params match", {
  d <- file.path(skills_root(), "devtox-endpoints-bmc", "templates")
  lines <- readLines(file.path(d, "devtox-endpoints-bmc.qmd"))
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  for (s in starts) {
    e <- min(ends[ends > s])
    expect_no_error(parse(text = lines[(s + 1):(e - 1)]))
  }
  y <- yaml::read_yaml(file.path(d, "devtox-endpoints-bmc.yml"))
  delim <- which(lines == "---")
  front <- yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  expect_setequal(names(y), names(front$params))
  qmd <- paste(lines, collapse = "\n")
  for (fn in c("estimate_bmr_curvep", "run_bmc_curvep", "plot_devtox_dose_response_pages")) {
    expect_true(fn %in% getNamespaceExports("zfbehavior"))
    expect_match(qmd, paste0(fn, "("), fixed = TRUE)
  }
})

bmc_yml <- function(run, upstream, table = "per_plate", bmr_source = "user", bmr = 30,
                    reason = "test") {
  list(
    run_label = run, project_root = "../../..", upstream = list(upstream),
    table = table, bmr_source = bmr_source, bmr = bmr, bmr_reason = reason,
    estimate_trsh_from = 10, estimate_trsh_to = 90, estimate_trsh_by = 20,
    estimate_n_samples = 5, rnge = 1000000, n_samples = 20, seed = 2022L, workers = 1L,
    hit_threshold = 0.5, title_cols = list(), title_wrap_width = 40,
    panel_order = "alphabetical", nrow = 1L, ncol = 5L, pdf_width = 15, pdf_height = 5
  )
}

test_that("devtox-endpoints-bmc: user BMR, pooled table, units, reuse, blocked upstream", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  imp <- render_import_qc(project, "dt", setup_devtox_fixture(project))
  expect_identical(imp$manifest$status, "complete")
  pp <- render_skill(project, "devtox-endpoints", "pp", devtox_yml("pp"))
  pool <- render_skill(project, "devtox-endpoints", "pool", devtox_yml("pool", "by_chemical_pooled"))

  r1 <- render_skill(project, "devtox-endpoints-bmc", "u30", bmc_yml("u30", "analysis/devtox-endpoints/pp"))
  expect_identical(r1$manifest$status, "complete")
  expect_match(r1$manifest$curvep_runs$bmc, "^computed")
  res <- readRDS(r1$out("bmc-results", "rds"))
  expect_s3_class(res, "tbl_df")
  expect_identical(nrow(res), 4L)
  expect_equal(res$BMC, 10^res$POD_med * 1e6)
  expect_identical(res$hit, res$hit_confidence > 0.5)
  expect_setequal(res$concentration_unit, c("uM", "ug/mL"))
  # Same numbers as calling the package directly.
  e <- readRDS(pp$out("endpoints", "rds"))
  e <- e[!e$is_vehicle_control, ]
  e$curve_id <- e$plate_id
  e$log_conc <- log10(e$concentration / 1e6)
  direct <- run_bmc_curvep(e, tibble::tibble(endpoint = "percent of affected", bmr = 30, RNGE = 1e6),
                           chemical_col = "curve_id", conc_col = "log_conc",
                           n_in_col = "n_affected", n_col = "N", n_samples = 20, seed = 2022)
  m <- match(direct$act_summary$chemical, res$plate_id)
  expect_equal(res$POD_med[m], direct$act_summary$POD_med)
  expect_equal(res$hit_confidence[m], direct$act_summary$hit_confidence)
  expect_true(file.exists(r1$out("dose-response-bmc", "pdf")))
  expect_true(file.exists(r1$out("curvep-raw", "rds")))
  expect_rds_tables_are_tibbles(r1$run_dir)

  # Re-render with only a plot change: Curvep result reused, same numbers.
  y2 <- bmc_yml("u30", "analysis/devtox-endpoints/pp")
  y2$ncol <- 2L
  r1b <- render_skill(project, "devtox-endpoints-bmc", "u30", y2)
  expect_identical(r1b$manifest$curvep_runs$bmc, "reused")
  expect_identical(r1b$manifest$summary$n_pdf_pages, 2L)
  expect_equal(readRDS(r1b$out("bmc-results", "rds")), res)
  # BMR change: recomputed.
  y3 <- bmc_yml("u30", "analysis/devtox-endpoints/pp", bmr = 40)
  r1c <- render_skill(project, "devtox-endpoints-bmc", "u30", y3)
  expect_match(r1c$manifest$curvep_runs$bmc, "^computed")

  # Pooled table: one curve per chemical (3).
  r2 <- render_skill(project, "devtox-endpoints-bmc", "pooled",
                     bmc_yml("pooled", "analysis/devtox-endpoints/pool", table = "pooled"))
  expect_identical(r2$manifest$summary$n_curves, 3L)
  # Pooled requested on a per-plate run: parameter error.
  expect_error(render_skill(project, "devtox-endpoints-bmc", "bad",
                            bmc_yml("bad", "analysis/devtox-endpoints/pp", table = "pooled")),
               "no `endpoints-pooled` table")
  # User BMR without reason: parameter error.
  expect_error(render_skill(project, "devtox-endpoints-bmc", "bad2",
                            bmc_yml("bad2", "analysis/devtox-endpoints/pp", reason = NULL)),
               "bmr_reason")
  # Upstream not complete: blocked.
  mf <- list.files(file.path(pp$run_dir, "outputs"), "manifest", full.names = TRUE)
  m <- yaml::read_yaml(mf)
  m$status <- "failed"
  yaml::write_yaml(m, mf)
  expect_error(render_skill(project, "devtox-endpoints-bmc", "bad3",
                            bmc_yml("bad3", "analysis/devtox-endpoints/pp")), "not `complete`")
})

test_that("devtox-endpoints-bmc: BMR estimate stops for review, then reuses the sweep", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  render_import_qc(project, "dt", setup_devtox_fixture(project))
  render_skill(project, "devtox-endpoints", "pp", devtox_yml("pp"))

  y <- bmc_yml("est", "analysis/devtox-endpoints/pp", bmr_source = "estimate", bmr = NULL, reason = NULL)
  r1 <- render_skill(project, "devtox-endpoints-bmc", "est", y)
  expect_identical(r1$manifest$status, "needs_review")
  expect_identical(r1$manifest$curvep_runs$bmr_sweep, "computed")
  expect_true(file.exists(r1$out("bmr-sweep", "rds")))
  expect_true(file.exists(r1$out("bmr-estimate", "pdf")))
  expect_false(file.exists(r1$out("bmc-results", "rds")))
  est <- readRDS(r1$out("bmr-estimate", "rds"))
  expect_true("bmr_exp" %in% names(est$outcome))

  y$bmr <- 30
  y$bmr_reason <- "test: chosen after review"
  r2 <- render_skill(project, "devtox-endpoints-bmc", "est", y)
  expect_identical(r2$manifest$status, "complete")
  expect_identical(r2$manifest$curvep_runs$bmr_sweep, "reused")
  expect_equal(readRDS(r2$out("bmr-estimate", "rds"))$outcome, est$outcome)
  expect_true(file.exists(r2$out("bmc-results", "rds")))
})

# ---- behavior-endpoints ------------------------------------------------------

test_that("behavior-endpoints template chunks parse and yml/qmd params match", {
  d <- file.path(skills_root(), "behavior-endpoints", "templates")
  lines <- readLines(file.path(d, "behavior-endpoints.qmd"))
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  for (s in starts) {
    e <- min(ends[ends > s])
    expect_no_error(parse(text = lines[(s + 1):(e - 1)]))
  }
  y <- yaml::read_yaml(file.path(d, "behavior-endpoints.yml"))
  delim <- which(lines == "---")
  front <- yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  expect_setequal(names(y), names(front$params))
  qmd <- paste(lines, collapse = "\n")
  for (fn in c("behavior_endpoint_input_cols", "replace_na_response_zero", "generate_behavior_endpoints",
               "validate_behavior_endpoints", "plot_behavior_dose_response_pages")) {
    expect_true(fn %in% getNamespaceExports("zfbehavior"))
    expect_match(qmd, paste0(fn, "("), fixed = TRUE)
  }
})

behavior_yml <- function(run, layout = "per_plate", segments = NULL, normalization = NULL,
                         upstream = list("analysis/dataset-import-qc/dt")) {
  list(
    run_label = run, project_root = "../../..", upstream = upstream,
    segments = segments %||% list(L1 = list(from = 1, to = 10), D1 = list(from = 11, to = 20),
                                  L2 = list(from = 21, to = 30), D2 = list(from = 31, to = 40),
                                  `D2-ext` = list(from = 41, to = 50)),
    similarity_segments = list(LD = list(from = 1, to = 50)),
    similarity_metrics = list("spearman", "pearson"),
    normalization = normalization %||% list(total_distance = "log_diff", distance_change_shift = "none",
                                            movement_similarity = "ratio"),
    layout = layout, chemical_col = "DTXSID", title_cols = list(), title_wrap_width = 40,
    panel_order = "alphabetical", ncol = 4L, pdf_width = 15, pdf_height_per_row = 2.2
  )
}

test_that("behavior-endpoints renders per plate and pooled, equal to the package call", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  imp <- render_import_qc(project, "dt", setup_devtox_fixture(project))
  comb <- readRDS(imp$combined)

  r1 <- render_skill(project, "behavior-endpoints", "pp", behavior_yml("pp"))
  expect_identical(r1$manifest$status, "complete")
  e1 <- readRDS(r1$out("endpoints", "rds"))
  d <- replace_na_response_zero(comb[comb$keep_well, ])
  direct <- suppressWarnings(generate_behavior_endpoints(
    d,
    auc_segments = list(L1 = 1:10, D1 = 11:20, L2 = 21:30, D2 = 31:40, `D2-ext` = 41:50),
    similarity_segments = list(LD = 1:50), similarity_metrics = c("spearman", "pearson")
  ))
  expect_equal(as.data.frame(e1), as.data.frame(direct))
  expect_identical(r1$manifest$summary$n_endpoints, 22L)
  pdfs <- vapply(r1$manifest$summary$pdfs, `[[`, "", "pdf")
  expect_setequal(pdfs, paste0("behavior-endpoints__pp__",
                               c("total-distance", "distance-change", "distance-shift", "movement-similarity"),
                               ".pdf"))
  expect_true(all(file.exists(file.path(r1$run_dir, "outputs", pdfs))))
  expect_true(readRDS(r1$out("endpoint-validation", "rds"))$ok)
  expect_rds_tables_are_tibbles(r1$run_dir)

  # Pooled by DTXSID: 3 chemicals, endpoints identical (pooling is plots only).
  r2 <- render_skill(project, "behavior-endpoints", "pool", behavior_yml("pool", "by_chemical_pooled"))
  expect_identical(r2$manifest$summary$n_panels, 3L)
  e2 <- readRDS(r2$out("endpoints", "rds"))
  expect_equal(e2$endpoint_value_norm, e1$endpoint_value_norm)
  expect_true("DTXSID" %in% names(e2))
})

test_that("behavior-endpoints: merged D2, non-default normalization, invalid segments", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  render_import_qc(project, "dt", setup_devtox_fixture(project))
  seg <- list(L1 = list(from = 1, to = 10), D1 = list(from = 11, to = 20),
              L2 = list(from = 21, to = 30), D2 = list(from = 31, to = 50))
  r <- render_skill(project, "behavior-endpoints", "m",
                    behavior_yml("m", segments = seg,
                                 normalization = list(total_distance = "log_ratio")))
  expect_identical(r$manifest$status, "complete")
  e <- readRDS(r$out("endpoints", "rds"))
  td <- e[e$endpoint_type == "total_distance", ]
  expect_setequal(unique(td$endpoint), c("L1", "D1", "L2", "D2"))
  expect_true(all(td$normalization_method == "log_ratio"))
  expect_false(any(td$is_default_normalization))
  expect_identical(unlist(r$manifest$method$non_default_normalization), "total_distance")
  expect_identical(r$manifest$summary$n_endpoints, 4L + 12L + 2L)

  bad <- seg
  bad$D2 <- list(from = 20, to = 50)
  expect_error(render_skill(project, "behavior-endpoints", "bad", behavior_yml("bad", segments = bad)),
               "overlap")
  bad2 <- seg
  bad2$D2 <- list(from = 31, to = 60)
  expect_error(render_skill(project, "behavior-endpoints", "bad2", behavior_yml("bad2", segments = bad2)),
               "not present in the data")
})

# ---- intraplate-vc-qc --------------------------------------------------------

test_that("intraplate-vc-qc template chunks parse and yml/qmd params match", {
  d <- file.path(skills_root(), "intraplate-vc-qc", "templates")
  lines <- readLines(file.path(d, "intraplate-vc-qc.qmd"))
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  expect_gt(length(starts), 0)
  for (s in starts) {
    e <- min(ends[ends > s])
    expect_no_error(parse(text = lines[(s + 1):(e - 1)]))
  }
  y <- yaml::read_yaml(file.path(d, "intraplate-vc-qc.yml"))
  delim <- which(lines == "---")
  front <- yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  expect_setequal(names(y), names(front$params))
  expect_identical(y$outlier_degree, 3L)
  expect_identical(y$similarity_threshold, 0.4)
  qmd <- paste(lines, collapse = "\n")
  for (fn in c("detect_behavior_vc_baseline_outliers", "summarize_behavior_vc_distribution",
               "plot_behavior_vc_baseline", "count_wells_by_dose", "plot_raw_responses_pages")) {
    expect_true(fn %in% getNamespaceExports("zfbehavior"))
    expect_match(qmd, paste0(fn, "("), fixed = TRUE)
  }
})

intraplate_yml <- function(run, upstream = list("analysis/behavior-endpoints/pp"),
                           outlier_degree = 3, similarity_threshold = 0.4) {
  list(
    run_label = run, project_root = "../../..", upstream = upstream,
    response_label = "distance moved (mm)",
    title_cols = list("CHEMICAL_NAME", "chemical_id", "plate_id"),
    outlier_degree = outlier_degree, similarity_threshold = similarity_threshold,
    exclude_plates = list(), keep_plates = list(), pdf_width = 15, pdf_height = 10
  )
}

test_that("intraplate-vc-qc flags plates, builds the consensus, requires decisions", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  imp <- render_import_qc(project, "dt", setup_devtox_fixture(project))
  ep <- render_skill(project, "behavior-endpoints", "pp", behavior_yml("pp"))
  expect_identical(ep$manifest$status, "complete")
  endpoints <- readRDS(ep$out("endpoints", "rds"))
  plates <- sort(unique(endpoints$plate_id))

  # Defaults on 4 plates: whatever is flagged equals the package call.
  r0 <- render_skill(project, "intraplate-vc-qc", "def", intraplate_yml("def"))
  direct <- detect_behavior_vc_baseline_outliers(endpoints, outlier_degree = 3)
  b0 <- readRDS(r0$out("baseline-outliers", "rds"))
  expect_equal(b0$denom, direct$denom)
  expect_equal(b0$is_outlier, direct$is_outlier)
  dist0 <- readRDS(r0$out("distribution", "rds"))
  expect_identical(nrow(dist0), 22L)
  expect_identical(r0$manifest$summary$n_endpoints, 22L)
  expect_identical(r0$manifest$summary$n_plates, length(plates))

  # No flags (huge fence, rule off) -> complete without decisions.
  r1 <- render_skill(project, "intraplate-vc-qc", "none",
                     intraplate_yml("none", outlier_degree = 1000, similarity_threshold = NULL))
  expect_identical(r1$manifest$status, "complete")
  expect_identical(r1$manifest$summary$n_flagged, 0L)
  expect_identical(r1$manifest$summary$n_qc_pdf_pages, 3L)
  expect_true(file.exists(r1$out("plate-decisions", "rds")))

  # Threshold 1: every plate's VC similarity is < 1 -> all flagged by the rule.
  yml <- intraplate_yml("all", outlier_degree = 1000, similarity_threshold = 1)
  r2 <- render_skill(project, "intraplate-vc-qc", "all", yml)
  expect_identical(r2$manifest$status, "needs_review")
  expect_setequal(unlist(r2$manifest$flagged_plates), plates)
  expect_identical(r2$manifest$summary$n_flagged_by_low_similarity, length(plates))
  expect_false(file.exists(r2$out("plate-decisions", "rds")))
  flags <- readRDS(r2$out("plate-flags", "rds"))
  expect_true(all(flags$n_flagged_endpoints == 0L))
  expect_true(all(grepl("LD_spearman = ", flags$low_similarity)))
  expect_true(all(flags$decision == "UNRESOLVED"))
  expect_true(all(c("n_total_distance", "n_distance_change_shift", "n_movement_similarity") %in% names(flags)))
  # 3 distribution pages + ceiling(4 / 4) trace page.
  expect_identical(r2$manifest$summary$n_qc_pdf_pages, 4L)

  # Decide: exclude the first plate, keep the rest -> complete.
  yml$exclude_plates <- list(list(plate_id = plates[1], reason = "test: exclude"))
  yml$keep_plates <- lapply(plates[-1], function(x) list(plate_id = x, reason = "test: keep"))
  r3 <- render_skill(project, "intraplate-vc-qc", "all", yml)
  expect_identical(r3$manifest$status, "complete")
  dec <- readRDS(r3$out("plate-decisions", "rds"))
  expect_identical(dec$plate_id[dec$excluded], plates[1])
  expect_identical(r3$manifest$excluded_plates[[1]]$plate_id, plates[1])
  expect_identical(r3$manifest$upstream[[1]]$run, "analysis/behavior-endpoints/pp")
  expect_identical(r3$manifest$import_run, "analysis/dataset-import-qc/dt")
  expect_rds_tables_are_tibbles(r3$run_dir)
  out_md5 <- vapply(r3$manifest$outputs, `[[`, "", "md5")
  out_paths <- file.path(r3$run_dir, vapply(r3$manifest$outputs, `[[`, "", "path"))
  expect_identical(unname(tools::md5sum(out_paths)), out_md5)

  # Unknown plate in a decision list is a parameter error.
  bad <- yml
  bad$run_label <- "bad"
  bad$keep_plates <- c(bad$keep_plates, list(list(plate_id = "NOPE", reason = "x")))
  expect_error(render_skill(project, "intraplate-vc-qc", "bad", bad), "unknown plate_id")

  # Upstream must be a behavior-endpoints run.
  expect_error(
    render_skill(project, "intraplate-vc-qc", "wrong",
                 intraplate_yml("wrong", upstream = list("analysis/dataset-import-qc/dt"))),
    "must be a behavior-endpoints run"
  )
})

test_that("intraplate-vc-qc blocks an upstream behavior-endpoints run that is not complete", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  render_import_qc(project, "dt", setup_devtox_fixture(project))
  ep <- render_skill(project, "behavior-endpoints", "pp", behavior_yml("pp"))
  mf <- file.path(ep$run_dir, "outputs", "behavior-endpoints__pp__manifest.yml")
  m <- yaml::read_yaml(mf)
  m$status <- "failed"
  m$status_reason <- "test"
  yaml::write_yaml(m, mf)
  expect_error(render_skill(project, "intraplate-vc-qc", "x", intraplate_yml("x")), "not `complete`")
})

# ---- behavior-endpoints-anova ------------------------------------------------

test_that("behavior-endpoints-anova template chunks parse and yml/qmd params match", {
  d <- file.path(skills_root(), "behavior-endpoints-anova", "templates")
  lines <- readLines(file.path(d, "behavior-endpoints-anova.qmd"))
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  expect_gt(length(starts), 0)
  for (s in starts) {
    e <- min(ends[ends > s])
    expect_no_error(parse(text = lines[(s + 1):(e - 1)]))
  }
  y <- yaml::read_yaml(file.path(d, "behavior-endpoints-anova.yml"))
  delim <- which(lines == "---")
  front <- yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  expect_setequal(names(y), names(front$params))
  qmd <- paste(lines, collapse = "\n")
  for (fn in c("run_anova", "run_rmanova", "run_anova_rmanova_hit_calling",
               "plot_behavior_anova_activity_pages")) {
    expect_true(fn %in% getNamespaceExports("zfbehavior"))
    expect_match(qmd, paste0(fn, "("), fixed = TRUE)
  }
})

anova_yml <- function(run, upstream = list("analysis/behavior-endpoints/pp"),
                      metrics = list("spearman"), rmanova = "auto") {
  list(
    run_label = run, project_root = "../../..", upstream = upstream,
    similarity_metrics = metrics, alpha = 0.05, rmanova = rmanova, seed = 1L,
    chemical_label_cols = list("CHEMICAL_NAME"), plate_label_cols = list("plate_id"),
    activity_exclude_tests = list(), activity_exclude_endpoints = list(), activity_exclude_reason = NULL,
    n_per_page = 2L, order = "alphabetical", chemical_wrap_width = 50L,
    pdf_width = 17, pdf_height = 16
  )
}

test_that("behavior-endpoints-anova equals the package calls; two metrics side by side", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  render_import_qc(project, "dt", setup_devtox_fixture(project))
  ep <- render_skill(project, "behavior-endpoints", "pp", behavior_yml("pp"))
  endpoints <- readRDS(ep$out("endpoints", "rds"))

  r1 <- render_skill(project, "behavior-endpoints-anova", "sp", anova_yml("sp"))
  expect_identical(r1$manifest$status, "complete")
  hits <- readRDS(r1$out("hits", "rds"))
  set.seed(1)
  a <- run_anova(endpoints, group_cols = c("plate_id", "endpoint"), value_col = "endpoint_value_raw",
                 endpoint_transforms = list(both_auc = "log10", spearman = "fisher_z"))
  set.seed(1)
  rm <- run_rmanova(endpoints, group_cols = "plate_id",
                    phase_levels = c("L1", "D1", "L2", "D2", "D2-ext"),
                    value_col = "endpoint_value_raw", endpoint_transforms = list(both_auc = "log10"))
  direct <- run_anova_rmanova_hit_calling(a, rm, anova_group_cols = c("plate_id", "endpoint"),
                                          rmanova_group_cols = "plate_id")
  expect_equal(readRDS(r1$out("anova", "rds"))$anova$p_value, a$anova$p_value)
  key <- function(x) paste(x$plate_id, x$test, ifelse(x$test == "RMANOVA", "", x$endpoint))
  expect_setequal(key(hits), key(direct))
  m <- match(key(direct), key(hits))
  expect_identical(hits$decision_state[m], direct$decision_state)
  expect_setequal(unique(hits$metric[!is.na(hits$metric)]), "spearman")
  expect_true(file.exists(r1$out("rmanova", "rds")))
  # 3 plates (1 per chemical), 2 plate rows per page -> 2 pages.
  expect_identical(r1$manifest$summary$n_pdf_pages, 2L)
  expect_identical(r1$manifest$summary$n_labels_not_fitting, 0L)
  expect_rds_tables_are_tibbles(r1$run_dir)

  # Spearman + Pearson: both tested separately; shared rows unchanged.
  r2 <- render_skill(project, "behavior-endpoints-anova", "sppe",
                     anova_yml("sppe", metrics = list("spearman", "pearson")))
  h2 <- readRDS(r2$out("hits", "rds"))
  expect_setequal(unique(h2$metric[!is.na(h2$metric)]), c("spearman", "pearson"))
  shared <- merge(hits, h2, by = c("plate_id", "test", "endpoint"))
  expect_identical(nrow(shared), nrow(hits))
  expect_identical(shared$decision_state.x, shared$decision_state.y)
  expect_true(any(grepl("Similarity metrics compared", readLines(file.path(r2$run_dir, "behavior-endpoints-anova.html"), warn = FALSE))))
  expect_true(all(readRDS(r1$out("activity", "rds"))$in_activity_pdf))
  expect_identical(r1$manifest$summary$n_hits_not_in_activity, 0L)

  # Plot-only exclusions: RM-ANOVA and one endpoint left out of the PDF, kept in hits.rds.
  y3 <- anova_yml("ex")
  y3$activity_exclude_tests <- list("RMANOVA")
  y3$activity_exclude_endpoints <- list("D1@both_auc")
  y3$activity_exclude_reason <- "test"
  r3 <- render_skill(project, "behavior-endpoints-anova", "ex", y3)
  h3 <- readRDS(r3$out("hits", "rds"))
  expect_identical(h3$decision_state, hits$decision_state)
  a3 <- readRDS(r3$out("activity", "rds"))
  out3 <- a3$test == "RMANOVA" | a3$endpoint == "D1@both_auc"
  expect_identical(a3$in_activity_pdf, !out3)
  expect_identical(r3$manifest$summary$n_hits_not_in_activity, sum(out3 & a3$decision_state == "hit"))
  expect_identical(unlist(r3$manifest$method$activity_exclude_tests), "RMANOVA")
  y4 <- y3
  y4$activity_exclude_reason <- NULL
  expect_error(render_skill(project, "behavior-endpoints-anova", "ex4", y4), "activity_exclude_reason")
  y5 <- anova_yml("ex5")
  y5$activity_exclude_tests <- list("TTEST")
  y5$activity_exclude_reason <- "x"
  expect_error(render_skill(project, "behavior-endpoints-anova", "ex5", y5), "ANOVA and/or RMANOVA")
  y6 <- anova_yml("ex6")
  y6$activity_exclude_endpoints <- list("D9@both_auc")
  y6$activity_exclude_reason <- "x"
  expect_error(render_skill(project, "behavior-endpoints-anova", "ex6", y6), "not in the endpoints run")
})

test_that("behavior-endpoints-anova: single distance-change segment, QC exclusions, blocked upstream", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  render_import_qc(project, "dt", setup_devtox_fixture(project))
  one_seg <- list(D2 = list(from = 31, to = 50))
  render_skill(project, "behavior-endpoints", "d2", behavior_yml("d2", segments = one_seg))
  r <- render_skill(project, "behavior-endpoints-anova", "d2",
                    anova_yml("d2", upstream = list("analysis/behavior-endpoints/d2")))
  expect_identical(r$manifest$status, "complete")
  expect_match(r$manifest$method$rmanova, "skipped")
  expect_false(file.exists(r$out("rmanova", "rds")))
  expect_false(any(readRDS(r$out("hits", "rds"))$test == "RMANOVA"))

  # intraplate-vc-qc exclusion is dropped.
  ep <- render_skill(project, "behavior-endpoints", "pp", behavior_yml("pp"))
  plates <- sort(unique(readRDS(ep$out("endpoints", "rds"))$plate_id))
  qc <- intraplate_yml("q", upstream = list("analysis/behavior-endpoints/pp"),
                       outlier_degree = 1000, similarity_threshold = 1)
  qc$exclude_plates <- list(list(plate_id = plates[1], reason = "test"))
  qc$keep_plates <- lapply(plates[-1], function(x) list(plate_id = x, reason = "test"))
  expect_identical(render_skill(project, "intraplate-vc-qc", "q", qc)$manifest$status, "complete")
  r2 <- render_skill(project, "behavior-endpoints-anova", "x",
                     anova_yml("x", upstream = list("analysis/behavior-endpoints/pp",
                                                    "analysis/intraplate-vc-qc/q")))
  h2 <- readRDS(r2$out("hits", "rds"))
  expect_false(plates[1] %in% h2$plate_id)
  expect_identical(r2$manifest$plates_excluded_by_qc[[1]]$plate_id, plates[1])

  # Non-correlation metric; incomplete upstream.
  expect_error(render_skill(project, "behavior-endpoints-anova", "bad",
                            anova_yml("bad", metrics = list("cosine"))),
               "spearman and/or pearson")
  mf <- file.path(ep$run_dir, "outputs", "behavior-endpoints__pp__manifest.yml")
  m <- yaml::read_yaml(mf)
  m$status <- "failed"
  yaml::write_yaml(m, mf)
  expect_error(render_skill(project, "behavior-endpoints-anova", "blk", anova_yml("blk")), "not `complete`")
})

# ---- behavior-endpoints-bmc --------------------------------------------------

test_that("behavior-endpoints-bmc template chunks parse and yml/qmd params match", {
  d <- file.path(skills_root(), "behavior-endpoints-bmc", "templates")
  lines <- readLines(file.path(d, "behavior-endpoints-bmc.qmd"))
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  expect_gt(length(starts), 0)
  for (s in starts) {
    e <- min(ends[ends > s])
    expect_no_error(parse(text = lines[(s + 1):(e - 1)]))
  }
  y <- yaml::read_yaml(file.path(d, "behavior-endpoints-bmc.yml"))
  delim <- which(lines == "---")
  front <- yaml::yaml.load(paste(lines[(delim[1] + 1):(delim[2] - 1)], collapse = "\n"))
  expect_setequal(names(y), names(front$params))
  # Defaults: total distance estimate (both, 5-95), distance change/shift
  # historical global MAD, similarity estimate (down, 5-95); seeds historical.
  expect_identical(y$bmr$total_distance$source, "estimate")
  expect_identical(y$bmr$total_distance$rnge, "both")
  expect_equal(unlist(y$bmr$total_distance$trsh), c(5, 95, 5))
  expect_identical(y$bmr$distance_change_shift$source, "control_mad")
  expect_identical(y$bmr$distance_change_shift$scope, "global")
  expect_false(y$bmr$distance_change_shift$plate_medians)
  expect_identical(y$bmr$distance_change_shift$n_mad, 3L)
  expect_identical(y$bmr$movement_similarity$rnge, "down")
  expect_identical(y$seed, 2026L)
  expect_identical(y$estimate_seed, 2022L)
  expect_identical(unlist(y$activity_exclude_endpoints), c("L1@down_auc", "L2@down_auc"))
  expect_false(is.null(y$activity_exclude_reason))
  qmd <- paste(lines, collapse = "\n")
  for (fn in c("estimate_bmr_from_control", "estimate_bmr_curvep", "run_bmc_curvep",
               "plot_behavior_dose_response_pages", "plot_behavior_bmc_activity_pages")) {
    expect_true(fn %in% getNamespaceExports("zfbehavior"))
    expect_match(qmd, paste0(fn, "("), fixed = TRUE)
  }
})

beh_bmc_yml <- function(run, upstream = list("analysis/behavior-endpoints/pp"), table = "per_plate",
                        bmr = NULL) {
  list(
    run_label = run, project_root = "../../..", upstream = upstream, table = table,
    chemical_col = NULL, endpoint_types = list(), similarity_metrics = list(),
    bmr = bmr %||% list(
      total_distance = list(source = "user", rnge = "both",
                            bmr = list(up = 20, down = list(L1 = 25, D1 = 25, L2 = 25, D2 = 25, `D2-ext` = 30)),
                            reason = "test"),
      distance_change_shift = list(source = "control_mad", rnge = "up", scope = "global",
                                   plate_medians = FALSE, n_mad = 3),
      movement_similarity = list(source = "user", rnge = "down",
                                 bmr = list(LD_pearson = 30, LD_spearman = 25), reason = "historical")
    ),
    estimate_n_samples = 2L, estimate_seed = 2022L, n_samples = 5L, seed = 1L, workers = 1L,
    hit_threshold = 0.5, chemical_label_cols = list("CHEMICAL_NAME"), plate_label_cols = list("plate_id"),
    ncol = 4L, pdf_width = 15, pdf_height_per_row = 2.2, panel_order = "alphabetical",
    activity_exclude_endpoints = list("L1@down_auc", "L2@down_auc"),
    activity_exclude_reason = "historically very noisy",
    n_per_page = 2L, order = "alphabetical", chemical_wrap_width = 50L,
    activity_pdf_width = 17, activity_pdf_height = 16
  )
}

test_that("behavior-endpoints-bmc: user and control_mad BMRs equal the package calls; reuse; plate medians", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  render_import_qc(project, "dt", setup_devtox_fixture(project))
  ep <- render_skill(project, "behavior-endpoints", "pp", behavior_yml("pp"))
  endpoints <- readRDS(ep$out("endpoints", "rds"))

  r1 <- render_skill(project, "behavior-endpoints-bmc", "u", beh_bmc_yml("u"))
  expect_identical(r1$manifest$status, "complete")
  bt <- readRDS(r1$out("bmr-table", "rds"))
  # control_mad global = estimate_bmr_from_control() per endpoint.
  dc <- endpoints[endpoints$endpoint_type == "distance_change_shift", ]
  direct <- estimate_bmr_from_control(dc, group_cols = "endpoint", value_col = "endpoint_value_norm")
  b_dc <- bt[bt$endpoint_type == "distance_change_shift", ]
  expect_equal(b_dc$bmr[match(direct$endpoint, b_dc$endpoint)], direct$bmr)
  expect_true(all(b_dc$RNGE == 1e6))
  # Total distance: both directions, user values per direction / endpoint.
  td <- bt[bt$endpoint_type == "total_distance", ]
  expect_identical(nrow(td), 10L)
  expect_true(all(td$bmr[td$direction == "up"] == 20))
  expect_equal(td$bmr[td$direction == "down" & td$endpoint == "D2-ext"], 30)

  res <- readRDS(r1$out("bmc-results", "rds"))
  # 4 plates x (5 x 2 + 15 + 2) fits.
  expect_identical(nrow(res), 4L * 27L)
  # BMC equals a direct run_bmc_curvep() on the same input.
  tr <- endpoints[!endpoints$is_vehicle_control & endpoints$endpoint_type == "movement_similarity", ]
  tr$log_conc <- log10(tr$concentration / 1e6)
  dir_bmc <- run_bmc_curvep(tr, tibble::tibble(endpoint = c("LD_pearson", "LD_spearman"), bmr = c(30, 25),
                                               RNGE = -1e6),
                            chemical_col = "plate_id", conc_col = "log_conc", value_col = "endpoint_value_norm",
                            n_samples = 5, seed = 1)$act_summary
  sim <- res[res$endpoint_type == "movement_similarity", ]
  m <- match(paste(dir_bmc$chemical, dir_bmc$endpoint), paste(sim$curve_id, sim$endpoint))
  expect_equal(sim$hit_confidence[m], unname(dir_bmc$hit_confidence))
  expect_equal(sim$BMC[m], 10^unname(dir_bmc$POD_med) * 1e6)
  # One plotted direction per curve x endpoint: the higher hit confidence.
  expect_true(all(table(paste(res$curve_id, res$endpoint)[res$plotted]) == 1))
  tdr <- res[res$endpoint_type == "total_distance", ]
  best <- c(tapply(tdr$hit_confidence, paste(tdr$curve_id, tdr$endpoint), max))
  expect_equal(unname(tdr$hit_confidence[tdr$plotted]), unname(best[paste(tdr$curve_id, tdr$endpoint)[tdr$plotted]]))
  expect_equal(res$hit, res$hit_confidence > 0.5)
  pdfs <- paste0("dose-response-", c("total-distance", "distance-change", "distance-shift", "movement-similarity"))
  expect_true(all(file.exists(vapply(pdfs, r1$out, "", ext = "pdf"))))
  expect_true(file.exists(r1$out("activity", "pdf")))
  act <- readRDS(r1$out("activity", "rds"))
  td_lab <- act$label[act$endpoint_type == "total_distance"]
  expect_true(all(grepl("^[\u2191\u2193] ", td_lab)))
  expect_false(any(grepl("[\u2191\u2193]", act$label[act$endpoint_type != "total_distance"])))
  expect_identical(r1$manifest$summary$n_labels_not_fitting, 0L)
  # Default activity exclusions: fitted and kept, not plotted.
  expect_true(all(c("L1@down_auc", "L2@down_auc") %in% res$endpoint))
  expect_identical(act$in_activity_pdf, !act$endpoint %in% c("L1@down_auc", "L2@down_auc"))
  expect_identical(r1$manifest$summary$n_hits_not_in_activity,
                   sum(act$hit & act$endpoint %in% c("L1@down_auc", "L2@down_auc")))
  expect_rds_tables_are_tibbles(r1$run_dir)

  # Re-render: every Curvep run reused, identical results.
  # Re-render with [] (all endpoints in the activity PDF): Curvep reused,
  # results identical.
  yb <- beh_bmc_yml("u")
  yb$activity_exclude_endpoints <- list()
  yb$activity_exclude_reason <- NULL
  r1b <- render_skill(project, "behavior-endpoints-bmc", "u", yb)
  expect_true(all(unlist(r1b$manifest$curvep_runs$bmc) == "reused"))
  expect_equal(readRDS(r1b$out("bmc-results", "rds")), res)
  expect_true(all(readRDS(r1b$out("activity", "rds"))$in_activity_pdf))
  expect_identical(r1b$manifest$summary$n_hits_not_in_activity, 0L)

  # Plate medians (4 plates): lower spread, equal to the package call.
  y <- beh_bmc_yml("pm")
  y$endpoint_types <- list("distance_change_shift")
  y$bmr$distance_change_shift$plate_medians <- TRUE
  r2 <- render_skill(project, "behavior-endpoints-bmc", "pm", y)
  b2 <- readRDS(r2$out("bmr-table", "rds"))
  direct2 <- estimate_bmr_from_control(dc, group_cols = "endpoint", value_col = "endpoint_value_norm",
                                       plate_col = "plate_id")
  expect_equal(b2$bmr[match(direct2$endpoint, b2$endpoint)], direct2$bmr)
  expect_true(all(b2$n_values == 4L))
  expect_setequal(unique(readRDS(r2$out("bmc-results", "rds"))$endpoint_type), "distance_change_shift")
})

test_that("behavior-endpoints-bmc: estimate stops for review then reuses; pooled; invalid settings; blocked", {
  skip_unless_render_possible()
  project <- withr::local_tempdir()
  render_import_qc(project, "dt", setup_devtox_fixture(project))
  ep <- render_skill(project, "behavior-endpoints", "pool", behavior_yml("pool", "by_chemical_pooled"))

  bmr <- list(
    total_distance = list(source = "estimate", rnge = "both", trsh = list(5, 95, 5)),
    distance_change_shift = list(source = "control_mad", rnge = "up", scope = "chemical", n_mad = 3),
    movement_similarity = list(source = "estimate", rnge = "down", trsh = NULL)
  )
  y <- beh_bmc_yml("est", upstream = list("analysis/behavior-endpoints/pool"), table = "pooled", bmr = bmr)
  r1 <- render_skill(project, "behavior-endpoints-bmc", "est", y)
  expect_identical(r1$manifest$status, "needs_review")
  expect_setequal(unlist(r1$manifest$summary$pending_bmr), c("total_distance", "movement_similarity"))
  est <- readRDS(r1$out("bmr-estimate-total-distance", "rds"))
  expect_setequal(est$outcome$RNGE, c(-1e6, 1e6))
  expect_identical(nrow(est$outcome), 10L)
  sw <- readRDS(r1$out("bmr-sweep-movement-similarity", "rds"))
  expect_true(all(sw$est$trsh_seq$n_trsh == 19L))
  # The control_mad type is already fitted: 3 chemicals x 15 endpoints.
  res1 <- readRDS(r1$out("bmc-results", "rds"))
  expect_setequal(unique(res1$endpoint_type), "distance_change_shift")
  expect_identical(length(unique(res1$curve_id)), 3L)
  bt <- readRDS(r1$out("bmr-table", "rds"))
  expect_false(anyNA(bt$curve_id[bt$endpoint_type == "distance_change_shift"]))

  # Partial BMR is an error; full BMR completes and reuses the sweeps.
  y$bmr$total_distance$bmr <- list(up = 25)
  y$bmr$total_distance$reason <- "reviewed"
  expect_error(render_skill(project, "behavior-endpoints-bmc", "est", y), "missing for")
  y$bmr$total_distance$bmr <- list(up = 25, down = 25)
  y$bmr$movement_similarity$bmr <- 10
  y$bmr$movement_similarity$reason <- "reviewed"
  r2 <- render_skill(project, "behavior-endpoints-bmc", "est", y)
  expect_identical(r2$manifest$status, "complete")
  expect_true(all(unlist(r2$manifest$curvep_runs$bmr_sweep) == "reused"))
  expect_identical(r2$manifest$curvep_runs$bmc$distance_change_shift, "reused")
  res2 <- readRDS(r2$out("bmc-results", "rds"))
  expect_setequal(unique(res2$endpoint_type), c("total_distance", "distance_change_shift", "movement_similarity"))
  expect_true(all(grepl("^pooled", res2$plate_label)))

  # Invalid combinations.
  pool_up <- list("analysis/behavior-endpoints/pool")
  bad <- beh_bmc_yml("bad", upstream = pool_up)
  bad$bmr$total_distance <- list(source = "control_mad", rnge = "both", plate_medians = TRUE)
  expect_error(render_skill(project, "behavior-endpoints-bmc", "bad", bad), "only for distance_change_shift")
  bad2 <- beh_bmc_yml("bad2", upstream = pool_up)
  bad2$bmr$distance_change_shift$scope <- "chemical"
  expect_error(render_skill(project, "behavior-endpoints-bmc", "bad2", bad2), "needs `table: pooled`")
  bad4 <- beh_bmc_yml("bad4", upstream = pool_up)
  bad4$activity_exclude_reason <- NULL
  expect_error(render_skill(project, "behavior-endpoints-bmc", "bad4", bad4), "activity_exclude_reason")
  bad5 <- beh_bmc_yml("bad5", upstream = pool_up)
  bad5$activity_exclude_endpoints <- list("D9@down_auc")
  expect_error(render_skill(project, "behavior-endpoints-bmc", "bad5", bad5), "not in the endpoints run")
  bad3 <- beh_bmc_yml("bad3", upstream = pool_up)
  bad3$bmr$movement_similarity$bmr <- list(LD_pearson = 30)
  expect_error(render_skill(project, "behavior-endpoints-bmc", "bad3", bad3), "missing for")

  # Incomplete upstream.
  mf <- file.path(ep$run_dir, "outputs", "behavior-endpoints__pool__manifest.yml")
  m <- yaml::read_yaml(mf)
  m$status <- "failed"
  yaml::write_yaml(m, mf)
  expect_error(render_skill(project, "behavior-endpoints-bmc", "blk",
                            beh_bmc_yml("blk", upstream = list("analysis/behavior-endpoints/pool"))),
               "not `complete`")
})
