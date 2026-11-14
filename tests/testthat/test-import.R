test_that("read_behavior_data() reads a CSV and renames to canonical columns", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      Plate = "P1",
      Well = "A1",
      Chem = "C1",
      VC = TRUE,
      Conc = 0,
      ConcUnit = "uM",
      Time = 1,
      Resp = 10,
      Keep = TRUE
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    concentration_unit = "ConcUnit",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  d <- read_behavior_data(tmp, column_mapping = mapping)

  expect_true(all(required_canonical_roles() %in% colnames(d)))
  expect_equal(d$plate_id, "P1")
  expect_equal(d$response_value, 10)
})

test_that("read_behavior_data() errors clearly when a required role is missing from column_mapping", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1), tmp)

  incomplete_mapping <- list(plate_id = "a")

  expect_error(
    read_behavior_data(tmp, column_mapping = incomplete_mapping),
    class = "zfbehavior_error_missing_role_mapping"
  )
})

test_that("read_behavior_data() errors clearly when a mapped column is not in the file", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1), tmp)

  mapping <- zf_fixture_column_mapping()
  mapping$plate_id <- "does_not_exist"

  expect_error(
    read_behavior_data(tmp, column_mapping = mapping),
    class = "zfbehavior_error_column_not_found"
  )
})

test_that("read_behavior_data() errors on an unrecognized canonical role name", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1), tmp)

  mapping <- zf_fixture_column_mapping()
  mapping$not_a_real_role <- "a"

  expect_error(
    read_behavior_data(tmp, column_mapping = mapping),
    class = "zfbehavior_error_unknown_role"
  )
})

test_that("read_behavior_data() errors when the file does not exist", {
  mapping <- zf_fixture_column_mapping()
  expect_error(
    read_behavior_data("no_such_file.csv", column_mapping = mapping),
    class = "zfbehavior_error_file_not_found"
  )
})

test_that("read_behavior_data() errors on unsupported file extensions", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines("a,b", tmp)

  mapping <- zf_fixture_column_mapping()
  expect_error(
    read_behavior_data(tmp, column_mapping = mapping),
    class = "zfbehavior_error_unsupported_filetype"
  )
})

test_that("read_behavior_data() reads a TSV and renames to canonical columns", {
  tmp <- withr::local_tempfile(fileext = ".tsv")
  readr::write_tsv(
    tibble::tibble(
      Plate = "P1",
      Well = "A1",
      Chem = "C1",
      VC = TRUE,
      Conc = 0,
      ConcUnit = "uM",
      Time = 1,
      Resp = 10,
      Keep = TRUE
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    concentration_unit = "ConcUnit",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  d <- read_behavior_data(tmp, column_mapping = mapping)

  expect_true(all(required_canonical_roles() %in% colnames(d)))
  expect_equal(d$plate_id, "P1")
  expect_equal(d$response_value, 10)
})

# ---- delimited_col_types / excel_col_types (split col_types API) ----------

test_that("read_behavior_data() supports an explicit delimited_col_types for CSV", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  # A column that looks all-NA except for one later real string value --
  # the exact landmine explicit col_types is meant to avoid.
  readr::write_csv(
    tibble::tibble(
      Plate = c("P1", "P1"),
      Well = c("A1", "A2"),
      Chem = c("C1", "C1"),
      VC = c(TRUE, TRUE),
      Conc = c(0, 0),
      ConcUnit = c("uM", "uM"),
      Time = c(1, 1),
      Resp = c(10, 10),
      Keep = c(TRUE, TRUE),
      Comment = c(NA, "dead")
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    concentration_unit = "ConcUnit",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  d <- read_behavior_data(
    tmp,
    column_mapping = mapping,
    delimited_col_types = readr::cols(Comment = readr::col_character())
  )

  expect_equal(d$Comment, c(NA, "dead"))
})

test_that("read_behavior_data() supports an explicit delimited_col_types for TSV", {
  tmp <- withr::local_tempfile(fileext = ".tsv")
  readr::write_tsv(
    tibble::tibble(
      Plate = c("P1", "P1"),
      Well = c("A1", "A2"),
      Chem = c("C1", "C1"),
      VC = c(TRUE, TRUE),
      Conc = c(0, 0),
      ConcUnit = c("uM", "uM"),
      Time = c(1, 1),
      Resp = c(10, 10),
      Keep = c(TRUE, TRUE),
      Comment = c(NA, "dead")
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    concentration_unit = "ConcUnit",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  d <- read_behavior_data(
    tmp,
    column_mapping = mapping,
    delimited_col_types = readr::cols(Comment = readr::col_character())
  )

  expect_equal(d$Comment, c(NA, "dead"))
})

test_that("read_behavior_data() supports an explicit excel_col_types for Excel", {
  skip_if_not_installed("writexl")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    data.frame(
      Plate = c("P1", "P1"),
      Well = c("A1", "A2"),
      Chem = c("C1", "C1"),
      VC = c("TRUE", "TRUE"),
      Conc = c(0, 0),
      Time = c(1, 1),
      Resp = c(10, 10),
      Keep = c("TRUE", "TRUE"),
      stringsAsFactors = FALSE
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  d <- read_behavior_data(
    tmp,
    column_mapping = mapping,
    excel_col_types = c(
      "text",
      "text",
      "text",
      "logical",
      "numeric",
      "numeric",
      "numeric",
      "logical"
    )
  )

  expect_true(is.logical(d$is_vehicle_control))
  expect_equal(d$plate_id, c("P1", "P1"))
})

test_that("read_behavior_data() supports Excel sheet selection", {
  skip_if_not_installed("writexl")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(
      Sheet1 = data.frame(a = 1),
      Results = data.frame(
        Plate = "P1",
        Well = "A1",
        Chem = "C1",
        VC = "TRUE",
        Conc = 0,
        Time = 1,
        Resp = 10,
        Keep = "TRUE",
        stringsAsFactors = FALSE
      )
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  d <- read_behavior_data(tmp, column_mapping = mapping, sheet = "Results")
  expect_equal(d$plate_id, "P1")
})

test_that("read_behavior_data() delimited_col_types does not affect Excel import and vice versa", {
  skip_if_not_installed("writexl")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    data.frame(
      Plate = "P1",
      Well = "A1",
      Chem = "C1",
      VC = "TRUE",
      Conc = 0,
      Time = 1,
      Resp = 10,
      Keep = "TRUE",
      stringsAsFactors = FALSE
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  # A readr::cols() spec supplied as delimited_col_types must be ignored
  # (not passed to readxl) when reading an .xlsx file.
  d <- read_behavior_data(
    tmp,
    column_mapping = mapping,
    delimited_col_types = readr::cols(Plate = readr::col_character())
  )
  expect_equal(d$plate_id, "P1")
})

# ---- Strict parsing: readr::problems() / Excel coercion warnings ----------

test_that("read_behavior_data() aborts on a readr parse problem (CSV)", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      Plate = "P1",
      Well = "A1",
      Chem = "C1",
      VC = TRUE,
      Conc = c(0, 1, "not_a_number"),
      Time = 1,
      Resp = 10,
      Keep = TRUE
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  expect_error(
    read_behavior_data(
      tmp,
      column_mapping = mapping,
      delimited_col_types = readr::cols(Conc = readr::col_double())
    ),
    class = "zfbehavior_error_parse_problems"
  )
})

test_that("read_behavior_data() aborts on an Excel coercion warning", {
  skip_if_not_installed("writexl")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    data.frame(
      Plate = "P1",
      Well = "A1",
      Chem = "C1",
      VC = "TRUE",
      Conc = "not_a_number",
      Time = 1,
      Resp = 10,
      Keep = "TRUE",
      stringsAsFactors = FALSE
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  expect_error(
    read_behavior_data(
      tmp,
      column_mapping = mapping,
      excel_col_types = c(
        "text",
        "text",
        "text",
        "logical",
        "numeric",
        "numeric",
        "numeric",
        "logical"
      )
    ),
    class = "zfbehavior_error_parse_problems"
  )
})

test_that("read_behavior_data() has no parse-problem escape hatch (no opt-out parameter)", {
  expect_false("on_parse_problem" %in% names(formals(read_behavior_data)))
})

# ---- column_mapping validation ---------------------------------------------

test_that("read_behavior_data() rejects an empty mapping value", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1), tmp)

  mapping <- zf_fixture_column_mapping()
  mapping$plate_id <- character(0)

  expect_error(
    read_behavior_data(tmp, column_mapping = mapping),
    class = "zfbehavior_error_invalid_mapping"
  )
})

test_that("read_behavior_data() rejects a multivalued mapping value", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1), tmp)

  mapping <- zf_fixture_column_mapping()
  mapping$plate_id <- c("a", "b")

  expect_error(
    read_behavior_data(tmp, column_mapping = mapping),
    class = "zfbehavior_error_invalid_mapping"
  )
})

test_that("read_behavior_data() rejects an NA mapping value", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1), tmp)

  mapping <- zf_fixture_column_mapping()
  mapping$plate_id <- NA_character_

  expect_error(
    read_behavior_data(tmp, column_mapping = mapping),
    class = "zfbehavior_error_invalid_mapping"
  )
})

test_that("read_behavior_data() rejects duplicate source-column assignments", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1), tmp)

  mapping <- zf_fixture_column_mapping()
  mapping$well_id <- mapping$plate_id # same source column as plate_id

  expect_error(
    read_behavior_data(tmp, column_mapping = mapping),
    class = "zfbehavior_error_invalid_mapping"
  )
})

test_that("read_behavior_data() rejects a missing/empty mapping name", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1), tmp)

  mapping <- zf_fixture_column_mapping()
  names(mapping)[1] <- ""

  expect_error(
    read_behavior_data(tmp, column_mapping = mapping),
    class = "zfbehavior_error_invalid_mapping"
  )
})

test_that("read_behavior_data() rejects duplicate canonical role names in column_mapping", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1, b = 2), tmp)

  mapping <- zf_fixture_column_mapping()
  mapping_dup <- c(mapping, list(plate_id = "b")) # plate_id appears twice

  expect_error(
    read_behavior_data(tmp, column_mapping = mapping_dup),
    class = "zfbehavior_error_invalid_mapping"
  )
})

# ---- optional_roles ---------------------------------------------------------

test_that("read_behavior_data() supports deriving a role after import via optional_roles", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      Plate = "P1",
      Well = "A1",
      Chem = "C1",
      VC = TRUE,
      Conc = 0,
      Resp = 10,
      Keep = TRUE,
      Binning = "0:00:00-0:01:00"
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    response_value = "Resp",
    keep_well = "Keep"
  )

  d <- read_behavior_data(tmp, column_mapping = mapping, optional_roles = "time_value")
  expect_false("time_value" %in% colnames(d))
  expect_true("Binning" %in% colnames(d))

  d$time_value <- parse_binning_to_time(d$Binning)
  expect_equal(d$time_value, 1)
})

test_that("read_behavior_data() errors when time_value is omitted without optional_roles", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      Plate = "P1",
      Well = "A1",
      Chem = "C1",
      VC = TRUE,
      Conc = 0,
      Resp = 10,
      Keep = TRUE
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    response_value = "Resp",
    keep_well = "Keep"
  )

  expect_error(
    read_behavior_data(tmp, column_mapping = mapping),
    class = "zfbehavior_error_missing_role_mapping"
  )
})

test_that("read_behavior_data() rejects optional_roles values that are not required roles", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(a = 1), tmp)

  mapping <- zf_fixture_column_mapping()
  expect_error(
    read_behavior_data(tmp, column_mapping = mapping, optional_roles = "concentration_unit"),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    read_behavior_data(tmp, column_mapping = mapping, optional_roles = "not_a_role_at_all"),
    class = "zfbehavior_error_invalid_argument"
  )
})

# ---- replace_na_response_zero() ---------------------------------------------

test_that("replace_na_response_zero() replaces NA with 0 and leaves other values untouched", {
  d <- tibble::tibble(response_value = c(1, NA, 3, NA))
  out <- replace_na_response_zero(d)
  expect_equal(out$response_value, c(1, 0, 3, 0))
})

test_that("replace_na_response_zero() supports a custom response_col", {
  d <- tibble::tibble(my_col = c(NA, 2))
  out <- replace_na_response_zero(d, response_col = "my_col")
  expect_equal(out$my_col, c(0, 2))
})

test_that("replace_na_response_zero() errors clearly when response_col is missing", {
  d <- tibble::tibble(a = 1)
  expect_error(
    replace_na_response_zero(d, response_col = "response_value"),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("replace_na_response_zero() is a no-op when there are no NA values", {
  d <- tibble::tibble(response_value = c(1, 2, 3))
  out <- replace_na_response_zero(d)
  expect_equal(out$response_value, c(1, 2, 3))
})

test_that("replace_na_response_zero() rejects a non-numeric response_col", {
  d <- tibble::tibble(response_value = c("1", NA, "3"))
  expect_error(
    replace_na_response_zero(d),
    class = "zfbehavior_error_invalid_argument"
  )

  d_factor <- tibble::tibble(response_value = factor(c("1", NA, "3")))
  expect_error(
    replace_na_response_zero(d_factor),
    class = "zfbehavior_error_invalid_argument"
  )

  d_logical <- tibble::tibble(response_value = c(TRUE, NA, FALSE))
  expect_error(
    replace_na_response_zero(d_logical),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("replace_na_response_zero() preserves integer type", {
  d <- tibble::tibble(response_value = c(1L, NA, 3L))
  out <- replace_na_response_zero(d)
  expect_true(is.integer(out$response_value))
  expect_equal(out$response_value, c(1L, 0L, 3L))
})

test_that("replace_na_response_zero() preserves double type", {
  d <- tibble::tibble(response_value = c(1.5, NA, 3.5))
  out <- replace_na_response_zero(d)
  expect_true(is.double(out$response_value))
  expect_equal(out$response_value, c(1.5, 0, 3.5))
})

test_that("replace_na_response_zero() also replaces NaN (documented is.na() behavior)", {
  d <- tibble::tibble(response_value = c(1, NaN, 3))
  out <- replace_na_response_zero(d)
  expect_equal(out$response_value, c(1, 0, 3))
})

test_that("read_behavior_data() preserves unmapped columns", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      Plate = "P1",
      Well = "A1",
      Chem = "C1",
      VC = TRUE,
      Conc = 0,
      ConcUnit = "uM",
      Time = 1,
      Resp = 10,
      Keep = TRUE,
      Extra = "kept"
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    concentration_unit = "ConcUnit",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  d <- read_behavior_data(tmp, column_mapping = mapping)
  expect_true("Extra" %in% colnames(d))
})

test_that("read_behavior_data() preserves unmapped columns useful for downstream labeling/plotting", {
  # This is the sole supported mechanism for carrying extra vendor metadata
  # (e.g. chemical names) through import now that optional_roles is
  # reserved strictly for required-role derivation. Regression-locks this
  # so it cannot silently regress.
  tmp <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      Plate = "P1",
      Well = "A1",
      Chem = "C1",
      ChemName = "Aspirin",
      VC = TRUE,
      Conc = 0,
      Time = 1,
      Resp = 10,
      Keep = TRUE
    ),
    tmp
  )

  mapping <- list(
    plate_id = "Plate",
    well_id = "Well",
    chemical_id = "Chem",
    is_vehicle_control = "VC",
    concentration = "Conc",
    time_value = "Time",
    response_value = "Resp",
    keep_well = "Keep"
  )

  d <- read_behavior_data(tmp, column_mapping = mapping)
  expect_equal(d$ChemName, "Aspirin")
})

# ---- canonical_behavior_roles() and per-function subset accessors ----------

test_that("canonical_behavior_roles() returns all 9 roles, keep_well included", {
  roles <- canonical_behavior_roles()
  expect_true(is.character(roles))
  expect_true(!is.null(names(roles)))
  expect_setequal(
    names(roles),
    c(
      "plate_id",
      "well_id",
      "chemical_id",
      "is_vehicle_control",
      "concentration",
      "concentration_unit",
      "time_value",
      "response_value",
      "keep_well"
    )
  )
  expect_true(grepl("^required", roles[["keep_well"]]))
  expect_true(grepl("^optional", roles[["concentration_unit"]]))
})

test_that("required_canonical_roles() is exported and matches canonical_behavior_roles()'s required subset", {
  roles <- canonical_behavior_roles()
  expect_setequal(
    required_canonical_roles(),
    names(roles)[grepl("^required", roles)]
  )
})

test_that("behavior_endpoint_input_cols() is exported and matches documented columns", {
  expect_setequal(
    behavior_endpoint_input_cols(),
    c(
      "plate_id",
      "well_id",
      "is_vehicle_control",
      "time_value",
      "response_value",
      "keep_well"
    )
  )
})

test_that("raw_plot_input_cols() is exported and matches documented columns", {
  expect_setequal(
    raw_plot_input_cols(),
    c(
      "plate_id",
      "well_id",
      "is_vehicle_control",
      "concentration",
      "time_value",
      "response_value",
      "keep_well"
    )
  )
})

test_that("interplate_qc_input_cols() is exported and matches documented columns", {
  expect_setequal(
    interplate_qc_input_cols(),
    c(
      "plate_id",
      "is_vehicle_control",
      "time_value",
      "response_value",
      "keep_well"
    )
  )
})
