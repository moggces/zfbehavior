# ---- check_filled_columns() ------------------------------------------------

test_that("check_filled_columns() reports only columns with NA values", {
  d <- tibble::tibble(a = c(1, NA, 3), b = c(1, 2, 3))
  result <- check_filled_columns(d, c("a", "b"))

  expect_equal(nrow(result), 1)
  expect_equal(result$column, "a")
  expect_equal(result$status, "has_na")
  expect_equal(result$n_na, 1)
})

test_that("check_filled_columns() returns zero rows when all checked columns are filled", {
  d <- tibble::tibble(a = c(1, 2, 3), b = c(1, 2, 3))
  result <- check_filled_columns(d, c("a", "b"))
  expect_equal(nrow(result), 0)
})

test_that("check_filled_columns() reports a missing column as status = column_missing, n_na = NA, not silently skipped", {
  d <- tibble::tibble(a = c(1, 2, 3))
  result <- check_filled_columns(d, c("a", "missing_col"))

  expect_equal(nrow(result), 1)
  expect_equal(result$column, "missing_col")
  expect_equal(result$status, "column_missing")
  expect_true(is.na(result$n_na))
})

test_that("check_filled_columns() returns a stable, correctly-typed zero-row tibble for an empty required_filled_cols", {
  d <- tibble::tibble(a = 1)
  result <- check_filled_columns(d, character(0))

  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("column", "status", "n_na"))
  expect_type(result$column, "character")
  expect_type(result$status, "character")
  expect_type(result$n_na, "integer")
})

# ---- check_plate_row_counts() ----------------------------------------------

test_that("check_plate_row_counts() flags plates with the wrong row count when expected_n is supplied", {
  d <- tibble::tibble(plate_id = c("p1", "p1", "p2"))
  result <- check_plate_row_counts(d, expected_n = 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$plate_id, "p2")
  expect_equal(result$n_rows, 1)
})

test_that("check_plate_row_counts() returns zero rows when every plate matches expected_n", {
  d <- tibble::tibble(plate_id = c("p1", "p1", "p2", "p2"))
  result <- check_plate_row_counts(d, expected_n = 2)
  expect_equal(nrow(result), 0)
})

test_that("check_plate_row_counts() falls back to the most common row count when expected_n is NULL", {
  d <- tibble::tibble(plate_id = c("p1", "p1", "p2", "p2", "p3"))
  result <- check_plate_row_counts(d, expected_n = NULL)

  expect_equal(nrow(result), 1)
  expect_equal(result$plate_id, "p3")
  expect_equal(result$n_rows, 1)
})

test_that("check_plate_row_counts() supports a custom plate_col", {
  d <- tibble::tibble(my_plate = c("p1", "p1", "p2"))
  result <- check_plate_row_counts(d, plate_col = "my_plate", expected_n = 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$my_plate, "p2")
})

test_that("check_plate_row_counts() errors clearly when plate_col is missing from d", {
  d <- tibble::tibble(a = 1)
  expect_error(
    check_plate_row_counts(d, plate_col = "plate_id", expected_n = 1),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("check_plate_row_counts() aborts on a tied (ambiguous) mode when expected_n is NULL", {
  # p1 has row-count 2 (once), p2 has row-count 3 (once) -- genuinely tied,
  # must not silently resolve via which.max()'s first-hit behavior.
  d <- tibble::tibble(plate_id = c("p1", "p1", "p2", "p2", "p2"))
  expect_error(
    check_plate_row_counts(d, expected_n = NULL),
    class = "zfbehavior_error_ambiguous_expectation"
  )
})

test_that("check_plate_row_counts() validates expected_n as a scalar non-negative integer-like count", {
  d <- tibble::tibble(plate_id = c("p1", "p1"))
  expect_error(
    check_plate_row_counts(d, expected_n = c(1, 2)),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    check_plate_row_counts(d, expected_n = NA),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    check_plate_row_counts(d, expected_n = -1),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    check_plate_row_counts(d, expected_n = 1.5),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    check_plate_row_counts(d, expected_n = "2"),
    class = "zfbehavior_error_invalid_argument"
  )
})

# ---- validate_behavior_data() -----------------------------------------------

test_that("validate_behavior_data() passes on well-formed fixture data", {
  d <- zf_fixture_canonical()
  result <- validate_behavior_data(d)

  expect_s3_class(result, "zfbehavior_validation_result")
  expect_true(result$ok)
  expect_true(all(result$checks$status == "passed"))
})

test_that("validate_behavior_data() detects missing required columns", {
  d <- zf_fixture_canonical() %>% dplyr::select(-"response_value")
  result <- validate_behavior_data(d)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "required_columns_present", ]
  expect_equal(check$status, "failed")
  expect_true(grepl("Missing required canonical column", check$message))
  # downstream checks must be reported as skipped, not silently absent
  expect_true(all(
    result$checks$status[result$checks$code != "required_columns_present"] == "skipped"
  ))
})

test_that("validate_behavior_data() detects wrong column types", {
  d <- zf_fixture_canonical() %>%
    dplyr::mutate(is_vehicle_control = as.character(.data$is_vehicle_control))
  result <- validate_behavior_data(d)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "column_types", ]
  expect_equal(check$status, "failed")
  expect_true(grepl("is_vehicle_control", check$message))
})

test_that("validate_behavior_data() continues independent checks after a type failure, marking dependent checks as skipped", {
  d <- zf_fixture_canonical() %>%
    dplyr::mutate(is_vehicle_control = as.character(.data$is_vehicle_control))
  result <- validate_behavior_data(d)

  # unique_plate_well_time does not depend on is_vehicle_control's type
  # and should still run.
  uniq_check <- result$checks[result$checks$code == "unique_plate_well_time", ]
  expect_equal(uniq_check$status, "passed")

  # vc_replicate_counts DOES depend on is_vehicle_control's type and must
  # be explicitly skipped, not silently omitted.
  vc_check <- result$checks[result$checks$code == "vc_replicate_counts", ]
  expect_equal(vc_check$status, "skipped")
})

test_that("validate_behavior_data() detects duplicate plate x well x time rows", {
  d <- zf_fixture_canonical()
  d_dup <- dplyr::bind_rows(d, d[1, ])
  result <- validate_behavior_data(d_dup)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "unique_plate_well_time", ]
  expect_equal(check$status, "failed")
  expect_true(grepl("not unique", check$message))
  expect_equal(nrow(result$diagnostics$duplicate_keys), 1)
})

test_that("validate_behavior_data() detects missing vehicle-control wells", {
  d <- zf_fixture_canonical() %>% dplyr::filter(!.data$is_vehicle_control)
  result <- validate_behavior_data(d)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "vc_replicate_counts", ]
  expect_equal(check$status, "failed")
  expect_true(grepl("No vehicle-control", check$message))
})

test_that("validate_behavior_data() flags inconsistent VC well counts across plates when no expected count given", {
  d <- zf_fixture_canonical()
  # remove one VC well from a single plate to break consistency
  one_plate <- d$plate_id[1]
  bad_well <- d %>%
    dplyr::filter(.data$plate_id == one_plate, .data$is_vehicle_control) %>%
    dplyr::pull(.data$well_id) %>%
    unique() %>%
    utils::head(1)

  d_broken <- d %>%
    dplyr::filter(!(.data$plate_id == one_plate & .data$well_id == bad_well))

  result <- validate_behavior_data(d_broken)
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "vc_replicate_counts", ]
  expect_equal(check$status, "failed")
  expect_true(grepl("Vehicle-control well count is inconsistent", check$message))
})

test_that("validate_behavior_data() enforces expected_vc_wells when supplied", {
  d <- zf_fixture_canonical()
  result_ok <- validate_behavior_data(d, expected_vc_wells = 16)
  expect_true(result_ok$ok)

  result_bad <- validate_behavior_data(d, expected_vc_wells = 99)
  expect_false(result_bad$ok)
  check <- result_bad$checks[result_bad$checks$code == "vc_replicate_counts", ]
  expect_true(grepl("do not have the expected", check$message))
})

test_that("validate_behavior_data() detects no usable (keep_well) rows", {
  d <- zf_fixture_canonical() %>% dplyr::mutate(keep_well = FALSE)
  result <- validate_behavior_data(d)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "keep_well_has_usable_rows", ]
  expect_equal(check$status, "failed")
  expect_true(grepl("nothing would be analyzed", check$message))
})

# ---- validate_behavior_data(): per-timepoint replicate counting -----------

test_that("validate_behavior_data() flags a plate with zero VC wells at only ONE timepoint", {
  d <- tibble::tibble(
    plate_id = "p1",
    well_id = c("vc1", "vc2", "x1", "x1"),
    chemical_id = c("vc", "vc", "x", "x"),
    is_vehicle_control = c(TRUE, TRUE, FALSE, FALSE),
    concentration = c(0, 0, 1, 1),
    concentration_unit = "uM",
    time_value = c(1, 1, 1, 2),
    response_value = 1,
    keep_well = TRUE
  )
  result <- validate_behavior_data(d, expected_vc_wells = 2)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "vc_replicate_counts", ]
  expect_equal(check$status, "failed")
  # time_value == 2 has zero VC wells -- confirmed via the diagnostic table,
  # not just the summary message.
  diag <- result$diagnostics$vc_replicate_counts
  expect_equal(diag$n_vc_wells[diag$time_value == 2], 0)
})

test_that("validate_behavior_data() flags a plate with zero dosed wells at only ONE timepoint", {
  d <- tibble::tibble(
    plate_id = "p1",
    well_id = c("vc1", "vc2", "x1", "x1"),
    chemical_id = c("vc", "vc", "x", "x"),
    is_vehicle_control = c(TRUE, TRUE, FALSE, FALSE),
    concentration = c(0, 0, 1, 1),
    concentration_unit = "uM",
    time_value = c(1, 2, 1, 2),
    response_value = 1,
    keep_well = TRUE
  )
  # Remove the dosed well at time_value == 2 only.
  d_broken <- d %>% dplyr::filter(!(.data$well_id == "x1" & .data$time_value == 2))
  result <- validate_behavior_data(d_broken, expected_dose_wells = 1)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "dose_replicate_counts", ]
  expect_equal(check$status, "failed")
})

test_that("validate_behavior_data() flags a plate with NO dosed wells at all when expected_dose_wells is supplied", {
  d <- tibble::tibble(
    plate_id = c("p1", "p2"),
    well_id = c("vc1", "vc2"),
    chemical_id = "vc",
    is_vehicle_control = TRUE,
    concentration = 0,
    concentration_unit = "uM",
    time_value = 1,
    response_value = 1,
    keep_well = TRUE
  )
  result <- validate_behavior_data(d, expected_dose_wells = 1)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "dose_replicate_counts", ]
  expect_true(grepl("NO dosed", check$message))
})

test_that("a plate with zero VC wells does not silently disappear from the count table", {
  # Two plates: p1 has VC wells, p2 has NONE. Previously (collapsing across
  # all rows), p2 could disappear entirely from vc_counts rather than being
  # flagged with an explicit zero.
  d <- tibble::tibble(
    plate_id = c("p1", "p1", "p2"),
    well_id = c("vc1", "vc2", "x1"),
    chemical_id = c("vc", "vc", "x"),
    is_vehicle_control = c(TRUE, TRUE, FALSE),
    concentration = c(0, 0, 1),
    concentration_unit = "uM",
    time_value = 1,
    response_value = 1,
    keep_well = TRUE
  )
  result <- validate_behavior_data(d, expected_vc_wells = 2)
  expect_false(result$ok)
  diag <- result$diagnostics$vc_replicate_counts
  expect_true("p2" %in% diag$plate_id)
  expect_equal(diag$n_vc_wells[diag$plate_id == "p2"], 0)
})

# ---- validate_behavior_data(): well-level invariants (NEW) -----------------

test_that("validate_behavior_data() detects a well whose chemical_id changes across its trajectory", {
  d <- tibble::tibble(
    plate_id = "p1",
    well_id = c("a", "a", "b", "b"),
    chemical_id = c("x", "y", "z", "z"),
    is_vehicle_control = FALSE,
    concentration = 1,
    concentration_unit = "uM",
    time_value = c(1, 2, 1, 2),
    response_value = 1,
    keep_well = TRUE
  )
  result <- validate_behavior_data(d)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "well_level_invariants", ]
  expect_equal(check$status, "failed")
  diag <- result$diagnostics$well_invariants
  expect_true(all(diag$well_id == "a"))
  expect_true(all(diag$column == "chemical_id"))
})

test_that("validate_behavior_data() detects a well whose keep_well changes across its trajectory", {
  d <- tibble::tibble(
    plate_id = "p1",
    well_id = c("a", "a"),
    chemical_id = "x",
    is_vehicle_control = FALSE,
    concentration = 1,
    concentration_unit = "uM",
    time_value = c(1, 2),
    response_value = 1,
    keep_well = c(TRUE, FALSE)
  )
  result <- validate_behavior_data(d)

  expect_false(result$ok)
  diag <- result$diagnostics$well_invariants
  expect_true("keep_well" %in% diag$column)
})

test_that("validate_behavior_data() detects a well whose concentration changes across its trajectory", {
  d <- tibble::tibble(
    plate_id = "p1",
    well_id = c("a", "a"),
    chemical_id = "x",
    is_vehicle_control = FALSE,
    concentration = c(1, 2),
    concentration_unit = "uM",
    time_value = c(1, 2),
    response_value = 1,
    keep_well = TRUE
  )
  result <- validate_behavior_data(d)

  expect_false(result$ok)
  diag <- result$diagnostics$well_invariants
  expect_true("concentration" %in% diag$column)
})

test_that("validate_behavior_data() passes well-level invariants when every well is stable over time", {
  d <- zf_fixture_canonical()
  result <- validate_behavior_data(d)
  check <- result$checks[result$checks$code == "well_level_invariants", ]
  expect_equal(check$status, "passed")
  expect_equal(nrow(result$diagnostics$well_invariants), 0)
})

# ---- validate_behavior_data(): trajectory completeness (NEW) --------------

test_that("validate_behavior_data() detects a well missing a timepoint present for its plate", {
  d <- tibble::tibble(
    plate_id = "p1",
    well_id = c("a", "a", "b"),
    chemical_id = "x",
    is_vehicle_control = FALSE,
    concentration = 1,
    concentration_unit = "uM",
    time_value = c(1, 2, 1),
    response_value = 1,
    keep_well = TRUE
  )
  result <- validate_behavior_data(d)

  expect_false(result$ok)
  check <- result$checks[result$checks$code == "trajectory_completeness", ]
  expect_equal(check$status, "failed")
  diag <- result$diagnostics$trajectory_completeness
  expect_equal(diag$well_id, "b")
  expect_equal(diag$n_actual_time, 1L)
  expect_equal(diag$n_expected_time, 2L)
})

test_that("validate_behavior_data() passes trajectory completeness on well-formed fixture data", {
  d <- zf_fixture_canonical()
  result <- validate_behavior_data(d)
  check <- result$checks[result$checks$code == "trajectory_completeness", ]
  expect_equal(check$status, "passed")
})

test_that("rotating well membership across timepoints is detected by BOTH new checks", {
  # well "a" is present at time 1 as chemical x, then "disappears" and a
  # DIFFERENT well "b" (same plate) picks up at time 2 under chemical y --
  # this should show up as a trajectory-completeness gap for both wells,
  # not silently pass as if "a" and "b" were the same observational unit.
  d <- tibble::tibble(
    plate_id = "p1",
    well_id = c("a", "b"),
    chemical_id = c("x", "y"),
    is_vehicle_control = FALSE,
    concentration = 1,
    concentration_unit = "uM",
    time_value = c(1, 2),
    response_value = 1,
    keep_well = TRUE
  )
  result <- validate_behavior_data(d)
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "trajectory_completeness", ]
  expect_equal(check$status, "failed")
  expect_equal(nrow(result$diagnostics$trajectory_completeness), 2)
})

# ---- validate_behavior_data(): filled-columns check (default ON) ----------

test_that("validate_behavior_data() default required_filled_cols excludes response_value", {
  # zf_fixture_canonical() has real NA values in response_value (legitimate
  # tracking dropouts, imputed only via a later, explicit
  # replace_na_response_zero() step) -- the default check must NOT flag
  # this, or it would incorrectly fail essentially every real dataset.
  d <- zf_fixture_canonical()
  expect_true(any(is.na(d$response_value)))

  result <- validate_behavior_data(d)
  expect_true(result$ok)
  check <- result$checks[result$checks$code == "filled_columns", ]
  expect_equal(check$status, "passed")
})

test_that("validate_behavior_data() flags NA in a required canonical column other than response_value", {
  d <- zf_fixture_canonical()
  d$chemical_id[1] <- NA

  result <- validate_behavior_data(d)
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "filled_columns", ]
  expect_equal(check$status, "failed")
  expect_true(grepl("unexpected missing", check$message))
  expect_true(grepl("chemical_id", check$message))
})

test_that("validate_behavior_data() supports a caller-supplied required_filled_cols including response_value", {
  d <- zf_fixture_canonical()
  result <- validate_behavior_data(
    d,
    required_filled_cols = c("chemical_id", "response_value")
  )
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "filled_columns", ]
  expect_true(grepl("response_value", check$message))
})

# ---- validate_behavior_data(): per-plate row-count check (default ON) -----

test_that("validate_behavior_data() flags a plate with the wrong row count when expected_row_count is supplied", {
  d <- zf_fixture_canonical()
  n_per_plate <- nrow(d) / dplyr::n_distinct(d$plate_id)

  d_broken <- d[-1, ] # drop one row from the first plate present

  result <- validate_behavior_data(d_broken, expected_row_count = n_per_plate)
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "plate_row_counts", ]
  expect_equal(check$status, "failed")
  expect_true(grepl("do not have the expected row count", check$message))
})

test_that("validate_behavior_data() passes the row-count check when every plate matches expected_row_count", {
  d <- zf_fixture_canonical()
  n_per_plate <- nrow(d) / dplyr::n_distinct(d$plate_id)

  result <- validate_behavior_data(d, expected_row_count = n_per_plate)
  expect_true(result$ok)
})

test_that("validate_behavior_data() row-count check falls back to most-common count when expected_row_count is NULL", {
  d <- zf_fixture_canonical()
  d_broken <- d[-1, ] # drop one row from whichever plate happens to be first

  result <- validate_behavior_data(d_broken)
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "plate_row_counts", ]
  expect_true(grepl("do not have the expected row count", check$message))
})

test_that("validate_behavior_data() surfaces an ambiguous row-count mode as status = ambiguous, not a silent pick", {
  d <- tibble::tibble(
    plate_id = c("p1", "p1", "p2", "p2", "p2"),
    well_id = c("a", "b", "c", "d", "e"),
    chemical_id = "vc",
    is_vehicle_control = TRUE,
    concentration = 0,
    concentration_unit = "uM",
    time_value = 1,
    response_value = 1,
    keep_well = TRUE
  )
  result <- validate_behavior_data(d)
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "plate_row_counts", ]
  expect_equal(check$status, "ambiguous")
})

test_that("print.zfbehavior_validation_result() prints without error", {
  d <- zf_fixture_canonical()
  result <- validate_behavior_data(d)
  expect_output(print(result), "OK")

  bad <- validate_behavior_data(d %>% dplyr::select(-"response_value"))
  expect_output(print(bad), "FAILED")
})

# ---- validate_behavior_data(): expected_dose_wells (mirrors expected_vc_wells) ----

test_that("validate_behavior_data() enforces expected_dose_wells when supplied", {
  d <- zf_fixture_canonical()
  result_ok <- validate_behavior_data(d, expected_dose_wells = 16)
  expect_true(result_ok$ok)

  result_bad <- validate_behavior_data(d, expected_dose_wells = 99)
  expect_false(result_bad$ok)
  check <- result_bad$checks[result_bad$checks$code == "dose_replicate_counts", ]
  expect_true(grepl("do not have.*the expected 99 dosed wells", check$message))
})

test_that("validate_behavior_data() dose-well check falls back to consistency-only when expected_dose_wells is NULL", {
  d <- zf_fixture_canonical()
  # remove one dosed well from a single plate/concentration to break
  # consistency (but not affect VC wells)
  one_plate <- d$plate_id[1]
  bad_well <- d %>%
    dplyr::filter(
      .data$plate_id == one_plate,
      !.data$is_vehicle_control
    ) %>%
    dplyr::pull(.data$well_id) %>%
    unique() %>%
    utils::head(1)

  d_broken <- d %>%
    dplyr::filter(!(.data$plate_id == one_plate & .data$well_id == bad_well))

  result <- validate_behavior_data(d_broken)
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "dose_replicate_counts", ]
  expect_true(grepl("inconsistent replicate well counts across concentrations", check$message))
})

# ---- validate_behavior_data(): invalid expected-count arguments ------------

test_that("validate_behavior_data() rejects invalid expected_vc_wells/expected_dose_wells/expected_row_count", {
  d <- zf_fixture_canonical()
  expect_error(
    validate_behavior_data(d, expected_vc_wells = -1),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    validate_behavior_data(d, expected_dose_wells = NA),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    validate_behavior_data(d, expected_row_count = c(1, 2)),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    validate_behavior_data(d, expected_row_count = "500"),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("validate_behavior_data() validates expected-count arguments BEFORE inspecting the dataset", {
  # An empty/malformed dataset should not mask an invalid argument error
  # with an unrelated column-missing error -- argument validation runs first.
  d <- tibble::tibble(a = 1)
  expect_error(
    validate_behavior_data(d, expected_vc_wells = -5),
    class = "zfbehavior_error_invalid_argument"
  )
})

# ---- validate_behavior_data(): result structure (checks/diagnostics/metadata) ----

test_that("validate_behavior_data() returns the structured checks/diagnostics/metadata shape", {
  d <- zf_fixture_canonical()
  result <- validate_behavior_data(d)

  expect_s3_class(result, "zfbehavior_validation_result")
  expect_named(result, c("ok", "checks", "diagnostics", "metadata"))
  expect_s3_class(result$checks, "tbl_df")
  expect_true(all(c("code", "status", "severity", "message", "n_affected") %in% colnames(result$checks)))
  expect_true(all(result$checks$status %in% c("passed", "failed", "skipped", "ambiguous")))
  expect_type(result$diagnostics, "list")
  expect_equal(result$metadata$n_rows, nrow(d))
  expect_type(result$metadata$parameters, "list")
})

# ---- check_id_correspondence() ---------------------------------------------

test_that("check_id_correspondence() returns zero rows for a clean 1:1 mapping", {
  d <- tibble::tibble(
    plate_id = c("p1", "p1", "p2", "p2"),
    chemical_id = c("c1", "c1", "c2", "c2")
  )
  result <- check_id_correspondence(d, "plate_id", "chemical_id")
  expect_equal(nrow(result), 0)
})

test_that("check_id_correspondence() flags a plate_id linked to more than one id_col value", {
  d <- tibble::tibble(
    plate_id = c("p1", "p1", "p2"),
    chemical_id = c("c1", "c2", "c2")
  )
  result <- check_id_correspondence(d, "plate_id", "chemical_id")

  expect_true("p1" %in% result$plate_id)
  expect_true("c2" %in% result$chemical_id)
})

test_that("check_id_correspondence() errors clearly when a column is missing", {
  d <- tibble::tibble(plate_id = "p1")
  expect_error(
    check_id_correspondence(d, "plate_id", "missing_col"),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("check_id_correspondence() treats a missing (NA) identifier value as a correspondence failure", {
  d <- tibble::tibble(
    plate_id = c("p1", "p1"),
    chemical_id = c("c1", NA)
  )
  result <- check_id_correspondence(d, "plate_id", "chemical_id")

  expect_equal(nrow(result), 1)
  expect_equal(result$plate_id, "p1")
  expect_true(grepl("missing_value", result$reason))
})

test_that("check_id_correspondence() explicitly represents NA as '<NA>' in the display string, not silently dropped", {
  d <- tibble::tibble(
    plate_id = c("p1", "p1", "p1"),
    chemical_id = c("c1", "c2", NA)
  )
  result <- check_id_correspondence(d, "plate_id", "chemical_id")

  row <- result[result$plate_id == "p1" & !is.na(result$plate_id), ]
  expect_equal(row$n_id_values, 2) # 2 distinct NON-missing values
  expect_true(grepl("<NA>", row$id_values)) # but NA is still shown explicitly
  expect_true(grepl("c1", row$id_values))
  expect_true(grepl("c2", row$id_values))
})

test_that("a single mapping such as p1 -> NA does not silently pass as a valid 1:1 correspondence", {
  d <- tibble::tibble(plate_id = "p1", chemical_id = NA_character_)
  result <- check_id_correspondence(d, "plate_id", "chemical_id")
  expect_equal(nrow(result), 1)
  expect_true(grepl("missing_value", result$reason))
})

# ---- validate_combined_dataset() -------------------------------------------

test_that("validate_combined_dataset() passes when unique_id_cols correspond 1:1 and counts match", {
  d <- tibble::tibble(
    plate_id = c("p1", "p1", "p2", "p2"),
    chemical_id = c("c1", "c1", "c2", "c2"),
    CHEMICAL_NAME = c("A", "A", "B", "B")
  )
  result <- validate_combined_dataset(
    d,
    unique_id_cols = "chemical_id",
    expected_n_distinct = list(CHEMICAL_NAME = 2)
  )
  expect_true(result$ok)
})

test_that("validate_combined_dataset() flags a broken 1:1 correspondence", {
  d <- tibble::tibble(
    plate_id = c("p1", "p1", "p2"),
    chemical_id = c("c1", "c2", "c2")
  )
  result <- validate_combined_dataset(d, unique_id_cols = "chemical_id")
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "id_correspondence:chemical_id", ]
  expect_equal(check$status, "failed")
  expect_true(grepl("not in 1:1 correspondence", check$message))
})

test_that("validate_combined_dataset() flags an unexpected distinct count", {
  d <- tibble::tibble(
    plate_id = c("p1", "p2", "p3"),
    CHEMICAL_NAME = c("A", "A", "B")
  )
  result <- validate_combined_dataset(
    d,
    expected_n_distinct = list(CHEMICAL_NAME = 3)
  )
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "distinct_count:CHEMICAL_NAME", ]
  expect_true(grepl("has 2 distinct value\\(s\\), expected 3", check$message))
})

test_that("validate_combined_dataset() reports missing referenced columns clearly", {
  d <- tibble::tibble(plate_id = "p1")
  result <- validate_combined_dataset(d, unique_id_cols = "missing_col")
  expect_false(result$ok)
  check <- result$checks[result$checks$code == "required_columns_present", ]
  expect_true(grepl("not found in d", check$message))
})

test_that("validate_combined_dataset() supports a custom plate_col", {
  d <- tibble::tibble(
    my_plate = c("p1", "p1", "p2"),
    chemical_id = c("c1", "c1", "c2")
  )
  result <- validate_combined_dataset(
    d,
    unique_id_cols = "chemical_id",
    plate_col = "my_plate"
  )
  expect_true(result$ok)
})

test_that("validate_combined_dataset() validates expected_n_distinct as a uniquely named list of valid counts", {
  d <- tibble::tibble(plate_id = "p1", CHEMICAL_NAME = "A")
  expect_error(
    validate_combined_dataset(d, expected_n_distinct = list(1, 2)),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    validate_combined_dataset(
      d,
      expected_n_distinct = list(CHEMICAL_NAME = 1, CHEMICAL_NAME = 2)
    ),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    validate_combined_dataset(d, expected_n_distinct = list(CHEMICAL_NAME = -1)),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("validate_combined_dataset() returns the structured checks/diagnostics/metadata shape", {
  d <- tibble::tibble(
    plate_id = c("p1", "p1", "p2", "p2"),
    chemical_id = c("c1", "c1", "c2", "c2"),
    CHEMICAL_NAME = c("A", "A", "B", "B")
  )
  result <- validate_combined_dataset(
    d,
    unique_id_cols = "chemical_id",
    expected_n_distinct = list(CHEMICAL_NAME = 2)
  )
  expect_named(result, c("ok", "checks", "diagnostics", "metadata"))
  expect_true(all(c("code", "status", "severity", "message", "n_affected") %in% colnames(result$checks)))
  expect_equal(result$metadata$n_rows, nrow(d))
})
