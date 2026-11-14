# ---- normalize_none() -------------------------------------------------------

test_that("normalize_none() is a true identity on endpoint_value", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "E1",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(10, 20)
  )
  out <- normalize_none(d)
  expect_equal(out$endpoint_value_norm, out$endpoint_value)
  expect_true("denom" %in% colnames(out))
})

# ---- normalize_ratio_to_control_percent() -----------------------------------

test_that("normalize_ratio_to_control_percent() matches the published formula", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "E1",
    is_vehicle_control = c(TRUE, TRUE, FALSE),
    endpoint_value = c(8, 12, 20) # VC median = 10
  )
  out <- normalize_ratio_to_control_percent(d)
  treated <- out$endpoint_value_norm[!out$is_vehicle_control]
  expect_equal(treated, (20 / 10) * 100 - 100)
})

test_that("normalize_ratio_to_control_percent() gives 0 for a value equal to the VC median", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "E1",
    is_vehicle_control = c(TRUE, TRUE, FALSE),
    endpoint_value = c(10, 10, 10)
  )
  out <- normalize_ratio_to_control_percent(d)
  expect_equal(out$endpoint_value_norm[!out$is_vehicle_control], 0)
})

# ---- normalize_log_diff_to_control_percent() --------------------------------

test_that("normalize_log_diff_to_control_percent() matches the published log10(AUC+1) formula", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "E1",
    is_vehicle_control = c(TRUE, TRUE, FALSE),
    endpoint_value = c(9, 11, 99) # log10(AUC+1): VC median = log10(11), treated = log10(100)=2
  )
  out <- normalize_log_diff_to_control_percent(d)
  vc_med_log <- stats::median(log10(c(9, 11) + 1))
  expect_equal(
    out$endpoint_value_norm[!out$is_vehicle_control],
    (log10(100) - vc_med_log) * 100
  )
})

# ---- normalize_log_ratio_to_control_percent() -------------------------------

test_that("normalize_log_ratio_to_control_percent() matches the log-ratio formula", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "E1",
    is_vehicle_control = c(TRUE, TRUE, FALSE),
    endpoint_value = c(10, 10, 100)
  )
  out <- normalize_log_ratio_to_control_percent(d)
  expect_equal(
    out$endpoint_value_norm[!out$is_vehicle_control],
    ((log10(100) - log10(10)) / log10(10)) * 100
  )
})

test_that("normalize_log_ratio_to_control_percent() errors by default on a zero (log-scale) vehicle-control denominator", {
  # log10(1) = 0 -> the vehicle-control denominator itself is 0 -> the
  # ratio is undefined for the WHOLE stratum, not just the treated row.
  # Default undefined = "error": never silently repair this.
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "E1",
    is_vehicle_control = c(TRUE, TRUE, FALSE),
    endpoint_value = c(1, 1, 5)
  )
  expect_error(
    normalize_log_ratio_to_control_percent(d),
    class = "zfbehavior_error_invalid_denominator"
  )
})

test_that("normalize_log_ratio_to_control_percent() undefined = 'na' marks the WHOLE affected stratum NA, including control rows", {
  # A prior version of this function converted only the Inf (treated-row)
  # case to 0 and left the NaN (control-row) case untouched/unflagged.
  # undefined = "na" must mark BOTH.
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "E1",
    is_vehicle_control = c(TRUE, TRUE, FALSE),
    endpoint_value = c(1, 1, 5) # log10(1) = 0 -> zero denom
  )
  out <- normalize_log_ratio_to_control_percent(d, undefined = "na")
  expect_true(all(is.na(out$endpoint_value_norm)))
})

test_that("normalize_log_ratio_to_control_percent() undefined = 'legacy_zero' reproduces the exact prior/legacy behavior (Inf -> 0, NaN left as-is)", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "E1",
    is_vehicle_control = c(TRUE, TRUE, FALSE),
    endpoint_value = c(1, 1, 5) # log10(1) = 0 -> division by zero denom
  )
  out <- normalize_log_ratio_to_control_percent(d, undefined = "legacy_zero")
  expect_equal(out$endpoint_value_norm[!out$is_vehicle_control], 0)
  # The control rows compute 0/0 = NaN and are NOT touched by
  # undefined = "legacy_zero" (is.infinite(NaN) is FALSE) -- this was
  # previously silently true and unasserted; now explicitly documented
  # and tested.
  expect_true(all(is.nan(out$endpoint_value_norm[out$is_vehicle_control])))
})

test_that("normalize_log_ratio_to_control_percent() undefined = 'error' names the affected plate x endpoint stratum in the error data", {
  d <- tibble::tibble(
    plate_id = c("P1", "P1", "P1", "P2", "P2", "P2"),
    endpoint = "E1",
    is_vehicle_control = rep(c(TRUE, TRUE, FALSE), 2),
    # P1: zero denom (bad); P2: denom = log10(10) = 1 (fine)
    endpoint_value = c(1, 1, 5, 10, 10, 100)
  )
  cnd <- rlang::catch_cnd(
    normalize_log_ratio_to_control_percent(d),
    classes = "error"
  )
  expect_s3_class(cnd, "zfbehavior_error_invalid_denominator")
  expect_equal(cnd$data$plate_id, "P1")
})

# ---- regression tests against legacy norm_endpoint() ------------------------

test_that("normalize_ratio_to_control_percent() matches legacy norm_endpoint(type='simi') (regression)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  legacy <- zf_legacy_outputs()

  sim_segments <- list(
    LD = unlist(
      segments[c("L1", "D1", "L2", "D2", "D2-ext")],
      use.names = FALSE
    )
  )
  sim <- suppressWarnings(calculate_movement_similarity_endpoints(
    d,
    sim_segments,
    metric = "spearman"
  ))
  sim_norm <- normalize_ratio_to_control_percent(sim)

  legacy_norm <- legacy$movement_similarity_norm %>%
    dplyr::filter(.data$endpoint == "LD_spearman") %>%
    dplyr::rename(well_id = "embryo_id", is_vehicle_control = "is_VC") %>%
    dplyr::mutate(endpoint = "LD")

  cmp <- zf_expect_matching_keys(
    sim_norm,
    legacy_norm,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )

  expect_equal(
    cmp$endpoint_value_norm_new,
    cmp$endpoint_value_norm_ref,
    tolerance = 1e-8
  )
})

# NOTE: this checks the normalize_log_diff_to_control_percent() FORMULA
# against the original norm_endpoint(type = "auc") formula, both applied to
# the same correct, bug-free per-segment AUC values (see the note above
# calculate_total_distance_endpoints()'s test for why "legacy" here means
# the isolated, bug-free reference, not any historical/production output --
# create_auc_endpoints() was never run in production). It is a
# formula-correctness check, not a reproduction of a historical result.
test_that("normalize_log_diff_to_control_percent() matches the original norm_endpoint(type='auc') formula (applied to correct, bug-free AUC values)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  legacy <- zf_legacy_outputs()

  td <- calculate_total_distance_endpoints(d, segments)
  td_norm <- normalize_log_diff_to_control_percent(td)

  legacy_norm <- legacy$total_distance_norm_auc %>%
    dplyr::rename(well_id = "embryo_id", is_vehicle_control = "is_VC")

  cmp <- zf_expect_matching_keys(
    td_norm,
    legacy_norm,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )

  expect_equal(
    cmp$endpoint_value_norm_new,
    cmp$endpoint_value_norm_ref,
    tolerance = 1e-8
  )
})

test_that("normalize_none() matches legacy norm_endpoint(type='auc_raw') for Distance Change/Shift (regression)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  legacy <- zf_legacy_outputs()

  dcs <- calculate_distance_change_shift_endpoints(d, segments)
  dcs_long <- dcs %>%
    tidyr::pivot_longer(
      cols = c("up_auc", "down_auc", "both_auc"),
      names_to = "auc_type",
      values_to = "endpoint_value"
    ) %>%
    dplyr::mutate(
      endpoint = paste0(.data$endpoint, "@", .data$auc_type),
      endpoint_value = abs(.data$endpoint_value)
    ) %>%
    dplyr::select(-"auc_type")
  dcs_norm <- normalize_none(dcs_long)

  legacy_norm <- legacy$distance_change_shift_norm %>%
    dplyr::rename(well_id = "embryo_id", is_vehicle_control = "is_VC")

  cmp <- zf_expect_matching_keys(
    dcs_norm,
    legacy_norm,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )

  expect_equal(
    cmp$endpoint_value_norm_new,
    cmp$endpoint_value_norm_ref,
    tolerance = 1e-8
  )
})

# ---- transform_for_normality() ----------------------------------------------

test_that("transform_for_normality() applies log10 correctly", {
  expect_equal(
    transform_for_normality(c(1, 10, 100), method = "log10"),
    c(0, 1, 2)
  )
})

test_that("transform_for_normality() applies Fisher-Z correctly", {
  x <- c(0, 0.5, -0.5)
  expected <- 0.5 * log((1 + x) / (1 - x))
  expect_equal(transform_for_normality(x, method = "fisher_z"), expected)
})

test_that("transform_for_normality() 'none' is a true identity", {
  x <- c(1, 2, 3)
  expect_equal(transform_for_normality(x, method = "none"), x)
})

test_that("transform_for_normality() errors on non-numeric input", {
  expect_error(
    transform_for_normality(c("a", "b"), method = "log10"),
    class = "zfbehavior_error_invalid_input"
  )
})

test_that("transform_for_normality() errors on invalid method", {
  expect_error(transform_for_normality(1:3, method = "bogus"))
})
