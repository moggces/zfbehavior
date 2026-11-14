# ---- calculate_total_distance_endpoints() ----------------------------------

test_that("calculate_total_distance_endpoints() computes a known trapezoid AUC", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = "A1",
    is_vehicle_control = TRUE,
    time_value = 1:4,
    response_value = c(10, 20, 5, 15),
    keep_well = TRUE
  )
  segments <- list(seg1 = 1:2, seg2 = 3:4)

  out <- calculate_total_distance_endpoints(d, segments)

  expect_equal(nrow(out), 2)
  expect_equal(out$endpoint_value[out$endpoint == "seg1"], (10 + 20) / 2 * 1)
  expect_equal(out$endpoint_value[out$endpoint == "seg2"], (5 + 15) / 2 * 1)
})

test_that("calculate_total_distance_endpoints() errors on missing canonical columns", {
  d <- tibble::tibble(plate_id = "P1")
  expect_error(
    calculate_total_distance_endpoints(d, list(seg1 = 1:2)),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("calculate_total_distance_endpoints() errors if any keep_well == FALSE row is present", {
  # Mirrors the real historical production pipeline
  # (original endpoint script: filter(KEEP_WELL_DATA)),
  # which ALWAYS filters dead/malformed wells before any endpoint
  # calculation. keep_well must be enforced, not merely documented.
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2"),
    is_vehicle_control = TRUE,
    time_value = c(1, 1),
    response_value = c(10, 20),
    keep_well = c(TRUE, FALSE)
  )
  expect_error(
    calculate_total_distance_endpoints(d, list(seg1 = 1)),
    class = "zfbehavior_error_excluded_wells_present"
  )
})

test_that("calculate_total_distance_endpoints() treats keep_well == NA the same as FALSE", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2"),
    is_vehicle_control = TRUE,
    time_value = c(1, 1),
    response_value = c(10, 20),
    keep_well = c(TRUE, NA)
  )
  expect_error(
    calculate_total_distance_endpoints(d, list(seg1 = 1)),
    class = "zfbehavior_error_excluded_wells_present"
  )
})

test_that("calculate_total_distance_endpoints() succeeds once excluded wells are filtered out first", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2"),
    is_vehicle_control = TRUE,
    time_value = c(1, 1),
    response_value = c(10, 20),
    keep_well = c(TRUE, FALSE)
  ) |>
    dplyr::filter(.data$keep_well)
  expect_no_error(calculate_total_distance_endpoints(d, list(seg1 = 1)))
})

test_that("calculate_total_distance_endpoints() errors clearly on NA response_value", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = "A1",
    is_vehicle_control = TRUE,
    time_value = 1:3,
    response_value = c(1, NA, 3),
    keep_well = TRUE
  )
  expect_error(
    calculate_total_distance_endpoints(d, list(seg1 = 1:3)),
    class = "zfbehavior_error_missing_response_values"
  )
})

test_that("calculate_total_distance_endpoints() errors on invalid/overlapping segments", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = "A1",
    is_vehicle_control = TRUE,
    time_value = 1:4,
    response_value = c(1, 2, 3, 4),
    keep_well = TRUE
  )
  expect_error(
    calculate_total_distance_endpoints(d, list(1:2, 3:4)),
    class = "zfbehavior_error_invalid_segments"
  )
  expect_error(
    calculate_total_distance_endpoints(d, list(a = 1:3, b = 3:4)),
    class = "zfbehavior_error_overlapping_segments"
  )
})

test_that("calculate_total_distance_endpoints() errors on duplicate plate_id x well_id x segment x time_value rows", {
  # Regression test for a real bug: a previous version of this check
  # called dplyr::distinct() on the complete key BEFORE dplyr::count(),
  # making every resulting count exactly 1 -- the duplicate-row abort
  # branch could never fire, and the duplicate rows silently affected
  # the AUC calculation (sum() over both copies).
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = "A1",
    is_vehicle_control = TRUE,
    time_value = c(1, 1, 2),
    response_value = c(10, 10, 20),
    keep_well = TRUE
  )
  expect_error(
    calculate_total_distance_endpoints(d, list(seg1 = 1:2)),
    class = "zfbehavior_error_duplicate_rows"
  )
})

test_that("calculate_total_distance_endpoints() duplicate error attaches the affected keys as data", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = "A1",
    is_vehicle_control = TRUE,
    time_value = c(1, 1, 2),
    response_value = c(10, 10, 20),
    keep_well = TRUE
  )
  cnd <- rlang::catch_cnd(
    calculate_total_distance_endpoints(d, list(seg1 = 1:2)),
    classes = "error"
  )
  expect_s3_class(cnd, "zfbehavior_error_duplicate_rows")
  expect_true(nrow(cnd$data) > 0)
  expect_equal(cnd$data$well_id, "A1")
})

# NOTE: this is NOT a regression test against any historical/production
# result. create_auc_endpoints() (the original Total Distance function) has
# an order-of-operations bug: it computes the trapezoid area across the
# full continuous time series BEFORE segment boundaries are known, so the
# half-trapezoid bridging two adjacent segments leaks entirely into the
# second segment -- inflating every segment's Total Distance except the
# first, by ~56-81 units/segment on this fixture (verified interactively).
# Production never actually ran this function (it is commented out in
# original endpoint script), so there is no real historical
# output to reproduce. `legacy$total_distance_raw` is instead the CORRECT,
# bug-free answer, obtained by calling the original, unmodified
# create_auc_endpoints() once PER SEGMENT IN ISOLATION (see
# data-raw/build_legacy_snapshot.R) so the boundary-leakage bug cannot
# occur. This test therefore checks that calculate_total_distance_endpoints()
# computes the mathematically correct per-segment AUC, using the original
# code path (in isolation) as an independent correctness oracle -- not that
# it reproduces any historical or published number. Compare with
# calculate_distance_change_shift_endpoints()/calculate_movement_similarity_endpoints()'s
# tests below, which DO reproduce real production output exactly (see
# zf_authoritative_outputs()).
test_that("calculate_total_distance_endpoints() computes the correct per-segment AUC (verified against isolated, bug-free create_auc_endpoints())", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  legacy <- zf_legacy_outputs()

  new_out <- calculate_total_distance_endpoints(d, segments)

  legacy_out <- legacy$total_distance_raw %>%
    dplyr::rename(well_id = "embryo_id", is_vehicle_control = "is_VC")

  cmp <- zf_expect_matching_keys(
    new_out,
    legacy_out,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )

  expect_equal(
    cmp$endpoint_value_new,
    cmp$endpoint_value_ref,
    tolerance = 1e-8
  )
})

# ---- calculate_distance_change_shift_endpoints() ----------------------------

test_that("calculate_distance_change_shift_endpoints() computes known up/down/both areas", {
  # Plate with 2 VC wells (median trace = c(5, 5)) and one treated well
  # that is strictly above at t=1 and strictly below at t=2.
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("V1", "V2", "T1", "T1"),
    is_vehicle_control = c(TRUE, TRUE, FALSE, FALSE),
    time_value = c(1, 1, 1, 2),
    response_value = c(5, 5, 10, 0),
    keep_well = TRUE
  )
  d <- dplyr::bind_rows(
    d,
    tibble::tibble(
      plate_id = "P1",
      well_id = c("V1", "V2"),
      is_vehicle_control = TRUE,
      time_value = 2,
      response_value = 5,
      keep_well = TRUE
    )
  )
  segments <- list(seg1 = 1:2)

  out <- calculate_distance_change_shift_endpoints(d, segments)
  treated <- out %>% dplyr::filter(.data$well_id == "T1")

  # diff = c(10-5, 0-5) = c(5, -5); up = c(5,0); down = c(0,5)
  expect_equal(treated$up_auc, (5 + 0) / 2 * 1)
  expect_equal(treated$down_auc, (0 + 5) / 2 * 1)
  expect_equal(treated$both_auc, (5 + 5) / 2 * 1)
})

test_that("calculate_distance_change_shift_endpoints() errors clearly on NA response_value", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("V1", "T1"),
    is_vehicle_control = c(TRUE, FALSE),
    time_value = c(1, 1),
    response_value = c(5, NA),
    keep_well = TRUE
  )
  expect_error(
    calculate_distance_change_shift_endpoints(d, list(seg1 = 1)),
    class = "zfbehavior_error_missing_response_values"
  )
})

test_that("calculate_distance_change_shift_endpoints() errors when no VC wells present at a timepoint", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = "T1",
    is_vehicle_control = FALSE,
    time_value = 1:2,
    response_value = c(1, 2),
    keep_well = TRUE
  )
  expect_error(
    calculate_distance_change_shift_endpoints(d, list(seg1 = 1:2)),
    class = "zfbehavior_error_missing_reference"
  )
})

test_that("calculate_distance_change_shift_endpoints() errors on duplicate plate_id x well_id x segment x time_value rows (treated well)", {
  # Distance Change/Shift previously had NO duplicate-key preflight at
  # all (unlike Total Distance's, which existed but was broken -- see
  # the analogous test above).
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("V1", "T1", "T1"),
    is_vehicle_control = c(TRUE, FALSE, FALSE),
    time_value = c(1, 1, 1),
    response_value = c(5, 10, 10),
    keep_well = TRUE
  )
  expect_error(
    calculate_distance_change_shift_endpoints(d, list(seg1 = 1)),
    class = "zfbehavior_error_duplicate_rows"
  )
})

test_that("calculate_distance_change_shift_endpoints() errors on duplicate plate_id x well_id x segment x time_value rows (control well)", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("V1", "V1", "T1"),
    is_vehicle_control = c(TRUE, TRUE, FALSE),
    time_value = c(1, 1, 1),
    response_value = c(5, 5, 10),
    keep_well = TRUE
  )
  expect_error(
    calculate_distance_change_shift_endpoints(d, list(seg1 = 1)),
    class = "zfbehavior_error_duplicate_rows"
  )
})

test_that("calculate_distance_change_shift_endpoints() matches REAL production output (regression, authoritative)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  auth <- zf_authoritative_outputs()

  new_out <- calculate_distance_change_shift_endpoints(d, segments)
  new_long <- new_out %>%
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

  cmp <- zf_expect_matching_keys(
    new_long,
    auth$distance_change_shift,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )

  expect_equal(
    cmp$endpoint_value_new,
    cmp$endpoint_value_ref,
    tolerance = 1e-8
  )
})

test_that("calculate_distance_change_shift_endpoints() matches original create_auc2_endpoints() (regression)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  legacy <- zf_legacy_outputs()

  new_out <- calculate_distance_change_shift_endpoints(d, segments)

  legacy_wide <- legacy$distance_change_shift_raw %>%
    tidyr::separate("endpoint", into = c("endpoint", "auc_type"), sep = "@") %>%
    tidyr::pivot_wider(
      names_from = "auc_type",
      values_from = "endpoint_value"
    ) %>%
    dplyr::rename(well_id = "embryo_id", is_vehicle_control = "is_VC")

  cmp <- zf_expect_matching_keys(
    new_out,
    legacy_wide,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )

  expect_equal(cmp$up_auc_new, cmp$up_auc_ref, tolerance = 1e-8)
  expect_equal(cmp$down_auc_new, cmp$down_auc_ref, tolerance = 1e-8)
  expect_equal(cmp$both_auc_new, cmp$both_auc_ref, tolerance = 1e-8)
})

# ---- calculate_movement_similarity_endpoints() ------------------------------

test_that("calculate_movement_similarity_endpoints() errors clearly on NA response_value", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = rep(c("V1", "V2"), each = 2),
    is_vehicle_control = TRUE,
    time_value = rep(1:2, 2),
    response_value = c(1, NA, 3, 4),
    keep_well = TRUE
  )
  expect_error(
    calculate_movement_similarity_endpoints(
      d,
      list(all = 1:2),
      metric = "spearman"
    ),
    class = "zfbehavior_error_missing_response_values"
  )
})

test_that("calculate_movement_similarity_endpoints() errors on duplicate plate_id x well_id x segment x time_value rows", {
  # Movement Similarity previously had NO duplicate-key preflight at all;
  # duplicates could reach pivot_wider() uncontrolled, risking list
  # columns / low-level warnings rather than a clear, actionable error.
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("V1", "V1", "V2"),
    is_vehicle_control = TRUE,
    time_value = c(1, 1, 1),
    response_value = c(1, 1, 2),
    keep_well = TRUE
  )
  expect_error(
    calculate_movement_similarity_endpoints(d, list(all = 1), metric = "spearman"),
    class = "zfbehavior_error_duplicate_rows"
  )
})

test_that("calculate_movement_similarity_endpoints() gives similarity 1 for identical traces", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = rep(c("V1", "V2"), each = 3),
    is_vehicle_control = TRUE,
    time_value = rep(1:3, 2),
    response_value = rep(c(1, 5, 2), 2),
    keep_well = TRUE
  )
  out <- calculate_movement_similarity_endpoints(
    d,
    list(all = 1:3),
    metric = "spearman"
  )
  expect_equal(unique(out$endpoint_value), 1)
})

test_that("calculate_movement_similarity_endpoints() treats undefined (zero-variance) correlation as similarity 0, not an error or 1", {
  # T1's trace is perfectly constant -> correlation with V1/V2 is undefined
  # (0/0), which stats::cor() returns as NA. Per an explicit user decision
  # (see the comment in cal_mean_similarity(), R/endpoints.R, for the full
  # investigation/rationale), this package treats undefined similarity as
  # 0 (no similarity signal to credit), matching the ORIGINAL LEGACY
  # scripts' behavior (cal_mean_simi_vals(),
  # original similarity code), NOT an error (this package's OWN
  # prior behavior) and NOT 1 (a briefly-tried, explicitly reverted
  # substitution).
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = rep(c("V1", "V2", "T1"), each = 3),
    is_vehicle_control = c(rep(TRUE, 6), rep(FALSE, 3)),
    time_value = rep(1:3, 3),
    response_value = c(1, 2, 3, 2, 3, 4, 5, 5, 5),
    keep_well = TRUE
  )
  out <- suppressWarnings(
    calculate_movement_similarity_endpoints(
      d,
      list(all = 1:3),
      metric = "spearman"
    )
  )
  t1_val <- out$endpoint_value[out$well_id == "T1"]
  expect_equal(t1_val, 0)
})

test_that("calculate_movement_similarity_endpoints() treats a fully-NA-then-zero-imputed well the same way (real-data scenario)", {
  # Mirrors the real 8-well scenario found in
  # the original study data: a well with response_value == NA at
  # EVERY timepoint (a fully missing tracking record), which becomes a
  # constant all-zero vector after replace_na_response_zero(). See
  # cal_mean_similarity() comment for full provenance.
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = rep(c("V1", "V2", "T1"), each = 3),
    is_vehicle_control = c(rep(TRUE, 6), rep(FALSE, 3)),
    time_value = rep(1:3, 3),
    response_value = c(1, 2, 3, 2, 3, 4, NA, NA, NA),
    keep_well = TRUE
  )
  d <- replace_na_response_zero(d)
  out <- suppressWarnings(
    calculate_movement_similarity_endpoints(
      d,
      list(all = 1:3),
      metric = "spearman"
    )
  )
  t1_val <- out$endpoint_value[out$well_id == "T1"]
  expect_equal(t1_val, 0)
})

test_that("calculate_movement_similarity_endpoints() reproduces the real 8-well zero-variance scenario (regression)", {
  # See data-raw/build_zero_variance_wells_fixture.R for full provenance
  # and citation: 8 real wells (across 8 different plates) in
  # the original study data with response_value == NA at every
  # timepoint, none flagged dead/malformed by upstream QC. This is the
  # actual real-world case (not a constructed example) that motivated the
  # zero-variance investigation in cal_mean_similarity() (originally
  # substituted 1, now reverted to 0 -- see that function's comment).
  fx <- zf_zero_variance_wells_fixture()
  fx <- replace_na_response_zero(fx)

  out <- suppressWarnings(
    calculate_movement_similarity_endpoints(
      fx,
      list(LD = 1:50),
      metric = "spearman"
    )
  )

  zero_variance_well_ids <- tibble::tribble(
    ~plate_id  , ~well_id ,
    "Plate_33" , "H4"     ,
    "Plate_41" , "A9"     ,
    "Plate_45" , "C3"     ,
    "Plate_48" , "E10"    ,
    "Plate_49" , "H12"    ,
    "Plate_76" , "C10"    ,
    "Plate_80" , "C11"    ,
    "Plate_93" , "A9"
  )

  check <- out |>
    dplyr::semi_join(
      zero_variance_well_ids,
      by = c("plate_id", "well_id")
    )

  expect_equal(nrow(check), 8)
  expect_true(all(check$endpoint_value == 0))
})

test_that("calculate_movement_similarity_endpoints() sets negative/NA similarity to 0 before averaging", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = rep(c("V1", "V2", "T1"), each = 3),
    is_vehicle_control = c(rep(TRUE, 6), rep(FALSE, 3)),
    time_value = rep(1:3, 3),
    # T1 perfectly anti-correlated with V1/V2 -> negative spearman -> treated as 0
    response_value = c(1, 2, 3, 1, 2, 3, 3, 2, 1),
    keep_well = TRUE
  )
  out <- calculate_movement_similarity_endpoints(
    d,
    list(all = 1:3),
    metric = "spearman"
  )
  t1_val <- out$endpoint_value[out$well_id == "T1"]
  expect_equal(t1_val, 0)
})

test_that("calculate_movement_similarity_endpoints() matches REAL production output (regression, authoritative, spearman+pearson)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  auth <- zf_authoritative_outputs()

  sim_segments <- list(
    LD = unlist(
      segments[c("L1", "D1", "L2", "D2", "D2-ext")],
      use.names = FALSE
    )
  )

  new_sp <- calculate_movement_similarity_endpoints(
    d,
    sim_segments,
    metric = "spearman"
  ) %>%
    dplyr::mutate(endpoint = "LD_spearman")
  new_pe <- calculate_movement_similarity_endpoints(
    d,
    sim_segments,
    metric = "pearson"
  ) %>%
    dplyr::mutate(endpoint = "LD_pearson")
  new_out <- dplyr::bind_rows(new_sp, new_pe)

  cmp <- zf_expect_matching_keys(
    new_out,
    auth$movement_similarity,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )

  expect_equal(
    cmp$endpoint_value_new,
    cmp$endpoint_value_ref,
    tolerance = 1e-8
  )
})

test_that("calculate_movement_similarity_endpoints() matches original create_simi_endpoints() (regression, spearman)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  legacy <- zf_legacy_outputs()

  sim_segments <- list(
    LD = unlist(
      segments[c("L1", "D1", "L2", "D2", "D2-ext")],
      use.names = FALSE
    )
  )
  new_out <- suppressWarnings(
    calculate_movement_similarity_endpoints(
      d,
      sim_segments,
      metric = "spearman"
    )
  )

  legacy_sim <- legacy$movement_similarity_raw %>%
    dplyr::filter(.data$endpoint == "LD_spearman") %>%
    dplyr::rename(well_id = "embryo_id", is_vehicle_control = "is_VC") %>%
    dplyr::mutate(endpoint = "LD")

  cmp <- zf_expect_matching_keys(
    new_out,
    legacy_sim,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )

  expect_equal(
    cmp$endpoint_value_new,
    cmp$endpoint_value_ref,
    tolerance = 1e-8
  )
})

test_that("calculate_movement_similarity_endpoints() supports pearson metric and matches legacy", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  legacy <- zf_legacy_outputs()

  sim_segments <- list(
    LD = unlist(
      segments[c("L1", "D1", "L2", "D2", "D2-ext")],
      use.names = FALSE
    )
  )
  new_out <- suppressWarnings(
    calculate_movement_similarity_endpoints(d, sim_segments, metric = "pearson")
  )

  legacy_sim <- legacy$movement_similarity_raw %>%
    dplyr::filter(.data$endpoint == "LD_pearson") %>%
    dplyr::rename(well_id = "embryo_id", is_vehicle_control = "is_VC") %>%
    dplyr::mutate(endpoint = "LD")

  cmp <- zf_expect_matching_keys(
    new_out,
    legacy_sim,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )

  expect_equal(
    cmp$endpoint_value_new,
    cmp$endpoint_value_ref,
    tolerance = 1e-8
  )
})

# ---- cosine similarity: known-value tests (regression for the fixed formula) --

test_that("calculate_movement_similarity_endpoints() cosine metric gives similarity 1 for identical traces", {
  # Regression test for a real bug: a previous version of
  # similarity_metric_matrix()'s cosine branch computed cosine DISTANCE
  # (1 - dot/(|x||y|)) instead of cosine SIMILARITY (dot/(|x||y|)),
  # returning 0 here instead of the correct 1.
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = rep(c("V1", "V2"), each = 3),
    is_vehicle_control = TRUE,
    time_value = rep(1:3, 2),
    response_value = rep(c(1, 5, 2), 2),
    keep_well = TRUE
  )
  out <- calculate_movement_similarity_endpoints(
    d,
    list(all = 1:3),
    metric = "cosine"
  )
  expect_equal(unique(out$endpoint_value), 1)
})

test_that("calculate_movement_similarity_endpoints() cosine metric gives similarity 0 for orthogonal traces", {
  # Two vectors are orthogonal (dot product 0) when e.g. one is nonzero
  # only where the other is zero. cos(90deg) = 0.
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = rep(c("V1", "V2"), each = 3),
    is_vehicle_control = TRUE,
    time_value = rep(1:3, 2),
    response_value = c(1, 0, 0, 0, 1, 0),
    keep_well = TRUE
  )
  out <- calculate_movement_similarity_endpoints(
    d,
    list(all = 1:3),
    metric = "cosine"
  )
  v1_val <- out$endpoint_value[out$well_id == "V1"]
  expect_equal(v1_val, 0)
})

test_that("calculate_movement_similarity_endpoints() cosine metric gives a negative value for contrasting (opposite-sign) traces, treated as 0 before averaging", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = rep(c("V1", "V2"), each = 3),
    is_vehicle_control = TRUE,
    time_value = rep(1:3, 2),
    response_value = c(1, 2, 3, -1, -2, -3),
    keep_well = TRUE
  )
  raw_mat <- similarity_metric_matrix(
    matrix(c(1, 2, 3, -1, -2, -3), ncol = 2),
    metric = "cosine"
  )
  expect_equal(raw_mat[1, 2], -1)

  out <- calculate_movement_similarity_endpoints(
    d,
    list(all = 1:3),
    metric = "cosine"
  )
  # Negative similarity is treated as 0 before averaging (cal_mean_similarity()),
  # matching the published method for pearson/spearman.
  v1_val <- out$endpoint_value[out$well_id == "V1"]
  expect_equal(v1_val, 0)
})

test_that("similarity_metric_matrix() cosine metric gives NaN for a zero-norm (all-zero) trace", {
  mat <- matrix(c(0, 0, 0, 1, 2, 3), ncol = 2)
  result <- similarity_metric_matrix(mat, metric = "cosine")
  expect_true(is.nan(result[1, 2]))
})

test_that("calculate_movement_similarity_endpoints() is invariant to input row order", {
  d <- zf_fixture_ready() |> dplyr::filter(.data$plate_id == unique(.data$plate_id)[1])
  segments <- list(all = 1:50)

  out_original <- calculate_movement_similarity_endpoints(d, segments, metric = "spearman")

  set.seed(42)
  d_shuffled <- d[sample(nrow(d)), ]
  out_shuffled <- calculate_movement_similarity_endpoints(d_shuffled, segments, metric = "spearman")

  cmp <- dplyr::inner_join(
    out_original,
    out_shuffled,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint"),
    suffix = c("_orig", "_shuf")
  )
  expect_equal(nrow(cmp), nrow(out_original))
  expect_equal(cmp$endpoint_value_orig, cmp$endpoint_value_shuf)
})

test_that("calculate_movement_similarity_endpoints() errors on invalid metric", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = "A1",
    is_vehicle_control = TRUE,
    time_value = 1,
    response_value = 1,
    keep_well = TRUE
  )
  expect_error(
    calculate_movement_similarity_endpoints(
      d,
      list(all = 1),
      metric = "not_a_metric"
    )
  )
})
