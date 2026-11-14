# ---- estimate_bmr_from_control(): synthetic unit tests --------------------

test_that("estimate_bmr_from_control() computes a known median + 3*MAD BMR", {
  d <- tibble::tibble(
    endpoint = "e1",
    endpoint_value_norm = c(10, 12, 11, 9, 100, 8),
    is_vehicle_control = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
  )
  out <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "endpoint_value_norm"
  )
  vc_vals <- c(10, 12, 11, 9, 8)
  expect_equal(out$bmr, stats::median(vc_vals) + 3 * stats::mad(vc_vals))
  expect_equal(out$n_values, 5L)
  expect_equal(out$vc_median, stats::median(vc_vals))
  expect_equal(out$vc_mad, stats::mad(vc_vals))
  expect_named(out, c("endpoint", "bmr", "n_values", "vc_median", "vc_mad"))
})

test_that("estimate_bmr_from_control() computes one BMR per group", {
  d <- tibble::tibble(
    endpoint = rep(c("e1", "e2"), each = 4),
    endpoint_value_norm = c(1, 2, 3, 4, 10, 20, 30, 40),
    is_vehicle_control = TRUE
  )
  out <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "endpoint_value_norm"
  )
  expect_equal(nrow(out), 2)
  expect_setequal(out$endpoint, c("e1", "e2"))
})

test_that("estimate_bmr_from_control() only uses vehicle-control rows", {
  d <- tibble::tibble(
    endpoint = "e1",
    endpoint_value_norm = c(1, 1, 1, 1000),
    is_vehicle_control = c(TRUE, TRUE, TRUE, FALSE)
  )
  out <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "endpoint_value_norm"
  )
  # median(1,1,1) + 3*mad(1,1,1) = 1 + 0 = 1, the 1000 treated value must
  # not leak into the BMR calculation.
  expect_equal(out$bmr, 1)
})

test_that("estimate_bmr_from_control() respects n_mad", {
  d <- tibble::tibble(
    endpoint = "e1",
    endpoint_value_norm = c(1, 2, 3, 4, 5),
    is_vehicle_control = TRUE
  )
  out <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "endpoint_value_norm",
    n_mad = 1
  )
  expect_equal(
    out$bmr,
    stats::median(d$endpoint_value_norm) + 1 * stats::mad(d$endpoint_value_norm)
  )
  expect_error(
    estimate_bmr_from_control(d, group_cols = "endpoint", value_col = "endpoint_value_norm", n_mad = -1),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("estimate_bmr_from_control() errors on missing columns", {
  d <- tibble::tibble(endpoint = "e1")
  expect_error(
    estimate_bmr_from_control(d, group_cols = "endpoint", value_col = "endpoint_value_norm"),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("estimate_bmr_from_control() requires an explicit value_col (no default)", {
  d <- tibble::tibble(
    endpoint = "e1",
    endpoint_value_norm = c(1, 2, 3),
    is_vehicle_control = TRUE
  )
  expect_error(
    estimate_bmr_from_control(d, group_cols = "endpoint"),
    class = "zfbehavior_error_missing_value_col"
  )
})

test_that("estimate_bmr_from_control() supports custom value_col/is_vc_col", {
  d <- tibble::tibble(
    endpoint = "e1",
    my_value = c(1, 2, 3),
    my_vc = TRUE
  )
  out <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "my_value",
    is_vc_col = "my_vc"
  )
  expect_equal(
    out$bmr,
    stats::median(d$my_value) + 3 * stats::mad(d$my_value)
  )
})

test_that("estimate_bmr_from_control() supports per-plate/per-batch BMR via group_cols (no plate_col needed)", {
  # A per-plate BMR is already fully possible via group_cols alone --
  # confirm this pre-existing capability with a dedicated test (this was
  # previously only demonstrated informally, not locked in by a test).
  d <- tibble::tibble(
    endpoint = "e1",
    plate_id = rep(c("p1", "p2"), each = 4),
    endpoint_value_norm = c(1, 2, 3, 4, 10, 20, 30, 40),
    is_vehicle_control = TRUE
  )
  out <- estimate_bmr_from_control(
    d,
    group_cols = c("endpoint", "plate_id"),
    value_col = "endpoint_value_norm"
  )
  expect_equal(nrow(out), 2)
  expect_setequal(out$plate_id, c("p1", "p2"))
  p1_vals <- c(1, 2, 3, 4)
  p2_vals <- c(10, 20, 30, 40)
  p1_row <- out %>% dplyr::filter(.data$plate_id == "p1")
  p2_row <- out %>% dplyr::filter(.data$plate_id == "p2")
  expect_equal(p1_row$bmr, stats::median(p1_vals) + 3 * stats::mad(p1_vals))
  expect_equal(p2_row$bmr, stats::median(p2_vals) + 3 * stats::mad(p2_vals))
})

test_that("estimate_bmr_from_control() computes a known median-of-per-plate-medians BMR when plate_col is supplied", {
  d <- tibble::tibble(
    endpoint = "e1",
    plate_id = rep(c("p1", "p2", "p3"), each = 4),
    endpoint_value_norm = c(10, 12, 11, 9, 20, 22, 21, 19, 5, 6, 5, 7),
    is_vehicle_control = TRUE
  )
  out <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "endpoint_value_norm",
    plate_col = "plate_id"
  )
  per_plate_medians <- c(
    stats::median(c(10, 12, 11, 9)),
    stats::median(c(20, 22, 21, 19)),
    stats::median(c(5, 6, 5, 7))
  )
  expected_bmr <- stats::median(per_plate_medians) +
    3 * stats::mad(per_plate_medians)
  expect_equal(nrow(out), 1)
  expect_equal(out$bmr, expected_bmr)
})

test_that("estimate_bmr_from_control() plate_col only uses vehicle-control rows before computing per-plate medians", {
  d <- tibble::tibble(
    endpoint = "e1",
    plate_id = rep(c("p1", "p2", "p3"), each = 4),
    endpoint_value_norm = c(1, 1, 1, 1000, 2, 2, 2, 1000, 4, 4, 4, 1000),
    is_vehicle_control = rep(c(TRUE, TRUE, TRUE, FALSE), 3)
  )
  out <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "endpoint_value_norm",
    plate_col = "plate_id"
  )
  # Plate medians 1, 2, 4 (treated 1000s excluded).
  expect_equal(out$bmr, 2 + 3 * stats::mad(c(1, 2, 4)))
  expect_equal(out$n_values, 3L)
})

test_that("estimate_bmr_from_control() plate_col errors on < 3 plates, flat medians, or plate in group_cols", {
  d <- tibble::tibble(
    endpoint = "e1",
    plate_id = rep(c("p1", "p2"), each = 3),
    endpoint_value_norm = c(1, 2, 3, 4, 5, 6),
    is_vehicle_control = TRUE
  )
  expect_error(
    estimate_bmr_from_control(d, group_cols = "endpoint", value_col = "endpoint_value_norm",
                              plate_col = "plate_id"),
    class = "zfbehavior_error_invalid_plate_medians"
  )
  # Normalized to each plate's own VC median: every plate median is 0.
  d0 <- tibble::tibble(
    endpoint = "L1",
    plate_id = rep(c("p1", "p2", "p3"), each = 3),
    endpoint_value_norm = rep(c(-5, 0, 5), 3),
    is_vehicle_control = TRUE
  )
  expect_error(
    estimate_bmr_from_control(d0, group_cols = "endpoint", value_col = "endpoint_value_norm",
                              plate_col = "plate_id"),
    class = "zfbehavior_error_invalid_plate_medians"
  )
  expect_error(
    estimate_bmr_from_control(d0, group_cols = c("endpoint", "plate_id"),
                              value_col = "endpoint_value_norm", plate_col = "plate_id"),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("estimate_bmr_from_control() errors on a group without vehicle-control values", {
  d <- tibble::tibble(
    endpoint = c("e1", "e1", "e2"),
    endpoint_value_norm = c(1, 2, 3),
    is_vehicle_control = c(TRUE, TRUE, FALSE)
  )
  expect_error(
    estimate_bmr_from_control(d, group_cols = "endpoint", value_col = "endpoint_value_norm"),
    class = "zfbehavior_error_no_vehicle_control"
  )
})

test_that("estimate_bmr_from_control() with plate_col differs from the default (raw-well) distribution on unbalanced plate sizes", {
  # Demonstrates the actual purpose of plate_col: with unbalanced VC well
  # counts per plate, pooling raw wells (default) and pooling per-plate
  # medians (plate_col) give genuinely DIFFERENT results -- this is the
  # expected, intended behavior difference, not a bug.
  d <- tibble::tibble(
    endpoint = "e1",
    plate_id = c(rep("p1", 8), rep("p2", 2), rep("p3", 2)),
    endpoint_value_norm = c(rep(10, 8), 100, 100, 50, 50),
    is_vehicle_control = TRUE
  )
  out_default <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "endpoint_value_norm"
  )
  out_plate <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "endpoint_value_norm",
    plate_col = "plate_id"
  )
  expect_false(isTRUE(all.equal(out_default$bmr, out_plate$bmr)))
})

test_that("estimate_bmr_from_control() errors when plate_col is missing from d", {
  d <- tibble::tibble(
    endpoint = "e1",
    endpoint_value_norm = c(1, 2, 3),
    is_vehicle_control = TRUE
  )
  expect_error(
    estimate_bmr_from_control(
      d,
      group_cols = "endpoint",
      value_col = "endpoint_value_norm",
      plate_col = "plate_id"
    ),
    class = "zfbehavior_error_missing_columns"
  )
})

# ---- run_bmc_curvep(): synthetic unit tests --------------------------------

test_that("run_bmc_curvep() runs end-to-end on synthetic data and returns expected shape", {
  set.seed(1)
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "e1",
    conc = rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 6),
    endpoint_value_norm = c(
      stats::rnorm(6, 100, 10),
      stats::rnorm(6, 100, 10),
      stats::rnorm(6, 100, 10),
      stats::rnorm(6, 300, 10),
      stats::rnorm(6, 300, 10)
    )
  )
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 150, RNGE = 1000000)

  out <- run_bmc_curvep(
    d,
    bmr_table,
    value_col = "endpoint_value_norm",
    n_samples = 5,
    seed = 1
  )
  expect_named(out, c("raw", "act_summary", "unmatched_in_d"))
  expect_named(out$raw, "e1")
  expect_s3_class(out$act_summary, "tbl_df")
  expect_equal(nrow(out$act_summary), 1)
  expect_true(all(
    c("chemical", "endpoint", "hit_confidence", "POD_med") %in%
      colnames(out$act_summary)
  ))
  expect_equal(out$act_summary$chemical, "chemA")
  expect_equal(out$act_summary$endpoint, "e1")
  expect_equal(nrow(out$unmatched_in_d), 0)
})

test_that("run_bmc_curvep() runs one endpoint per bmr_table row, across chemicals", {
  set.seed(2)
  d <- tibble::tibble(
    chemical = rep(c("chemA", "chemB"), each = 15),
    endpoint = "e1",
    conc = rep(rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 3), 2),
    endpoint_value_norm = stats::rnorm(30, 100, 10)
  )
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 100, RNGE = 1000000)

  out <- run_bmc_curvep(
    d,
    bmr_table,
    value_col = "endpoint_value_norm",
    n_samples = 5,
    seed = 1
  )
  expect_equal(nrow(out$act_summary), 2)
  expect_setequal(out$act_summary$chemical, c("chemA", "chemB"))
})

test_that("run_bmc_curvep() unmatched = \"drop\" silently excludes endpoints absent from bmr_table and records them", {
  set.seed(3)
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = rep(c("e1", "e2"), each = 15),
    conc = rep(rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 3), 2),
    endpoint_value_norm = stats::rnorm(30, 100, 10)
  )
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 100, RNGE = 1000000)

  out <- run_bmc_curvep(
    d,
    bmr_table,
    value_col = "endpoint_value_norm",
    n_samples = 5,
    seed = 1,
    unmatched = "drop"
  )
  expect_equal(nrow(out$act_summary), 1)
  expect_equal(out$act_summary$endpoint, "e1")
  expect_equal(out$unmatched_in_d$endpoint, "e2")
})

test_that("run_bmc_curvep() unmatched = \"error\" (the default) errors on endpoints in d absent from bmr_table", {
  set.seed(3)
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = rep(c("e1", "e2"), each = 15),
    conc = rep(rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 3), 2),
    endpoint_value_norm = stats::rnorm(30, 100, 10)
  )
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 100, RNGE = 1000000)
  expect_error(
    run_bmc_curvep(
      d,
      bmr_table,
      value_col = "endpoint_value_norm",
      n_samples = 5,
      seed = 1
    ),
    class = "zfbehavior_error_unmatched_bmr_endpoint"
  )
})

test_that("run_bmc_curvep() silently skips bmr_table rows with no matching data in d, regardless of unmatched", {
  set.seed(3)
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "e1",
    conc = rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 3),
    endpoint_value_norm = stats::rnorm(15, 100, 10)
  )
  bmr_table <- tibble::tibble(
    endpoint = c("e1", "e2"),
    bmr = c(100, 100),
    RNGE = c(1000000, 1000000)
  )
  out <- run_bmc_curvep(
    d,
    bmr_table,
    value_col = "endpoint_value_norm",
    n_samples = 5,
    seed = 1
  )
  expect_equal(nrow(out$act_summary), 1)
  expect_equal(out$act_summary$endpoint, "e1")
})

test_that("run_bmc_curvep() is deterministic given the same seed", {
  set.seed(4)
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "e1",
    conc = rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 6),
    endpoint_value_norm = stats::rnorm(30, 100, 10)
  )
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 100, RNGE = 1000000)

  out1 <- run_bmc_curvep(
    d,
    bmr_table,
    value_col = "endpoint_value_norm",
    n_samples = 20,
    seed = 42
  )
  out2 <- run_bmc_curvep(
    d,
    bmr_table,
    value_col = "endpoint_value_norm",
    n_samples = 20,
    seed = 42
  )
  expect_equal(out1$act_summary, out2$act_summary)
})

test_that("run_bmc_curvep() errors on missing columns in d", {
  d <- tibble::tibble(chemical = "chemA")
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 100, RNGE = 1000000)
  expect_error(
    run_bmc_curvep(d, bmr_table, value_col = "endpoint_value_norm"),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("run_bmc_curvep() errors on missing columns in bmr_table", {
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "e1",
    conc = 1,
    endpoint_value_norm = 1
  )
  bmr_table <- tibble::tibble(endpoint = "e1")
  expect_error(
    run_bmc_curvep(d, bmr_table, value_col = "endpoint_value_norm"),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("run_bmc_curvep() requires an explicit value_col in continuous mode (no default)", {
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "e1",
    conc = 1,
    endpoint_value_norm = 1
  )
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 100, RNGE = 1000000)
  expect_error(
    run_bmc_curvep(d, bmr_table),
    class = "zfbehavior_error_missing_value_col"
  )
})

test_that("run_bmc_curvep() supports custom column names", {
  set.seed(5)
  d <- tibble::tibble(
    my_chem = "chemA",
    my_endpoint = "e1",
    my_conc = rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 6),
    my_value = stats::rnorm(30, 100, 10)
  )
  bmr_table <- tibble::tibble(
    my_endpoint = "e1",
    my_bmr = 100,
    my_rnge = 1000000
  )

  out <- run_bmc_curvep(
    d,
    bmr_table,
    endpoint_col = "my_endpoint",
    chemical_col = "my_chem",
    conc_col = "my_conc",
    value_col = "my_value",
    bmr_col = "my_bmr",
    rnge_col = "my_rnge",
    n_samples = 5,
    seed = 1
  )
  expect_equal(nrow(out$act_summary), 1)
})

# ---- run_bmc_curvep(): A3 per-plate/multi-key BMR table (bmr_key_cols) ----

test_that("run_bmc_curvep() supports a per-plate BMR table via bmr_key_cols, with no cross-contamination", {
  set.seed(7)
  d <- tibble::tibble(
    chemical = rep(c("chemA_p1", "chemA_p2"), each = 15),
    endpoint = "e1",
    plate_id = rep(c("p1", "p2"), each = 15),
    conc = rep(rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 3), 2),
    endpoint_value_norm = c(
      stats::rnorm(15, 100, 10),
      stats::rnorm(15, 300, 10)
    )
  )
  bmr_table <- tibble::tibble(
    endpoint = "e1",
    plate_id = c("p1", "p2"),
    bmr = c(100, 250),
    RNGE = c(1000000, 1000000)
  )
  out <- run_bmc_curvep(
    d,
    bmr_table,
    value_col = "endpoint_value_norm",
    bmr_key_cols = "plate_id",
    n_samples = 5,
    seed = 1
  )
  expect_equal(nrow(out$act_summary), 2)
  p1_row <- out$act_summary %>% dplyr::filter(.data$chemical == "chemA_p1")
  p2_row <- out$act_summary %>% dplyr::filter(.data$chemical == "chemA_p2")
  expect_equal(p1_row$TRSH, 100)
  expect_equal(p2_row$TRSH, 250)
})

test_that("run_bmc_curvep() rejects a duplicated endpoint in bmr_table when bmr_key_cols is not supplied", {
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "e1",
    conc = 1,
    endpoint_value_norm = 1
  )
  bmr_table <- tibble::tibble(
    endpoint = c("e1", "e1"),
    bmr = c(100, 200),
    RNGE = c(1000000, 1000000)
  )
  expect_error(
    run_bmc_curvep(d, bmr_table, value_col = "endpoint_value_norm"),
    class = "zfbehavior_error_duplicate_bmr_endpoint"
  )
})

test_that("run_bmc_curvep() still rejects a duplicated (endpoint, bmr_key_cols) combination", {
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "e1",
    plate_id = "p1",
    conc = 1,
    endpoint_value_norm = 1
  )
  bmr_table <- tibble::tibble(
    endpoint = c("e1", "e1"),
    plate_id = c("p1", "p1"),
    bmr = c(100, 200),
    RNGE = c(1000000, 1000000)
  )
  expect_error(
    run_bmc_curvep(
      d,
      bmr_table,
      value_col = "endpoint_value_norm",
      bmr_key_cols = "plate_id"
    ),
    class = "zfbehavior_error_duplicate_bmr_endpoint"
  )
})

# ---- run_bmc_curvep(): B3 dichotomous-mode structural validation ----------

test_that("run_bmc_curvep() errors on non-integer/negative/out-of-range dichotomous counts", {
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 30, RNGE = 1e6)

  d_neg <- tibble::tibble(
    endpoint = "e1",
    chemical = "c1",
    conc = -5,
    n_affected = c(3, -1),
    N = c(8, 8)
  )
  expect_error(
    run_bmc_curvep(d_neg, bmr_table, n_in_col = "n_affected", n_col = "N"),
    class = "zfbehavior_error_invalid_dichotomous_counts"
  )

  d_gt_n <- tibble::tibble(
    endpoint = "e1",
    chemical = "c1",
    conc = -5,
    n_affected = 10,
    N = 8
  )
  expect_error(
    run_bmc_curvep(d_gt_n, bmr_table, n_in_col = "n_affected", n_col = "N"),
    class = "zfbehavior_error_invalid_dichotomous_counts"
  )

  d_noninteger_n <- tibble::tibble(
    endpoint = "e1",
    chemical = "c1",
    conc = -5,
    n_affected = 1,
    N = 8.5
  )
  expect_error(
    run_bmc_curvep(
      d_noninteger_n,
      bmr_table,
      n_in_col = "n_affected",
      n_col = "N"
    ),
    class = "zfbehavior_error_invalid_dichotomous_counts"
  )
})

test_that("run_bmc_curvep() errors on non-finite conc", {
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 100, RNGE = 1e6)
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "e1",
    conc = c(1, NA, Inf),
    endpoint_value_norm = c(1, 2, 3)
  )
  expect_error(
    run_bmc_curvep(d, bmr_table, value_col = "endpoint_value_norm"),
    class = "zfbehavior_error_invalid_conc"
  )
})

# ---- estimate_bmr_curvep(): continuous mode ---------------------------------

test_that("estimate_bmr_curvep() (continuous) returns the sweep and the knee-point estimate", {
  set.seed(6)
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "LD_spearman",
    conc = rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 6),
    endpoint_value_norm = stats::rnorm(30, 0, 20)
  )
  # Rcurvep::estimate_dataset_bmr() can warn on flat/noisy synthetic data
  # (its exponential fit degenerates; qc == "check") -- expected.
  out <- suppressWarnings(estimate_bmr_curvep(
    d,
    conc_col = "conc",
    rnge = -1000000,
    n_samples = 3,
    value_col = "endpoint_value_norm",
    trsh_seq = c(10, 30, 50, 70, 90),
    seed = 1
  ))
  expect_named(out, c("sweep", "estimate", "trsh_seq"))
  expect_equal(out$trsh_seq$trsh_from, 10)
  expect_equal(out$trsh_seq$trsh_to, 90)
  expect_equal(out$trsh_seq$n_trsh, 5L)
  expect_s3_class(out$sweep, "rcurvep")
  expect_setequal(unique(out$sweep$result$act_set$TRSH), c(10, 30, 50, 70, 90))
  expect_true(all(c("bmr_ori", "bmr_exp") %in% colnames(out$estimate$outcome)))
  expect_equal(out$estimate$outcome$endpoint, "LD_spearman")
})

test_that("estimate_bmr_curvep() estimates both directions and derives a TRSH range per endpoint", {
  set.seed(7)
  d <- tibble::tibble(
    chemical = rep(paste0("c", 1:4), each = 30),
    endpoint = rep(c("L1", "D1"), each = 60),
    conc = rep(rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 6), 4),
    v = c(stats::rnorm(60, 0, 20), stats::rnorm(60, 0, 200))
  )
  out <- suppressWarnings(estimate_bmr_curvep(
    d, conc_col = "conc", rnge = c(-1e6, 1e6), n_samples = 2,
    value_col = "v", trsh_seq = NULL, seed = 1
  ))
  expect_equal(nrow(out$estimate$outcome), 4)
  expect_setequal(out$estimate$outcome$RNGE, c(-1e6, 1e6))
  tr <- out$trsh_seq[match(c("L1", "D1"), out$trsh_seq$endpoint), ]
  expect_equal(tr$n_trsh, c(19L, 19L))
  q95 <- vapply(c(L1 = "L1", D1 = "D1"), function(e) unname(stats::quantile(abs(d$v[d$endpoint == e]), 0.95)), numeric(1))
  expect_equal(tr$trsh_to, unname(signif(q95, 3)))
  expect_true(tr$trsh_to[2] > 5 * tr$trsh_to[1])
  expect_setequal(
    unique(out$sweep$result$act_set$TRSH[out$sweep$result$act_set$endpoint == "L1"]),
    signif(seq(0, q95[["L1"]], length.out = 20)[-1], 3)
  )
  expect_error(
    estimate_bmr_curvep(d, conc_col = "conc", rnge = c(1e6, 1e6), n_samples = 2, value_col = "v"),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    estimate_bmr_curvep(d, conc_col = "conc", rnge = 1e6, n_samples = 2, value_col = "v",
                        trsh_seq = c(5, 10)),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("run_bmc_curvep() runs both directions of one endpoint and keeps RNGE", {
  set.seed(8)
  d <- tibble::tibble(
    chemical = "chemA",
    endpoint = "L1",
    conc = rep(log10(c(5, 15, 50, 150, 500) / 1e6), each = 6),
    v = c(stats::rnorm(18, 0, 5), stats::rnorm(12, -80, 5))
  )
  bmr_table <- tibble::tibble(endpoint = "L1", bmr = c(30, 40), RNGE = c(-1e6, 1e6))
  out <- run_bmc_curvep(d, bmr_table, value_col = "v", n_samples = 5, seed = 1)
  expect_setequal(names(out$raw), c("L1@down", "L1@up"))
  expect_equal(nrow(out$act_summary), 2)
  dn <- out$act_summary[out$act_summary$RNGE < 0, ]
  up <- out$act_summary[out$act_summary$RNGE > 0, ]
  expect_equal(dn$TRSH, 30)
  expect_equal(up$TRSH, 40)
  expect_gt(dn$hit_confidence, up$hit_confidence)
  # Two rows with the same direction stay an error.
  expect_error(
    run_bmc_curvep(d, tibble::tibble(endpoint = "L1", bmr = c(30, 40), RNGE = c(1e6, 2e6)),
                   value_col = "v"),
    class = "zfbehavior_error_duplicate_bmr_endpoint"
  )
})

test_that("estimate_bmr_curvep() validates its arguments", {
  d <- tibble::tibble(chemical = "c", endpoint = "e", conc = 1, v = 1, n = 1, N = 2)
  expect_error(
    estimate_bmr_curvep(tibble::tibble(endpoint = "e1"), conc_col = "conc", rnge = 1e6,
                        n_samples = 2, value_col = "v"),
    class = "zfbehavior_error_missing_columns"
  )
  expect_error(
    estimate_bmr_curvep(d, conc_col = "conc", rnge = 1e6, n_samples = 2),
    class = "zfbehavior_error_missing_value_col"
  )
  expect_error(
    estimate_bmr_curvep(d, conc_col = "conc", rnge = 1e6, n_samples = 2, n_in_col = "n"),
    class = "zfbehavior_error_incomplete_dichotomous_cols"
  )
  expect_error(
    estimate_bmr_curvep(d, conc_col = "conc", rnge = 1e6, n_samples = 2, value_col = "v",
                        n_in_col = "n", n_col = "N"),
    class = "zfbehavior_error_incomplete_dichotomous_cols"
  )
  expect_error(
    estimate_bmr_curvep(dplyr::mutate(d, conc = -Inf), conc_col = "conc", rnge = 1e6,
                        n_samples = 2, value_col = "v"),
    class = "zfbehavior_error_invalid_conc"
  )
  expect_error(estimate_bmr_curvep(d, value_col = "v"))
})

# ---- plot_behavior_dose_response() (internal) / _pages() ---------------------

behavior_plot_fixture <- function() {
  set.seed(3)
  expand <- expand.grid(
    rep = 1:4, conc = c(0, 1, 10), endpoint = c("L1", "D1"),
    plate_id = c("pA", "pB", "pC"), stringsAsFactors = FALSE
  )
  tibble::tibble(
    plate_id = expand$plate_id,
    DTXSID = ifelse(expand$plate_id == "pC", "DTX2", "DTX1"),
    well_id = paste0("W", expand$rep, "_", expand$conc),
    endpoint = expand$endpoint,
    concentration = expand$conc,
    concentration_unit = "uM",
    is_vehicle_control = expand$conc == 0,
    endpoint_value_norm = stats::rnorm(nrow(expand), expand$conc * 3, 2),
    endpoint_value_raw = stats::rnorm(nrow(expand), 100, 5)
  )
}

text_layer_data <- function(p) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  if (length(i) == 0) return(NULL)
  ggplot2::ggplot_build(p)$data[[i]]
}

test_that("plot_behavior_dose_response() draws one box per dose group, VC first, endpoint rows in order", {
  d <- dplyr::filter(behavior_plot_fixture(), .data$plate_id == "pA")
  p <- zfbehavior:::plot_behavior_dose_response(d, endpoint_order = c("L1", "D1"), title = "pA")
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_equal(levels(b$layout$layout$endpoint), c("L1", "D1"))
  expect_equal(b$layout$panel_scales_x[[1]]$get_labels()[1], "VC")
  boxes <- b$data[[1]]
  expect_equal(nrow(boxes), 6) # 3 dose groups x 2 endpoints
  expect_equal(p$scales$get_scales("x")$name, "uM")
  expect_equal(p$labels$y %||% p$scales$get_scales("y")$name, "normalized response")
  expect_null(text_layer_data(p))
  # Default value is the normalized column.
  expect_equal(sort(b$data[[2]]$y), sort(d$endpoint_value_norm))
  p_raw <- zfbehavior:::plot_behavior_dose_response(d, value_col = "endpoint_value_raw")
  expect_equal(sort(ggplot2::ggplot_build(p_raw)$data[[2]]$y), sort(d$endpoint_value_raw))
})

test_that("plot_behavior_dose_response() pooled: one box per dose, points coloured by plate", {
  d <- dplyr::filter(behavior_plot_fixture(), .data$DTXSID == "DTX1")
  p <- zfbehavior:::plot_behavior_dose_response(d, point_colour_col = "plate_id")
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 6)
  expect_equal(length(unique(b$data[[2]]$colour)), 2)
})

test_that("plot_behavior_dose_response() labels BMC per endpoint, none for NA BMC, & for low active fraction", {
  d <- dplyr::filter(behavior_plot_fixture(), .data$plate_id == "pA")
  bmc <- tibble::tibble(endpoint = c("L1", "D1"), BMC = c(5, NA), score = c(0.9, 0.2), baf = c(0.3, NA))
  p <- zfbehavior:::plot_behavior_dose_response(d, bmc, batch_active_fraction_col = "baf")
  td <- text_layer_data(p)
  expect_equal(nrow(td), 1)
  expect_match(td$label, "^BMC: 5e\\+00")
  expect_match(td$label, "&", fixed = TRUE)
  # bmc row for an endpoint absent from d adds no facet.
  p2 <- zfbehavior:::plot_behavior_dose_response(d, tibble::tibble(endpoint = "ZZ", BMC = 1, score = 1))
  expect_equal(length(unique(ggplot2::ggplot_build(p2)$layout$layout$endpoint)), 2)
  expect_error(
    zfbehavior:::plot_behavior_dose_response(d, tibble::tibble(endpoint = c("L1", "L1"), BMC = 1, score = 1)),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("plot_behavior_dose_response() keeps distinct concentrations that format identically", {
  d <- tibble::tibble(
    endpoint = "L1", concentration = c(0, 1.0001, 1.0002), is_vehicle_control = c(TRUE, FALSE, FALSE),
    endpoint_value_norm = c(0, 1, 2)
  )
  b <- ggplot2::ggplot_build(zfbehavior:::plot_behavior_dose_response(d))
  expect_equal(length(b$layout$panel_scales_x[[1]]$get_limits()), 3)
})

test_that("plot_behavior_dose_response() errors on missing columns", {
  expect_error(
    zfbehavior:::plot_behavior_dose_response(tibble::tibble(endpoint = "L1", concentration = 1)),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("plot_behavior_dose_response_pages() pages units, matches BMC per unit, validates args", {
  d <- behavior_plot_fixture()
  out <- plot_behavior_dose_response_pages(d, group_cols = "plate_id", ncol = 2)
  expect_s3_class(out, "arrangelist")
  expect_length(out, 2) # 3 plates, 2 per page
  pooled <- plot_behavior_dose_response_pages(d, group_cols = "DTXSID", point_colour_col = "plate_id")
  expect_length(pooled, 1)

  d$plate_id <- factor(d$plate_id, levels = c("pC", "pB", "pA"))
  d$t <- paste("plate", d$plate_id)
  bmc <- tibble::tibble(plate_id = "pB", endpoint = "D1", BMC = 3, score = 0.8)
  pls <- zfbehavior:::plot_behavior_dose_response_by_group(d, group_cols = "plate_id", bmc_data = bmc,
                                                          title_col = "t")
  expect_equal(names(pls), c("pC", "pB", "pA"))
  expect_equal(pls[["pC"]]$labels$title, "plate pC")
  expect_equal(nrow(text_layer_data(pls[["pB"]])), 1)
  expect_null(text_layer_data(pls[["pA"]]))

  expect_error(plot_behavior_dose_response_pages(d, group_cols = "nope"),
               class = "zfbehavior_error_missing_columns")
  expect_error(plot_behavior_dose_response_pages(d, group_cols = "plate_id", ncol = 0),
               class = "zfbehavior_error_invalid_argument")
  expect_error(plot_behavior_dose_response_pages(d, group_cols = "plate_id", bmc_data = bmc,
                                                 bmc_group_cols = c("a", "b")),
               class = "zfbehavior_error_mismatched_group_cols")
})

# ---- Real historical production ground-truth regression tests -------------
# See data-raw/build_bmc_fixture.R and data-raw/build_bmc_snapshot.R for
# full provenance. Uses a DIFFERENT 3-chemical fixture than the rest of
# this test suite (PE-4188/PE-4215/PE-4276, NOT PE-4188/PE-4189/PE-4190),
# deliberately chosen so 2 of the 3 chemicals (Loperamide hydrochloride,
# Methylene blue trihydrate) are REAL, STRONG active hits in the ground
# truth, not just non-hits.

test_that("estimate_bmr_from_control() reproduces the real historical global-MAD BMR exactly on the FULL production dataset", {
  # Uses the full 112-plate endpoint dataset of the original analysis (not
  # the small fixture), because the BMR is a dataset-wide quantity. Private
  # data: skipped when not available (see helper-private-data.R).
  full_data_path <- zf_private_file("data", "zebrafish-dnt_v2_endp_v1_endp.rds")

  d <- readRDS(full_data_path) %>%
    dplyr::filter(!is.na(.data$endpoint)) %>%
    dplyr::transmute(
      endpoint = .data$endpoint,
      is_vehicle_control = .data$IS_VEHICLE,
      endpoint_value_norm = .data$endpoint_value_norm
    )

  out <- estimate_bmr_from_control(
    d,
    group_cols = "endpoint",
    value_col = "endpoint_value_norm"
  )
  expected <- zf_bmc_outputs()$bmr_used

  # LD_pearson/LD_spearman use a DIFFERENT (semi-manual, knee-point-based)
  # BMR-finding method in the historical pipeline -- see the "@section
  # Workflow" section of ?bmc -- so they are excluded from this
  # global-MAD-formula check.
  cmp <- out %>%
    dplyr::filter(!grepl("pearson|spearman", .data$endpoint)) %>%
    dplyr::inner_join(expected, by = "endpoint", suffix = c("", "_expected"))
  expect_equal(nrow(cmp), 15)
  expect_equal(cmp$bmr, cmp$bmr_select, tolerance = 1e-6)
})

test_that("run_bmc_curvep() reproduces real historical BMC results on the 3-chemical fixture (D1@both_auc, D2-ext@up_auc)", {
  # Restricted to 2 of the 17 historical endpoints (not all 17) to keep
  # routine `devtools::test()` runtime reasonable -- n_samples = 1000
  # (the historical value, needed for a meaningful hit_confidence/POD
  # comparison) takes roughly 15 seconds per endpoint x chemical
  # combination; the full 17-endpoint x 3-chemical grid would take
  # several minutes.
  #
  # IMPORTANT: hit_confidence and POD_med/POD_cil/POD_ciu are
  # bootstrap-simulation-derived (Rcurvep::combi_run_rcurvep(seed = 2025,
  # n_samples = 1000)) and subject to R-version-dependent Monte-Carlo
  # noise (confirmed empirically: R's default random-sampling algorithm
  # changed across R versions, and Rcurvep's internal bootstrap relies on
  # R's RNG) -- the SAME category of noise documented for
  # DescTools::DunnettTest()/multcomp::glht() p-values. Confirmed
  # (interactive session, R 4.5.3) that POD_med usually matches exactly
  # but not always (e.g. Loperamide hydrochloride's D1@both_auc POD_med
  # differs by ~0.03 on the log10-molar scale); hit_confidence differs by
  # up to ~0.07. Tolerances below reflect this investigated, documented
  # noise -- not a loosely-guessed value.
  test_endpoints <- c("D1@both_auc", "D2-ext@up_auc")

  bmc_input <- zf_bmc_fixture_input() %>%
    dplyr::filter(.data$endpoint %in% test_endpoints)

  bmr_table <- zf_bmc_outputs()$bmr_used %>%
    dplyr::filter(.data$endpoint %in% test_endpoints) %>%
    dplyr::rename(bmr = "bmr_select")

  out <- run_bmc_curvep(
    bmc_input,
    bmr_table,
    value_col = "endpoint_value_norm",
    seed = 2025,
    n_samples = 1000
  )

  expected <- zf_bmc_outputs()$act_summary %>%
    dplyr::filter(.data$endpoint %in% test_endpoints)

  cmp <- out$act_summary %>%
    dplyr::inner_join(
      expected,
      by = c("chemical", "endpoint"),
      suffix = c("", "_expected")
    )
  expect_equal(nrow(cmp), 6) # 3 chemicals x 2 endpoints

  expect_equal(cmp$POD_med, cmp$POD_med_expected, tolerance = 0.05)
  expect_equal(cmp$hit_confidence, cmp$hit_confidence_expected, tolerance = 0.1)
})
