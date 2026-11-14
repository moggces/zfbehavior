# ---- calculate_devtox_endpoints(): synthetic unit tests --------------------

devtox_wells <- function() {
  tibble::tibble(
    chemical_id = "chem1",
    plate_id = "plate1",
    well_id = paste0("W", 1:6),
    is_vehicle_control = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    concentration = c(0, 0, 1, 1, 10, 10),
    concentration_unit = "uM",
    keep_well = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )
}

test_that("calculate_devtox_endpoints() counts N, n_affected and pct_affected, VC included by default", {
  out <- calculate_devtox_endpoints(devtox_wells(), group_cols = c("chemical_id", "plate_id"))
  expect_equal(nrow(out), 3)
  expect_equal(out$is_vehicle_control, c(TRUE, FALSE, FALSE))
  expect_equal(out$concentration, c(0, 1, 10))
  expect_true(all(out$N == 2))
  expect_true(all(out$n_affected == 1))
  expect_equal(out$pct_affected, c(50, 50, 50))
  expect_true(all(out$endpoint == "percent of affected"))
  expect_true(all(out$concentration_unit == "uM"))
  expect_false("log_conc" %in% colnames(out))
})

test_that("calculate_devtox_endpoints() drops the VC dose group when include_vehicle_control = FALSE", {
  out <- calculate_devtox_endpoints(
    devtox_wells(),
    group_cols = "plate_id",
    include_vehicle_control = FALSE
  )
  expect_equal(nrow(out), 2)
  expect_false(any(out$is_vehicle_control))
})

test_that("calculate_devtox_endpoints() works without a concentration_unit column", {
  d <- dplyr::select(devtox_wells(), -"concentration_unit")
  out <- calculate_devtox_endpoints(d, group_cols = "plate_id")
  expect_false("concentration_unit" %in% colnames(out))
  expect_equal(nrow(out), 3)
})

test_that("calculate_devtox_endpoints() groups separately per group_cols", {
  d <- tibble::tibble(
    chemical_id = rep(c("chem1", "chem2"), each = 4),
    well_id = paste0("W", 1:8),
    is_vehicle_control = FALSE,
    concentration = 1,
    keep_well = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  out <- calculate_devtox_endpoints(d, group_cols = "chemical_id")
  expect_equal(out$chemical_id, c("chem1", "chem2"))
  expect_equal(out$N, c(4, 4))
  expect_equal(out$n_affected, c(1, 3))
})

test_that("calculate_devtox_endpoints() pools plates, counting repeated well positions per plate", {
  # Same well positions on two plates must be counted as different wells.
  d <- tibble::tibble(
    DTXSID = "DTX1",
    plate_id = rep(c("pA", "pB"), each = 4),
    well_id = rep(c("A1", "A2", "A3", "A4"), 2),
    is_vehicle_control = rep(c(TRUE, TRUE, FALSE, FALSE), 2),
    concentration = rep(c(0, 0, 1, 1), 2),
    concentration_unit = "uM",
    keep_well = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
  per_plate <- calculate_devtox_endpoints(d, group_cols = c("DTXSID", "plate_id"))
  expect_equal(nrow(per_plate), 4)

  pooled <- calculate_devtox_endpoints(d, group_cols = "DTXSID")
  expect_equal(nrow(pooled), 2)
  expect_equal(pooled$N, c(4, 4))
  expect_equal(pooled$n_affected, c(1, 3))
})

test_that("calculate_devtox_endpoints() keeps differing dose sets of pooled plates as separate dose groups", {
  d <- tibble::tibble(
    DTXSID = "DTX1",
    plate_id = rep(c("pA", "pB"), each = 2),
    well_id = rep(c("A1", "A2"), 2),
    is_vehicle_control = FALSE,
    concentration = c(1, 1, 1, 3),
    concentration_unit = "uM",
    keep_well = TRUE
  )
  out <- calculate_devtox_endpoints(d, group_cols = "DTXSID")
  expect_equal(out$concentration, c(1, 3))
  expect_equal(out$N, c(3, 1))
})

test_that("calculate_devtox_endpoints() errors when a pooled unit mixes concentration units", {
  d <- tibble::tibble(
    DTXSID = "DTX1",
    plate_id = c("pA", "pA", "pB", "pB"),
    well_id = c("A1", "A2", "A1", "A2"),
    is_vehicle_control = c(TRUE, FALSE, TRUE, FALSE),
    concentration = c(0, 1, 0, 1),
    concentration_unit = c("uM", "uM", "ug/mL", "ug/mL"),
    keep_well = TRUE
  )
  expect_error(
    calculate_devtox_endpoints(d, group_cols = "DTXSID"),
    class = "zfbehavior_error_mixed_concentration_units"
  )
  # Per plate is fine: each plate has one unit.
  out <- calculate_devtox_endpoints(d, group_cols = c("DTXSID", "plate_id"))
  expect_equal(out$concentration_unit, c("uM", "uM", "ug/mL", "ug/mL"))
})

test_that("calculate_devtox_endpoints() merges pooled VC wells into one VC dose group", {
  d <- tibble::tibble(
    DTXSID = "DTX1",
    plate_id = c("pA", "pA", "pB", "pB"),
    well_id = c("A1", "A2", "A1", "A2"),
    is_vehicle_control = c(TRUE, FALSE, TRUE, FALSE),
    concentration = c(0, 1, 0, 1),
    concentration_unit = c("mg/L", "uM", NA, "uM"),
    keep_well = c(FALSE, TRUE, TRUE, TRUE)
  )
  out <- calculate_devtox_endpoints(d, group_cols = "DTXSID")
  vc <- dplyr::filter(out, .data$is_vehicle_control)
  expect_equal(nrow(vc), 1)
  expect_equal(vc$N, 2)
  expect_equal(vc$n_affected, 1)
  expect_equal(vc$concentration_unit, "uM")
})

test_that("calculate_devtox_endpoints() deduplicates timepoint-level rows to wells", {
  d <- tibble::tibble(
    chemical_id = "chem1",
    plate_id = "p1",
    well_id = rep(paste0("W", 1:4), each = 3),
    is_vehicle_control = FALSE,
    concentration = 1,
    keep_well = rep(c(TRUE, FALSE, TRUE, FALSE), each = 3)
  )
  out <- calculate_devtox_endpoints(d, group_cols = "chemical_id")
  expect_equal(out$N, 4)
  expect_equal(out$n_affected, 2)
})

test_that("calculate_devtox_endpoints() errors when one well has two keep_well values", {
  d <- tibble::tibble(
    plate_id = "p1",
    well_id = c("W1", "W1"),
    is_vehicle_control = FALSE,
    concentration = 1,
    keep_well = c(TRUE, FALSE)
  )
  expect_error(
    calculate_devtox_endpoints(d, group_cols = "plate_id"),
    class = "zfbehavior_error_inconsistent_well"
  )
})

test_that("calculate_devtox_endpoints() errors on missing columns, NA keep_well, NA group values", {
  expect_error(
    calculate_devtox_endpoints(tibble::tibble(chemical_id = "c"), group_cols = "chemical_id"),
    class = "zfbehavior_error_missing_columns"
  )
  d <- devtox_wells()
  d$keep_well[3] <- NA
  expect_error(
    calculate_devtox_endpoints(d, group_cols = "plate_id"),
    class = "zfbehavior_error_missing_keep_well"
  )
  d <- devtox_wells()
  d$DTXSID <- c(NA, rep("DTX1", 5))
  expect_error(
    calculate_devtox_endpoints(d, group_cols = "DTXSID"),
    class = "zfbehavior_error_missing_group_cols"
  )
  expect_error(
    calculate_devtox_endpoints(devtox_wells(), group_cols = character()),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("calculate_devtox_endpoints() no longer accepts concentration_unit_micromolar", {
  expect_error(
    calculate_devtox_endpoints(
      devtox_wells(),
      group_cols = "plate_id",
      concentration_unit_micromolar = TRUE
    ),
    "unused argument"
  )
})

test_that("calculate_devtox_endpoints() supports uneven N across dose groups", {
  d <- tibble::tibble(
    chemical_id = "chem1",
    well_id = paste0("W", 1:31),
    is_vehicle_control = FALSE,
    concentration = c(rep(1, 15), rep(10, 16)),
    keep_well = TRUE
  )
  out <- calculate_devtox_endpoints(d, group_cols = "chemical_id")
  expect_equal(out$N, c(15, 16))
})

# ---- estimate_bmr_curvep(): count mode (devtox) ------------------------------

test_that("estimate_bmr_curvep() (counts) returns the sweep and the estimate, custom columns", {
  d <- tibble::tibble(
    my_endpoint = "percent of affected",
    my_chemical = rep(c("chemA", "chemB"), each = 5),
    my_conc = rep(seq(-6, -2, length.out = 5), 2),
    my_n_in = c(0, 1, 2, 5, 8, 0, 0, 1, 3, 6),
    my_n = 8
  )
  # The knee-point exponential fit needs >= 4 unique TRSH values.
  out <- suppressWarnings(estimate_bmr_curvep(
    d,
    conc_col = "my_conc",
    rnge = 1000000,
    n_samples = 2,
    endpoint_col = "my_endpoint",
    chemical_col = "my_chemical",
    n_in_col = "my_n_in",
    n_col = "my_n",
    trsh_seq = c(10, 30, 50, 70, 90),
    seed = 1
  ))
  expect_named(out, c("sweep", "estimate", "trsh_seq"))
  expect_true("percent of affected" %in% out$estimate$outcome$endpoint)
  expect_setequal(unique(out$sweep$result$act_set$chemical), c("chemA", "chemB"))
  # Re-estimating from the saved sweep needs no Curvep run.
  again <- suppressWarnings(Rcurvep::estimate_dataset_bmr(out$sweep, plot = FALSE))
  expect_equal(again$outcome, out$estimate$outcome)
})

# ---- run_bmc_curvep() dichotomous mode: synthetic unit tests ---------------

test_that("run_bmc_curvep() supports dichotomous mode via n_in_col/n_col", {
  d <- tibble::tibble(
    endpoint = "percent of affected",
    chemical = "chemA",
    conc = seq(-6, -2, length.out = 5),
    n_affected = c(0, 1, 2, 5, 8),
    N = 8
  )
  bmr_table <- tibble::tibble(
    endpoint = "percent of affected",
    bmr = 30,
    RNGE = 1000000
  )
  out <- run_bmc_curvep(
    d,
    bmr_table,
    n_in_col = "n_affected",
    n_col = "N",
    n_samples = 5,
    seed = 1
  )
  expect_true(is.list(out))
  expect_true(all(c("raw", "act_summary") %in% names(out)))
  expect_equal(nrow(out$act_summary), 1)
  expect_true("hit_confidence" %in% colnames(out$act_summary))
})

test_that("run_bmc_curvep() errors if only one of n_in_col/n_col is supplied", {
  d <- tibble::tibble(
    endpoint = "e1",
    chemical = "c1",
    conc = -5,
    n_affected = 1,
    N = 8
  )
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 30, RNGE = 1e6)
  expect_error(
    run_bmc_curvep(d, bmr_table, n_in_col = "n_affected"),
    class = "zfbehavior_error_incomplete_dichotomous_cols"
  )
  expect_error(
    run_bmc_curvep(d, bmr_table, n_col = "N"),
    class = "zfbehavior_error_incomplete_dichotomous_cols"
  )
})

test_that("run_bmc_curvep() dichotomous mode errors on missing columns in d", {
  d <- tibble::tibble(endpoint = "e1", chemical = "c1", conc = -5)
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 30, RNGE = 1e6)
  expect_error(
    run_bmc_curvep(d, bmr_table, n_in_col = "n_affected", n_col = "N"),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("run_bmc_curvep() continuous mode is unaffected by the dichotomous extension", {
  # Regression guard: confirm the existing continuous-mode behavior
  # (value_col-based) is completely unchanged when n_in_col/n_col are
  # left at their NULL defaults.
  d <- tibble::tibble(
    endpoint = "e1",
    chemical = rep(c("c1", "c2"), each = 5),
    conc = rep(seq(-6, -2, length.out = 5), 2),
    endpoint_value_norm = c(1, 2, 3, 20, 25, 1, 1, 2, 2, 3)
  )
  bmr_table <- tibble::tibble(endpoint = "e1", bmr = 10, RNGE = 1e6)
  out <- run_bmc_curvep(
    d,
    bmr_table,
    value_col = "endpoint_value_norm",
    n_samples = 5,
    seed = 1
  )
  expect_equal(nrow(out$act_summary), 2)
})

# ---- plot_devtox_dose_response(): synthetic unit tests ---------------------

devtox_plot_endpoints <- function() {
  tibble::tibble(
    DTXSID = rep(c("DTX1", "DTX2"), each = 6),
    plate_id = rep(c("pA", "pB", "pC", "pD"), each = 3),
    is_vehicle_control = rep(c(TRUE, FALSE, FALSE), 4),
    concentration = rep(c(0, 1, 10), 4),
    concentration_unit = "uM",
    N = 4,
    n_affected = c(0, 1, 4, 1, 2, 3, 0, 0, 1, 0, 1, 1)
  ) |>
    dplyr::mutate(
      pct_affected = 100 * .data$n_affected / .data$N,
      endpoint = "percent of affected"
    )
}

text_layers <- function(p) {
  which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
}

test_that("plot_devtox_dose_response() draws VC first, fixed 0-100 axis, unit as x title", {
  e <- dplyr::filter(devtox_plot_endpoints(), .data$plate_id == "pA")
  p <- plot_devtox_dose_response(e)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$layout$panel_scales_y[[1]]$get_limits(), c(0, 100))
  expect_equal(built$layout$panel_scales_x[[1]]$get_labels()[1], "VC")
  expect_equal(p$scales$get_scales("x")$name, "uM")
  expect_length(text_layers(p), 0)
})

test_that("plot_devtox_dose_response() works from calculate_devtox_endpoints() output directly", {
  d <- devtox_wells()
  e <- calculate_devtox_endpoints(d, group_cols = "plate_id")
  p <- plot_devtox_dose_response(e, title = "plate1")
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), 3)
  expect_equal(built$data[[1]]$y, c(50, 50, 50))
})

test_that("plot_devtox_dose_response() draws one colored line per line_col value", {
  e <- dplyr::filter(devtox_plot_endpoints(), .data$DTXSID == "DTX1")
  p <- plot_devtox_dose_response(e, line_col = "plate_id")
  built <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(built$data[[1]]$colour)), 2)
})

test_that("plot_devtox_dose_response() annotates BMC, suppresses NA BMC, appends & for low active fraction", {
  e <- dplyr::filter(devtox_plot_endpoints(), .data$plate_id == "pA")
  p <- plot_devtox_dose_response(e, tibble::tibble(BMC = 5, BMCL = 4, score = 0.9))
  built <- ggplot2::ggplot_build(p)
  expect_match(built$data[[text_layers(p)]]$label, "^BMC: 5e\\+00")

  p_na <- plot_devtox_dose_response(e, tibble::tibble(BMC = NA_real_, BMCL = NA_real_, score = 0.2))
  expect_length(text_layers(p_na), 0)

  p_amp <- plot_devtox_dose_response(
    e,
    tibble::tibble(BMC = 5, BMCL = 4, score = 0.9, baf = 0.2),
    batch_active_fraction_col = "baf"
  )
  expect_match(ggplot2::ggplot_build(p_amp)$data[[text_layers(p_amp)]]$label, "&", fixed = TRUE)
})

test_that("plot_devtox_dose_response() errors on missing columns and multi-row bmc_data", {
  expect_error(
    plot_devtox_dose_response(tibble::tibble(concentration = 1)),
    class = "zfbehavior_error_missing_columns"
  )
  e <- dplyr::filter(devtox_plot_endpoints(), .data$plate_id == "pA")
  expect_error(
    plot_devtox_dose_response(e, tibble::tibble(BMC = 1:2, BMCL = 1:2, score = 1:2)),
    class = "zfbehavior_error_invalid_argument"
  )
})

# ---- plot_devtox_dose_response_pages() --------------------------------------

test_that("plot_devtox_dose_response_pages() makes one panel per group, paginated by nrow x ncol", {
  e <- devtox_plot_endpoints()
  out <- plot_devtox_dose_response_pages(e, group_cols = "plate_id", nrow = 1, ncol = 3)
  expect_s3_class(out, "arrangelist")
  expect_length(out, 2) # 4 panels, 3 per page

  by_chem <- plot_devtox_dose_response_pages(
    e,
    group_cols = "DTXSID",
    line_col = "plate_id"
  )
  expect_length(by_chem, 1)
})

test_that("the internal per-group helper respects factor order, title_col and bmc matching", {
  e <- devtox_plot_endpoints() |>
    dplyr::mutate(
      plate_id = factor(.data$plate_id, levels = c("pD", "pC", "pB", "pA")),
      title = paste("plate", .data$plate_id)
    )
  bmc <- tibble::tibble(plate_id = "pB", BMC = 3, BMCL = 2, score = 0.8)
  pls <- plot_devtox_dose_response_by_group(
    e,
    group_cols = "plate_id",
    bmc_data = bmc,
    title_col = "title"
  )
  expect_equal(names(pls), c("pD", "pC", "pB", "pA"))
  expect_equal(pls[["pD"]]$labels$title, "plate pD")
  expect_length(text_layers(pls[["pB"]]), 1)
  expect_length(text_layers(pls[["pA"]]), 0)
})

test_that("plot_devtox_dose_response_pages() validates its arguments", {
  e <- devtox_plot_endpoints()
  expect_error(
    plot_devtox_dose_response_pages(e, group_cols = "chemical"),
    class = "zfbehavior_error_missing_columns"
  )
  expect_error(
    plot_devtox_dose_response_pages(e, group_cols = "plate_id", ncol = 0),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    plot_devtox_dose_response_pages(
      e,
      group_cols = "plate_id",
      bmc_data = tibble::tibble(p = "pA", BMC = 1, BMCL = 1, score = 1),
      bmc_group_cols = c("p", "q")
    ),
    class = "zfbehavior_error_mismatched_group_cols"
  )
})

# ---- Real historical production ground-truth regression tests -------------
# See data-raw/build_devtox_fixture.R and data-raw/build_devtox_snapshot.R
# for full provenance. Uses a devtox-specific 3-chemical fixture
# (PE-4188/PE-4202/PE-4205), deliberately chosen to contain NO real
# multi-larva wells, so calculate_devtox_endpoints()'s documented
# multi-larva-well exclusion (a departure from historical ground truth --
# see R/devtox.R's `@section Multi-larva wells` note) does not confound
# this exact-match comparison.

test_that("calculate_devtox_endpoints() + run_bmc_curvep() reproduce real historical devtox BMC results exactly", {
  fixture_ready <- zf_devtox_fixture_ready()

  devtox_endpoints <- calculate_devtox_endpoints(
    fixture_ready,
    group_cols = c("chemical_id", "plate_id", "CHEMICAL_NAME"),
    include_vehicle_control = FALSE
  ) %>%
    dplyr::mutate(log_conc = log10(.data$concentration / 1e6)) %>%
    tidyr::unite(
      "chemical",
      c("CHEMICAL_NAME", "chemical_id", "plate_id"),
      sep = "@"
    )

  expected <- zf_devtox_outputs()
  bmr_devtox <- expected$bmr_devtox

  bmr_table <- tibble::tibble(
    endpoint = "percent of affected",
    bmr = bmr_devtox,
    RNGE = 1000000
  )

  out <- run_bmc_curvep(
    devtox_endpoints,
    bmr_table,
    conc_col = "log_conc",
    n_in_col = "n_affected",
    n_col = "N",
    seed = 2022,
    n_samples = 1000
  )

  expected_act_summary <- expected$act_summary

  cmp <- out$act_summary %>%
    dplyr::inner_join(
      expected_act_summary,
      by = "chemical",
      suffix = c("", "_expected")
    )
  expect_equal(nrow(cmp), 3)

  # Confirmed (interactive session) to reproduce bit-for-bit in this
  # environment -- see data-raw/build_devtox_snapshot.R's header comment
  # for the full investigation. A tolerance is still used (not
  # expect_identical()) since this is a bootstrap-simulation-derived
  # result subject to the SAME category of R-version-dependent
  # Monte-Carlo noise documented for the continuous-mode BMC module
  # (see data-raw/build_bmc_snapshot.R), even though it happened to match
  # exactly here.
  expect_equal(cmp$POD_med, cmp$POD_med_expected, tolerance = 0.05)
  expect_equal(cmp$hit_confidence, cmp$hit_confidence_expected, tolerance = 0.1)
})

test_that("plot titles wrap at title_wrap_width (devtox, behavior, raw responses)", {
  long <- paste(rep("beta-Methylamino-L-Alanine hydrochloride", 3), collapse = " | ")
  e <- dplyr::filter(devtox_plot_endpoints(), .data$plate_id == "pA")
  p <- plot_devtox_dose_response(e, title = long)
  lines <- strsplit(p$labels$title, "\n", fixed = TRUE)[[1]]
  expect_gt(length(lines), 1)
  expect_true(all(nchar(lines) <= 40))
  expect_identical(plot_devtox_dose_response(e, title = long, title_wrap_width = Inf)$labels$title, long)
  expect_error(
    plot_devtox_dose_response(e, title = long, title_wrap_width = 0),
    class = "zfbehavior_error_invalid_argument"
  )

  e2 <- dplyr::mutate(devtox_plot_endpoints(), t = long)
  pls <- plot_devtox_dose_response_by_group(e2, group_cols = "plate_id", title_col = "t",
                                            title_wrap_width = 30)
  expect_true(all(nchar(strsplit(pls[[1]]$labels$title, "\n")[[1]]) <= 30))

  d <- tibble::tibble(
    endpoint = "D1@both_auc", concentration = c(0, 1, 10), endpoint_value_norm = c(1, 2, 3),
    is_vehicle_control = c(TRUE, FALSE, FALSE)
  )
  pb <- zfbehavior:::plot_behavior_dose_response(d, title = long)
  expect_true(all(nchar(strsplit(pb$labels$title, "\n")[[1]]) <= 40))
})
