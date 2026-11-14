# ---- summarize_raw_responses() ----------------------------------------------

test_that("summarize_raw_responses() computes known median/Q25/Q75", {
  d <- tibble::tibble(
    plate_id = "P1",
    time_value = rep(1, 4),
    response_value = c(1, 2, 3, 4),
    keep_well = TRUE
  )
  out <- summarize_raw_responses(d, group_cols = "plate_id")
  expect_equal(out$n, 4)
  expect_equal(out$center, 2.5)
  expect_equal(out$lower, stats::quantile(c(1, 2, 3, 4), 0.25, names = FALSE))
  expect_equal(out$upper, stats::quantile(c(1, 2, 3, 4), 0.75, names = FALSE))
})

test_that("summarize_raw_responses() computes known mean +/- 95% CI", {
  d <- tibble::tibble(
    plate_id = "P1",
    time_value = rep(1, 4),
    response_value = c(1, 2, 3, 4),
    keep_well = TRUE
  )
  out <- summarize_raw_responses(
    d,
    group_cols = "plate_id",
    summary_type = "mean"
  )
  se <- stats::sd(c(1, 2, 3, 4)) / sqrt(4)
  expect_equal(out$center, 2.5)
  expect_equal(out$lower, 2.5 - 1.96 * se)
  expect_equal(out$upper, 2.5 + 1.96 * se)
})

test_that("summarize_raw_responses() impute_zero vs na_omit give different results", {
  d <- tibble::tibble(
    plate_id = "P1",
    time_value = rep(1, 3),
    response_value = c(10, 20, NA),
    keep_well = TRUE
  )
  zero_out <- summarize_raw_responses(
    d,
    group_cols = "plate_id",
    missing_value_policy = "impute_zero"
  )
  omit_out <- summarize_raw_responses(
    d,
    group_cols = "plate_id",
    missing_value_policy = "na_omit"
  )
  expect_equal(zero_out$center, median(c(10, 20, 0)))
  expect_equal(omit_out$center, median(c(10, 20)))
  expect_false(isTRUE(all.equal(zero_out$center, omit_out$center)))
})

test_that("summarize_raw_responses() errors on missing columns", {
  d <- tibble::tibble(plate_id = "P1")
  expect_error(
    summarize_raw_responses(d, group_cols = "plate_id"),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("summarize_raw_responses() errors if any keep_well == FALSE row is present", {
  d <- tibble::tibble(
    plate_id = "P1",
    time_value = c(1, 1),
    response_value = c(1, 2),
    keep_well = c(TRUE, FALSE)
  )
  expect_error(
    summarize_raw_responses(d, group_cols = "plate_id"),
    class = "zfbehavior_error_excluded_wells_present"
  )
})

test_that("summarize_raw_responses() adds n_kept_wells/n_total_wells when well_counts is supplied", {
  d_full <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2", "A3", "B1", "B2"),
    is_vehicle_control = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    concentration = c(0, 0, 0, 50, 50),
    time_value = 1,
    response_value = c(1, 2, 3, 4, 5),
    keep_well = c(TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  totals <- count_wells_by_dose(
    d_full,
    group_cols = c("plate_id", "is_vehicle_control", "concentration")
  )
  d_kept <- d_full |> dplyr::filter(.data$keep_well)

  out <- summarize_raw_responses(
    d_kept,
    group_cols = c("plate_id", "is_vehicle_control", "concentration"),
    well_counts = totals
  )
  vc_row <- out[out$is_vehicle_control, ]
  treated_row <- out[!out$is_vehicle_control, ]
  expect_equal(vc_row$n_kept_wells, 2)
  expect_equal(vc_row$n_total_wells, 3)
  expect_equal(treated_row$n_kept_wells, 2)
  expect_equal(treated_row$n_total_wells, 2)
})

test_that("summarize_raw_responses() without well_counts has no n_kept_wells/n_total_wells columns", {
  d <- tibble::tibble(
    plate_id = "P1",
    time_value = rep(1, 4),
    response_value = c(1, 2, 3, 4),
    keep_well = TRUE
  )
  out <- summarize_raw_responses(d, group_cols = "plate_id")
  expect_false("n_kept_wells" %in% colnames(out))
  expect_false("n_total_wells" %in% colnames(out))
})

test_that("summarize_raw_responses() errors if well_counts is missing a group present in the data", {
  d <- tibble::tibble(
    plate_id = "P1",
    time_value = 1,
    response_value = 1,
    keep_well = TRUE
  )
  totals <- tibble::tibble(
    plate_id = "OTHER_PLATE",
    n_kept_wells = 1,
    n_total_wells = 1
  )
  expect_error(
    summarize_raw_responses(d, group_cols = "plate_id", well_counts = totals),
    class = "zfbehavior_error_missing_well_counts"
  )
})

test_that("summarize_raw_responses() errors if well_counts has a duplicate join-key row (Finding 7)", {
  d <- tibble::tibble(
    plate_id = "P1",
    time_value = 1,
    response_value = 1,
    keep_well = TRUE
  )
  # Two rows for the SAME plate_id -- an invalid, ambiguous well_counts
  # table that would otherwise silently duplicate d's rows on join.
  totals <- tibble::tibble(
    plate_id = c("P1", "P1"),
    n_kept_wells = c(1, 2),
    n_total_wells = c(2, 2)
  )
  expect_error(
    summarize_raw_responses(d, group_cols = "plate_id", well_counts = totals),
    class = "zfbehavior_error_duplicate_well_counts"
  )
})

test_that("summarize_raw_responses() does not duplicate response rows from a well_counts join (Finding 7 regression)", {
  # Directly demonstrates the failure mode Finding 7 fixes: prior to
  # validation, a well_counts table coarser than the caller's actual join
  # keys (e.g. counting only by plate_id when concentration also varies)
  # would duplicate response rows during the join, changing n/summary
  # statistics. Now this must error instead of silently duplicating.
  d <- tibble::tibble(
    plate_id = "P1",
    concentration = c(0, 50),
    time_value = c(1, 1),
    response_value = c(1, 2),
    keep_well = TRUE
  )
  # well_counts keyed by plate_id alone (coarser than the caller's
  # plate_id + concentration groups) -- ambiguous for a many-to-one join.
  totals <- tibble::tibble(
    plate_id = c("P1", "P1"),
    n_kept_wells = c(1, 1),
    n_total_wells = c(1, 1)
  )
  expect_error(
    summarize_raw_responses(
      d,
      group_cols = c("plate_id", "concentration"),
      well_counts = totals
    ),
    class = "zfbehavior_error_duplicate_well_counts"
  )
})

# ---- count_wells_by_dose() ----------------------------------------------------

test_that("count_wells_by_dose() computes known kept/total counts per dose group", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2", "A3", "B1", "B2"),
    is_vehicle_control = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    concentration = c(0, 0, 0, 50, 50),
    keep_well = c(TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  out <- count_wells_by_dose(
    d,
    group_cols = c("plate_id", "is_vehicle_control", "concentration")
  )
  vc_row <- out[out$is_vehicle_control, ]
  treated_row <- out[!out$is_vehicle_control, ]
  expect_equal(vc_row$n_kept_wells, 2)
  expect_equal(vc_row$n_total_wells, 3)
  expect_equal(treated_row$n_kept_wells, 2)
  expect_equal(treated_row$n_total_wells, 2)
})

test_that("count_wells_by_dose() treats NA keep_well as excluded, not counted in n_kept_wells", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2"),
    keep_well = c(TRUE, NA)
  )
  out <- count_wells_by_dose(d, group_cols = "plate_id")
  expect_equal(out$n_kept_wells, 1)
  expect_equal(out$n_total_wells, 2)
})

test_that("count_wells_by_dose() does NOT error on data containing keep_well == FALSE rows", {
  # Unlike check_no_excluded_wells()-guarded functions, this function is
  # specifically designed to receive the FULL, unfiltered dataset.
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2"),
    keep_well = c(TRUE, FALSE)
  )
  expect_no_error(count_wells_by_dose(d, group_cols = "plate_id"))
})

test_that("count_wells_by_dose() errors on missing columns", {
  d <- tibble::tibble(plate_id = "P1")
  expect_error(
    count_wells_by_dose(d, group_cols = "plate_id"),
    class = "zfbehavior_error_missing_columns"
  )
})

# ---- make_dose_label() (via internal call, not exported) ---------------------

test_that("make_dose_label() appends (n_kept/n_total) when both are supplied", {
  out <- make_dose_label(
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    n_kept = c(15, 14),
    n_total = c(16, 16)
  )
  expect_equal(levels(out), c("VC(15/16)", "50uM(14/16)"))
})

test_that("make_dose_label() is unchanged (no suffix) when n_kept/n_total are NULL", {
  out <- make_dose_label(
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE)
  )
  expect_equal(levels(out), c("VC", "50uM"))
})

test_that("make_dose_label() errors if only one of n_kept/n_total is supplied", {
  expect_error(
    make_dose_label(
      concentration = 0,
      is_vehicle_control = TRUE,
      n_kept = 15
    ),
    class = "zfbehavior_error_missing_well_counts"
  )
  expect_error(
    make_dose_label(
      concentration = 0,
      is_vehicle_control = TRUE,
      n_total = 16
    ),
    class = "zfbehavior_error_missing_well_counts"
  )
})

# ---- segments_to_shading_df() ------------------------------------------------

test_that("segments_to_shading_df() derives real (possibly unequal) segment spans", {
  segments <- list(L1 = 1:10, D1 = 11:20, L2 = 21:30, D2 = 31:50)
  out <- segments_to_shading_df(segments)

  expect_equal(out$segment, c("L1", "D1", "L2", "D2"))
  expect_equal(out$xmin, c(1, 11, 21, 31))
  expect_equal(out$xmax, c(10, 20, 30, 50)) # D2 correctly spans 20 minutes
})

test_that("segments_to_shading_df() defaults to shading light ('L') phases only", {
  segments <- list(L1 = 1:10, D1 = 11:20, L2 = 21:30, D2 = 31:40)
  out <- segments_to_shading_df(segments)

  expect_equal(out$fill, rep("yellow", 4))
  expect_equal(out$alpha[out$segment == "L1"], 0.2)
  expect_equal(out$alpha[out$segment == "L2"], 0.2)
  expect_equal(out$alpha[out$segment == "D1"], 0)
  expect_equal(out$alpha[out$segment == "D2"], 0)
})

test_that("segments_to_shading_df() honors explicit segment_style overrides", {
  segments <- list(Excitatory = 1:5, Refractory = 6:10)
  style <- list(
    Excitatory = list(fill = "orange", alpha = 0.3),
    Refractory = list(fill = "grey", alpha = 0.1)
  )
  out <- segments_to_shading_df(segments, segment_style = style)

  expect_equal(out$fill, c("orange", "grey"))
  expect_equal(out$alpha, c(0.3, 0.1))
})

test_that("segments_to_shading_df() validates segments the same way as endpoint functions", {
  expect_error(
    segments_to_shading_df(list(1:2, 3:4)),
    class = "zfbehavior_error_invalid_segments"
  )
})

# ---- sequential_blues_colors() -----------------------------------------------

test_that("sequential_blues_colors() is deterministic regardless of input row order", {
  wells_order_a <- c("A1", "H12", "B2", "G1")
  wells_order_b <- c("G1", "A1", "B2", "H12")

  out_a <- sequential_blues_colors(wells_order_a)
  names(out_a) <- wells_order_a
  out_b <- sequential_blues_colors(wells_order_b)
  names(out_b) <- wells_order_b

  expect_equal(out_a[sort(wells_order_a)], out_b[sort(wells_order_a)])
})

test_that("sequential_blues_colors() gives a monotonic gradient by alphabetical well order", {
  wells <- c("A1", "B1", "C1")
  out <- sequential_blues_colors(wells)
  # first (alphabetically) well gets the darkest shade (reversed Blues ramp)
  expect_equal(
    out[1],
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(3)[3]
  )
})

# ---- dose_color_map() --------------------------------------------------------

test_that("dose_color_map() assigns blue to vehicle control", {
  conc <- c(0, 0, 5, 50)
  is_vc <- c(TRUE, TRUE, FALSE, FALSE)
  out <- dose_color_map(conc, is_vc)
  expect_equal(out[is_vc], c("blue", "blue"))
})

test_that("dose_color_map() pops the lowest treated dose to pure red", {
  conc <- c(0, 5, 15, 50, 150, 500)
  is_vc <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  out <- dose_color_map(conc, is_vc)
  expect_equal(out[conc == 5], "red")
})

test_that("dose_color_map() gives a monotonically darkening red gradient for higher doses", {
  conc <- c(0, 5, 15, 50, 150, 500)
  is_vc <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  out <- dose_color_map(conc, is_vc)
  treated_colors <- out[!is_vc]
  # Simple relative-luminance proxy (sum of RGB channels) should generally
  # decrease (darker) with increasing concentration, after the forced-red
  # first dose.
  rgb_sum <- function(hex) sum(grDevices::col2rgb(hex))
  luminance <- vapply(treated_colors, rgb_sum, numeric(1))
  # Compare doses 2 through n (excluding the forced-red first dose, which
  # is not part of the natural ramp)
  expect_true(all(diff(luminance[-1]) <= 0))
})

test_that("dose_color_map() scales correctly for a single treated dose", {
  conc <- c(0, 5)
  is_vc <- c(TRUE, FALSE)
  out <- dose_color_map(conc, is_vc)
  expect_equal(out[!is_vc], "red")
})

test_that("dose_color_map() is deterministic (repeated calls agree)", {
  conc <- c(0, 5, 15, 50)
  is_vc <- c(TRUE, FALSE, FALSE, FALSE)
  out1 <- dose_color_map(conc, is_vc)
  out2 <- dose_color_map(conc, is_vc)
  expect_equal(out1, out2)
})

# ---- sequential_well_colors_within_dose() ------------------------------------

test_that("sequential_well_colors_within_dose() uses blues for VC and a red family per dose", {
  well_id <- c("A1", "A2", "B1", "B2")
  concentration <- c(0, 0, 5, 5)
  is_vc <- c(TRUE, TRUE, FALSE, FALSE)

  out <- sequential_well_colors_within_dose(well_id, concentration, is_vc)

  # VC wells: blues family (not red)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", out[is_vc])))
  # Treated wells (lowest & only dose): red family, both distinct from each other
  expect_false(out[3] == out[4])
})

test_that("sequential_well_colors_within_dose() gives the same color for a singleton dose group", {
  well_id <- c("A1", "B1")
  concentration <- c(0, 500)
  is_vc <- c(TRUE, FALSE)
  out <- sequential_well_colors_within_dose(well_id, concentration, is_vc)
  expect_length(out, 2)
})

# ---- plot_raw_responses() ----------------------------------------------------

make_raw_plot_fixture <- function() {
  tibble::tibble(
    plate_id = "P1",
    well_id = rep(c("A1", "A2", "B1", "B2"), each = 5),
    is_vehicle_control = rep(c(TRUE, TRUE, FALSE, FALSE), each = 5),
    concentration = rep(c(0, 0, 50, 50), each = 5),
    concentration_unit = "uM",
    time_value = rep(1:5, 4),
    response_value = c(
      1,
      2,
      3,
      2,
      1,
      1,
      2,
      4,
      2,
      1,
      5,
      6,
      7,
      6,
      5,
      5,
      7,
      8,
      7,
      5
    ),
    keep_well = TRUE
  )
}

test_that("plot_raw_responses() returns a ggplot object for level='summary'", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:3, D1 = 4:5)
  p <- plot_raw_responses(d, segments, level = "summary")
  expect_s3_class(p, "ggplot")
})

test_that("plot_raw_responses() returns a ggplot object for level='well'", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:3, D1 = 4:5)
  p <- plot_raw_responses(d, segments, level = "well")
  expect_s3_class(p, "ggplot")
})

test_that("plot_raw_responses() defaults level='well' to vehicle-control wells only", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:3, D1 = 4:5)
  p <- plot_raw_responses(d, segments, level = "well")
  b <- ggplot2::ggplot_build(p)
  line_layer <- b$data[[length(b$data) - 1]]
  expect_equal(length(unique(line_layer$colour)), 2) # only A1, A2 (VC)
})

test_that("plot_raw_responses() with wells='all' includes treated wells too", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:3, D1 = 4:5)
  p <- plot_raw_responses(d, segments, level = "well", wells = "all")
  b <- ggplot2::ggplot_build(p)
  line_layer <- b$data[[length(b$data) - 1]]
  expect_equal(length(unique(line_layer$colour)), 4) # A1, A2, B1, B2
})

test_that("plot_raw_responses() emits a classed warning for level='well', missing_value_policy='na_omit' when responses are actually missing", {
  d <- make_raw_plot_fixture()
  d$response_value[d$well_id == "A1" & d$time_value == 2] <- NA_real_
  segments <- list(L1 = 1:3, D1 = 4:5)

  w <- tryCatch(
    {
      plot_raw_responses(
        d,
        segments,
        level = "well",
        missing_value_policy = "na_omit"
      )
      NULL
    },
    warning = function(w) w
  )
  expect_s3_class(w, "zfbehavior_warning_missing_response_dropped")
  expect_match(conditionMessage(w), "1 response_value")
  expect_match(conditionMessage(w), "A1")
})

test_that("plot_raw_responses() emits no na_omit warning when there are no missing responses", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:3, D1 = 4:5)

  expect_no_warning(
    plot_raw_responses(
      d,
      segments,
      level = "well",
      missing_value_policy = "na_omit"
    )
  )
})

test_that("plot_raw_responses() level='well', missing_value_policy='na_omit' does not impute NA responses to zero", {
  d <- make_raw_plot_fixture()
  d$response_value[d$well_id == "A1" & d$time_value == 2] <- NA_real_
  segments <- list(L1 = 1:3, D1 = 4:5)

  p <- suppressWarnings(plot_raw_responses(
    d,
    segments,
    level = "well",
    missing_value_policy = "na_omit"
  ))
  line_layer_data <- p$layers[[
    which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  ]]$data
  expect_true(anyNA(
    line_layer_data$response_value[line_layer_data$well_id == "A1"]
  ))
})

test_that("plot_raw_responses() shades only light-named segments by default", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:3, D1 = 4:5)
  p <- plot_raw_responses(d, segments, level = "summary")
  b <- ggplot2::ggplot_build(p)
  # first two layers are the shading rects (one per segment)
  alphas <- vapply(b$data[1:2], function(x) x$alpha, numeric(1))
  expect_equal(alphas, c(0.2, 0))
})

test_that("plot_raw_responses() renders the uncertainty band by default", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:3, D1 = 4:5)
  p <- plot_raw_responses(d, segments, level = "summary")
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRibbon" %in% geom_classes)
})

test_that("plot_raw_responses() can suppress the uncertainty band", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:3, D1 = 4:5)
  p <- plot_raw_responses(
    d,
    segments,
    level = "summary",
    show_uncertainty_band = FALSE
  )
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomRibbon" %in% geom_classes)
})

test_that("plot_raw_responses() errors on missing canonical columns", {
  d <- tibble::tibble(plate_id = "P1")
  expect_error(
    plot_raw_responses(d, list(L1 = 1:2)),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("plot_raw_responses() errors if any keep_well == FALSE row is present", {
  d <- make_raw_plot_fixture()
  d$keep_well[1] <- FALSE
  expect_error(
    plot_raw_responses(d, list(L1 = 1:3, D1 = 4:5)),
    class = "zfbehavior_error_excluded_wells_present"
  )
})

test_that("plot_raw_responses() level='well' never connects the same well_id across two different plates (plate-disambiguation regression)", {
  # Two plates ("P1", "P2") with identical well_id AND dose values, each
  # internally consistent but on an obviously distinct response scale.
  # Individual well lines/colors must be disambiguated by plate (an
  # internal plate_id+well_id key), even though pooling IS allowed at
  # level = "summary" (see summary_group_cols tests below) -- individual
  # well traces are never averaged/merged across plates.
  d1 <- make_raw_plot_fixture()
  d2 <- d1 |>
    dplyr::mutate(
      plate_id = "P2",
      response_value = .data$response_value + 1000 # obviously distinct scale
    )
  d <- dplyr::bind_rows(d1, d2)

  p <- plot_raw_responses(d, list(L1 = 1:3, D1 = 4:5), level = "well", wells = "all")
  b <- ggplot2::ggplot_build(p)
  line_layer <- b$data[[length(b$data) - 1]]

  # 4 wells per plate x 2 plates = 8 distinct plot_well_key colors, not 4
  # (which would indicate the same well_id was reused/merged across plates).
  expect_equal(length(unique(line_layer$colour)), 8)

  # No single color's points span both the low (<1000) and high (>1000)
  # response ranges -- i.e. no color mixes P1 and P2 data together.
  for (col in unique(line_layer$colour)) {
    ys <- line_layer$y[line_layer$colour == col]
    expect_true(all(ys < 1000) || all(ys > 1000))
  }
})

test_that("plot_raw_responses() level='summary' with the default summary_group_cols keeps multiple plates as separate (same-colored) lines, not merged", {
  # Default summary_group_cols = "plate_id": two plates sharing a dose
  # label must be summarized (and plotted) as two INDEPENDENT curves, not
  # silently interleaved/averaged into one continuous trace.
  d1 <- make_raw_plot_fixture()
  d2 <- d1 |>
    dplyr::mutate(
      plate_id = "P2",
      response_value = .data$response_value + 1000 # obviously distinct scale
    )
  d <- dplyr::bind_rows(d1, d2)

  p <- plot_raw_responses(d, list(L1 = 1:3, D1 = 4:5), level = "summary")
  line_layer_data <- p$layers[[
    which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  ]]$data

  # 2 dose_labels x 2 plates = 4 distinct plot_group_key groups (each
  # plate's curve stays a distinct group, not merged into 2).
  expect_equal(length(unique(line_layer_data$plot_group_key)), 4)

  # No single group mixes P1's (<1000) and P2's (>1000) values together.
  for (grp in unique(line_layer_data$plot_group_key)) {
    centers <- line_layer_data$center[line_layer_data$plot_group_key == grp]
    expect_true(all(centers < 1000) || all(centers > 1000))
  }
})

test_that("plot_raw_responses() level='summary' with summary_group_cols = character(0) genuinely pools wells across plates into one combined curve", {
  # Explicit opt-in for the "one chemical, multiple replicate plates,
  # pooled trend line" use case: dropping plate_id from the grouping
  # entirely must produce statistics computed from BOTH plates' wells
  # together, matching a manual pooled calculation exactly.
  d1 <- make_raw_plot_fixture()
  d2 <- d1 |> dplyr::mutate(plate_id = "P2")
  d <- dplyr::bind_rows(d1, d2)

  p <- plot_raw_responses(
    d,
    list(L1 = 1:3, D1 = 4:5),
    level = "summary",
    summary_group_cols = character(0)
  )
  line_layer_data <- p$layers[[
    which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  ]]$data

  # Only 2 dose_labels total now (VC, 50uM) -- no separate per-plate groups.
  expect_equal(length(unique(line_layer_data$plot_group_key)), 2)

  # Manually verify the pooled VC median at time_value == 1 matches
  # combining both plates' VC wells (A1, A2) at that timepoint, both via
  # a direct manual calculation AND via summarize_raw_responses() itself
  # (with plate_id excluded from group_cols, mirroring
  # summary_group_cols = character(0)).
  expected_vc_t1 <- stats::median(c(
    d1$response_value[d1$well_id %in% c("A1", "A2") & d1$time_value == 1],
    d2$response_value[d2$well_id %in% c("A1", "A2") & d2$time_value == 1]
  ))

  s_pooled <- summarize_raw_responses(
    d |>
      dplyr::mutate(
        dose_label = make_dose_label(
          .data$concentration,
          .data$concentration_unit,
          .data$is_vehicle_control
        )
      ),
    group_cols = c("dose_label", "concentration", "is_vehicle_control")
  )
  vc_row <- s_pooled[s_pooled$is_vehicle_control & s_pooled$time_value == 1, ]
  expect_equal(vc_row$center, expected_vc_t1)
  expect_equal(vc_row$n, 4) # 2 VC wells x 2 plates

  # And confirm the ACTUAL plotted line data (not just an independent
  # recomputation) shows this same pooled value at time_value == 1.
  actual_vc_t1 <- line_layer_data$center[
    line_layer_data$is_vehicle_control & line_layer_data$time_value == 1
  ]
  expect_equal(actual_vc_t1, vc_row$center)
})

test_that("plot_raw_responses_by_group() pools multiple plates per chemical when group_cols excludes plate_id and summary_group_cols = character(0)", {
  # Mirrors devtox.R's chemical_group_cols precedent: grouping only by a
  # chemical identifier (not plate_id) is a legitimate request to batch
  # MULTIPLE PLATES into one chunk, not an error.
  d1 <- make_raw_plot_fixture() |> dplyr::mutate(chemical_id = "chemA")
  d2 <- d1 |> dplyr::mutate(plate_id = "P2")
  d <- dplyr::bind_rows(d1, d2)

  plots <- plot_raw_responses_by_group(
    d,
    group_cols = "chemical_id",
    segments = list(L1 = 1:3, D1 = 4:5),
    level = "summary",
    summary_group_cols = character(0)
  )

  expect_length(plots, 1)
  expect_named(plots, "chemA")

  line_layer_data <- plots[["chemA"]]$layers[[
    which(vapply(
      plots[["chemA"]]$layers,
      function(l) inherits(l$geom, "GeomLine"),
      logical(1)
    ))
  ]]$data
  # Pooled across both plates: only 2 dose_label groups, not 4.
  expect_equal(length(unique(line_layer_data$plot_group_key)), 2)
})

test_that("plot_raw_responses() smoke test on the real fixture (both levels)", {
  d <- zf_fixture_canonical() |>
    dplyr::filter(.data$keep_well, .data$plate_id == "Plate_01")
  segments <- zf_fixture_segments()

  p_summary <- plot_raw_responses(d, segments, level = "summary")
  expect_s3_class(p_summary, "ggplot")

  p_well <- plot_raw_responses(d, segments, level = "well")
  expect_s3_class(p_well, "ggplot")
})

test_that("plot_raw_responses() appends (n_kept/n_total) to legend labels when well_counts is supplied", {
  d_full <- make_raw_plot_fixture()
  # Simulate one excluded VC well (A2) in the full dataset.
  d_full$keep_well[d_full$well_id == "A2"] <- FALSE
  totals <- count_wells_by_dose(
    d_full,
    group_cols = c("plate_id", "is_vehicle_control", "concentration")
  )
  d_kept <- d_full |> dplyr::filter(.data$keep_well)

  p <- plot_raw_responses(
    d_kept,
    list(L1 = 1:3, D1 = 4:5),
    level = "summary",
    well_counts = totals
  )
  line_layer_data <- p$layers[[
    which(vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomLine"),
      logical(1)
    ))
  ]]$data
  expect_setequal(
    as.character(unique(line_layer_data$dose_label)),
    c("VC(1/2)", "50uM(2/2)")
  )
})

test_that("plot_raw_responses() appends (n_kept/n_total) to facet labels for level='well'", {
  d_full <- make_raw_plot_fixture()
  d_full$keep_well[d_full$well_id == "A2"] <- FALSE
  totals <- count_wells_by_dose(
    d_full,
    group_cols = c("plate_id", "is_vehicle_control", "concentration")
  )
  d_kept <- d_full |> dplyr::filter(.data$keep_well)

  p <- plot_raw_responses(
    d_kept,
    list(L1 = 1:3, D1 = 4:5),
    level = "well",
    wells = "all",
    well_counts = totals
  )
  b <- ggplot2::ggplot_build(p)
  expect_setequal(
    levels(b$layout$layout$dose_label),
    c("VC(1/2)", "50uM(2/2)")
  )
})

test_that("plot_raw_responses() without well_counts has unchanged (no-suffix) labels", {
  d <- make_raw_plot_fixture()
  p <- plot_raw_responses(d, list(L1 = 1:3, D1 = 4:5), level = "summary")
  line_layer_data <- p$layers[[
    which(vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomLine"),
      logical(1)
    ))
  ]]$data
  expect_setequal(
    as.character(unique(line_layer_data$dose_label)),
    c("VC", "50uM")
  )
})

test_that("plot_raw_responses() errors if well_counts is missing a dose group present in d", {
  d <- make_raw_plot_fixture()
  totals <- tibble::tibble(
    plate_id = "OTHER_PLATE",
    is_vehicle_control = TRUE,
    concentration = 0,
    n_kept_wells = 1,
    n_total_wells = 1
  )
  expect_error(
    plot_raw_responses(
      d,
      list(L1 = 1:3, D1 = 4:5),
      level = "summary",
      well_counts = totals
    ),
    class = "zfbehavior_error_missing_well_counts"
  )
})

test_that("plot_raw_responses() errors on nonnumeric/invalid well_counts values (Finding 7)", {
  d <- make_raw_plot_fixture()

  totals_negative <- tibble::tibble(
    plate_id = "P1",
    is_vehicle_control = c(TRUE, FALSE),
    concentration = c(0, 50),
    n_kept_wells = c(-1, 2),
    n_total_wells = c(2, 2)
  )
  expect_error(
    plot_raw_responses(
      d,
      list(L1 = 1:3, D1 = 4:5),
      level = "summary",
      well_counts = totals_negative
    ),
    class = "zfbehavior_error_invalid_well_counts"
  )

  totals_noninteger <- tibble::tibble(
    plate_id = "P1",
    is_vehicle_control = c(TRUE, FALSE),
    concentration = c(0, 50),
    n_kept_wells = c(1.5, 2),
    n_total_wells = c(2, 2)
  )
  expect_error(
    plot_raw_responses(
      d,
      list(L1 = 1:3, D1 = 4:5),
      level = "summary",
      well_counts = totals_noninteger
    ),
    class = "zfbehavior_error_invalid_well_counts"
  )

  totals_kept_gt_total <- tibble::tibble(
    plate_id = "P1",
    is_vehicle_control = c(TRUE, FALSE),
    concentration = c(0, 50),
    n_kept_wells = c(3, 2),
    n_total_wells = c(2, 2)
  )
  expect_error(
    plot_raw_responses(
      d,
      list(L1 = 1:3, D1 = 4:5),
      level = "summary",
      well_counts = totals_kept_gt_total
    ),
    class = "zfbehavior_error_invalid_well_counts"
  )

  totals_nonfinite <- tibble::tibble(
    plate_id = "P1",
    is_vehicle_control = c(TRUE, FALSE),
    concentration = c(0, 50),
    n_kept_wells = c(NA_real_, 2),
    n_total_wells = c(2, 2)
  )
  expect_error(
    plot_raw_responses(
      d,
      list(L1 = 1:3, D1 = 4:5),
      level = "summary",
      well_counts = totals_nonfinite
    ),
    class = "zfbehavior_error_invalid_well_counts"
  )
})

test_that("plot_raw_responses() adds a title when title_col has one distinct value", {
  d <- make_raw_plot_fixture()
  d$plot_label <- "ChemX@Blind01@P1"
  p <- plot_raw_responses(
    d,
    list(L1 = 1:3, D1 = 4:5),
    level = "summary",
    title_col = "plot_label"
  )
  expect_equal(p$labels$title, "ChemX@Blind01@P1")
})

test_that("plot_raw_responses() wraps long titles at title_wrap_width", {
  d <- make_raw_plot_fixture()
  d$plot_label <- paste(rep("Microcystin-YR (MC-YR) PE-4618 B0118", 3), collapse = "@")
  p <- plot_raw_responses(d, list(L1 = 1:3, D1 = 4:5), level = "summary",
                          title_col = "plot_label", title_wrap_width = 40)
  lines <- strsplit(p$labels$title, "\n", fixed = TRUE)[[1]]
  expect_gt(length(lines), 1)
  expect_true(all(nchar(lines) <= 40))
  # Default 60; unbroken text cannot wrap.
  d$plot_label <- "ChemX@Blind01@P1"
  expect_equal(plot_raw_responses(d, list(L1 = 1:3, D1 = 4:5), title_col = "plot_label")$labels$title,
               "ChemX@Blind01@P1")
})

test_that("plot_raw_responses() has no title when title_col is NULL", {
  d <- make_raw_plot_fixture()
  p <- plot_raw_responses(d, list(L1 = 1:3, D1 = 4:5), level = "summary")
  expect_null(p$labels$title)
})

test_that("plot_raw_responses() errors if title_col is missing from d", {
  d <- make_raw_plot_fixture()
  expect_error(
    plot_raw_responses(
      d,
      list(L1 = 1:3, D1 = 4:5),
      title_col = "does_not_exist"
    ),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("plot_raw_responses() errors if title_col has more than one distinct value", {
  d <- make_raw_plot_fixture()
  d$plot_label <- rep(c("A", "B"), length.out = nrow(d))
  expect_error(
    plot_raw_responses(
      d,
      list(L1 = 1:3, D1 = 4:5),
      title_col = "plot_label"
    ),
    class = "zfbehavior_error_ambiguous_title"
  )
})

test_that("plot_raw_responses() errors if title_col is all NA", {
  d <- make_raw_plot_fixture()
  d$plot_label <- NA_character_
  expect_error(
    plot_raw_responses(
      d,
      list(L1 = 1:3, D1 = 4:5),
      title_col = "plot_label"
    ),
    class = "zfbehavior_error_ambiguous_title"
  )
})

test_that("plot_raw_responses() title works for level='well' too", {
  d <- make_raw_plot_fixture()
  d$plot_label <- "ChemX@Blind01@P1"
  p <- plot_raw_responses(
    d,
    list(L1 = 1:3, D1 = 4:5),
    level = "well",
    title_col = "plot_label"
  )
  expect_equal(p$labels$title, "ChemX@Blind01@P1")
})

# ---- plot_raw_responses_by_group() -------------------------------------------

test_that("plot_raw_responses_by_group() returns one named plot per group", {
  d <- zf_fixture_canonical() |> dplyr::filter(.data$keep_well)
  segments <- zf_fixture_segments()

  plots <- plot_raw_responses_by_group(
    d,
    group_cols = "plate_id",
    segments = segments,
    level = "summary"
  )

  expect_length(plots, 3)
  expect_setequal(names(plots), unique(d$plate_id))
  expect_true(all(vapply(plots, inherits, logical(1), "ggplot")))
})

test_that("plot_raw_responses_by_group() defaults each plot's title to its own group key when title_col is NULL (Finding 5)", {
  d <- zf_fixture_canonical() |> dplyr::filter(.data$keep_well)
  segments <- zf_fixture_segments()

  plots <- plot_raw_responses_by_group(
    d,
    group_cols = "plate_id",
    segments = segments,
    level = "summary"
  )

  actual_titles <- vapply(plots, function(p) p$labels$title, character(1))
  expect_equal(unname(actual_titles), names(plots))
})

test_that("plot_raw_responses_by_group() errors on missing group_cols", {
  d <- make_raw_plot_fixture()
  expect_error(
    plot_raw_responses_by_group(
      d,
      group_cols = "does_not_exist",
      segments = list(L1 = 1:5)
    ),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("plot_raw_responses_by_group() passes well_counts through to each group's plot", {
  d_full <- zf_fixture_canonical()
  totals <- count_wells_by_dose(
    d_full,
    group_cols = c("plate_id", "is_vehicle_control", "concentration")
  )
  d_kept <- d_full |> dplyr::filter(.data$keep_well)
  segments <- zf_fixture_segments()

  plots <- plot_raw_responses_by_group(
    d_kept,
    group_cols = "plate_id",
    segments = segments,
    level = "summary",
    well_counts = totals
  )

  # Every plot's dose labels should carry a "(n/n)" count suffix.
  for (p in plots) {
    line_layer_data <- p$layers[[
      which(vapply(
        p$layers,
        function(l) inherits(l$geom, "GeomLine"),
        logical(1)
      ))
    ]]$data
    expect_true(all(grepl(
      "\\(\\d+/\\d+\\)$",
      as.character(line_layer_data$dose_label)
    )))
  }
})

test_that("plot_raw_responses_by_group() passes title_col through to each group's plot", {
  d <- zf_fixture_canonical() |>
    dplyr::filter(.data$keep_well) |>
    dplyr::mutate(plot_label = paste0("Chem@", .data$plate_id))
  segments <- zf_fixture_segments()

  plots <- plot_raw_responses_by_group(
    d,
    group_cols = "plate_id",
    segments = segments,
    level = "summary",
    title_col = "plot_label"
  )

  expected_titles <- paste0("Chem@", names(plots))
  actual_titles <- vapply(plots, function(p) p$labels$title, character(1))
  expect_equal(unname(actual_titles), expected_titles)
})

# ---- plot_raw_responses_pages() -----------------------------------------------

page_content_grid <- function(page) {
  # marrangeGrob(top = "") adds one title-bar grob in layout row 1 (spanning
  # every column) above the content grid -- exclude row 1 to isolate the
  # panel cells.
  lay <- page$layout
  cells <- lay[lay$name == "arrange" & lay$t > 1, ]
  c(nrow = length(unique(cells$t)), ncol = length(unique(cells$l)))
}

test_that("plot_raw_responses_pages() defaults to a 3 x 2 grid for level = 'summary'", {
  d <- make_raw_plot_fixture()
  d7 <- dplyr::bind_rows(lapply(paste0("P", 1:7), function(pid) {
    d |> dplyr::mutate(plate_id = pid)
  }))
  segments <- list(L1 = 1:2, D1 = 3:5)

  grob <- plot_raw_responses_pages(
    d7,
    group_cols = "plate_id",
    segments = segments,
    level = "summary"
  )
  expect_true(inherits(grob, "arrangelist") || is.list(grob))
  # 7 groups at 6 panels/page -> 2 pages.
  expect_equal(length(grob), 2)
  expect_equal(page_content_grid(grob[[1]]), c(nrow = 3, ncol = 2))
})

test_that("plot_raw_responses_pages() defaults to a 2 x 2 grid for level = 'well'", {
  d <- make_raw_plot_fixture()
  d5 <- dplyr::bind_rows(lapply(paste0("P", 1:5), function(pid) {
    d |> dplyr::mutate(plate_id = pid)
  }))
  segments <- list(L1 = 1:2, D1 = 3:5)

  grob <- plot_raw_responses_pages(
    d5,
    group_cols = "plate_id",
    segments = segments,
    level = "well"
  )
  # 5 groups at 4 panels/page -> 2 pages.
  expect_equal(length(grob), 2)
  expect_equal(page_content_grid(grob[[1]]), c(nrow = 2, ncol = 2))
})

test_that("plot_raw_responses_pages() no longer accepts n_per_page", {
  d <- make_raw_plot_fixture()
  expect_error(
    plot_raw_responses_pages(
      d,
      group_cols = "plate_id",
      segments = list(L1 = 1:2, D1 = 3:5),
      n_per_page = 2
    ),
    "unused argument"
  )
})

test_that("plot_raw_responses_pages() overrides the default grid with an explicit nrow x ncol", {
  d <- make_raw_plot_fixture()
  d3 <- dplyr::bind_rows(
    d,
    d |> dplyr::mutate(plate_id = "P2"),
    d |> dplyr::mutate(plate_id = "P3")
  )
  segments <- list(L1 = 1:2, D1 = 3:5)

  # nrow = 1, ncol = 2 -> 2 panels/page -> 2 pages (the summary default
  # 3 x 2 would give 1 page).
  grob <- plot_raw_responses_pages(
    d3,
    group_cols = "plate_id",
    segments = segments,
    level = "summary",
    nrow = 1,
    ncol = 2
  )
  expect_equal(length(grob), 2)
  expect_equal(page_content_grid(grob[[1]]), c(nrow = 1, ncol = 2))

  # nrow = 3, ncol = 1 at level = "well" -> 1 page (well default 2 x 2
  # would also give 1 page, so check the grid shape).
  grob_well <- plot_raw_responses_pages(
    d3,
    group_cols = "plate_id",
    segments = segments,
    level = "well",
    nrow = 3,
    ncol = 1
  )
  expect_equal(length(grob_well), 1)
  expect_equal(page_content_grid(grob_well[[1]]), c(nrow = 3, ncol = 1))
})

test_that("plot_raw_responses_pages() errors if only one of nrow/ncol is supplied", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:2, D1 = 3:5)

  expect_error(
    plot_raw_responses_pages(
      d,
      group_cols = "plate_id",
      segments = segments,
      level = "summary",
      nrow = 2
    ),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    plot_raw_responses_pages(
      d,
      group_cols = "plate_id",
      segments = segments,
      level = "summary",
      ncol = 2
    ),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("plot_raw_responses_pages() errors on invalid nrow/ncol values", {
  d <- make_raw_plot_fixture()
  segments <- list(L1 = 1:2, D1 = 3:5)

  expect_error(
    plot_raw_responses_pages(
      d,
      group_cols = "plate_id",
      segments = segments,
      level = "summary",
      nrow = 0,
      ncol = 2
    ),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    plot_raw_responses_pages(
      d,
      group_cols = "plate_id",
      segments = segments,
      level = "summary",
      nrow = 1.5,
      ncol = 2
    ),
    class = "zfbehavior_error_invalid_argument"
  )
  expect_error(
    plot_raw_responses_pages(
      d,
      group_cols = "plate_id",
      segments = segments,
      level = "summary",
      nrow = -1,
      ncol = 2
    ),
    class = "zfbehavior_error_invalid_argument"
  )
})

#' Recursively collect all text-grob label strings from a gtable/grob tree
#'
#' Used to verify ACTUAL rendered child content (not just page count/class)
#' for plot_raw_responses_pages()'s child titles (Finding 5).
collect_grob_text <- function(g, depth = 0) {
  if (depth > 8) {
    return(character(0))
  }
  out <- character(0)
  if (!is.null(g$label)) {
    out <- c(out, as.character(g$label))
  }
  for (child in g$grobs) {
    out <- c(out, collect_grob_text(child, depth + 1))
  }
  for (child in g$children) {
    out <- c(out, collect_grob_text(child, depth + 1))
  }
  out
}

test_that("plot_raw_responses_pages() renders each plate's generated group key as a visible child title (Finding 5)", {
  d <- make_raw_plot_fixture()
  d2 <- dplyr::bind_rows(
    d,
    d |> dplyr::mutate(plate_id = "P2"),
    d |> dplyr::mutate(plate_id = "P3")
  )
  segments <- list(L1 = 1:2, D1 = 3:5)

  grob <- plot_raw_responses_pages(
    d2,
    group_cols = "plate_id",
    segments = segments,
    level = "summary",
    nrow = 1,
    ncol = 2
  )

  page1_text <- collect_grob_text(grob[[1]])
  page2_text <- collect_grob_text(grob[[2]])

  expect_true("P1" %in% page1_text)
  expect_true("P2" %in% page1_text)
  expect_true("P3" %in% page2_text)
})

test_that("plot_raw_responses_pages() smoke test on the real fixture (both levels)", {
  d <- zf_fixture_canonical() |> dplyr::filter(.data$keep_well)
  segments <- zf_fixture_segments()

  grob_summary <- plot_raw_responses_pages(
    d,
    group_cols = "plate_id",
    segments = segments,
    level = "summary"
  )
  expect_true(inherits(grob_summary, "arrangelist") || is.list(grob_summary))
  # 3 distinct plates in the real fixture at the default 3 x 2 -> 1 page.
  expect_equal(length(grob_summary), 1)

  grob_well <- plot_raw_responses_pages(
    d,
    group_cols = "plate_id",
    segments = segments,
    level = "well"
  )
  expect_true(inherits(grob_well, "arrangelist") || is.list(grob_well))
  # 3 plates at the default 2 x 2 -> 1 page.
  expect_equal(length(grob_well), 1)
})

test_that("plot_raw_responses_pages() passes well_counts and title_col through paging", {
  d_full <- zf_fixture_canonical()
  totals <- count_wells_by_dose(
    d_full,
    group_cols = c("plate_id", "is_vehicle_control", "concentration")
  )
  d_kept <- d_full |>
    dplyr::filter(.data$keep_well) |>
    dplyr::mutate(plot_label = paste0("Chem@", .data$plate_id))
  segments <- zf_fixture_segments()

  grob <- plot_raw_responses_pages(
    d_kept,
    group_cols = "plate_id",
    segments = segments,
    level = "summary",
    well_counts = totals,
    title_col = "plot_label"
  )
  expect_true(inherits(grob, "arrangelist") || is.list(grob))
  # 3 distinct plates at the default 3 x 2 -> 1 page.
  expect_equal(length(grob), 1)
})

test_that("plot_raw_responses_pages() errors on missing group_cols (via plot_raw_responses_by_group())", {
  d <- make_raw_plot_fixture()
  expect_error(
    plot_raw_responses_pages(
      d,
      group_cols = "does_not_exist",
      segments = list(L1 = 1:5)
    ),
    class = "zfbehavior_error_missing_columns"
  )
})
