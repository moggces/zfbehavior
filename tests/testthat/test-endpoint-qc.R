# ---- detect_outliers_iqr() (internal) ---------------------------------------------------

test_that("detect_outliers_iqr() flags known Tukey-fence outliers (degree = 1.5)", {
  x <- c(1, 2, 3, 4, 5, 100)
  out <- detect_outliers_iqr(x, degree = 1.5)
  expect_equal(out, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("detect_outliers_iqr() with degree = 3 is more lenient than degree = 1.5", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 20)
  n_flagged_15 <- sum(detect_outliers_iqr(x, degree = 1.5))
  n_flagged_3 <- sum(detect_outliers_iqr(x, degree = 3))
  expect_true(n_flagged_3 <= n_flagged_15)
})

test_that("detect_outliers_iqr() flags nothing when IQR is 0 (all identical values)", {
  x <- rep(5, 5)
  out <- detect_outliers_iqr(x)
  expect_equal(out, rep(FALSE, 5))
})

test_that("detect_outliers_iqr() errors on non-numeric input", {
  expect_error(
    detect_outliers_iqr(c("a", "b")),
    class = "zfbehavior_error_invalid_input"
  )
})

test_that("detect_outliers_iqr() na.rm = TRUE ignores NA when computing the fence, but the NA position itself stays NA", {
  x <- c(1, 2, 3, 4, 5, NA)
  out <- detect_outliers_iqr(x, na.rm = TRUE)
  # na.rm = TRUE lets the fence itself be computed from the non-NA values,
  # but comparing NA < bound / NA > bound still yields NA (not FALSE) at
  # the NA position -- this is standard R comparison semantics, not a bug.
  expect_true(is.na(out[6]))
  expect_true(all(!out[1:5]))
})

# ---- summarize_behavior_vc_distribution() ------------------------------------

vc_well_fixture <- function() {
  tibble::tibble(
    endpoint_type = "total_distance",
    endpoint = "E1",
    plate_id = rep(c("P1", "P2", "P3"), each = 2),
    is_vehicle_control = TRUE,
    denom = rep(c(1, 2, 3), each = 2),
    endpoint_value_norm = c(1, 2, 3, 4, 5, 100)
  )
}

test_that("summarize_behavior_vc_distribution() level = 'plate' (default) summarizes one denom per plate", {
  out <- summarize_behavior_vc_distribution(vc_well_fixture())
  expect_s3_class(out, "tbl_df")
  expect_equal(out$level, "plate")
  expect_equal(out$n, 3)
  expect_equal(out$mean, 2)
  expect_equal(out$median, 2)
  expect_equal(out$sd, 1)
  expect_equal(out$q1, 1.5)
  expect_equal(out$q3, 2.5)
  expect_equal(out$lower_fence, 1.5 - 3)
  expect_equal(out$upper_fence, 2.5 + 3)
  expect_equal(out$n_outliers, 0)
  expect_equal(c(out$min, out$max), c(1, 3))
})

test_that("summarize_behavior_vc_distribution() level = 'well' summarizes VC wells' normalized values", {
  out <- summarize_behavior_vc_distribution(vc_well_fixture(), level = "well")
  expect_equal(out$level, "well")
  expect_equal(out$n, 6)
  expect_equal(out$mean, mean(c(1:5, 100)))
  expect_equal(out$mad, stats::mad(c(1:5, 100)))
  expect_equal(out$n_outliers, 1)
  expect_true(out$sd_excluding_outliers < out$sd)
})

test_that("summarize_behavior_vc_distribution() ignores treated wells", {
  d <- vc_well_fixture()
  d <- dplyr::bind_rows(d, dplyr::mutate(d, is_vehicle_control = FALSE, denom = 1000, endpoint_value_norm = 1000))
  expect_equal(summarize_behavior_vc_distribution(d)$max, 3)
  expect_equal(summarize_behavior_vc_distribution(d, level = "well")$n, 6)
})

test_that("summarize_behavior_vc_distribution() groups independently by group_cols", {
  d <- dplyr::bind_rows(
    vc_well_fixture(),
    dplyr::mutate(vc_well_fixture(), endpoint = "E2", denom = denom * 10)
  )
  out <- summarize_behavior_vc_distribution(d)
  expect_equal(nrow(out), 2)
  expect_equal(out$mean[out$endpoint == "E2"], 20)
})

test_that("summarize_behavior_vc_distribution() errors on missing columns, bad level, non-constant denom", {
  expect_error(
    summarize_behavior_vc_distribution(tibble::tibble(endpoint = "E1")),
    class = "zfbehavior_error_missing_columns"
  )
  expect_error(
    summarize_behavior_vc_distribution(dplyr::select(vc_well_fixture(), -"endpoint_value_norm"), level = "well"),
    class = "zfbehavior_error_missing_columns"
  )
  expect_error(summarize_behavior_vc_distribution(vc_well_fixture(), level = "x"))
  d <- vc_well_fixture()
  d$denom[1] <- 99
  expect_error(summarize_behavior_vc_distribution(d), class = "zfbehavior_error_invalid_input")
})

test_that("summarize_behavior_vc_distribution(level = 'well') matches original get_vc_distrib() (regression)", {
  # See data-raw/build_endpoint_qc_snapshot.R: dcs_norm is Distance
  # Change/Shift data computed with the package functions; vc_distrib is the
  # ORIGINAL, unmodified get_vc_distrib() run on that same data.
  ref <- zf_endpoint_qc_outputs()

  new_out <- summarize_behavior_vc_distribution(
    ref$dcs_norm, level = "well", group_cols = "endpoint"
  )
  legacy_out <- ref$vc_distrib |>
    dplyr::rename(median_legacy = "med", sd_legacy = "sd", mean_legacy = "mean")

  cmp <- dplyr::inner_join(new_out, legacy_out, by = "endpoint")

  expect_equal(nrow(cmp), nrow(new_out))
  expect_equal(cmp$mean, cmp$mean_legacy, tolerance = 1e-8)
  expect_equal(cmp$median, cmp$median_legacy, tolerance = 1e-8)
  expect_equal(cmp$sd, cmp$sd_legacy, tolerance = 1e-8)
  expect_equal(cmp$sd_excluding_outliers, cmp$sd_ex, tolerance = 1e-8)
})

# ---- detect_behavior_vc_baseline_outliers() ----------------------------------

baseline_fixture <- function(denom, endpoint = "E1") {
  tibble::tibble(
    endpoint_type = "total_distance",
    endpoint = endpoint,
    plate_id = paste0("P", seq_along(denom)),
    is_vehicle_control = TRUE,
    denom = denom
  )
}

test_that("detect_behavior_vc_baseline_outliers() returns all rows with fences and direction", {
  d <- baseline_fixture(c(10, 11, 12, 13, 14, 1000, -1000))
  out <- detect_behavior_vc_baseline_outliers(d)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 7)
  expect_named(out, c("endpoint_type", "endpoint", "plate_id", "denom", "q1", "q3",
                      "lower_fence", "upper_fence", "direction", "is_outlier"))
  expect_equal(out$direction[out$plate_id == "P6"], "high")
  expect_equal(out$direction[out$plate_id == "P7"], "low")
  expect_equal(sum(out$is_outlier), 2)
  expect_equal(out$q1[1], stats::quantile(d$denom, 0.25, names = FALSE))
})

test_that("detect_behavior_vc_baseline_outliers() flagged_only and outlier_degree", {
  d <- baseline_fixture(c(1, 2, 3, 4, 5, 6, 7, 20))
  # Q1 = 2.75, Q3 = 6.25, IQR = 3.5 -> degree 1.5 upper = 11.5 (flags 20);
  # degree 3 upper = 16.75 (flags 20); degree 5 upper = 23.75 (none)
  expect_equal(detect_behavior_vc_baseline_outliers(d, outlier_degree = 1.5, flagged_only = TRUE)$plate_id, "P8")
  expect_equal(nrow(detect_behavior_vc_baseline_outliers(d, outlier_degree = 5, flagged_only = TRUE)), 0)
  expect_error(detect_behavior_vc_baseline_outliers(d, outlier_degree = 0),
               class = "zfbehavior_error_invalid_input")
})

test_that("detect_behavior_vc_baseline_outliers() flags no plates when baselines are similar", {
  out <- detect_behavior_vc_baseline_outliers(baseline_fixture(c(10, 11, 9, 10.5, 9.5)))
  expect_false(any(out$is_outlier))
})

test_that("detect_behavior_vc_baseline_outliers() ignores non-vehicle-control rows", {
  d <- tibble::tibble(
    endpoint_type = "t", endpoint = "E1",
    plate_id = c("P1", "P1", "P2", "P2"),
    is_vehicle_control = c(TRUE, FALSE, TRUE, FALSE),
    denom = c(10, 99999, 11, 99999)
  )
  out <- detect_behavior_vc_baseline_outliers(d)
  expect_equal(nrow(out), 2)
  expect_false(any(out$is_outlier))
})

test_that("detect_behavior_vc_baseline_outliers() errors on missing columns", {
  expect_error(
    detect_behavior_vc_baseline_outliers(tibble::tibble(endpoint = "E1")),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("detect_behavior_vc_baseline_outliers(degree 1.5) matches original get_vc_denom_outlier() (regression)", {
  # See data-raw/build_endpoint_qc_snapshot.R for provenance.
  ref <- zf_endpoint_qc_outputs()

  new_out <- detect_behavior_vc_baseline_outliers(
    ref$dcs_norm, group_cols = "endpoint", outlier_degree = 1.5, flagged_only = TRUE
  )
  legacy_out <- ref$vc_denom_outlier |> dplyr::rename(plate_id = "outlier")

  expect_equal(nrow(new_out), nrow(legacy_out))
  expect_setequal(
    paste(new_out$endpoint, new_out$plate_id),
    paste(legacy_out$endpoint, legacy_out$plate_id)
  )
})

# ---- plot_behavior_vc_baseline() ---------------------------------------------

test_that("plot_behavior_vc_baseline() returns a ggplot: boxplot, points, red outlier labels", {
  d <- dplyr::bind_rows(
    baseline_fixture(c(10, 11, 12, 13, 14, 1000)),
    baseline_fixture(c(1, 2, 3, 2, 1, 2), endpoint = "E2")
  )
  out <- detect_behavior_vc_baseline_outliers(d)
  p <- plot_behavior_vc_baseline(out, title = "T")
  expect_s3_class(p, "ggplot")
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
  expect_equal(geoms, c("GeomBoxplot", "GeomPoint", "GeomPoint", "GeomText"))
  expect_equal(p$layers[[3]]$aes_params$colour, "red")
  expect_equal(p$layers[[4]]$data$plate_id, "P6")
  expect_equal(p$layers[[2]]$aes_params$alpha, 0.4)
  expect_equal(p$labels$title, "T")
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("plot_behavior_vc_baseline() without outliers or labels draws no text layer", {
  out <- detect_behavior_vc_baseline_outliers(baseline_fixture(c(10, 11, 9, 10.5, 9.5)))
  p <- plot_behavior_vc_baseline(out, label_col = NULL)
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
  expect_false("GeomText" %in% geoms)
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("plot_behavior_vc_baseline() uses a custom label_col and works with plotly", {
  out <- detect_behavior_vc_baseline_outliers(baseline_fixture(c(10, 11, 12, 13, 14, 1000)))
  out$plate_label <- paste0("chem@", out$plate_id)
  p <- plot_behavior_vc_baseline(out, label_col = "plate_label")
  expect_equal(p$layers[[4]]$data$.label, "chem@P6")
  skip_if_not_installed("plotly")
  expect_s3_class(plotly::ggplotly(p, tooltip = "text"), "plotly")
})

test_that("plot_behavior_vc_baseline() errors on missing columns", {
  expect_error(
    plot_behavior_vc_baseline(tibble::tibble(endpoint = "E1")),
    class = "zfbehavior_error_missing_columns"
  )
})
