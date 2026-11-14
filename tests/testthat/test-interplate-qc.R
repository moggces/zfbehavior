# ---- calculate_plate_reference_pattern() -------------------------------------

test_that("calculate_plate_reference_pattern() filters to vehicle-control wells only", {
  d <- tibble::tibble(
    plate_id = c("P1", "P1", "P1", "P1"),
    is_vehicle_control = c(TRUE, TRUE, FALSE, FALSE),
    time_value = c(1, 1, 1, 1),
    response_value = c(1, 3, 1000, 2000),
    keep_well = TRUE
  )
  out <- calculate_plate_reference_pattern(d)
  expect_equal(out$n, 2)
  expect_equal(out$center, 2) # median(1, 3), not influenced by treated wells
})

test_that("calculate_plate_reference_pattern() groups independently by plate_id", {
  d <- tibble::tibble(
    plate_id = c("P1", "P1", "P2", "P2"),
    is_vehicle_control = TRUE,
    time_value = c(1, 1, 1, 1),
    response_value = c(1, 3, 10, 20),
    keep_well = TRUE
  )
  out <- calculate_plate_reference_pattern(d)
  expect_equal(nrow(out), 2)
  expect_equal(out$center[out$plate_id == "P1"], 2)
  expect_equal(out$center[out$plate_id == "P2"], 15)
})

test_that("calculate_plate_reference_pattern() errors on missing columns", {
  d <- tibble::tibble(plate_id = "P1")
  expect_error(
    calculate_plate_reference_pattern(d),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("calculate_plate_reference_pattern() errors if any keep_well == FALSE row is present", {
  d <- tibble::tibble(
    plate_id = c("P1", "P1"),
    is_vehicle_control = TRUE,
    time_value = c(1, 1),
    response_value = c(1, 3),
    keep_well = c(TRUE, FALSE)
  )
  expect_error(
    calculate_plate_reference_pattern(d),
    class = "zfbehavior_error_excluded_wells_present"
  )
})

test_that("calculate_plate_reference_pattern() passes through summary_type and missing_value_policy", {
  d <- tibble::tibble(
    plate_id = c("P1", "P1", "P1"),
    is_vehicle_control = TRUE,
    time_value = c(1, 1, 1),
    response_value = c(10, 20, NA),
    keep_well = TRUE
  )
  zero_out <- calculate_plate_reference_pattern(
    d,
    missing_value_policy = "impute_zero"
  )
  omit_out <- calculate_plate_reference_pattern(
    d,
    missing_value_policy = "na_omit"
  )
  expect_equal(zero_out$center, median(c(10, 20, 0)))
  expect_equal(omit_out$center, median(c(10, 20)))
})

# ---- fit_plate_pca() -----------------------------------------------------------

#' Build a synthetic plate x time reference-pattern tibble for PCA tests
#'
#' Each plate gets a shared base curve shape, scaled by a per-plate
#' amplitude and shifted by a per-plate offset (i.e. smooth, correlated
#' variation across all timepoints for a given plate), plus a small amount
#' of independent per-timepoint noise. This deliberately mirrors how real
#' vehicle-control reference curves vary (whole-curve amplitude/offset
#' differences between plates, not purely independent per-timepoint noise):
#' with `scale. = TRUE` PCA (required to match the published methodology),
#' PURELY independent per-timepoint noise gets standardized per-timepoint
#' and can make an unlucky "normal" plate look like a high-PC outlier by
#' chance alone, which would make outlier tests flaky for the wrong reason.
#' `seed = 2, n_plates = 12` is used throughout this file specifically
#' because it was confirmed (interactively) to produce zero
#' false-positive-flagged plates when no outlier is planted, for BOTH
#' `ellipse_normal` and `ellipse_t`, under the CORRECTED
#' `ellipse_center_cov_cutoff()` cutoff (`2 * qf(conf_level, 2, n - 1)`).
#' `n_plates = 12` is required (not the previously-used `8`): at `n = 8`,
#' the classical (self-referential) covariance estimate mathematically
#' cannot produce a squared Mahalanobis distance exceeding the correct
#' small-sample F-based cutoff by enough margin for a planted outlier to
#' reliably clear it -- this is the exact sample-size ceiling documented
#' as a known, deferred limitation (`fit_plate_pca()`/
#' `detect_plate_pca_outliers()` do not yet enforce a minimum-cohort-size
#' eligibility rule).
fake_reference_pattern <- function(
  n_plates = 12,
  n_time = 15,
  outlier_plate = NULL,
  seed = 2
) {
  set.seed(seed)
  base_curve <- seq(1, n_time)
  purrr::map_df(seq_len(n_plates), function(i) {
    plate_id <- paste0("P", i)
    amplitude <- stats::rnorm(1, mean = 1, sd = 0.05)
    offset <- stats::rnorm(1, mean = 0, sd = 0.3)
    noise <- stats::rnorm(n_time, sd = 0.1)
    curve <- base_curve * amplitude + offset + noise
    if (!is.null(outlier_plate) && plate_id == outlier_plate) {
      curve <- base_curve * 3 + 20 # obvious, large systematic shift
    }
    tibble::tibble(
      plate_id = plate_id,
      time_value = seq_len(n_time),
      center = curve
    )
  })
}

test_that("fit_plate_pca() returns a zfbehavior_plate_pca_result with expected structure", {
  ref <- fake_reference_pattern()
  pca <- fit_plate_pca(ref, n_pc = 2)

  expect_s3_class(pca, "zfbehavior_plate_pca_result")
  expect_true(all(c("plate_id", "PC1", "PC2") %in% colnames(pca$scores)))
  expect_equal(nrow(pca$scores), 12)
  expect_equal(pca$n_pc, 2)
  expect_length(pca$percent_var, 2)
  expect_true(all(pca$percent_var >= 0 & pca$percent_var <= 100))
})

test_that("fit_plate_pca() errors listing incomplete plates (missing timepoints)", {
  ref <- fake_reference_pattern()
  # Drop one timepoint for one plate, creating an incomplete matrix.
  ref_incomplete <- ref[!(ref$plate_id == "P3" & ref$time_value == 5), ]

  err <- tryCatch(
    fit_plate_pca(ref_incomplete, n_pc = 2),
    error = function(e) e
  )
  expect_s3_class(err, "zfbehavior_error_incomplete_reference_pattern")
  expect_match(conditionMessage(err), "P3")
})

test_that("fit_plate_pca() errors on missing columns", {
  d <- tibble::tibble(plate_id = "P1")
  expect_error(
    fit_plate_pca(d),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("fit_plate_pca() errors when n_pc exceeds available components", {
  ref <- fake_reference_pattern(n_plates = 3, n_time = 5)
  expect_error(
    fit_plate_pca(ref, n_pc = 10),
    class = "zfbehavior_error_invalid_n_pc"
  )
})

# ---- detect_plate_pca_outliers() -----------------------------------------------

test_that("detect_plate_pca_outliers() ellipse_normal flags a planted outlier plate", {
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15, outlier_plate = "P5")
  pca <- fit_plate_pca(ref, n_pc = 2)
  out <- detect_plate_pca_outliers(pca, method = "ellipse_normal")

  expect_true("P5" %in% out$plate_id[out$is_outlier])
  expect_true(all(
    c("plate_id", "PC1", "PC2", "distance", "cutoff", "is_outlier") %in%
      colnames(out)
  ))
})

test_that("detect_plate_pca_outliers() ellipse_t flags a planted outlier plate", {
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15, outlier_plate = "P5")
  pca <- fit_plate_pca(ref, n_pc = 2)
  out <- detect_plate_pca_outliers(pca, method = "ellipse_t")

  expect_true("P5" %in% out$plate_id[out$is_outlier])
})

test_that("detect_plate_pca_outliers() flags nothing when all plates are similar", {
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15)
  pca <- fit_plate_pca(ref, n_pc = 2)
  out_norm <- detect_plate_pca_outliers(pca, method = "ellipse_normal")
  out_t <- detect_plate_pca_outliers(pca, method = "ellipse_t")

  expect_equal(sum(out_norm$is_outlier), 0)
  expect_equal(sum(out_t$is_outlier), 0)
})

test_that("detect_plate_pca_outliers() mahalanobis_npc flags a planted outlier plate with default n_pc", {
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15, outlier_plate = "P5")
  pca <- fit_plate_pca(ref, n_pc = 2)
  out <- detect_plate_pca_outliers(pca, method = "mahalanobis_npc")

  expect_true("P5" %in% out$plate_id[out$is_outlier])
  expect_true(all(c("distance", "cutoff", "is_outlier") %in% colnames(out)))
})

test_that("detect_plate_pca_outliers() mahalanobis_npc honors an explicit n_pc smaller than available", {
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15, outlier_plate = "P5")
  pca <- fit_plate_pca(ref, n_pc = 5)
  out <- detect_plate_pca_outliers(pca, method = "mahalanobis_npc", n_pc = 2)

  expect_true(all(c("PC1", "PC2") %in% colnames(out)))
  expect_false("PC3" %in% colnames(out))
})

test_that("detect_plate_pca_outliers() mahalanobis_npc errors if n_pc exceeds available components", {
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15)
  pca <- fit_plate_pca(ref, n_pc = 3)
  expect_error(
    detect_plate_pca_outliers(pca, method = "mahalanobis_npc", n_pc = 10),
    class = "zfbehavior_error_invalid_n_pc"
  )
})

test_that("detect_plate_pca_outliers() errors on a non-zfbehavior_plate_pca_result input", {
  expect_error(
    detect_plate_pca_outliers(list(scores = tibble::tibble(plate_id = "P1"))),
    class = "zfbehavior_error_invalid_input"
  )
})

test_that("detect_plate_pca_outliers() ellipse methods error if fewer than 2 PCs are available", {
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15)
  pca1 <- fit_plate_pca(ref, n_pc = 1)
  expect_error(
    detect_plate_pca_outliers(pca1, method = "ellipse_normal"),
    class = "zfbehavior_error_invalid_n_pc"
  )
})

#' Even-odd (ray-casting) point-in-polygon test, base R only
#'
#' Used below to independently verify that `detect_plate_pca_outliers()`'s
#' `is_outlier` classification agrees with actual geometric containment in
#' the exact polygon `ggplot2::stat_ellipse()` draws (via
#' `ggplot2:::calculate_ellipse()`), rather than merely re-checking the
#' classifier's own formula against itself.
point_in_polygon <- function(x, y, poly_x, poly_y) {
  n <- length(poly_x)
  inside <- FALSE
  j <- n
  for (i in seq_len(n)) {
    if (
      ((poly_y[i] > y) != (poly_y[j] > y)) &&
        (x <
          (poly_x[j] - poly_x[i]) * (y - poly_y[i]) /
            (poly_y[j] - poly_y[i]) +
            poly_x[i])
    ) {
      inside <- !inside
    }
    j <- i
  }
  inside
}

test_that("detect_plate_pca_outliers() cutoff formula matches ggplot2::stat_ellipse()'s internal radius exactly", {
  # ggplot2:::calculate_ellipse() uses radius^2 = 2 * qf(level, 2, n - 1)
  # (dfn = 2) for BOTH type = "norm" and type = "t" -- the two ellipse
  # types differ only in which location/covariance estimator is used, not
  # in the reference distribution. detect_plate_pca_outliers()'s cutoff
  # (on the squared-Mahalanobis-distance scale) must match this exactly
  # for classification to agree with the rendered ellipse boundary.
  ref <- fake_reference_pattern(outlier_plate = "P5")
  pca <- fit_plate_pca(ref, n_pc = 2)
  n <- nrow(pca$scores)

  expected_cutoff <- 2 * stats::qf(0.95, 2, n - 1)

  out_norm <- detect_plate_pca_outliers(pca, method = "ellipse_normal")
  out_t <- detect_plate_pca_outliers(pca, method = "ellipse_t")

  expect_equal(unique(out_norm$cutoff), expected_cutoff)
  expect_equal(unique(out_t$cutoff), expected_cutoff)
})

test_that("detect_plate_pca_outliers() is_outlier agrees exactly with geometric containment in the ggplot2::stat_ellipse() polygon (near-boundary agreement)", {
  # This is the direct fix verification for the review's Finding 1: prior
  # to this fix, ellipse_normal used a different covariance estimator
  # (cov.wt(method = "ML")) and cutoff (qchisq) than ggplot2::stat_ellipse()
  # actually draws, so a plate could be classified as an outlier while
  # visibly INSIDE its own rendered ellipse (or vice versa). This test
  # independently recomputes the exact polygon ggplot2::stat_ellipse()
  # would draw (via the internal, unexported ggplot2:::calculate_ellipse(),
  # NOT detect_plate_pca_outliers()'s own formula) and checks every
  # plate's is_outlier status against ACTUAL point-in-polygon containment,
  # for both ellipse_normal and ellipse_t.
  ref <- fake_reference_pattern(outlier_plate = "P5")
  pca <- fit_plate_pca(ref, n_pc = 2)
  xy <- as.matrix(pca$scores[, c("PC1", "PC2")])

  out_norm <- detect_plate_pca_outliers(pca, method = "ellipse_normal")
  out_t <- detect_plate_pca_outliers(pca, method = "ellipse_t")

  ell_norm <- ggplot2:::calculate_ellipse(
    data.frame(x = xy[, 1], y = xy[, 2]),
    vars = c("x", "y"),
    type = "norm",
    level = 0.95,
    segments = 500
  )
  ell_t <- ggplot2:::calculate_ellipse(
    data.frame(x = xy[, 1], y = xy[, 2]),
    vars = c("x", "y"),
    type = "t",
    level = 0.95,
    segments = 500
  )

  for (i in seq_len(nrow(pca$scores))) {
    inside_norm <- point_in_polygon(
      xy[i, 1],
      xy[i, 2],
      ell_norm$x,
      ell_norm$y
    )
    expect_equal(
      inside_norm,
      !out_norm$is_outlier[i],
      info = paste0("ellipse_normal disagreement for ", pca$scores$plate_id[i])
    )

    inside_t <- point_in_polygon(xy[i, 1], xy[i, 2], ell_t$x, ell_t$y)
    expect_equal(
      inside_t,
      !out_t$is_outlier[i],
      info = paste0("ellipse_t disagreement for ", pca$scores$plate_id[i])
    )
  }
})

test_that("detect_plate_pca_outliers() reproduces the published real-data outlier (regression)", {
  # See data-raw/build_interplate_qc_fixture.R for full provenance and
  # citation. This is REAL published ground truth (Truong et al. 2026):
  # Lab-A had 37 plates after basic QC and exactly 1 was excluded by
  # inter-plate QC, confirmed by the user to be "B11_sqPlate23".
  fx <- zf_interplate_qc_fixture()
  expect_equal(length(unique(fx$plate_id)), 37)

  ref <- calculate_plate_reference_pattern(fx)
  pca <- fit_plate_pca(ref, n_pc = 2)

  out_norm <- detect_plate_pca_outliers(pca, method = "ellipse_normal")
  expect_equal(sum(out_norm$is_outlier), 1)
  expect_equal(out_norm$plate_id[out_norm$is_outlier], "B11_sqPlate23")

  out_t <- detect_plate_pca_outliers(pca, method = "ellipse_t")
  expect_equal(sum(out_t$is_outlier), 1)
  expect_equal(out_t$plate_id[out_t$is_outlier], "B11_sqPlate23")
})

test_that("detect_plate_pca_outliers() mahalanobis_npc does NOT reproduce the known real-data outlier (documented limitation)", {
  # This test intentionally locks in the CURRENT (unvalidated/limited)
  # behavior of the experimental mahalanobis_npc method, so a future change
  # to its formula/default n_pc is a deliberate, visible decision rather
  # than a silent behavior change. See ?detect_plate_pca_outliers and
  # data-raw/build_interplate_qc_fixture.R for the full explanation: this
  # method flags ZERO outliers on the one real ground-truth case available,
  # unlike ellipse_normal/ellipse_t (see test above).
  fx <- zf_interplate_qc_fixture()
  ref <- calculate_plate_reference_pattern(fx)
  pca20 <- fit_plate_pca(ref, n_pc = 20)

  out_mah <- detect_plate_pca_outliers(pca20, method = "mahalanobis_npc")
  expect_equal(sum(out_mah$is_outlier), 0)
})

# ---- plot_plate_pca_ellipse() ---------------------------------------------------

test_that("plot_plate_pca_ellipse() returns a ggplot with ellipse and point layers", {
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15, outlier_plate = "P5")
  pca <- fit_plate_pca(ref, n_pc = 2)
  outliers <- detect_plate_pca_outliers(pca, method = "ellipse_normal")

  p <- plot_plate_pca_ellipse(pca, outliers)
  expect_s3_class(p, "ggplot")

  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPoint" %in% geom_classes)
})

test_that("plot_plate_pca_ellipse() errors on missing outlier status for some plates", {
  ref <- fake_reference_pattern(n_plates = 5, n_time = 10)
  pca <- fit_plate_pca(ref, n_pc = 2)
  outliers <- tibble::tibble(plate_id = "P1", is_outlier = FALSE)

  expect_error(
    plot_plate_pca_ellipse(pca, outliers),
    class = "zfbehavior_error_missing_outlier_status"
  )
})

test_that("plot_plate_pca_ellipse() errors on a non-zfbehavior_plate_pca_result input", {
  expect_error(
    plot_plate_pca_ellipse(
      list(),
      tibble::tibble(plate_id = "P1", is_outlier = FALSE)
    ),
    class = "zfbehavior_error_invalid_input"
  )
})

test_that("plot_plate_pca_ellipse() smoke test on the real fixture", {
  fx <- zf_interplate_qc_fixture()
  ref <- calculate_plate_reference_pattern(fx)
  pca <- fit_plate_pca(ref, n_pc = 2)
  outliers <- detect_plate_pca_outliers(pca, method = "ellipse_normal")

  p <- plot_plate_pca_ellipse(pca, outliers, title = "Lab-A")
  expect_s3_class(p, "ggplot")
})

# ---- plot_plate_reference_overlay() ---------------------------------------------

test_that("plot_plate_reference_overlay() returns a ggplot with line layers", {
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15, outlier_plate = "P5")
  pca <- fit_plate_pca(ref, n_pc = 2)
  outliers <- detect_plate_pca_outliers(pca, method = "ellipse_normal")
  segments <- list(L1 = 1:5, D1 = 6:10, L2 = 11:15)

  p <- plot_plate_reference_overlay(ref, outliers, segments)
  expect_s3_class(p, "ggplot")

  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomLine" %in% geom_classes)
  expect_true("GeomRibbon" %in% geom_classes)
})

test_that("plot_plate_reference_overlay() draws the pooled ribbon LAST and fully opaque", {
  # The pooled median/IQR ribbon is an intentional occlusion MASK (see
  # roxygen docs and the original plot_dmso_overlaid_pub()):
  # it must be the LAST layer (so it paints over in-band line segments)
  # and fully opaque (alpha = 1), not merely a semi-transparent background
  # tint drawn underneath the lines. This test locks in both properties so
  # a future edit can't silently reintroduce the "ribbon drawn first /
  # translucent" bug fixed in this session.
  ref <- fake_reference_pattern(n_plates = 12, n_time = 15, outlier_plate = "P5")
  pca <- fit_plate_pca(ref, n_pc = 2)
  outliers <- detect_plate_pca_outliers(pca, method = "ellipse_normal")
  segments <- list(L1 = 1:5, D1 = 6:10, L2 = 11:15)

  p <- plot_plate_reference_overlay(ref, outliers, segments)

  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  ribbon_idx <- which(geom_classes == "GeomRibbon")
  line_idx <- which(geom_classes == "GeomLine")

  expect_length(ribbon_idx, 1)
  expect_true(all(ribbon_idx > line_idx)) # ribbon drawn after every line layer

  ribbon_layer <- p$layers[[ribbon_idx]]
  ribbon_alpha <- rlang::`%||%`(
    ribbon_layer$aes_params$alpha,
    ribbon_layer$geom_params$alpha
  )
  expect_equal(ribbon_alpha, 1)
})

test_that("plot_plate_reference_overlay() errors on missing reference_pattern columns", {
  outliers <- tibble::tibble(plate_id = "P1", is_outlier = FALSE)
  expect_error(
    plot_plate_reference_overlay(
      tibble::tibble(plate_id = "P1"),
      outliers,
      segments = list(L1 = 1:5)
    ),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("plot_plate_reference_overlay() errors on missing outlier status for some plates", {
  ref <- fake_reference_pattern(n_plates = 5, n_time = 10)
  outliers <- tibble::tibble(plate_id = "P1", is_outlier = FALSE)
  segments <- list(L1 = 1:5, D1 = 6:10)

  expect_error(
    plot_plate_reference_overlay(ref, outliers, segments),
    class = "zfbehavior_error_missing_outlier_status"
  )
})

test_that("plot_plate_reference_overlay() smoke test on the real fixture", {
  fx <- zf_interplate_qc_fixture()
  ref <- calculate_plate_reference_pattern(fx)
  pca <- fit_plate_pca(ref, n_pc = 2)
  outliers <- detect_plate_pca_outliers(pca, method = "ellipse_normal")
  segments <- list(L1 = 1:10, D1 = 11:20, L2 = 21:30, D2 = 31:50)

  p <- plot_plate_reference_overlay(
    ref,
    outliers,
    segments,
    response_label = "dist. moved (mm*min)"
  )
  expect_s3_class(p, "ggplot")
})
