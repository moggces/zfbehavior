#' Module 2: raw-response visualization and inter-plate VC QC
#'
#' Module 2 of the zfbehavior workflow looks at the raw movement traces and
#' compares plates through their vehicle-control (VC) larvae. It is run by
#' the skills `raw-response-plots` and `interplate-vc-qc`. It changes no
#' data: the QC skill outputs a list of plates to exclude, which later
#' skills drop.
#'
#' @section Raw-response plots:
#' [count_wells_by_dose()] counts kept and total wells per dose group on the
#' full data. After removing `keep_well = FALSE` wells,
#' [plot_raw_responses_pages()] draws, per plate, the median trace of each
#' dose group or the traces of single wells ([plot_raw_responses()] for one
#' plate, [summarize_raw_responses()] for the numbers).
#'
#' @section Inter-plate vehicle-control QC:
#' Following Truong et al. (2026, Section 2.6.2.3), plates whose VC movement
#' pattern is unusual compared with the other plates are flagged:
#' \enumerate{
#'   \item [calculate_plate_reference_pattern()]: per plate, the median VC
#'     response at each timepoint (the reference pattern).
#'   \item [fit_plate_pca()]: PCA of the plates' reference patterns.
#'   \item [detect_plate_pca_outliers()]: plates outside the 95% confidence
#'     ellipse of the first two components.
#'   \item [plot_plate_pca_ellipse()] and [plot_plate_reference_overlay()]
#'     show the result; a person decides which flagged plates to exclude.
#' }
#' The within-plate VC check is Module 3 (`intraplate-vc-qc`, see
#' [module3]).
#'
#' @references
#' Truong L, Hsieh J-H, Ellis L, et al. (2026). Zebrafish behavioral
#' assessment as a first-tier whole-organism NAM for developmental
#' neurotoxicity: a multi-laboratory evaluation. NeuroToxicology 116:103504.
#' \doi{10.1016/j.neuro.2026.103504}
#'
#' @seealso Skills `raw-response-plots` and `interplate-vc-qc`
#'   (`system.file("skills", package = "zfbehavior")`).
#' @name interplate-qc
#' @aliases module2
NULL

#' Columns needed for inter-plate QC
#'
#' The canonical columns required by [calculate_plate_reference_pattern()].
#' Part of Module 2 (inter-plate VC QC; skill `interplate-vc-qc`).
#'
#' @seealso [module2], [canonical-columns].
#' @return A character vector of required column names.
#' @export
#'
#' @examples
#' interplate_qc_input_cols()
interplate_qc_input_cols <- function() {
  c(
    "plate_id",
    "is_vehicle_control",
    "time_value",
    "response_value",
    "keep_well"
  )
}

check_interplate_qc_input <- function(d, fn_name) {
  missing_cols <- setdiff(interplate_qc_input_cols(), colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires canonical columns that are missing."),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", ")),
        "i" = "Use read_behavior_data() with an explicit column_mapping, or rename columns to match canonical_behavior_roles().",
        "i" = "See interplate_qc_input_cols() or ?\"canonical-columns\" for the full required-column picture."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  # keep_well = FALSE wells must be filtered out first.
  check_no_excluded_wells(d, fn_name)
  invisible(TRUE)
}

#' Vehicle-control reference pattern of each plate
#'
#' Computes, per plate, the median (or mean) response of the vehicle-control
#' wells at each timepoint: one reference curve per plate, the input of
#' [fit_plate_pca()]. Uses [summarize_raw_responses()]. Part of Module 2
#' (inter-plate VC QC; skill `interplate-vc-qc`).
#'
#' @param d A tibble with the columns of [interplate_qc_input_cols()], only
#'   `keep_well = TRUE` rows.
#' @param summary_type `"median"` (default, as published) or `"mean"`.
#' @param missing_value_policy `"impute_zero"` (default; gives the complete
#'   plate x time table [fit_plate_pca()] needs) or `"na_omit"`.
#'
#' @return A tibble with `plate_id`, `time_value`, `n`, `center` (the
#'   reference value), `lower`, `upper`.
#' @seealso [module2].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   plate_id = rep(c("P1", "P2"), each = 6),
#'   is_vehicle_control = TRUE,
#'   time_value = rep(rep(1:3, each = 2), 2),
#'   response_value = c(1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 7),
#'   keep_well = TRUE
#' )
#' calculate_plate_reference_pattern(d)
calculate_plate_reference_pattern <- function(
  d,
  summary_type = c("median", "mean"),
  missing_value_policy = c("impute_zero", "na_omit")
) {
  summary_type <- match.arg(summary_type)
  missing_value_policy <- match.arg(missing_value_policy)

  check_interplate_qc_input(d, "calculate_plate_reference_pattern")

  d_vc <- d |> dplyr::filter(.data$is_vehicle_control)

  summarize_raw_responses(
    d_vc,
    group_cols = "plate_id",
    summary_type = summary_type,
    missing_value_policy = missing_value_policy
  )
}

#' PCA of plate reference patterns
#'
#' Runs a principal component analysis ([stats::prcomp()]) on the plate x
#' timepoint table of reference patterns. Part of Module 2 (inter-plate VC
#' QC; skill `interplate-vc-qc`). The sign of a component is arbitrary, so
#' the plot may be mirrored compared with other software; distances and
#' outlier calls are not affected.
#'
#' @param reference_pattern Output of [calculate_plate_reference_pattern()]
#'   (`plate_id`, `time_value`, `center`), a value for every plate and
#'   timepoint.
#' @param n_pc Number of components kept. Default `2`.
#'
#' @return A `zfbehavior_plate_pca_result` list: `scores` (tibble,
#'   `plate_id`, `PC1`, ...), `percent_var` (variance explained per
#'   component), `n_pc`, and `prcomp` (the [stats::prcomp()] object).
#' @seealso [module2], [detect_plate_pca_outliers()].
#' @export
fit_plate_pca <- function(reference_pattern, n_pc = 2) {
  required_cols <- c("plate_id", "time_value", "center")
  missing_cols <- setdiff(required_cols, colnames(reference_pattern))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        "fit_plate_pca() requires columns that are missing.",
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", ")),
        "i" = "Use calculate_plate_reference_pattern() to produce a compatible input."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }

  wide <- reference_pattern |>
    dplyr::select("plate_id", "time_value", "center") |>
    tidyr::pivot_wider(names_from = "time_value", values_from = "center")

  na_counts <- rowSums(is.na(dplyr::select(wide, -"plate_id")))
  incomplete_plates <- wide$plate_id[na_counts > 0]
  if (length(incomplete_plates) > 0) {
    rlang::abort(
      c(
        "fit_plate_pca() requires a complete plate x time_value matrix (no missing values).",
        "x" = paste0(
          "Plate(s) with missing time_value(s): ",
          paste(incomplete_plates, collapse = ", ")
        ),
        "i" = "This usually means some plates are missing timepoints present in other plates, or missing_value_policy = \"na_omit\" was used in calculate_plate_reference_pattern().",
        "i" = "Use missing_value_policy = \"impute_zero\" (the default) in calculate_plate_reference_pattern(), or otherwise ensure every plate has every timepoint."
      ),
      class = "zfbehavior_error_incomplete_reference_pattern"
    )
  }

  mat <- wide |>
    tibble::column_to_rownames("plate_id") |>
    as.matrix()

  n_plates <- nrow(mat)
  n_time <- ncol(mat)
  max_pc <- min(n_plates - 1, n_time)
  if (n_pc > max_pc) {
    rlang::abort(
      c(
        "fit_plate_pca() cannot compute `n_pc` components from this input.",
        "x" = paste0(
          "Requested n_pc = ",
          n_pc,
          ", but only ",
          max_pc,
          " component(s) are available (",
          n_plates,
          " plates, ",
          n_time,
          " timepoints)."
        )
      ),
      class = "zfbehavior_error_invalid_n_pc"
    )
  }

  pca <- stats::prcomp(mat, center = TRUE, scale. = TRUE)
  percent_var <- (pca$sdev^2 / sum(pca$sdev^2)) * 100
  percent_var <- percent_var[seq_len(n_pc)]
  names(percent_var) <- paste0("PC", seq_len(n_pc))

  scores <- pca$x[, seq_len(n_pc), drop = FALSE] |>
    as.data.frame() |>
    tibble::rownames_to_column("plate_id") |>
    tibble::as_tibble()

  result <- list(
    scores = scores,
    percent_var = percent_var,
    n_pc = n_pc,
    prcomp = pca
  )
  class(result) <- "zfbehavior_plate_pca_result"
  result
}

#' Center, covariance, and cutoff for a confidence ellipse, matching
#' `ggplot2::stat_ellipse()` exactly
#'
#' Shared by `detect_plate_pca_outliers()` (classification) and
#' `plot_plate_pca_ellipse()` (rendering, via `ggplot2::stat_ellipse()`) so
#' a plate's outlier status and its position relative to the drawn ellipse
#' are always geometrically consistent -- fixes a prior mismatch where
#' classification used a different covariance estimator (`cov.wt(method =
#' "ML")`) and a different reference distribution (`qchisq()`) than the
#' ellipse actually drawn by `ggplot2::stat_ellipse()`.
#'
#' Reproduces `ggplot2:::calculate_ellipse()`'s internal math exactly:
#' \itemize{
#'   \item `type = "norm"`: `stats::cov.wt(xy)` (default `method =
#'     "unbiased"` -- NOT `"ML"`).
#'   \item `type = "t"`: `MASS::cov.trob(xy)` (robust location/covariance).
#'   \item Both types use the SAME cutoff formula on the squared-
#'     Mahalanobis-distance scale: `2 * qf(conf_level, 2, n - 1)`.
#'     `ggplot2::stat_ellipse()` uses this identical F-distribution radius
#'     for both ellipse types; they differ only in which location/
#'     covariance estimator is used, not in the reference distribution.
#' }
#'
#' @param xy A numeric matrix with exactly 2 columns (e.g. PC1, PC2).
#' @param type One of `"norm"` or `"t"`.
#' @param conf_level Confidence level.
#' @return A list with `center` (named numeric length 2), `cov` (2x2
#'   matrix), and `cutoff` (numeric scalar, on the squared-Mahalanobis-
#'   distance scale -- compare directly against `stats::mahalanobis()`
#'   output, which is already squared).
#' @keywords internal
#' @noRd
ellipse_center_cov_cutoff <- function(xy, type = c("norm", "t"), conf_level) {
  type <- match.arg(type)
  n <- nrow(xy)
  if (type == "norm") {
    fit <- stats::cov.wt(xy)
  } else {
    fit <- MASS::cov.trob(xy)
  }
  list(
    center = fit$center,
    cov = fit$cov,
    cutoff = 2 * stats::qf(conf_level, 2, n - 1)
  )
}

#' Flag plates outside the PCA confidence ellipse
#'
#' Flags plates whose (PC1, PC2) point lies outside the `conf_level`
#' confidence ellipse of all plates. The region is the same as the one drawn
#' by [plot_plate_pca_ellipse()] (`ggplot2::stat_ellipse()`). Part of Module
#' 2 (inter-plate VC QC; skill `interplate-vc-qc`); a person then decides
#' which flagged plates to exclude.
#'
#' @param pca_result Output of [fit_plate_pca()].
#' @param method
#'   \itemize{
#'     \item `"ellipse_normal"` (default): bivariate normal ellipse, as
#'       published.
#'     \item `"ellipse_t"`: robust (multivariate t) ellipse.
#'     \item `"mahalanobis_npc"`: experimental; Mahalanobis distance over
#'       `n_pc` components with a chi-squared cutoff. Not validated.
#'   }
#' @param conf_level Confidence level. Default `0.95`.
#' @param n_pc For `"mahalanobis_npc"`: components to use. Default `NULL`
#'   (all in `pca_result`).
#'
#' @return A tibble with `plate_id`, the components used, `distance`
#'   (squared Mahalanobis distance), `cutoff` and `is_outlier`.
#' @seealso [module2], [plot_plate_pca_ellipse()].
#'
#' @examples
#' # 20 plates, 4 VC wells each; plate P20 moves much more in the dark
#' set.seed(2)
#' plates <- sprintf("P%02d", 1:20)
#' d <- expand.grid(
#'   plate_id = plates, well_id = 1:4, time_value = 1:20,
#'   stringsAsFactors = FALSE
#' )
#' d$is_vehicle_control <- TRUE
#' d$keep_well <- TRUE
#' level <- stats::rnorm(20, 10, 2)[match(d$plate_id, plates)]
#' dark <- ifelse(d$plate_id == "P20", 20, 5) * (d$time_value > 10)
#' d$response_value <- stats::rnorm(nrow(d), level + dark)
#' ref <- calculate_plate_reference_pattern(d)
#' pca <- fit_plate_pca(ref)
#' out <- detect_plate_pca_outliers(pca)
#' out[out$is_outlier, ]
#' plot_plate_pca_ellipse(pca, out)
#' plot_plate_reference_overlay(ref, out, segments = list(L1 = 1:10, D1 = 11:20))
#' @export
detect_plate_pca_outliers <- function(
  pca_result,
  method = c("ellipse_normal", "ellipse_t", "mahalanobis_npc"),
  conf_level = 0.95,
  n_pc = NULL
) {
  method <- match.arg(method)
  if (!inherits(pca_result, "zfbehavior_plate_pca_result")) {
    rlang::abort(
      "`pca_result` must be a zfbehavior_plate_pca_result object, from fit_plate_pca().",
      class = "zfbehavior_error_invalid_input"
    )
  }

  if (method %in% c("ellipse_normal", "ellipse_t")) {
    if (!all(c("PC1", "PC2") %in% colnames(pca_result$scores))) {
      rlang::abort(
        c(
          paste0(
            "detect_plate_pca_outliers(method = \"",
            method,
            "\") requires at least 2 principal components."
          ),
          "i" = "Re-run fit_plate_pca() with n_pc >= 2."
        ),
        class = "zfbehavior_error_invalid_n_pc"
      )
    }

    xy <- as.matrix(pca_result$scores[, c("PC1", "PC2")])

    ellipse_type <- if (method == "ellipse_normal") "norm" else "t"
    fit <- ellipse_center_cov_cutoff(xy, type = ellipse_type, conf_level = conf_level)
    distance <- stats::mahalanobis(xy, fit$center, fit$cov)
    cutoff <- fit$cutoff

    result <- pca_result$scores |>
      dplyr::select("plate_id", "PC1", "PC2") |>
      dplyr::mutate(
        distance = distance,
        cutoff = cutoff,
        is_outlier = distance > cutoff
      )
  } else {
    pc_cols <- setdiff(colnames(pca_result$scores), "plate_id")
    if (is.null(n_pc)) {
      n_pc <- length(pc_cols)
    }
    if (n_pc > length(pc_cols)) {
      rlang::abort(
        c(
          "detect_plate_pca_outliers(method = \"mahalanobis_npc\") requested more PCs than are available.",
          "x" = paste0(
            "Requested n_pc = ",
            n_pc,
            ", but pca_result only has ",
            length(pc_cols),
            " component(s)."
          ),
          "i" = "Re-run fit_plate_pca() with a larger n_pc."
        ),
        class = "zfbehavior_error_invalid_n_pc"
      )
    }
    use_cols <- pc_cols[seq_len(n_pc)]

    mat <- as.matrix(pca_result$scores[, use_cols, drop = FALSE])
    center <- colMeans(mat)
    cov_mat <- stats::cov(mat)
    distance <- stats::mahalanobis(mat, center, cov_mat)
    cutoff <- stats::qchisq(conf_level, df = n_pc)

    result <- pca_result$scores |>
      dplyr::select("plate_id", dplyr::all_of(use_cols)) |>
      dplyr::mutate(
        distance = distance,
        cutoff = cutoff,
        is_outlier = distance > cutoff
      )
  }

  result
}

#' PCA plot of plates with confidence ellipses
#'
#' Plots PC1 against PC2 of the plates, with the normal (dashed) and t
#' (solid) confidence ellipses, and outlier plates in orange (as in Truong
#' et al. 2026, Fig. 3). Part of Module 2 (inter-plate VC QC; skill
#' `interplate-vc-qc`).
#'
#' @param pca_result Output of [fit_plate_pca()] with `n_pc >= 2`.
#' @param outliers Output of [detect_plate_pca_outliers()].
#' @param conf_level Ellipse confidence level; use the one of
#'   [detect_plate_pca_outliers()]. Default `0.95`.
#' @param title Optional title.
#'
#' @return A `ggplot` object.
#' @seealso [module2]; example in [detect_plate_pca_outliers()].
#' @export
plot_plate_pca_ellipse <- function(
  pca_result,
  outliers,
  conf_level = 0.95,
  title = NULL
) {
  if (!inherits(pca_result, "zfbehavior_plate_pca_result")) {
    rlang::abort(
      "`pca_result` must be a zfbehavior_plate_pca_result object, from fit_plate_pca().",
      class = "zfbehavior_error_invalid_input"
    )
  }
  if (!all(c("PC1", "PC2") %in% colnames(pca_result$scores))) {
    rlang::abort(
      c(
        "plot_plate_pca_ellipse() requires pca_result to have at least 2 principal components.",
        "i" = "Re-run fit_plate_pca() with n_pc >= 2."
      ),
      class = "zfbehavior_error_invalid_n_pc"
    )
  }
  required_cols <- c("plate_id", "is_outlier")
  missing_cols <- setdiff(required_cols, colnames(outliers))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        "plot_plate_pca_ellipse() requires `outliers` columns that are missing.",
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", ")),
        "i" = "Use detect_plate_pca_outliers() to produce a compatible input."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }

  plot_df <- pca_result$scores |>
    dplyr::select("plate_id", "PC1", "PC2") |>
    dplyr::left_join(
      outliers |> dplyr::select("plate_id", "is_outlier"),
      by = "plate_id"
    )

  if (anyNA(plot_df$is_outlier)) {
    rlang::abort(
      "`outliers` is missing an is_outlier value for some plate(s) present in `pca_result`.",
      class = "zfbehavior_error_missing_outlier_status"
    )
  }

  pv <- pca_result$percent_var
  x_lab <- paste0("PC1: ", round(pv[["PC1"]], 2), "%")
  y_lab <- paste0("PC2: ", round(pv[["PC2"]], 2), "%")

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$PC1, y = .data$PC2)
  )
  p <- p +
    ggplot2::stat_ellipse(
      type = "norm",
      linetype = 2,
      level = conf_level,
      color = "grey30"
    ) +
    ggplot2::stat_ellipse(
      type = "t",
      level = conf_level,
      color = "grey30"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$is_outlier),
      size = 3
    ) +
    ggplot2::scale_color_manual(
      "Outlier",
      values = c(`FALSE` = "black", `TRUE` = "orange")
    ) +
    ggplot2::scale_x_continuous(x_lab) +
    ggplot2::scale_y_continuous(y_lab) +
    ggplot2::theme_bw(base_size = 14)

  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  p
}

#' Overlay of plate reference patterns
#'
#' Draws each plate's reference pattern over time, outlier plates in orange,
#' with light phases shaded (as in Truong et al. 2026, Fig. 3). An opaque
#' grey band of the median and interquartile range over all plates is drawn
#' on top, so only the parts of a curve outside the usual range stay
#' visible. Part of Module 2 (inter-plate VC QC; skill `interplate-vc-qc`).
#'
#' @param reference_pattern Output of [calculate_plate_reference_pattern()].
#' @param outliers Output of [detect_plate_pca_outliers()].
#' @param segments,segment_style Phases and their shading, as in
#'   [plot_raw_responses()].
#' @param response_label Y-axis label. Default `"response value"`.
#'
#' @return A `ggplot` object.
#' @seealso [module2]; example in [detect_plate_pca_outliers()].
#' @export
plot_plate_reference_overlay <- function(
  reference_pattern,
  outliers,
  segments,
  segment_style = NULL,
  response_label = "response value"
) {
  required_cols <- c("plate_id", "time_value", "center")
  missing_cols <- setdiff(required_cols, colnames(reference_pattern))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        "plot_plate_reference_overlay() requires `reference_pattern` columns that are missing.",
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", ")),
        "i" = "Use calculate_plate_reference_pattern() to produce a compatible input."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  required_outlier_cols <- c("plate_id", "is_outlier")
  missing_outlier_cols <- setdiff(required_outlier_cols, colnames(outliers))
  if (length(missing_outlier_cols) > 0) {
    rlang::abort(
      c(
        "plot_plate_reference_overlay() requires `outliers` columns that are missing.",
        "x" = paste0("Missing: ", paste(missing_outlier_cols, collapse = ", ")),
        "i" = "Use detect_plate_pca_outliers() to produce a compatible input."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  check_segments(segments)

  d <- reference_pattern |>
    dplyr::left_join(
      outliers |> dplyr::select("plate_id", "is_outlier"),
      by = "plate_id"
    )

  if (anyNA(d$is_outlier)) {
    rlang::abort(
      "`outliers` is missing an is_outlier value for some plate(s) present in `reference_pattern`.",
      class = "zfbehavior_error_missing_outlier_status"
    )
  }

  # Draw non-outlier plates first, outlier plates last, so outlier lines
  # render on top and are not visually buried under the rest.
  d <- d |> dplyr::arrange(.data$is_outlier)

  pooled <- reference_pattern |>
    dplyr::group_by(.data$time_value) |>
    dplyr::summarise(
      lower = stats::quantile(
        .data$center,
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      upper = stats::quantile(
        .data$center,
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      .groups = "drop"
    )

  shading_df <- segments_to_shading_df(segments, segment_style)
  shading_layers <- purrr::pmap(
    shading_df,
    function(segment, xmin, xmax, fill, alpha) {
      ggplot2::annotate(
        "rect",
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = fill,
        alpha = alpha
      )
    }
  )

  p <- ggplot2::ggplot()
  p <- p + shading_layers
  p <- p +
    ggplot2::geom_line(
      data = d,
      mapping = ggplot2::aes(
        x = .data$time_value,
        y = .data$center,
        color = .data$is_outlier,
        group = .data$plate_id
      ),
      alpha = 0.8,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = d,
      mapping = ggplot2::aes(
        x = .data$time_value,
        y = .data$center,
        color = .data$is_outlier,
        group = .data$plate_id
      ),
      size = 0.8,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_color_manual(
      "Outlier",
      values = c(`FALSE` = "black", `TRUE` = "orange")
    ) +
    ggplot2::labs(x = "time", y = response_label) +
    ggplot2::theme_bw() +
    # Drawn LAST and fully OPAQUE (alpha = 1): this ribbon MASKS any
    # in-band line segment underneath it rather than merely tinting the
    # background. See the "pooled median/IQR ribbon is an opacity MASK"
    # section above -- do not move this layer earlier or reduce its alpha
    # without re-reading that note, as doing so silently defeats the
    # masking behavior (as happened in an earlier version of this
    # function).
    ggplot2::geom_ribbon(
      data = pooled,
      mapping = ggplot2::aes(
        x = .data$time_value,
        ymin = .data$lower,
        ymax = .data$upper
      ),
      fill = "grey70",
      color = NA,
      alpha = 1,
      inherit.aes = FALSE
    )

  p
}
