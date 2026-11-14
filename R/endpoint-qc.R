#' Flag outliers using a Tukey (IQR) fence (internal)
#'
#' A value is flagged if it falls outside
#' `[Q1 - degree*IQR, Q3 + degree*IQR]`.
#'
#' @param x A numeric vector.
#' @param degree Fence multiplier (default `3`, the "far outlier" fence;
#'   `1.5` is the standard Tukey fence).
#' @param na.rm Whether to ignore `NA` values when computing quantiles/IQR
#'   (default `FALSE`).
#'
#' @return A logical vector, same length as `x`; `TRUE` where `x` is an
#'   outlier.
#' @keywords internal
#' @noRd
detect_outliers_iqr <- function(x, degree = 3, na.rm = FALSE) {
  if (!is.numeric(x)) {
    rlang::abort(
      "`x` must be numeric.",
      class = "zfbehavior_error_invalid_input"
    )
  }
  q1 <- stats::quantile(x, 0.25, na.rm = na.rm, names = FALSE)
  q3 <- stats::quantile(x, 0.75, na.rm = na.rm, names = FALSE)
  iqr <- stats::IQR(x, na.rm = na.rm)
  x < (q1 - degree * iqr) | x > (q3 + degree * iqr)
}

check_endpoint_qc_cols <- function(d, required_cols, fn) {
  missing_cols <- setdiff(required_cols, colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        paste0(fn, "() requires columns that are missing."),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", "))
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  invisible(TRUE)
}

check_outlier_degree <- function(outlier_degree, fn) {
  if (!is.numeric(outlier_degree) || length(outlier_degree) != 1 ||
      is.na(outlier_degree) || outlier_degree <= 0) {
    rlang::abort(
      paste0(fn, "(): `outlier_degree` must be one positive number."),
      class = "zfbehavior_error_invalid_input"
    )
  }
  invisible(TRUE)
}

#' One vehicle-control baseline per plate x group (internal)
#' @noRd
behavior_vc_baselines <- function(d, group_cols, fn) {
  check_endpoint_qc_cols(
    d,
    c(group_cols, "plate_id", "is_vehicle_control", "denom"),
    fn
  )
  out <- d |>
    dplyr::filter(.data$is_vehicle_control) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(group_cols, "plate_id"))),
                    .data$denom)
  dup <- out |>
    dplyr::count(dplyr::across(dplyr::all_of(c(group_cols, "plate_id")))) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    rlang::abort(
      c(
        paste0(fn, "(): `denom` is not constant within plate x ",
               paste(group_cols, collapse = " x "), "."),
        "i" = "`denom` must be the plate's vehicle-control baseline (one value per plate and endpoint)."
      ),
      class = "zfbehavior_error_invalid_input"
    )
  }
  out
}

#' Flag plates whose vehicle-control baseline is unusual
#'
#' For each endpoint, compares the plates' vehicle-control medians
#' (`denom` of [generate_behavior_endpoints()]) with a Tukey fence,
#' `[Q1 - outlier_degree * IQR, Q3 + outlier_degree * IQR]`, and flags plates
#' outside it. On a flagged plate the VC larvae behaved unusually (e.g.
#' barely moved), which distorts every normalized value of that plate. Part
#' of Module 3 (intra-plate VC QC; skill `intraplate-vc-qc`); a person
#' decides which flagged plates to exclude.
#'
#' @param d Output of [generate_behavior_endpoints()] (`plate_id`,
#'   `is_vehicle_control`, `denom`, `group_cols`).
#' @param group_cols Columns defining one endpoint. Default
#'   `c("endpoint_type", "endpoint")`.
#' @param outlier_degree Fence multiplier. Default `3` (far outliers); `1.5`
#'   is the usual Tukey fence.
#' @param flagged_only Return only flagged rows. Default `FALSE`.
#'
#' @return A tibble, one row per plate x endpoint: `group_cols`, `plate_id`,
#'   `denom`, `q1`, `q3`, `lower_fence`, `upper_fence`, `direction`
#'   (`"low"`, `"high"`, `"none"`) and `is_outlier`.
#' @seealso [module3], [plot_behavior_vc_baseline()].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   endpoint_type = "total_distance",
#'   endpoint = "L1",
#'   plate_id = paste0("P", 1:6),
#'   is_vehicle_control = TRUE,
#'   denom = c(2.9, 3.0, 2.95, 3.05, 2.98, 1.0)
#' )
#' detect_behavior_vc_baseline_outliers(d, flagged_only = TRUE)
detect_behavior_vc_baseline_outliers <- function(
  d,
  group_cols = c("endpoint_type", "endpoint"),
  outlier_degree = 3,
  flagged_only = FALSE
) {
  fn <- "detect_behavior_vc_baseline_outliers"
  check_outlier_degree(outlier_degree, fn)
  base <- behavior_vc_baselines(d, group_cols, fn)

  out <- base |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      q1 = stats::quantile(.data$denom, 0.25, na.rm = TRUE, names = FALSE),
      q3 = stats::quantile(.data$denom, 0.75, na.rm = TRUE, names = FALSE),
      lower_fence = .data$q1 - outlier_degree * (.data$q3 - .data$q1),
      upper_fence = .data$q3 + outlier_degree * (.data$q3 - .data$q1)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      direction = dplyr::case_when(
        .data$denom < .data$lower_fence ~ "low",
        .data$denom > .data$upper_fence ~ "high",
        TRUE ~ "none"
      ),
      is_outlier = .data$direction != "none"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, "plate_id"))))

  if (flagged_only) {
    out <- dplyr::filter(out, .data$is_outlier)
  }
  tibble::as_tibble(out)
}

#' Summarize the vehicle-control values of behavioral endpoints
#'
#' Summary statistics, per endpoint, of the vehicle-control values. Part of
#' Module 3 (intra-plate VC QC; skill `intraplate-vc-qc`).
#' \describe{
#'   \item{`level = "plate"` (default)}{one value per plate, its VC median
#'     `denom`: how plate baselines vary (the values behind
#'     [detect_behavior_vc_baseline_outliers()]).}
#'   \item{`level = "well"`}{one value per VC well, `endpoint_value_norm`,
#'     pooled over plates: the larva-to-larva spread within plates (median
#'     about 0 by construction).}
#' }
#'
#' @param d Output of [generate_behavior_endpoints()].
#' @param level `"plate"` (default) or `"well"`.
#' @param group_cols Columns defining one endpoint. Default
#'   `c("endpoint_type", "endpoint")`.
#' @param outlier_degree Tukey-fence multiplier for the fences,
#'   `n_outliers` and `sd_excluding_outliers`. Default `3`.
#'
#' @return A tibble, one row per endpoint: `group_cols`, `level`, `n`,
#'   `mean`, `median`, `sd`, `mad`, `q1`, `q3`, `lower_fence`,
#'   `upper_fence`, `n_outliers`, `sd_excluding_outliers`, `min`, `max`.
#' @seealso [module3].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   endpoint_type = "total_distance",
#'   endpoint = "L1",
#'   plate_id = rep(paste0("P", 1:3), each = 2),
#'   is_vehicle_control = TRUE,
#'   denom = rep(c(2.9, 3.0, 3.1), each = 2),
#'   endpoint_value_norm = c(-5, 5, -3, 3, -1, 1)
#' )
#' summarize_behavior_vc_distribution(d)
#' summarize_behavior_vc_distribution(d, level = "well")
summarize_behavior_vc_distribution <- function(
  d,
  level = c("plate", "well"),
  group_cols = c("endpoint_type", "endpoint"),
  outlier_degree = 3
) {
  fn <- "summarize_behavior_vc_distribution"
  level <- rlang::arg_match(level)
  check_outlier_degree(outlier_degree, fn)

  if (level == "plate") {
    vals <- behavior_vc_baselines(d, group_cols, fn) |>
      dplyr::rename(.value = "denom")
  } else {
    check_endpoint_qc_cols(
      d,
      c(group_cols, "is_vehicle_control", "endpoint_value_norm"),
      fn
    )
    vals <- d |>
      dplyr::filter(.data$is_vehicle_control) |>
      dplyr::rename(.value = "endpoint_value_norm")
  }

  vals |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      level = level,
      n = dplyr::n(),
      mean = mean(.data$.value, na.rm = TRUE),
      median = stats::median(.data$.value, na.rm = TRUE),
      sd = stats::sd(.data$.value, na.rm = TRUE),
      mad = stats::mad(.data$.value, na.rm = TRUE),
      q1 = stats::quantile(.data$.value, 0.25, na.rm = TRUE, names = FALSE),
      q3 = stats::quantile(.data$.value, 0.75, na.rm = TRUE, names = FALSE),
      lower_fence = .data$q1 - outlier_degree * (.data$q3 - .data$q1),
      upper_fence = .data$q3 + outlier_degree * (.data$q3 - .data$q1),
      n_outliers = sum(
        detect_outliers_iqr(.data$.value, degree = outlier_degree, na.rm = TRUE),
        na.rm = TRUE
      ),
      sd_excluding_outliers = stats::sd(
        .data$.value[
          !detect_outliers_iqr(.data$.value, degree = outlier_degree, na.rm = TRUE)
        ],
        na.rm = TRUE
      ),
      min = min(.data$.value, na.rm = TRUE),
      max = max(.data$.value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}

#' Plot vehicle-control baselines across plates
#'
#' Boxplot per endpoint with one point per plate (its VC median, `denom`);
#' outlier plates of [detect_behavior_vc_baseline_outliers()] in red and
#' labelled. For hover text use `plotly::ggplotly(p, tooltip = "text")`.
#' Endpoints in one plot should share a scale (e.g. one `endpoint_type`).
#' Part of Module 3 (intra-plate VC QC; skill `intraplate-vc-qc`).
#'
#' @param outliers Output of [detect_behavior_vc_baseline_outliers()]
#'   (needs `endpoint`, `plate_id`, `denom`, `is_outlier`), optionally with
#'   an extra `label_col` column.
#' @param label_col Column used for outlier labels and hover text. Default
#'   `"plate_id"`; `NULL` for no labels.
#' @param x_col Column on the x axis. Default `"endpoint"`.
#' @param title Plot title. Default `NULL` (none).
#' @param y_label Y-axis label. Default `"Vehicle-control baseline (denom)"`.
#' @param jitter_alpha Transparency of non-outlier points. Default `0.4`.
#' @param seed Seed for the jitter, so the plot is reproducible. Default `1`.
#'
#' @return A `ggplot` object.
#' @seealso [module3].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   endpoint_type = "total_distance",
#'   endpoint = rep(c("L1", "D1"), each = 6),
#'   plate_id = rep(paste0("P", 1:6), 2),
#'   is_vehicle_control = TRUE,
#'   denom = c(2.9, 3.0, 2.95, 3.05, 2.98, 1.0, 3.1, 3.2, 3.15, 3.12, 3.18, 3.14)
#' )
#' plot_behavior_vc_baseline(detect_behavior_vc_baseline_outliers(d))
plot_behavior_vc_baseline <- function(
  outliers,
  label_col = "plate_id",
  x_col = "endpoint",
  title = NULL,
  y_label = "Vehicle-control baseline (denom)",
  jitter_alpha = 0.4,
  seed = 1
) {
  check_endpoint_qc_cols(
    outliers,
    c(x_col, "plate_id", "denom", "is_outlier", label_col),
    "plot_behavior_vc_baseline"
  )
  d <- outliers
  d[[x_col]] <- factor(d[[x_col]], levels = unique(d[[x_col]]))
  lab <- if (is.null(label_col)) d$plate_id else as.character(d[[label_col]])
  d$.label <- lab
  d$.text <- paste0(
    lab, "<br>", d[[x_col]], "<br>denom: ", signif(d$denom, 4),
    ifelse(d$is_outlier, "<br>OUTLIER", "")
  )
  set.seed(seed)
  d$.x <- as.numeric(d[[x_col]]) + stats::runif(nrow(d), -0.2, 0.2)
  normal <- d[!d$is_outlier, , drop = FALSE]
  flagged <- d[d$is_outlier, , drop = FALSE]

  # `text` (plotly hover) sits in the plot-level mapping: as a layer-level
  # aesthetic ggplot2 would warn "Ignoring unknown aesthetics".
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data[[x_col]], y = .data$denom, text = .data$.text)
  ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = .data[[x_col]], text = NULL),
      outlier.shape = NA
    ) +
    ggplot2::geom_point(
      data = normal,
      ggplot2::aes(x = .data$.x),
      size = 1.8, alpha = jitter_alpha
    ) +
    ggplot2::geom_point(
      data = flagged,
      ggplot2::aes(x = .data$.x),
      size = 2.4, colour = "red"
    ) +
    ggplot2::labs(x = NULL, y = y_label, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (!is.null(label_col) && nrow(flagged) > 0) {
    p <- p + ggplot2::geom_text(
      data = flagged,
      ggplot2::aes(x = .data$.x, label = .data$.label),
      colour = "red", size = 2.8, hjust = -0.15, vjust = 0.5
    )
  }
  p
}
