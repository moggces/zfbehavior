#' Columns needed for behavioral endpoints
#'
#' The canonical columns required by [generate_behavior_endpoints()]. Part
#' of Module 3 (skill `behavior-endpoints`).
#'
#' @seealso [module3], [canonical-columns].
#' @return A character vector of required column names.
#' @export
#'
#' @examples
#' behavior_endpoint_input_cols()
behavior_endpoint_input_cols <- function() {
  c(
    "plate_id",
    "well_id",
    "is_vehicle_control",
    "time_value",
    "response_value",
    "keep_well"
  )
}

#' Check that no excluded (keep_well != TRUE) wells are present
#'
#' Wells with `keep_well = FALSE` (or `NA`) must be removed before endpoint
#' calculation and raw-response plots. Structural check only.
#'
#' @param d A tibble with a `keep_well` column.
#' @param fn_name Name of the calling function, for the error message.
#' @keywords internal
#' @noRd
check_no_excluded_wells <- function(d, fn_name) {
  n_excluded <- sum(is.na(d$keep_well) | !d$keep_well)
  if (n_excluded > 0) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires all rows to have keep_well == TRUE (dead/malformed wells must be excluded before endpoint calculation)."
        ),
        "x" = paste0(
          "Found ",
          n_excluded,
          " row(s) with keep_well == FALSE or NA."
        ),
        "i" = "Filter with dplyr::filter(.data$keep_well) before calling this function.",
        "i" = "Dead or malformed larvae (keep_well = FALSE) must not contribute to endpoints."
      ),
      class = "zfbehavior_error_excluded_wells_present"
    )
  }
  invisible(TRUE)
}

check_endpoint_input <- function(d, fn_name) {
  missing_cols <- setdiff(behavior_endpoint_input_cols(), colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires canonical columns that are missing."),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", ")),
        "i" = "Use read_behavior_data() with an explicit column_mapping, or rename columns to match canonical_behavior_roles().",
        "i" = "See behavior_endpoint_input_cols() or ?\"canonical-columns\" for the full required-column picture."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  check_no_excluded_wells(d, fn_name)
  if (anyNA(d$response_value)) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires `response_value` to contain no NA values."
        ),
        "x" = paste0(
          "Found ",
          sum(is.na(d$response_value)),
          " NA response_value entrie(s)."
        ),
        "i" = "Call replace_na_response_zero() first, or otherwise impute/exclude missing response values explicitly.",
        "i" = "See ?replace_na_response_zero for why zero-imputation (not na.rm) is required for correct AUC/similarity math."
      ),
      class = "zfbehavior_error_missing_response_values"
    )
  }
  invisible(TRUE)
}

check_segments <- function(segments) {
  if (
    !is.list(segments) || is.null(names(segments)) || any(names(segments) == "")
  ) {
    rlang::abort(
      c(
        "`segments` must be a named list.",
        "x" = "Every segment must have a name (e.g. list(L1 = 1:10, D1 = 11:20)).",
        "i" = "Names become the endpoint/phase labels; values are the time_value entries belonging to that segment."
      ),
      class = "zfbehavior_error_invalid_segments"
    )
  }
  if (any(duplicated(names(segments)))) {
    rlang::abort(
      c(
        "`segments` names must be unique.",
        "x" = paste0(
          "Duplicated: ",
          paste(
            unique(names(segments)[duplicated(names(segments))]),
            collapse = ", "
          )
        )
      ),
      class = "zfbehavior_error_invalid_segments"
    )
  }
  invisible(TRUE)
}

#' Assign a segment label to each row based on time_value
#'
#' @param d Data with a `time_value` column.
#' @param segments Named list of numeric vectors (segment name -> time_value
#'   entries belonging to that segment).
#' @return `d` filtered to rows whose `time_value` appears in some segment,
#'   with a new `segment` column giving the matching segment name. Rows
#'   whose `time_value` does not belong to any segment are dropped.
#' @keywords internal
#' @noRd
assign_segment <- function(d, segments) {
  lookup <- purrr::imap(
    segments,
    function(times, name) tibble::tibble(time_value = times, segment = name)
  )
  lookup <- dplyr::bind_rows(lookup)

  if (any(duplicated(lookup$time_value))) {
    dup_times <- unique(lookup$time_value[duplicated(lookup$time_value)])
    rlang::abort(
      c(
        "`segments` overlap: the same time_value appears in more than one segment.",
        "x" = paste0(
          "Overlapping time_value(s): ",
          paste(dup_times, collapse = ", ")
        )
      ),
      class = "zfbehavior_error_overlapping_segments"
    )
  }

  dplyr::inner_join(d, lookup, by = "time_value")
}

#' Abort if plate_id x well_id x segment x time_value is not unique
#'
#' Shared preflight for the three `calculate_*_endpoints()` functions,
#' called after `assign_segment()`; counts the raw rows.
#'
#' @param d1 A tibble already passed through `assign_segment()` (has a
#'   `segment` column).
#' @param fn_name Name of the calling function, for the error message.
#' @keywords internal
#' @noRd
check_no_duplicate_segment_rows <- function(d1, fn_name) {
  dup_check <- d1 |>
    dplyr::count(
      .data$plate_id,
      .data$well_id,
      .data$segment,
      .data$time_value
    ) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup_check) > 0) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires plate_id x well_id x segment x time_value to be unique; cannot compute endpoints unambiguously with duplicate rows."
        ),
        "x" = paste0(
          nrow(dup_check),
          " duplicated plate_id x well_id x segment x time_value combination(s) found."
        ),
        "i" = "See the attached `data` for the affected keys."
      ),
      class = "zfbehavior_error_duplicate_rows",
      data = dup_check
    )
  }
  invisible(TRUE)
}

cal_trapezoid_area <- function(time_value, response_value) {
  c(
    NA,
    (response_value[-1] + response_value[-length(time_value)]) *
      diff(time_value) /
      2
  )
}

#' Calculate Total Distance endpoints (area under the curve per segment)
#'
#' The published "Total Distance" endpoint: for each
#' plate x well x segment, the endpoint value is the trapezoidal area under
#' the raw response-value curve across the segment's timepoints. This is
#' the absolute quantity of movement in a segment (e.g. a light or dark
#' phase), with NO vehicle-control referencing -- unlike Distance
#' Change/Shift (see `calculate_distance_change_shift_endpoints()`).
#'
#' Truong et al. (2026); Hsieh et al. (2019).
#'
#' @param d A tibble with canonical columns: `plate_id`, `well_id`,
#'   `is_vehicle_control`, `time_value`, `response_value`. One row per
#'   plate x well x timepoint.
#' @param segments A named list mapping segment/phase labels to the
#'   `time_value` entries belonging to that segment, e.g.
#'   `list(L1 = 1:10, D1 = 11:20, L2 = 21:30, D2 = 31:50)`. Only rows whose
#'   `time_value` falls in one of these segments are used; segments must not
#'   overlap.
#'
#' @return A tibble with columns `plate_id`, `well_id`, `is_vehicle_control`,
#'   `endpoint` (the segment name), and `endpoint_value` (the AUC, in
#'   response_value-units * time_value-units).
#'
#' @keywords internal
#' @noRd
calculate_total_distance_endpoints <- function(d, segments) {
  check_endpoint_input(d, "calculate_total_distance_endpoints")
  check_segments(segments)

  d1 <- assign_segment(d, segments)
  check_no_duplicate_segment_rows(d1, "calculate_total_distance_endpoints")

  result <- d1 |>
    dplyr::group_by(
      .data$plate_id,
      .data$well_id,
      .data$is_vehicle_control,
      .data$segment
    ) |>
    dplyr::arrange(.data$time_value, .by_group = TRUE) |>
    dplyr::summarise(
      # na.rm = TRUE here only drops the single structural leading NA that
      # cal_trapezoid_area() always emits for the first timepoint in a
      # segment (there is no previous point to form a trapezoid with). It
      # does NOT mask missing response_value data: check_endpoint_input()
      # (via check_endpoint_input(), called above) already rejects any NA
      # in response_value, requiring callers to use
      # replace_na_response_zero() first.
      endpoint_value = sum(
        cal_trapezoid_area(.data$time_value, .data$response_value),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(endpoint = "segment")

  result
}

#' Calculate Distance Change and Distance Shift endpoints
#'
#' The "Distance Change" and "Distance Shift" endpoints (Truong et al. 2026,
#' Table 2; Konrad et al. 2026). For each plate and segment, a reference trace is the
#' per-timepoint median response among vehicle-control wells. For each well,
#' the area between that well's trace and the reference trace is computed:
#'
#' \itemize{
#'   \item \strong{both} (Distance Change): area of \code{|well - reference|}
#'     (always non-negative; magnitude only).
#'   \item \strong{up} (Distance Shift, hyperactivity component): area of
#'     \code{max(well - reference, 0)}.
#'   \item \strong{down} (Distance Shift, hypoactivity component): area of
#'     \code{max(reference - well, 0)}.
#' }
#'
#' Distance Shift itself is \code{up - down} for a given segment (i.e.
#' signed magnitude); callers wanting the signed Distance Shift value should
#' compute \code{up_auc - down_auc} from the wide-form output, or use the
#' long-form endpoint names `<segment>@up_auc` / `<segment>@down_auc`
#' directly as Truong et al. (2026) define them (Distance Shift hyperactivity
#' = up, hypoactivity = down).
#'
#' @inheritParams calculate_total_distance_endpoints
#'
#' @return A tibble with columns `plate_id`, `well_id`, `is_vehicle_control`,
#'   `endpoint` (segment name), `up_auc`, `down_auc`, `both_auc`.
#'
#' @keywords internal
#' @noRd
calculate_distance_change_shift_endpoints <- function(d, segments) {
  check_endpoint_input(d, "calculate_distance_change_shift_endpoints")
  check_segments(segments)

  d1 <- assign_segment(d, segments)
  check_no_duplicate_segment_rows(d1, "calculate_distance_change_shift_endpoints")

  ref <- d1 |>
    dplyr::filter(.data$is_vehicle_control) |>
    dplyr::group_by(.data$plate_id, .data$time_value, .data$segment) |>
    dplyr::summarise(
      reference_value = stats::median(.data$response_value),
      .groups = "drop"
    )

  d2 <- d1 |>
    dplyr::left_join(ref, by = c("plate_id", "time_value", "segment"))

  if (anyNA(d2$reference_value)) {
    rlang::abort(
      "Could not compute a vehicle-control reference for some plate x segment x time_value combinations (no vehicle-control wells present at that timepoint).",
      class = "zfbehavior_error_missing_reference"
    )
  }

  result <- d2 |>
    dplyr::mutate(diff_value = .data$response_value - .data$reference_value) |>
    dplyr::group_by(
      .data$plate_id,
      .data$well_id,
      .data$is_vehicle_control,
      .data$segment
    ) |>
    dplyr::arrange(.data$time_value, .by_group = TRUE) |>
    dplyr::summarise(
      # na.rm = TRUE (x3 below) only drops the structural leading NA that
      # cal_trapezoid_area() emits for the first timepoint in each segment
      # (see comment in calculate_total_distance_endpoints()); it does not
      # mask missing response_value data, which check_endpoint_input()
      # already rejects.
      up_auc = sum(
        cal_trapezoid_area(.data$time_value, pmax(.data$diff_value, 0)),
        na.rm = TRUE
      ),
      down_auc = sum(
        cal_trapezoid_area(.data$time_value, pmax(-.data$diff_value, 0)),
        na.rm = TRUE
      ),
      both_auc = sum(
        cal_trapezoid_area(.data$time_value, abs(.data$diff_value)),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(endpoint = "segment")

  result
}

similarity_metric_matrix <- function(wide_mat, metric) {
  mat <- as.matrix(wide_mat)
  if (metric == "pearson") {
    result <- stats::cor(mat, method = "pearson", use = "pairwise.complete.obs")
  } else if (metric == "spearman") {
    result <- stats::cor(
      mat,
      method = "spearman",
      use = "pairwise.complete.obs"
    )
  } else if (metric == "euclidean") {
    result <- as.matrix(stats::dist(
      t(mat),
      diag = TRUE,
      upper = TRUE,
      method = "euclidean"
    ))
  } else if (metric == "cosine") {
    # Cosine similarity dot/(|x||y|) (not cosine distance). A zero-norm
    # well gives NaN, handled in cal_mean_similarity().
    y <- t(mat) %*% mat
    result <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  } else {
    rlang::abort(
      paste0("Unsupported similarity metric: ", metric),
      class = "zfbehavior_error_invalid_metric"
    )
  }
  as.matrix(result)
}

cal_mean_similarity <- function(vec) {
  out <- unlist(vec)

  # Undefined similarity (NA: a zero-variance or zero-norm trace, e.g. a
  # larva that did not move) counts as 0, as in the original method: a
  # motionless well has no similarity signal. (Setting it to 1 was tried
  # and reverted.) Negative similarity also counts as 0.
  out[is.na(out)] <- 0

  out[out < 0] <- 0
  mean(out)
}

#' Calculate Movement Similarity endpoints
#'
#' The "Movement Similarity" endpoint (Hsieh et al. 2019; Truong et al.
#' 2026, Table 2; Konrad et al. 2026). For a given (typically the full
#' assay) time segment, each well's
#' movement trace is compared to every vehicle-control well's trace on the
#' same plate, using the requested similarity metric. For a treated well,
#' the endpoint value is the mean similarity to all vehicle-control wells
#' (excluding itself). For a vehicle-control well, the endpoint value is the
#' mean similarity to all OTHER vehicle-control wells on the plate.
#' Negative similarity values (possible for pearson/spearman) are treated as
#' 0 before averaging, matching the published method.
#'
#' @section Zero-variance (constant/motionless) wells:
#' A well whose response trace has zero variance across the similarity
#' window (e.g. an all-zero/no-movement trace) makes Pearson/Spearman
#' correlation mathematically undefined (0/0). This package substitutes
#' undefined similarity with `0` (no similarity signal to credit), NOT an
#' error and NOT `1`: a motionless test well's correlation against a
#' normally-behaving vehicle-control well is genuinely undefined, not
#' maximal, so there is nothing to reward here (as in the original method;
#' see `cal_mean_similarity()`).
#'
#' @inheritParams calculate_total_distance_endpoints
#' @param segments A named list with typically ONE entry spanning the full
#'   set of timepoints to include (e.g.
#'   `list(LD = c(1:50))`), unlike the AUC/Distance-Change functions where
#'   multiple named segments are typical. Multiple entries are allowed if
#'   similarity across several distinct time windows is desired.
#' @param metric One of `"spearman"` (default, matches Truong et al. 2026),
#'   `"pearson"`, `"cosine"`, or `"euclidean"`.
#'
#' @return A tibble with columns `plate_id`, `well_id`, `is_vehicle_control`,
#'   `endpoint` (segment name), `endpoint_value` (mean similarity to
#'   vehicle-control wells).
#'
#' @keywords internal
#' @noRd
calculate_movement_similarity_endpoints <- function(
  d,
  segments,
  metric = c("spearman", "pearson", "cosine", "euclidean")
) {
  metric <- match.arg(metric)
  check_endpoint_input(d, "calculate_movement_similarity_endpoints")
  check_segments(segments)

  d1 <- assign_segment(d, segments)
  check_no_duplicate_segment_rows(d1, "calculate_movement_similarity_endpoints")

  segment_names <- names(segments)
  out <- purrr::map_df(segment_names, function(seg_name) {
    d_seg <- d1 |> dplyr::filter(.data$segment == seg_name)

    wide <- d_seg |>
      dplyr::arrange(.data$well_id, .data$time_value) |>
      dplyr::select("plate_id", "well_id", "time_value", "response_value") |>
      tidyr::pivot_wider(names_from = "well_id", values_from = "response_value")

    plates <- unique(wide$plate_id)

    per_plate <- purrr::map_df(plates, function(pid) {
      wide_p <- wide |>
        dplyr::filter(.data$plate_id == pid) |>
        dplyr::select(-"plate_id", -"time_value")
      well_ids <- colnames(wide_p)

      sim_mat <- similarity_metric_matrix(wide_p, metric = metric)

      vc_ids <- d_seg |>
        dplyr::filter(.data$plate_id == pid, .data$is_vehicle_control) |>
        dplyr::distinct(.data$well_id) |>
        dplyr::pull(.data$well_id)

      well_info <- d_seg |>
        dplyr::filter(.data$plate_id == pid) |>
        dplyr::distinct(.data$well_id, .data$is_vehicle_control)

      purrr::pmap_df(well_info, function(well_id, is_vehicle_control) {
        other_vc <- setdiff(vc_ids, well_id)
        sim_vals <- sim_mat[well_id, other_vc]
        tibble::tibble(
          plate_id = pid,
          well_id = well_id,
          is_vehicle_control = is_vehicle_control,
          endpoint = seg_name,
          endpoint_value = cal_mean_similarity(sim_vals)
        )
      })
    })

    per_plate
  })

  out
}
