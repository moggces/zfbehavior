#' Compute the vehicle-control denominator (median) per plate x endpoint
#'
#' @param d A tibble with `plate_id`, `endpoint`, `is_vehicle_control`,
#'   `endpoint_value`.
#' @return A tibble with `plate_id`, `endpoint`, `denom` (median VC value).
#' @keywords internal
#' @noRd
calculate_vc_denominator <- function(d) {
  d |>
    dplyr::filter(.data$is_vehicle_control) |>
    dplyr::group_by(.data$plate_id, .data$endpoint) |>
    dplyr::summarise(
      denom = stats::median(.data$endpoint_value),
      .groups = "drop"
    )
}

join_vc_denominator <- function(d) {
  denom <- calculate_vc_denominator(d)
  dplyr::left_join(d, denom, by = c("plate_id", "endpoint"))
}

#' Normalization: identity (no-op)
#'
#' The DEFAULT normalization for Distance Change and Distance Shift
#' endpoints (see `calculate_distance_change_shift_endpoints()`). These
#' endpoints are already expressed relative to the plate's vehicle-control
#' median by construction (the reference trace is subtracted before the
#' area is computed), so no further plate normalization is applied -- this
#' matches the published methodology (Truong et al. 2026), where Distance Change /
#' Distance Shift responses are "used directly without normalization".
#'
#' @param d A tibble with an `endpoint_value` column (typically the output
#'   of `calculate_distance_change_shift_endpoints()`, reshaped to long form
#'   with one row per plate x well x endpoint).
#'
#' @return `d` with an added `endpoint_value_norm` column equal to
#'   `endpoint_value`, and a `denom` column (VC median, included for
#'   diagnostic/QC purposes even though it is not applied).
#'
#' @keywords internal
#' @noRd
normalize_none <- function(d) {
  d1 <- join_vc_denominator(d)
  d1 |> dplyr::mutate(endpoint_value_norm = .data$endpoint_value)
}

#' Normalization: ratio-to-vehicle-control-median, percent scale
#'
#' The DEFAULT normalization for Movement Similarity endpoints (see
#' `calculate_movement_similarity_endpoints()`), following the published
#' formula (Hsieh et al. 2019; Truong et al. 2026):
#' \deqn{Response = (V_{chemical} / V_{vehicle\ control}) \times 100 - 100}
#' where \eqn{V_{vehicle\ control}} is the median endpoint value among
#' vehicle-control wells on the same plate. A value of 0 indicates no
#' difference from vehicle control; negative values indicate reduced
#' similarity (or, more generally, a reduced value relative to control).
#'
#' @inheritParams normalize_none
#' @return `d` with added `denom` (VC median) and `endpoint_value_norm`
#'   columns.
#' @keywords internal
#' @noRd
normalize_ratio_to_control_percent <- function(d) {
  d1 <- join_vc_denominator(d)
  d1 |>
    dplyr::mutate(
      endpoint_value_norm = (.data$endpoint_value / .data$denom) * 100 - 100
    )
}

#' Normalization: log10 difference from vehicle-control median, percent scale
#'
#' The DEFAULT normalization for Total Distance endpoints (see
#' `calculate_total_distance_endpoints()`), following the published
#' formula (Hsieh et al. 2019, "Application of Benchmark Concentration (BMC)
#' Analysis on Zebrafish Data"): first each AUC value is transformed with
#' \eqn{\log_{10}(AUC + 1)} (the +1 avoids \eqn{-\infty} for an AUC of 0),
#' then the plate-normalized response is:
#' \deqn{Response = (V_{chemical} - V_{vehicle\ control}) \times 100}
#' where both \eqn{V_{chemical}} and \eqn{V_{vehicle\ control}} (the
#' plate's vehicle-control median) are on the log10(AUC+1) scale. Positive
#' values indicate increased (hyperactive) total distance relative to
#' vehicle control; negative values indicate decreased (hypoactive) total
#' distance.
#'
#' @inheritParams normalize_none
#' @return `d` with added `endpoint_value` (overwritten with the
#'   log10(x + 1)-transformed value), `denom`, and `endpoint_value_norm`
#'   columns.
#' @keywords internal
#' @noRd
normalize_log_diff_to_control_percent <- function(d) {
  d1 <- d |> dplyr::mutate(endpoint_value = log10(.data$endpoint_value + 1))
  d2 <- join_vc_denominator(d1)
  d2 |>
    dplyr::mutate(
      endpoint_value_norm = (.data$endpoint_value - .data$denom) * 100
    )
}

#' Normalization: log10 ratio to vehicle-control median, percent scale
#'
#' An older alternative normalization. Endpoint values are first log10-transformed, then expressed
#' as a percent ratio to the plate's vehicle-control median:
#' \deqn{Response = \frac{\log_{10}(V_{chemical}) - \log_{10}(V_{vehicle\ control})}{\log_{10}(V_{vehicle\ control})} \times 100}
#' Not the default for any endpoint type.
#'
#' @section Zero/non-finite vehicle-control denominators (`undefined`):
#' The vehicle-control denominator here is `log10(V_{vehicle\ control})` --
#' zero exactly when the plate's vehicle-control median endpoint value is
#' `1` (a real, reachable case, not merely theoretical). A zero
#' denominator makes this stratum's ratio mathematically undefined for
#' EVERY row in that plate x endpoint stratum, not just treated wells:
#' treated rows with a nonzero numerator become `+Inf`/`-Inf`, and
#' vehicle-control rows themselves become `0/0 = NaN` (silently, since
#' `is.infinite(NaN)` is `FALSE`). `undefined` sets how this is handled:
#' \itemize{
#'   \item `"error"` (default): abort (class
#'     `zfbehavior_error_invalid_denominator`) if any plate x endpoint
#'     stratum has a zero, `NA`, or otherwise non-finite denominator,
#'     naming the affected stratum/strata. Per this package's convention
#'     of never silently repairing ambiguous scientific configuration,
#'     this is the default.
#'   \item `"na"`: explicitly replace `endpoint_value_norm` with `NA_real_`
#'     for every row (vehicle-control AND treated) in an affected stratum
#'     -- a visible, opt-in "cannot be computed" marker rather than a
#'     silent `0` or a hidden `NaN`.
#'   \item `"legacy_zero"`: `Inf`/`-Inf` results become `0`; `NaN` results
#'     (the control rows) are left as they are. Not recommended.
#' }
#'
#' @inheritParams normalize_none
#' @param undefined One of `"error"` (default), `"na"`, or `"legacy_zero"`;
#'   see `@section Zero/non-finite vehicle-control denominators` above.
#' @return `d` with added `endpoint_value` (log10-transformed), `denom`, and
#'   `endpoint_value_norm` columns.
#' @keywords internal
#' @noRd
normalize_log_ratio_to_control_percent <- function(
  d,
  undefined = c("error", "na", "legacy_zero")
) {
  undefined <- match.arg(undefined)
  d1 <- d |> dplyr::mutate(endpoint_value = log10(.data$endpoint_value))
  d2 <- join_vc_denominator(d1)

  # is.finite(NA) is FALSE, so a genuinely missing denominator (e.g. no
  # vehicle-control rows present at all for that plate x endpoint stratum
  # -- see join_vc_denominator()) is also caught here, not just an
  # explicit zero.
  bad_denom <- !is.finite(d2$denom) | d2$denom == 0

  if (undefined == "error" && any(bad_denom)) {
    affected <- d2[bad_denom, c("plate_id", "endpoint"), drop = FALSE] |>
      dplyr::distinct()
    rlang::abort(
      c(
        "normalize_log_ratio_to_control_percent() found a zero or non-finite vehicle-control denominator (on the log10 scale) for some plate x endpoint stratum/strata.",
        "x" = paste0(
          nrow(affected),
          " affected plate x endpoint stratum/strata."
        ),
        "i" = "This makes the ratio undefined for EVERY row in the affected stratum (including vehicle-control rows, which silently become NaN, not just treated rows, which become Inf).",
        "i" = 'Pass undefined = "na" to explicitly mark affected rows NA instead of erroring, or undefined = "legacy_zero" to reproduce this function\'s prior/legacy behavior (Inf -> 0, NaN left as-is) -- not recommended for new analyses.'
      ),
      class = "zfbehavior_error_invalid_denominator",
      data = affected
    )
  }

  result <- d2 |>
    dplyr::mutate(
      endpoint_value_norm = ((.data$endpoint_value - .data$denom) /
        .data$denom) *
        100
    )

  if (undefined == "na") {
    result$endpoint_value_norm[bad_denom] <- NA_real_
  } else if (undefined == "legacy_zero") {
    result$endpoint_value_norm <- ifelse(
      is.infinite(result$endpoint_value_norm),
      0,
      result$endpoint_value_norm
    )
  }

  result
}

#' Transform endpoint values for approximate normality before ANOVA/RMANOVA
#'
#' Separate from plate normalization (see `normalize_*()` functions above),
#' this applies a distributional transform recommended before parametric
#' statistical tests (ANOVA, RMANOVA, t-tests), following Konrad et al.
#' (2026, Section 2.5): Distance Change should be log10-transformed;
#' Movement Similarity (a correlation, bounded 0-1) should be
#' Fisher-Z-transformed; Total Distance and Distance Shift are already
#' approximately normal and require no transform. This function makes that
#' choice explicit and testable.
#'
#' @param x A numeric vector of endpoint values.
#' @param method One of `"log10"`, `"fisher_z"`, or `"none"`.
#'
#' @return A numeric vector, transformed as requested.
#' @keywords internal
#' @noRd
transform_for_normality <- function(
  x,
  method = c("log10", "fisher_z", "none")
) {
  method <- match.arg(method)
  if (!is.numeric(x)) {
    rlang::abort(
      "`x` must be numeric.",
      class = "zfbehavior_error_invalid_input"
    )
  }
  switch(
    method,
    "log10" = log10(x),
    "fisher_z" = 0.5 * log((1 + x) / (1 - x)),
    "none" = x
  )
}
