#' Module 3: endpoint calculation, normalization, and intra-plate VC QC
#'
#' Module 3 of the zfbehavior workflow turns the time series of each well
#' into endpoints, normalizes them to the plate's vehicle control (VC), and
#' checks each plate's VC larvae. It is run by the skills
#' `behavior-endpoints`, `intraplate-vc-qc` and `devtox-endpoints`.
#'
#' @section Behavioral endpoints:
#' [generate_behavior_endpoints()] computes, per well, for each phase
#' (segment, e.g. L1, D1, L2, D2):
#' \describe{
#'   \item{Total Distance}{area under the movement curve (AUC) in the phase.}
#'   \item{Distance Change and Distance Shift}{area between the well's curve
#'     and the plate's VC median curve: `both` (Distance Change, absolute
#'     difference), `up` and `down` (Distance Shift, movement above and below
#'     the VC curve).}
#'   \item{Movement Similarity}{over a whole-assay window, the mean
#'     correlation (default Spearman) of the well's curve with the VC wells
#'     of the plate; negative values count as 0, and a well without movement
#'     has similarity 0.}
#' }
#' Input: kept wells only, missing responses set to zero
#' ([replace_na_response_zero()]).
#'
#' @section Normalization and value columns:
#' Each endpoint is normalized per plate against `denom`, the median of the
#' plate's VC wells (on the transformed scale, if any). Defaults, as
#' published:
#' \itemize{
#'   \item Total Distance, `"log_diff"`: `(log10(x + 1) - denom) * 100`, with
#'     `denom` the VC median of `log10(x + 1)`.
#'   \item Distance Change/Shift, `"none"`: used as is (already relative to
#'     the VC curve).
#'   \item Movement Similarity, `"ratio"`: `x / denom * 100 - 100`.
#' }
#' `"log_ratio"`, `(log10(x) - denom) / denom * 100` with `denom` the VC
#' median of `log10(x)`, is an older alternative.
#' The output keeps three values per row: `endpoint_value_raw` (before
#' normalization; used by the statistics of Module 4),
#' `endpoint_value_transformed` (e.g. after log10 for `"log_diff"`) and
#' `endpoint_value_norm` (normalized; used by the BMC of Module 5).
#'
#' @section Intra-plate vehicle-control QC:
#' Following Truong et al. (2026, Section 2.6.2.2), plates whose VC larvae
#' behaved unusually are flagged; a person decides which to exclude:
#' \itemize{
#'   \item [detect_behavior_vc_baseline_outliers()]: per endpoint, plates
#'     whose VC median (`denom`) lies outside a Tukey fence over all plates
#'     (Q1 - 3 IQR, Q3 + 3 IQR by default).
#'   \item Plates whose VC wells move inconsistently: movement-similarity
#'     `denom` below a threshold (0.4 published), applied by the skill.
#'   \item [summarize_behavior_vc_distribution()] and
#'     [plot_behavior_vc_baseline()] describe and show the baselines.
#' }
#' [validate_behavior_endpoints()] checks the structure of the endpoint
#' table (columns, unique keys, complete endpoints per well).
#'
#' @section Developmental toxicity endpoint:
#' [calculate_devtox_endpoints()] counts, per dose group, the larvae (`N`,
#' wells present) and the affected larvae (`n_affected`, wells with
#' `keep_well = FALSE`: dead or malformed). This relies on the single-larva
#' well contract ([canonical-columns]); `N` may differ between dose groups.
#' [plot_devtox_dose_response_pages()] plots percent affected. BMCs are
#' Module 5 ([bmc]).
#'
#' @references
#' Truong L, Hsieh J-H, Ellis L, et al. (2026). Zebrafish behavioral
#' assessment as a first-tier whole-organism NAM for developmental
#' neurotoxicity: a multi-laboratory evaluation. NeuroToxicology 116:103504.
#' \doi{10.1016/j.neuro.2026.103504}
#'
#' Hsieh J-H, Ryan K, Sedykh A, et al. (2019). Application of benchmark
#' concentration (BMC) analysis on zebrafish data: a new perspective for
#' quantifying toxicity in alternative animal models. Toxicological Sciences
#' 167(1):92-104. \doi{10.1093/toxsci/kfy258}
#'
#' @seealso Skills `behavior-endpoints`, `intraplate-vc-qc` and
#'   `devtox-endpoints` (`system.file("skills", package = "zfbehavior")`).
#' @name generate-endpoints
#' @aliases module3 endpoint-qc devtox
NULL

#' The approved default endpoint-type -> normalization-method pairing
#'
#' \itemize{
#'   \item Total Distance -> `"log_diff"` (`normalize_log_diff_to_control_percent()`)
#'   \item Distance Change/Shift -> `"none"` (`normalize_none()`)
#'   \item Movement Similarity -> `"ratio"` (`normalize_ratio_to_control_percent()`)
#' }
#' Single source of the default pairing used by `generate_behavior_endpoints()`.
#'
#' @return A named list mapping endpoint type to default normalization
#'   method string.
#' @keywords internal
#' @noRd
zf_default_normalization_map <- function() {
  list(
    total_distance = "log_diff",
    distance_change_shift = "none",
    movement_similarity = "ratio"
  )
}

#' Resolve a normalization method string to its `normalize_*()` function
#' @keywords internal
#' @noRd
zf_normalization_fn <- function(method) {
  switch(
    method,
    "none" = normalize_none,
    "ratio" = normalize_ratio_to_control_percent,
    "log_diff" = normalize_log_diff_to_control_percent,
    "log_ratio" = normalize_log_ratio_to_control_percent,
    rlang::abort(
      c(
        paste0("Unsupported normalization method: \"", method, "\"."),
        "i" = 'Must be one of "none", "ratio", "log_diff", "log_ratio".'
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  )
}

#' Column names reserved by the output of `generate_behavior_endpoints()`
#'
#' Used to validate that a caller's `extra_id_cols` does not collide with a
#' structural column this function itself creates.
#'
#' @return A character vector.
#' @keywords internal
#' @noRd
zf_reserved_endpoint_output_cols <- function() {
  c(
    "plate_id",
    "well_id",
    "chemical_id",
    "concentration",
    "concentration_unit",
    "is_vehicle_control",
    "keep_well",
    "endpoint_type",
    "segment",
    "component",
    "metric",
    "normalization_method",
    "is_default_normalization",
    "endpoint",
    "endpoint_value_raw",
    "endpoint_value_transformed",
    "denom",
    "endpoint_value_norm"
  )
}

#' Extract well-level identifier columns, validating they are invariant
#'
#' Every column in `id_cols` must take exactly one distinct (non-missing)
#' value within each `plate_id` x `well_id` group -- i.e. it is a genuine
#' well-level design field, not something that (incorrectly) varies across
#' a well's own timepoint trajectory. Aborts (rather than silently using
#' the first observed value) if this does not hold, naming the affected
#' wells/columns.
#'
#' @param d A tibble with `plate_id`, `well_id`, and every column in
#'   `id_cols`.
#' @param id_cols Character vector of column names to extract.
#' @param fn_name Name of the calling function, for the error message.
#' @return A tibble with `plate_id`, `well_id`, and one row per distinct
#'   well, with `id_cols` attached.
#' @keywords internal
#' @noRd
extract_well_identifiers <- function(d, id_cols, fn_name) {
  invariants <- d |>
    dplyr::group_by(.data$plate_id, .data$well_id) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(id_cols),
        ~ dplyr::n_distinct(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(id_cols),
      names_to = "column",
      values_to = "n_distinct_values"
    ) |>
    dplyr::filter(.data$n_distinct_values > 1)

  if (nrow(invariants) > 0) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires identifier columns to be constant within each well's trajectory."
        ),
        "x" = paste0(
          nrow(invariants),
          " plate_id x well_id x column combination(s) have more than one distinct non-missing value."
        ),
        "i" = "See the attached `data` for the affected wells/columns."
      ),
      class = "zfbehavior_error_non_invariant_identifiers",
      data = invariants
    )
  }

  d |> dplyr::distinct(dplyr::across(dplyr::all_of(c("plate_id", "well_id", id_cols))))
}

#' Apply a normalization method to a long-form endpoint block, recording
#' raw/transformed/norm values in separate stable columns
#'
#' Keeps the value before normalization as `endpoint_value_raw` and the
#' normalizer's (possibly log-transformed) value as
#' `endpoint_value_transformed`, whatever the method.
#'
#' @param d_long A tibble with `plate_id`, `endpoint`, `is_vehicle_control`,
#'   `endpoint_value` (plus any other columns, which pass through
#'   unchanged).
#' @param method One of `"none"`, `"ratio"`, `"log_diff"`, `"log_ratio"`.
#' @return `d_long` with `endpoint_value` replaced by
#'   `endpoint_value_raw`/`endpoint_value_transformed`, plus `denom` and
#'   `endpoint_value_norm` (both added by the normalizer).
#' @keywords internal
#' @noRd
apply_normalization_block <- function(d_long, method) {
  fn <- zf_normalization_fn(method)
  raw_vals <- d_long$endpoint_value
  # undefined = "error" (this function's own default) is used for
  # log_ratio here rather than exposed as a generate_behavior_endpoints() argument:
  # log_ratio is never the approved default for any endpoint type, so a
  # caller choosing it via an explicit normalization override should get
  # the same safe, no-silent-repair default as calling
  # normalize_log_ratio_to_control_percent() directly.
  normed <- fn(d_long)
  normed$endpoint_value_raw <- raw_vals
  normed |> dplyr::rename(endpoint_value_transformed = "endpoint_value")
}

#' Calculate and normalize the behavioral endpoints
#'
#' Computes Total Distance, Distance Change/Shift (per phase) and Movement
#' Similarity (per window and metric) for every well, normalizes each to the
#' plate's vehicle control, and returns one long table. Part of Module 3
#' (endpoint calculation; skill `behavior-endpoints`); see [module3] for the
#' endpoint definitions and normalizations.
#'
#' @param d A tibble with the columns of [behavior_endpoint_input_cols()]
#'   plus `chemical_id`, `concentration` and optionally
#'   `concentration_unit`; only `keep_well = TRUE` rows, without missing
#'   responses ([replace_na_response_zero()]).
#' @param auc_segments Named list of phases and their `time_value`s for
#'   Total Distance and Distance Change/Shift, e.g.
#'   `list(L1 = 1:10, D1 = 11:20, L2 = 21:30, D2 = 31:50)`; must not overlap.
#' @param similarity_segments Named list of windows for Movement Similarity,
#'   usually the whole assay, e.g. `list(LD = 1:50)`.
#' @param similarity_metrics Similarity metrics: `"spearman"` (default),
#'   `"pearson"`, `"cosine"`, `"euclidean"`.
#' @param normalization Named list to change the normalization of an
#'   endpoint type (`total_distance`, `distance_change_shift`,
#'   `movement_similarity`) to `"none"`, `"ratio"`, `"log_diff"` or
#'   `"log_ratio"`. Default `list()` (the defaults in [module3]).
#' @param extra_id_cols Other well-level columns of `d` to keep (e.g. a
#'   blinded ID). Default none.
#'
#' @return A tibble, one row per well x endpoint:
#' \describe{
#'   \item{`plate_id`, `well_id`, `chemical_id`, `concentration`,
#'     `concentration_unit`, `keep_well`, `is_vehicle_control`,
#'     `extra_id_cols`}{identifiers from `d`.}
#'   \item{`endpoint_type`}{`"total_distance"`, `"distance_change_shift"` or
#'     `"movement_similarity"`.}
#'   \item{`segment`, `component`, `metric`}{phase or window; `up` / `down` /
#'     `both` for Distance Change/Shift; similarity metric.}
#'   \item{`endpoint`}{endpoint name, e.g. `"L1"`, `"D1@up_auc"`,
#'     `"LD_spearman"`.}
#'   \item{`normalization_method`, `is_default_normalization`}{method used,
#'     and whether it is the default.}
#'   \item{`endpoint_value_raw`, `endpoint_value_transformed`, `denom`,
#'     `endpoint_value_norm`}{value before normalization, after the
#'     transform, the plate's VC median, and the normalized value.}
#' }
#'
#' @seealso [module3], [validate_behavior_endpoints()],
#'   [plot_behavior_dose_response_pages()].
#'
#' @examples
#' set.seed(1)
#' d <- expand.grid(
#'   well_id = sprintf("A%02d", 1:8), time_value = 1:20,
#'   stringsAsFactors = FALSE
#' )
#' d$plate_id <- "P1"
#' d$chemical_id <- "C1"
#' d$is_vehicle_control <- d$well_id %in% sprintf("A%02d", 1:4)
#' d$concentration <- ifelse(d$is_vehicle_control, 0, 10)
#' d$keep_well <- TRUE
#' d$response_value <- stats::rpois(nrow(d), ifelse(d$time_value > 10, 20, 5))
#' e <- generate_behavior_endpoints(
#'   d,
#'   auc_segments = list(L1 = 1:10, D1 = 11:20),
#'   similarity_segments = list(LD = 1:20)
#' )
#' unique(e$endpoint)
#' head(e[, c("well_id", "endpoint", "endpoint_value_raw", "endpoint_value_norm")])
#' @export
generate_behavior_endpoints <- function(
  d,
  auc_segments,
  similarity_segments,
  similarity_metrics = "spearman",
  normalization = list(),
  extra_id_cols = character(0)
) {
  fn_name <- "generate_behavior_endpoints"

  default_norm <- zf_default_normalization_map()
  bad_norm_names <- setdiff(names(normalization), names(default_norm))
  if (length(bad_norm_names) > 0) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires `normalization` names to be a subset of the known endpoint types."
        ),
        "x" = paste0("Not a known endpoint type: ", paste(bad_norm_names, collapse = ", ")),
        "i" = paste0(
          "Known endpoint types: ",
          paste(names(default_norm), collapse = ", ")
        )
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  norm_choice <- utils::modifyList(default_norm, normalization)

  valid_methods <- c("none", "ratio", "log_diff", "log_ratio")
  bad_methods <- norm_choice[!vapply(norm_choice, `%in%`, logical(1), table = valid_methods)]
  if (length(bad_methods) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires every `normalization` value to be a supported method."),
        "x" = paste0(
          "Invalid: ",
          paste(paste0(names(bad_methods), " = \"", unlist(bad_methods), "\""), collapse = ", ")
        ),
        "i" = paste0("Must be one of ", paste(valid_methods, collapse = ", "), ".")
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }

  valid_metrics <- c("spearman", "pearson", "cosine", "euclidean")
  bad_metrics <- setdiff(similarity_metrics, valid_metrics)
  if (length(bad_metrics) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires every `similarity_metrics` value to be a supported metric."),
        "x" = paste0("Invalid: ", paste(bad_metrics, collapse = ", ")),
        "i" = paste0("Must be one of ", paste(valid_metrics, collapse = ", "), ".")
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }

  bad_extra_id_cols <- intersect(extra_id_cols, zf_reserved_endpoint_output_cols())
  if (length(bad_extra_id_cols) > 0) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires `extra_id_cols` to not collide with a reserved output column name."
        ),
        "x" = paste0("Reserved: ", paste(bad_extra_id_cols, collapse = ", "))
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }

  id_cols <- c("chemical_id", "concentration")
  has_unit <- "concentration_unit" %in% colnames(d)
  if (has_unit) {
    id_cols <- c(id_cols, "concentration_unit")
  }
  id_cols <- c(id_cols, "keep_well", extra_id_cols)

  missing_id_cols <- setdiff(id_cols, colnames(d))
  if (length(missing_id_cols) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires identifier columns that are missing from `d`."),
        "x" = paste0("Missing: ", paste(missing_id_cols, collapse = ", "))
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }

  well_ids <- extract_well_identifiers(d, id_cols, fn_name)

  # ---- Total Distance ----
  td_raw <- calculate_total_distance_endpoints(d, auc_segments)
  td_long <- td_raw |>
    dplyr::mutate(
      segment = .data$endpoint,
      component = NA_character_,
      metric = NA_character_
    )
  td_normed <- apply_normalization_block(td_long, norm_choice$total_distance)
  td_final <- td_normed |>
    dplyr::mutate(
      endpoint_type = "total_distance",
      normalization_method = norm_choice$total_distance,
      is_default_normalization = identical(norm_choice$total_distance, default_norm$total_distance)
    )

  # ---- Distance Change/Shift ----
  dcs_raw <- calculate_distance_change_shift_endpoints(d, auc_segments)
  dcs_long <- dcs_raw |>
    tidyr::pivot_longer(
      cols = c("up_auc", "down_auc", "both_auc"),
      names_to = "auc_type",
      values_to = "endpoint_value"
    ) |>
    dplyr::mutate(
      segment = .data$endpoint,
      component = sub("_auc$", "", .data$auc_type),
      metric = NA_character_,
      endpoint = paste0(.data$endpoint, "@", .data$auc_type)
    ) |>
    dplyr::select(-"auc_type")
  dcs_normed <- apply_normalization_block(dcs_long, norm_choice$distance_change_shift)
  dcs_final <- dcs_normed |>
    dplyr::mutate(
      endpoint_type = "distance_change_shift",
      normalization_method = norm_choice$distance_change_shift,
      is_default_normalization = identical(norm_choice$distance_change_shift, default_norm$distance_change_shift)
    )

  # ---- Movement Similarity ----
  sim_blocks <- purrr::map(similarity_metrics, function(m) {
    calculate_movement_similarity_endpoints(d, similarity_segments, metric = m) |>
      dplyr::mutate(
        segment = .data$endpoint,
        component = NA_character_,
        metric = m,
        endpoint = paste0(.data$endpoint, "_", m)
      )
  })
  sim_long <- dplyr::bind_rows(sim_blocks)
  sim_normed <- apply_normalization_block(sim_long, norm_choice$movement_similarity)
  sim_final <- sim_normed |>
    dplyr::mutate(
      endpoint_type = "movement_similarity",
      normalization_method = norm_choice$movement_similarity,
      is_default_normalization = identical(norm_choice$movement_similarity, default_norm$movement_similarity)
    )

  all_endpoints <- dplyr::bind_rows(td_final, dcs_final, sim_final)

  all_endpoints |>
    dplyr::left_join(well_ids, by = c("plate_id", "well_id")) |>
    dplyr::select(
      "plate_id",
      "well_id",
      dplyr::all_of(id_cols),
      "is_vehicle_control",
      "endpoint_type",
      "segment",
      "component",
      "metric",
      "normalization_method",
      "is_default_normalization",
      "endpoint",
      "endpoint_value_raw",
      "endpoint_value_transformed",
      "denom",
      "endpoint_value_norm"
    )
}
