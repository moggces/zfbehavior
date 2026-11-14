#' Construct a `zfbehavior_validation_result`
#'
#' Single internal constructor for the `zfbehavior_validation_result` class,
#' used by every validator in this file, so the class is only ever built in
#' one place.
#'
#' @param checks A tibble with columns `code`, `status`
#'   (`"passed"`/`"failed"`/`"skipped"`/`"ambiguous"`), `severity`
#'   (`"none"`/`"warning"`/`"error"`), `message`, `n_affected`.
#' @param diagnostics A named list of diagnostic tibbles (duplicate keys,
#'   replicate counts, well invariants, etc.), one entry per check that
#'   has structured detail beyond a single summary row.
#' @param metadata A named list, at minimum `n_rows` and `parameters` (the
#'   validator's own call arguments, for provenance).
#' @keywords internal
#' @noRd
new_zfbehavior_validation_result <- function(
  checks,
  diagnostics = list(),
  metadata = list()
) {
  ok <- !any(checks$status %in% c("failed", "ambiguous"))
  structure(
    list(ok = ok, checks = checks, diagnostics = diagnostics, metadata = metadata),
    class = "zfbehavior_validation_result"
  )
}

#' Build one row of a `zfbehavior_validation_result$checks` tibble
#' @keywords internal
#' @noRd
zfbehavior_check_row <- function(code, status, message, n_affected = NA_integer_) {
  severity <- switch(
    status,
    "passed" = "none",
    "skipped" = "warning",
    "error"
  )
  tibble::tibble(
    code = code,
    status = status,
    severity = severity,
    message = message,
    n_affected = as.integer(n_affected)
  )
}

#' Check that columns contain no missing values
#'
#' Reports the columns of `required_filled_cols` that are missing from `d`
#' or contain `NA`. Part of Module 1 (consistency checks; skill
#' `dataset-import-qc`), used by `validate_behavior_data()`.
#'
#' @param d A tibble.
#' @param required_filled_cols Columns that must have no `NA`.
#'
#' @return A tibble of problems only, with `column`, `status` (`"has_na"` or
#'   `"column_missing"`) and `n_na`; zero rows if all columns are complete.
#'
#' @keywords internal
#' @noRd
check_filled_columns <- function(d, required_filled_cols) {
  empty_out <- tibble::tibble(
    column = character(0),
    status = character(0),
    n_na = integer(0)
  )
  if (length(required_filled_cols) == 0) {
    return(empty_out)
  }
  results <- lapply(required_filled_cols, function(col) {
    if (!col %in% colnames(d)) {
      tibble::tibble(column = col, status = "column_missing", n_na = NA_integer_)
    } else {
      n_na <- sum(is.na(d[[col]]))
      status <- if (n_na > 0) "has_na" else "ok"
      tibble::tibble(column = col, status = status, n_na = as.integer(n_na))
    }
  })
  out <- dplyr::bind_rows(results)
  out[out$status != "ok", , drop = FALSE]
}

#' Check that every plate has the expected number of rows
#'
#' Reports plates whose number of rows (wells x timepoints) differs from
#' `expected_n`, or, without `expected_n`, from the most common count. Part
#' of Module 1 (consistency checks; skill `dataset-import-qc`), used by
#' `validate_behavior_data()`.
#'
#' @param d A tibble.
#' @param plate_col Plate column. Default `"plate_id"`.
#' @param expected_n Optional expected rows per plate. Default `NULL`: the
#'   most common count; if two counts are equally common the function stops
#'   and asks for `expected_n`.
#'
#' @return A tibble of the deviating plates (`plate_col`, `n_rows`); zero
#'   rows if all match.
#'
#' @keywords internal
#' @noRd
check_plate_row_counts <- function(
  d,
  plate_col = "plate_id",
  expected_n = NULL
) {
  check_columns_exist(d, plate_col, "check_plate_row_counts", "plate_col")
  check_scalar_nonneg_count(expected_n, "expected_n", "check_plate_row_counts")

  counts <- d |>
    dplyr::count(.data[[plate_col]], name = "n_rows")

  resolved <- resolve_target_count(counts$n_rows, expected_n)
  if (isTRUE(resolved$ambiguous)) {
    rlang::abort(
      c(
        "check_plate_row_counts() cannot infer an expected row count: more than one row count is equally common.",
        "x" = paste0(
          "Tied candidate row count(s): ",
          paste(resolved$modes, collapse = ", ")
        ),
        "i" = "Supply expected_n explicitly to resolve this ambiguity."
      ),
      class = "zfbehavior_error_ambiguous_expectation"
    )
  }

  counts[counts$n_rows != resolved$target, , drop = FALSE]
}

#' Check one plate (or several) of canonical behavioral data
#'
#' Runs the per-plate consistency checks of Module 1 (skill
#' `dataset-import-qc`) on data from [read_behavior_data()]. It reports
#' problems and does not stop or change the data. Plate size, replicate
#' counts and rows per plate are not assumed: give the expected values, or
#' the checks only require them to be consistent within the data.
#'
#' @section Checks:
#' Each row of `checks` is one check (`code`):
#' \describe{
#'   \item{`required_columns_present`, `column_types`}{all required roles
#'     present with the right type (logical, numeric).}
#'   \item{`unique_plate_well_time`}{one row per plate x well x time.}
#'   \item{`well_level_invariants`}{chemical, vehicle-control flag,
#'     concentration, unit and `keep_well` constant within a well.}
#'   \item{`trajectory_completeness`}{every well has every timepoint of its
#'     plate.}
#'   \item{`vc_replicate_counts`, `dose_replicate_counts`}{vehicle-control
#'     wells per plate x timepoint, and wells per dose group x timepoint,
#'     equal the expected numbers (or are consistent).}
#'   \item{`keep_well_has_usable_rows`}{at least one row with
#'     `keep_well = TRUE`.}
#'   \item{`filled_columns`}{no `NA` in `required_filled_cols`
#'     (`check_filled_columns()`).}
#'   \item{`plate_row_counts`}{rows per plate (`check_plate_row_counts()`).}
#' }
#' `response_value` is not in the default `required_filled_cols`: missing
#' responses are expected (tracking gaps) and are set to zero later with
#' [replace_na_response_zero()].
#'
#' @param d A tibble with the canonical columns ([canonical-columns]).
#' @param expected_vc_wells Optional vehicle-control wells per plate. Default
#'   `NULL` (only consistency is checked).
#' @param expected_dose_wells Optional wells per dose group. Default `NULL`
#'   (only consistency within a plate is checked).
#' @param required_filled_cols Columns that must have no `NA`. Default: the
#'   required roles except `response_value`.
#' @param expected_row_count Optional rows per plate (wells x timepoints).
#'   Default `NULL` (the most common count).
#'
#' @return A `zfbehavior_validation_result`: a list with `ok` (`TRUE` if no
#'   check failed), `checks` (tibble: `code`, `status` passed / failed /
#'   skipped / ambiguous, `severity`, `message`, `n_affected`),
#'   `diagnostics` (named list of detail tibbles, e.g. `duplicate_keys`,
#'   `vc_replicate_counts`) and `metadata` (rows checked, arguments).
#'   Printing shows the checks that did not pass.
#'
#' @seealso [canonical-columns] (Module 1), [validate_combined_dataset()].
#'
#' @examples
#' d <- expand.grid(
#'   plate_id = "P1", well_id = c("A01", "A02", "B01", "B02"),
#'   time_value = 1:3, stringsAsFactors = FALSE
#' )
#' d$chemical_id <- "C1"
#' d$is_vehicle_control <- d$well_id %in% c("A01", "A02")
#' d$concentration <- ifelse(d$is_vehicle_control, 0, 10)
#' d$response_value <- c(1.2, 0.8, 2.1, 1.7)[match(d$well_id, unique(d$well_id))]
#' d$keep_well <- TRUE
#' validate_behavior_data(d, expected_vc_wells = 2, expected_row_count = 12)
#' @export
validate_behavior_data <- function(
  d,
  expected_vc_wells = NULL,
  expected_dose_wells = NULL,
  required_filled_cols = setdiff(required_canonical_roles(), "response_value"),
  expected_row_count = NULL
) {
  check_scalar_nonneg_count(expected_vc_wells, "expected_vc_wells", "validate_behavior_data")
  check_scalar_nonneg_count(expected_dose_wells, "expected_dose_wells", "validate_behavior_data")
  check_scalar_nonneg_count(expected_row_count, "expected_row_count", "validate_behavior_data")

  metadata <- list(
    n_rows = nrow(d),
    parameters = list(
      expected_vc_wells = expected_vc_wells,
      expected_dose_wells = expected_dose_wells,
      required_filled_cols = required_filled_cols,
      expected_row_count = expected_row_count
    )
  )

  checks <- list()
  diagnostics <- list()

  required_cols <- required_canonical_roles()
  missing_cols <- setdiff(required_cols, colnames(d))
  if (length(missing_cols) > 0) {
    checks[["required_columns_present"]] <- zfbehavior_check_row(
      "required_columns_present",
      "failed",
      paste0(
        "Missing required canonical column(s): ",
        paste(missing_cols, collapse = ", ")
      ),
      length(missing_cols)
    )
    skip_msg <- "Skipped: required canonical column(s) missing."
    for (code in c(
      "column_types",
      "unique_plate_well_time",
      "well_level_invariants",
      "trajectory_completeness",
      "vc_replicate_counts",
      "dose_replicate_counts",
      "keep_well_has_usable_rows",
      "filled_columns",
      "plate_row_counts"
    )) {
      checks[[code]] <- zfbehavior_check_row(code, "skipped", skip_msg)
    }
    return(new_zfbehavior_validation_result(
      dplyr::bind_rows(checks),
      diagnostics,
      metadata
    ))
  }
  checks[["required_columns_present"]] <- zfbehavior_check_row(
    "required_columns_present",
    "passed",
    "All required canonical columns are present.",
    0L
  )

  type_checks <- list(
    is_vehicle_control = "logical",
    concentration = "numeric",
    time_value = "numeric",
    response_value = "numeric",
    keep_well = "logical"
  )
  bad_type_cols <- character(0)
  type_msgs <- character(0)
  for (col in names(type_checks)) {
    expected_type <- type_checks[[col]]
    actual <- d[[col]]
    ok_type <- switch(
      expected_type,
      "logical" = is.logical(actual),
      "numeric" = is.numeric(actual),
      TRUE
    )
    if (!ok_type) {
      bad_type_cols <- c(bad_type_cols, col)
      type_msgs <- c(
        type_msgs,
        paste0(
          "'",
          col,
          "' should be ",
          expected_type,
          " but is ",
          paste(class(actual), collapse = "/"),
          "."
        )
      )
    }
  }
  if (length(bad_type_cols) > 0) {
    checks[["column_types"]] <- zfbehavior_check_row(
      "column_types",
      "failed",
      paste(type_msgs, collapse = " "),
      length(bad_type_cols)
    )
  } else {
    checks[["column_types"]] <- zfbehavior_check_row(
      "column_types",
      "passed",
      "All type-checked columns have the expected type.",
      0L
    )
  }

  vc_type_ok <- !"is_vehicle_control" %in% bad_type_cols
  time_type_ok <- !"time_value" %in% bad_type_cols
  keep_well_type_ok <- !"keep_well" %in% bad_type_cols

  # plate x well x time uniqueness (independent of the type checks above)
  dup_check <- d |>
    dplyr::count(.data$plate_id, .data$well_id, .data$time_value) |>
    dplyr::filter(.data$n > 1)
  diagnostics$duplicate_keys <- dup_check
  if (nrow(dup_check) > 0) {
    checks[["unique_plate_well_time"]] <- zfbehavior_check_row(
      "unique_plate_well_time",
      "failed",
      paste0(
        nrow(dup_check),
        " plate_id x well_id x time_value combination(s) are not unique ",
        "(duplicate rows found)."
      ),
      nrow(dup_check)
    )
  } else {
    checks[["unique_plate_well_time"]] <- zfbehavior_check_row(
      "unique_plate_well_time",
      "passed",
      "plate_id x well_id x time_value is unique.",
      0L
    )
  }

  # well-level invariants: chemical_id/is_vehicle_control/concentration/
  # concentration_unit/keep_well must not change within a well's trajectory
  invariant_cols <- intersect(
    c(
      "chemical_id",
      "is_vehicle_control",
      "concentration",
      "concentration_unit",
      "keep_well"
    ),
    colnames(d)
  )
  well_invariants <- d |>
    dplyr::group_by(.data$plate_id, .data$well_id) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(invariant_cols),
        ~ dplyr::n_distinct(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(invariant_cols),
      names_to = "column",
      values_to = "n_distinct_values"
    ) |>
    dplyr::filter(.data$n_distinct_values > 1)
  diagnostics$well_invariants <- well_invariants
  if (nrow(well_invariants) > 0) {
    checks[["well_level_invariants"]] <- zfbehavior_check_row(
      "well_level_invariants",
      "failed",
      paste0(
        nrow(well_invariants),
        " plate_id x well_id x column combination(s) have more than one ",
        "distinct non-missing value across that well's trajectory ",
        "(a well's design fields must not change over time)."
      ),
      nrow(well_invariants)
    )
  } else {
    checks[["well_level_invariants"]] <- zfbehavior_check_row(
      "well_level_invariants",
      "passed",
      "Well-level design fields are constant within every well's trajectory.",
      0L
    )
  }

  # trajectory completeness: every well should have a row at every
  # timepoint present for its plate
  plate_time <- dplyr::distinct(d, .data$plate_id, .data$time_value)
  well_time <- dplyr::distinct(d, .data$plate_id, .data$well_id, .data$time_value)
  expected_time_counts <- plate_time |>
    dplyr::count(.data$plate_id, name = "n_expected_time")
  actual_time_counts <- well_time |>
    dplyr::count(.data$plate_id, .data$well_id, name = "n_actual_time")
  trajectory_completeness <- actual_time_counts |>
    dplyr::left_join(expected_time_counts, by = "plate_id") |>
    dplyr::filter(.data$n_actual_time != .data$n_expected_time)
  diagnostics$trajectory_completeness <- trajectory_completeness
  if (nrow(trajectory_completeness) > 0) {
    checks[["trajectory_completeness"]] <- zfbehavior_check_row(
      "trajectory_completeness",
      "failed",
      paste0(
        nrow(trajectory_completeness),
        " well(s) do not have a row at every timepoint present for their plate."
      ),
      nrow(trajectory_completeness)
    )
  } else {
    checks[["trajectory_completeness"]] <- zfbehavior_check_row(
      "trajectory_completeness",
      "passed",
      "Every well has a row at every timepoint present for its plate.",
      0L
    )
  }

  # vehicle-control / dosed-well replicate counts, PER plate x time_value
  if (!vc_type_ok || !time_type_ok) {
    skip_msg <- "Skipped: is_vehicle_control and/or time_value has an unexpected type."
    checks[["vc_replicate_counts"]] <- zfbehavior_check_row("vc_replicate_counts", "skipped", skip_msg)
    checks[["dose_replicate_counts"]] <- zfbehavior_check_row("dose_replicate_counts", "skipped", skip_msg)
  } else {
    plate_time_grid <- dplyr::distinct(d, .data$plate_id, .data$time_value)

    vc_counts <- d |>
      dplyr::filter(.data$is_vehicle_control) |>
      dplyr::distinct(.data$plate_id, .data$well_id, .data$time_value) |>
      dplyr::count(.data$plate_id, .data$time_value, name = "n_vc_wells")

    vc_counts_complete <- plate_time_grid |>
      dplyr::left_join(vc_counts, by = c("plate_id", "time_value")) |>
      dplyr::mutate(n_vc_wells = dplyr::coalesce(.data$n_vc_wells, 0L))
    diagnostics$vc_replicate_counts <- vc_counts_complete

    if (nrow(vc_counts) == 0) {
      checks[["vc_replicate_counts"]] <- zfbehavior_check_row(
        "vc_replicate_counts",
        "failed",
        "No vehicle-control (is_vehicle_control == TRUE) wells found.",
        0L
      )
    } else if (!is.null(expected_vc_wells)) {
      wrong_strata <- vc_counts_complete |>
        dplyr::filter(.data$n_vc_wells != expected_vc_wells)
      if (nrow(wrong_strata) > 0) {
        checks[["vc_replicate_counts"]] <- zfbehavior_check_row(
          "vc_replicate_counts",
          "failed",
          paste0(
            nrow(wrong_strata),
            " plate x timepoint stratum/strata do not have the expected ",
            expected_vc_wells,
            " vehicle-control wells."
          ),
          nrow(wrong_strata)
        )
      } else {
        checks[["vc_replicate_counts"]] <- zfbehavior_check_row(
          "vc_replicate_counts",
          "passed",
          paste0("Every plate x timepoint has exactly ", expected_vc_wells, " vehicle-control wells."),
          0L
        )
      }
    } else {
      unique_counts <- unique(vc_counts_complete$n_vc_wells)
      if (length(unique_counts) > 1) {
        checks[["vc_replicate_counts"]] <- zfbehavior_check_row(
          "vc_replicate_counts",
          "failed",
          paste0(
            "Vehicle-control well count is inconsistent across plate x timepoint strata (values: ",
            paste(sort(unique_counts), collapse = ", "),
            "). Consider passing expected_vc_wells to enforce a specific count."
          ),
          length(unique_counts)
        )
      } else {
        checks[["vc_replicate_counts"]] <- zfbehavior_check_row(
          "vc_replicate_counts",
          "passed",
          "Vehicle-control well count is consistent across all plate x timepoint strata.",
          0L
        )
      }
    }

    dose_counts_raw <- d |>
      dplyr::filter(!.data$is_vehicle_control) |>
      dplyr::distinct(
        .data$plate_id,
        .data$chemical_id,
        .data$concentration,
        .data$well_id,
        .data$time_value
      ) |>
      dplyr::count(
        .data$plate_id,
        .data$chemical_id,
        .data$concentration,
        .data$time_value,
        name = "n_wells"
      )

    # Complete the plate x chemical x concentration x time_value grid, so a
    # stratum with genuinely ZERO dosed wells at one timepoint (while the
    # same plate x chemical x concentration has dosed wells at OTHER
    # timepoints) is represented as an explicit 0, rather than silently
    # disappearing from the count table (mirrors the vc_counts_complete
    # treatment above).
    dose_strata <- dplyr::distinct(
      dose_counts_raw,
      .data$plate_id,
      .data$chemical_id,
      .data$concentration
    )
    time_by_plate <- dplyr::distinct(d, .data$plate_id, .data$time_value)
    dose_grid <- dplyr::inner_join(
      dose_strata,
      time_by_plate,
      by = "plate_id",
      relationship = "many-to-many"
    )
    dose_counts <- dose_grid |>
      dplyr::left_join(
        dose_counts_raw,
        by = c("plate_id", "chemical_id", "concentration", "time_value")
      ) |>
      dplyr::mutate(n_wells = dplyr::coalesce(.data$n_wells, 0L))
    diagnostics$dose_replicate_counts <- dose_counts

    # plates with zero dosed wells at all (across every timepoint)
    plates_with_dose <- unique(dose_counts_raw$plate_id)
    plates_all <- unique(d$plate_id)
    plates_no_dose <- setdiff(plates_all, plates_with_dose)

    dose_msgs <- character(0)
    dose_failed <- FALSE
    dose_n_affected <- 0L

    if (!is.null(expected_dose_wells) && length(plates_no_dose) > 0) {
      dose_failed <- TRUE
      dose_n_affected <- dose_n_affected + length(plates_no_dose)
      dose_msgs <- c(
        dose_msgs,
        paste0(
          length(plates_no_dose),
          " plate(s) have NO dosed (non-vehicle-control) wells at all."
        )
      )
    }

    if (nrow(dose_counts) > 0) {
      if (!is.null(expected_dose_wells)) {
        wrong_dose_counts <- dose_counts |>
          dplyr::filter(.data$n_wells != expected_dose_wells)
        if (nrow(wrong_dose_counts) > 0) {
          dose_failed <- TRUE
          dose_n_affected <- dose_n_affected + nrow(wrong_dose_counts)
          dose_msgs <- c(
            dose_msgs,
            paste0(
              nrow(wrong_dose_counts),
              " plate x chemical x concentration x timepoint combination(s) do not have ",
              "the expected ",
              expected_dose_wells,
              " dosed wells."
            )
          )
        }
      } else {
        dose_counts_by_stratum <- dose_counts |>
          dplyr::group_by(.data$plate_id, .data$time_value) |>
          dplyr::summarise(
            n_distinct_counts = dplyr::n_distinct(.data$n_wells),
            .groups = "drop"
          ) |>
          dplyr::filter(.data$n_distinct_counts > 1)
        if (nrow(dose_counts_by_stratum) > 0) {
          dose_failed <- TRUE
          dose_n_affected <- dose_n_affected + nrow(dose_counts_by_stratum)
          dose_msgs <- c(
            dose_msgs,
            paste0(
              nrow(dose_counts_by_stratum),
              " plate x timepoint stratum/strata have inconsistent replicate well counts across concentrations.",
              " Consider passing expected_dose_wells to enforce a specific count."
            )
          )
        }
      }
    }

    if (dose_failed) {
      checks[["dose_replicate_counts"]] <- zfbehavior_check_row(
        "dose_replicate_counts",
        "failed",
        paste(dose_msgs, collapse = " "),
        dose_n_affected
      )
    } else {
      checks[["dose_replicate_counts"]] <- zfbehavior_check_row(
        "dose_replicate_counts",
        "passed",
        "Dosed-well replicate counts are as expected/consistent.",
        0L
      )
    }
  }

  # keep_well / response_value sanity: at least one usable row
  if (!keep_well_type_ok) {
    checks[["keep_well_has_usable_rows"]] <- zfbehavior_check_row(
      "keep_well_has_usable_rows",
      "skipped",
      "Skipped: keep_well has an unexpected type."
    )
  } else {
    n_usable <- d |> dplyr::filter(.data$keep_well) |> nrow()
    if (n_usable == 0) {
      checks[["keep_well_has_usable_rows"]] <- zfbehavior_check_row(
        "keep_well_has_usable_rows",
        "failed",
        "No rows have keep_well == TRUE; nothing would be analyzed.",
        0L
      )
    } else {
      checks[["keep_well_has_usable_rows"]] <- zfbehavior_check_row(
        "keep_well_has_usable_rows",
        "passed",
        paste0(n_usable, " row(s) have keep_well == TRUE."),
        0L
      )
    }
  }

  # required columns fully filled (no unexpected NA)
  filled_check <- check_filled_columns(d, required_filled_cols)
  diagnostics$missing_values <- filled_check
  if (nrow(filled_check) > 0) {
    checks[["filled_columns"]] <- zfbehavior_check_row(
      "filled_columns",
      "failed",
      paste0(
        "Column(s) with unexpected missing (NA) values: ",
        paste0(
          filled_check$column,
          " (",
          ifelse(
            filled_check$status == "column_missing",
            "column not found",
            paste0(filled_check$n_na, " NA")
          ),
          ")",
          collapse = ", "
        ),
        "."
      ),
      nrow(filled_check)
    )
  } else {
    checks[["filled_columns"]] <- zfbehavior_check_row(
      "filled_columns",
      "passed",
      "No unexpected missing values in required_filled_cols.",
      0L
    )
  }

  # per-plate row-count check (may itself be ambiguous)
  row_counts <- d |> dplyr::count(.data$plate_id, name = "n_rows")
  resolved_row_count <- resolve_target_count(row_counts$n_rows, expected_row_count)
  if (isTRUE(resolved_row_count$ambiguous)) {
    checks[["plate_row_counts"]] <- zfbehavior_check_row(
      "plate_row_counts",
      "ambiguous",
      paste0(
        "Cannot infer an expected row count: more than one row count is equally common ",
        "(tied candidates: ",
        paste(resolved_row_count$modes, collapse = ", "),
        "). Supply expected_row_count explicitly."
      ),
      NA_integer_
    )
    diagnostics$plate_row_counts <- row_counts
  } else {
    row_count_check <- row_counts[row_counts$n_rows != resolved_row_count$target, , drop = FALSE]
    diagnostics$plate_row_counts <- row_count_check
    if (nrow(row_count_check) > 0) {
      checks[["plate_row_counts"]] <- zfbehavior_check_row(
        "plate_row_counts",
        "failed",
        paste0(
          nrow(row_count_check),
          " plate(s) do not have the expected row count",
          if (!is.null(expected_row_count)) {
            paste0(" (", expected_row_count, ")")
          } else {
            " (most common row count in the data)"
          },
          "."
        ),
        nrow(row_count_check)
      )
    } else {
      checks[["plate_row_counts"]] <- zfbehavior_check_row(
        "plate_row_counts",
        "passed",
        "Every plate has the expected row count.",
        0L
      )
    }
  }

  new_zfbehavior_validation_result(dplyr::bind_rows(checks), diagnostics, metadata)
}

#' Check one-to-one correspondence between two identifier columns
#'
#' Reports values of `plate_col` linked to more than one value of `id_col`
#' (or to a missing value), and the reverse. A stray identifier on a single
#' well is found this way. Part of Module 1 (consistency checks; skill
#' `dataset-import-qc`), used by `validate_combined_dataset()`.
#'
#' @param d A tibble.
#' @param plate_col First identifier column (usually the plate).
#' @param id_col Second identifier column (e.g. a chemical or blinded ID
#'   expected to be unique per plate).
#'
#' @return A tibble with one row per flagged value of either column: the
#'   value, the linked values (`id_values` / `plate_values`, missing shown
#'   as `"<NA>"`), their number, and `reason` (`"multiple_values"`,
#'   `"missing_value"`). Zero rows if the correspondence is one-to-one.
#'
#' @keywords internal
#' @noRd
check_id_correspondence <- function(d, plate_col, id_col) {
  check_columns_exist(d, c(plate_col, id_col), "check_id_correspondence", "plate_col and id_col")

  display_values <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    paste(sort(unique(x)), collapse = ", ")
  }

  # Fixed temporary names, renamed afterwards (avoids `:=`).
  summarise_side <- function(group_col, other_col, n_name, values_name) {
    out <- d |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c(group_col, other_col)))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
      dplyr::summarise(
        .zfbehavior_n_other = dplyr::n_distinct(.data[[other_col]], na.rm = TRUE),
        .zfbehavior_other_values = display_values(.data[[other_col]]),
        has_missing = anyNA(.data[[other_col]]),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        reason = dplyr::case_when(
          .data$.zfbehavior_n_other > 1 & .data$has_missing ~ "multiple_values, missing_value",
          .data$.zfbehavior_n_other > 1 ~ "multiple_values",
          .data$has_missing ~ "missing_value",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::filter(!is.na(.data$reason)) |>
      dplyr::select(-"has_missing")
    out[[n_name]] <- out[[".zfbehavior_n_other"]]
    out[[values_name]] <- out[[".zfbehavior_other_values"]]
    out[[".zfbehavior_n_other"]] <- NULL
    out[[".zfbehavior_other_values"]] <- NULL
    out
  }

  by_plate <- summarise_side(plate_col, id_col, "n_id_values", "id_values")
  by_id <- summarise_side(id_col, plate_col, "n_plate_values", "plate_values")

  dplyr::bind_rows(by_plate, by_id)
}

#' Check identifiers across the combined dataset
#'
#' Runs the dataset-wide checks of Module 1 (skill `dataset-import-qc`)
#' after all plates are combined: identifier columns that must be one-to-one
#' with the plate, and expected numbers of distinct values (e.g. chemicals,
#' fewer than plates when chemicals are replicated).
#'
#' @param d The combined tibble.
#' @param unique_id_cols Columns that must be one-to-one with `plate_col`
#'   (`check_id_correspondence()`). Default none.
#' @param plate_col Plate column. Default `"plate_id"`.
#' @param expected_n_distinct Named list: column = expected number of
#'   distinct values, e.g. `list(CHEMICAL_NAME = 123)`. Default none.
#'
#' @return A `zfbehavior_validation_result` (see [validate_behavior_data()])
#'   with checks `id_correspondence:<col>` and `distinct_count:<col>`.
#'
#' @seealso [canonical-columns] (Module 1), [validate_behavior_data()].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   plate_id = c("p1", "p1", "p2", "p2"),
#'   chemical_id = c("c1", "c1", "c2", "c2"),
#'   CHEMICAL_NAME = c("A", "A", "B", "B")
#' )
#' validate_combined_dataset(
#'   d,
#'   unique_id_cols = "chemical_id",
#'   expected_n_distinct = list(CHEMICAL_NAME = 2)
#' )
validate_combined_dataset <- function(
  d,
  unique_id_cols = character(0),
  plate_col = "plate_id",
  expected_n_distinct = list()
) {
  check_named_list_of_counts(expected_n_distinct, "expected_n_distinct", "validate_combined_dataset")

  metadata <- list(
    n_rows = nrow(d),
    parameters = list(
      unique_id_cols = unique_id_cols,
      plate_col = plate_col,
      expected_n_distinct = expected_n_distinct
    )
  )

  checks <- list()
  diagnostics <- list()

  referenced_cols <- unique(c(plate_col, unique_id_cols, names(expected_n_distinct)))
  missing_cols <- setdiff(referenced_cols, colnames(d))
  if (length(missing_cols) > 0) {
    checks[["required_columns_present"]] <- zfbehavior_check_row(
      "required_columns_present",
      "failed",
      paste0(
        "Column(s) referenced but not found in d: ",
        paste(unique(missing_cols), collapse = ", ")
      ),
      length(missing_cols)
    )
    return(new_zfbehavior_validation_result(
      dplyr::bind_rows(checks),
      diagnostics,
      metadata
    ))
  }
  checks[["required_columns_present"]] <- zfbehavior_check_row(
    "required_columns_present",
    "passed",
    "All referenced columns are present.",
    0L
  )

  id_correspondence_diag <- list()
  for (id_col in unique_id_cols) {
    correspondence <- check_id_correspondence(d, plate_col, id_col)
    code <- paste0("id_correspondence:", id_col)
    id_correspondence_diag[[id_col]] <- correspondence
    if (nrow(correspondence) > 0) {
      checks[[code]] <- zfbehavior_check_row(
        code,
        "failed",
        paste0(
          id_col,
          " is not in 1:1 correspondence with ",
          plate_col,
          " (",
          nrow(correspondence),
          " mismatched value(s), including any missing values)."
        ),
        nrow(correspondence)
      )
    } else {
      checks[[code]] <- zfbehavior_check_row(
        code,
        "passed",
        paste0(id_col, " is in 1:1 correspondence with ", plate_col, "."),
        0L
      )
    }
  }
  diagnostics$id_correspondence <- id_correspondence_diag

  distinct_count_diag <- list()
  for (col in names(expected_n_distinct)) {
    n_actual <- dplyr::n_distinct(d[[col]])
    n_expected <- expected_n_distinct[[col]]
    code <- paste0("distinct_count:", col)
    distinct_count_diag[[col]] <- tibble::tibble(
      column = col,
      n_actual = n_actual,
      n_expected = n_expected
    )
    if (n_actual != n_expected) {
      checks[[code]] <- zfbehavior_check_row(
        code,
        "failed",
        paste0(
          "Column '",
          col,
          "' has ",
          n_actual,
          " distinct value(s), expected ",
          n_expected,
          "."
        ),
        1L
      )
    } else {
      checks[[code]] <- zfbehavior_check_row(
        code,
        "passed",
        paste0("Column '", col, "' has the expected ", n_expected, " distinct value(s)."),
        0L
      )
    }
  }
  diagnostics$distinct_counts <- distinct_count_diag

  new_zfbehavior_validation_result(dplyr::bind_rows(checks), diagnostics, metadata)
}

#' Required columns/types of `generate_behavior_endpoints()` output
#'
#' @keywords internal
#' @noRd
endpoint_result_schema_table <- function() {
  tibble::tibble(
    column = c(
      "plate_id",
      "well_id",
      "chemical_id",
      "concentration",
      "is_vehicle_control",
      "keep_well",
      "endpoint_type",
      "segment",
      "endpoint",
      "endpoint_value_raw",
      "endpoint_value_transformed",
      "denom",
      "endpoint_value_norm"
    ),
    type = c(
      "character",
      "character",
      "character",
      "numeric",
      "logical",
      "logical",
      "character",
      "character",
      "character",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )
}

#' Check the structure of a behavioral endpoint table
#'
#' Checks the output of [generate_behavior_endpoints()]: required columns and
#' types, one row per well x endpoint, every endpoint type present for every
#' well, the same wells as the input (with `source_d`), and consistent
#' normalization columns. It does not check values (e.g. `NA` or
#' infinite). Part of Module 3 (skill `behavior-endpoints`).
#'
#' @param d Output of [generate_behavior_endpoints()].
#' @param source_d Optional input given to [generate_behavior_endpoints()],
#'   to check that the wells match. Default `NULL` (check skipped).
#' @param expected_endpoint_types Optional endpoint types every well must
#'   have. Default `NULL`: the types present in `d`.
#'
#' @return A `zfbehavior_validation_result` (see [validate_behavior_data()]).
#' @seealso [module3].
#' @export
validate_behavior_endpoints <- function(
  d,
  source_d = NULL,
  expected_endpoint_types = NULL
) {
  metadata <- list(
    n_rows = nrow(d),
    parameters = list(
      has_source_d = !is.null(source_d),
      expected_endpoint_types = expected_endpoint_types
    )
  )

  checks <- list()
  diagnostics <- list()

  schema <- endpoint_result_schema_table()
  missing_cols <- setdiff(schema$column, colnames(d))
  if (length(missing_cols) > 0) {
    checks[["required_columns_present"]] <- zfbehavior_check_row(
      "required_columns_present",
      "failed",
      paste0(
        "Missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      ),
      length(missing_cols)
    )
    skip_msg <- "Skipped: required column(s) missing."
    for (code in c(
      "column_types",
      "unique_endpoint_key",
      "endpoint_coverage",
      "identifier_preservation",
      "normalization_provenance_consistent"
    )) {
      checks[[code]] <- zfbehavior_check_row(code, "skipped", skip_msg)
    }
    return(new_zfbehavior_validation_result(
      dplyr::bind_rows(checks),
      diagnostics,
      metadata
    ))
  }
  checks[["required_columns_present"]] <- zfbehavior_check_row(
    "required_columns_present",
    "passed",
    "All required columns are present.",
    0L
  )

  # column types
  bad_type_cols <- character(0)
  type_msgs <- character(0)
  for (i in seq_len(nrow(schema))) {
    col <- schema$column[i]
    expected_type <- schema$type[i]
    actual <- d[[col]]
    ok_type <- switch(
      expected_type,
      "logical" = is.logical(actual),
      "numeric" = is.numeric(actual),
      "character" = is.character(actual),
      TRUE
    )
    if (!ok_type) {
      bad_type_cols <- c(bad_type_cols, col)
      type_msgs <- c(
        type_msgs,
        paste0(
          "'",
          col,
          "' should be ",
          expected_type,
          " but is ",
          paste(class(actual), collapse = "/"),
          "."
        )
      )
    }
  }
  if (length(bad_type_cols) > 0) {
    checks[["column_types"]] <- zfbehavior_check_row(
      "column_types",
      "failed",
      paste(type_msgs, collapse = " "),
      length(bad_type_cols)
    )
  } else {
    checks[["column_types"]] <- zfbehavior_check_row(
      "column_types",
      "passed",
      "All checked columns have the expected type.",
      0L
    )
  }

  # complete endpoint key uniqueness: plate_id x well_id x endpoint_type x
  # segment x component x metric (component/metric may be legitimately
  # NA for endpoint types that don't use them; dplyr::count() treats NA
  # as its own group correctly for this purpose).
  key_cols <- c("plate_id", "well_id", "endpoint_type", "segment")
  key_cols <- c(key_cols, intersect(c("component", "metric"), colnames(d)))
  dup_check <- d |>
    dplyr::count(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::filter(.data$n > 1)
  diagnostics$duplicate_endpoint_keys <- dup_check
  if (nrow(dup_check) > 0) {
    checks[["unique_endpoint_key"]] <- zfbehavior_check_row(
      "unique_endpoint_key",
      "failed",
      paste0(
        nrow(dup_check),
        " combination(s) of ",
        paste(key_cols, collapse = " x "),
        " are not unique (duplicate endpoint rows found)."
      ),
      nrow(dup_check)
    )
  } else {
    checks[["unique_endpoint_key"]] <- zfbehavior_check_row(
      "unique_endpoint_key",
      "passed",
      paste0(paste(key_cols, collapse = " x "), " is unique."),
      0L
    )
  }

  # endpoint coverage: does every well have every endpoint_type present
  # in the data (or, if supplied, every expected_endpoint_types entry)?
  types_to_check <- if (!is.null(expected_endpoint_types)) {
    expected_endpoint_types
  } else {
    unique(d$endpoint_type)
  }
  wells <- dplyr::distinct(d, .data$plate_id, .data$well_id)
  well_type_grid <- tidyr::expand_grid(
    wells,
    endpoint_type = types_to_check
  )
  actual_well_types <- dplyr::distinct(d, .data$plate_id, .data$well_id, .data$endpoint_type)
  missing_well_types <- dplyr::anti_join(
    well_type_grid,
    actual_well_types,
    by = c("plate_id", "well_id", "endpoint_type")
  )
  diagnostics$missing_endpoint_coverage <- missing_well_types
  if (nrow(missing_well_types) > 0) {
    checks[["endpoint_coverage"]] <- zfbehavior_check_row(
      "endpoint_coverage",
      "failed",
      paste0(
        nrow(missing_well_types),
        " well x endpoint_type combination(s) are missing."
      ),
      nrow(missing_well_types)
    )
  } else {
    checks[["endpoint_coverage"]] <- zfbehavior_check_row(
      "endpoint_coverage",
      "passed",
      "Every well has every checked endpoint_type represented.",
      0L
    )
  }

  # identifier preservation / no row multiplication vs. source_d
  if (is.null(source_d)) {
    checks[["identifier_preservation"]] <- zfbehavior_check_row(
      "identifier_preservation",
      "skipped",
      "Skipped: no source_d supplied."
    )
  } else {
    source_wells <- dplyr::distinct(source_d, .data$plate_id, .data$well_id)
    result_wells <- dplyr::distinct(d, .data$plate_id, .data$well_id)
    missing_from_result <- dplyr::anti_join(source_wells, result_wells, by = c("plate_id", "well_id"))
    extra_in_result <- dplyr::anti_join(result_wells, source_wells, by = c("plate_id", "well_id"))
    diagnostics$identifier_preservation <- list(
      missing_from_result = missing_from_result,
      extra_in_result = extra_in_result
    )
    n_affected <- nrow(missing_from_result) + nrow(extra_in_result)
    if (n_affected > 0) {
      checks[["identifier_preservation"]] <- zfbehavior_check_row(
        "identifier_preservation",
        "failed",
        paste0(
          nrow(missing_from_result),
          " well(s) in source_d are missing from the endpoint result; ",
          nrow(extra_in_result),
          " well(s) in the endpoint result are not in source_d."
        ),
        n_affected
      )
    } else {
      checks[["identifier_preservation"]] <- zfbehavior_check_row(
        "identifier_preservation",
        "passed",
        "Every well in source_d is represented in the endpoint result, with no unexpected extra wells.",
        0L
      )
    }
  }

  # normalization provenance internal consistency: is_default_normalization
  # (if present) must agree with the approved default map for each row's
  # endpoint_type.
  if (all(c("normalization_method", "is_default_normalization") %in% colnames(d))) {
    default_map <- zf_default_normalization_map()
    expected_default <- unlist(default_map[d$endpoint_type], use.names = FALSE)
    actual_is_default <- d$normalization_method == expected_default
    inconsistent <- d[actual_is_default != d$is_default_normalization, , drop = FALSE]
    diagnostics$normalization_provenance_inconsistent <- inconsistent
    if (nrow(inconsistent) > 0) {
      checks[["normalization_provenance_consistent"]] <- zfbehavior_check_row(
        "normalization_provenance_consistent",
        "failed",
        paste0(
          nrow(inconsistent),
          " row(s) have is_default_normalization inconsistent with normalization_method/endpoint_type."
        ),
        nrow(inconsistent)
      )
    } else {
      checks[["normalization_provenance_consistent"]] <- zfbehavior_check_row(
        "normalization_provenance_consistent",
        "passed",
        "is_default_normalization is consistent with normalization_method/endpoint_type for every row.",
        0L
      )
    }
  } else {
    checks[["normalization_provenance_consistent"]] <- zfbehavior_check_row(
      "normalization_provenance_consistent",
      "skipped",
      "Skipped: normalization_method/is_default_normalization column(s) missing."
    )
  }

  new_zfbehavior_validation_result(dplyr::bind_rows(checks), diagnostics, metadata)
}

#' @export
print.zfbehavior_validation_result <- function(x, ...) {
  if (x$ok) {
    cat(
      "zfbehavior validation: OK (",
      x$metadata$n_rows,
      " rows checked)\n",
      sep = ""
    )
  } else {
    cat(
      "zfbehavior validation: FAILED (",
      x$metadata$n_rows,
      " rows checked)\n",
      sep = ""
    )
  }
  not_passed <- x$checks[x$checks$status != "passed", , drop = FALSE]
  for (i in seq_len(nrow(not_passed))) {
    cat(
      " - [",
      toupper(not_passed$status[i]),
      "] ",
      not_passed$code[i],
      ": ",
      not_passed$message[i],
      "\n",
      sep = ""
    )
  }
  invisible(x)
}
