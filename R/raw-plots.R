#' Columns needed for raw-response plots
#'
#' The canonical columns required by [plot_raw_responses()] and
#' [plot_raw_responses_pages()]. Part of Module 2 (raw-response
#' visualization; skill `raw-response-plots`).
#'
#' @seealso [module2], [canonical-columns].
#' @return A character vector of required column names.
#' @export
#'
#' @examples
#' raw_plot_input_cols()
raw_plot_input_cols <- function() {
  c(
    "plate_id",
    "well_id",
    "is_vehicle_control",
    "concentration",
    "time_value",
    "response_value",
    "keep_well"
  )
}

check_raw_plot_input <- function(d, fn_name) {
  missing_cols <- setdiff(raw_plot_input_cols(), colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires canonical columns that are missing."),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", ")),
        "i" = "Use read_behavior_data() with an explicit column_mapping, or rename columns to match canonical_behavior_roles().",
        "i" = "See raw_plot_input_cols() or ?\"canonical-columns\" for the full required-column picture."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  # keep_well = FALSE wells must be filtered out before plotting.
  check_no_excluded_wells(d, fn_name)
  invisible(TRUE)
}

#' Count kept and total wells per dose group
#'
#' Counts, per group, the wells with `keep_well = TRUE` and all wells. Call
#' it on the full dataset, before removing `keep_well = FALSE` wells; the
#' result labels dose groups in plots as e.g. `"VC(15/16)"` (`well_counts`
#' of [plot_raw_responses()]). With the single-larva well contract
#' ([canonical-columns]) the total equals the larvae assayed. Part of
#' Module 2 (skills `raw-response-plots`, `interplate-vc-qc`; also used by
#' `intraplate-vc-qc`).
#'
#' @param d The full dataset with `group_cols`, `well_id` and `keep_well`.
#' @param group_cols Columns defining a dose group, e.g.
#'   `c("plate_id", "is_vehicle_control", "concentration")`.
#'
#' @return A tibble with `group_cols`, `n_kept_wells` and `n_total_wells`.
#' @seealso [module2].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   plate_id = "P1",
#'   well_id = c("A1", "A2", "A3", "B1", "B2"),
#'   is_vehicle_control = c(TRUE, TRUE, TRUE, FALSE, FALSE),
#'   concentration = c(0, 0, 0, 50, 50),
#'   keep_well = c(TRUE, TRUE, FALSE, TRUE, TRUE)
#' )
#' count_wells_by_dose(d, group_cols = c("plate_id", "is_vehicle_control", "concentration"))
count_wells_by_dose <- function(d, group_cols) {
  required_cols <- c(group_cols, "well_id", "keep_well")
  missing_cols <- setdiff(required_cols, colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        "count_wells_by_dose() requires columns that are missing.",
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", "))
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }

  group_syms <- rlang::syms(group_cols)
  d |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::summarise(
      n_kept_wells = dplyr::n_distinct(
        .data$well_id[.data$keep_well %in% TRUE]
      ),
      n_total_wells = dplyr::n_distinct(.data$well_id),
      .groups = "drop"
    )
}

#' Build an ordered dose/group label: vehicle-control first, then ascending
#' concentration
#'
#' @param concentration Numeric concentration values.
#' @param concentration_unit Character concentration units, or `NULL` if not
#'   available (in which case no unit suffix is added to the label).
#' @param is_vehicle_control Logical vector, `TRUE` for vehicle-control rows.
#' @param n_kept Optional numeric vector (same length as `concentration`),
#'   number of kept wells for that row's dose group. If supplied together
#'   with `n_total`, appends `"(n_kept/n_total)"` to the label (e.g.
#'   `"VC(15/16)"`), reproducing the original `update_dose_label()`'s
#'   well-count annotation (generalized to a fraction -- see
#'   [count_wells_by_dose()]). Default `NULL`: no count suffix, matching
#'   prior behavior exactly.
#' @param n_total Optional numeric vector (same length as `concentration`),
#'   total wells (kept + excluded) for that row's dose group. Must be
#'   supplied together with `n_kept`, or not at all.
#' @return An ordered factor: `"VC"` for vehicle-control rows (always the
#'   first level), then `"<concentration><unit>"` for treated rows, in
#'   ascending concentration order. Each level optionally suffixed with
#'   `"(n_kept/n_total)"` (see `n_kept`/`n_total`).
#' @keywords internal
#' @noRd
make_dose_label <- function(
  concentration,
  concentration_unit = NULL,
  is_vehicle_control,
  n_kept = NULL,
  n_total = NULL
) {
  if (is.null(concentration_unit)) {
    concentration_unit <- rep("", length(concentration))
  }
  label <- ifelse(
    is_vehicle_control,
    "VC",
    paste0(concentration, concentration_unit)
  )
  if (!is.null(n_kept) || !is.null(n_total)) {
    if (is.null(n_kept) || is.null(n_total)) {
      rlang::abort(
        "make_dose_label() requires both `n_kept` and `n_total`, or neither.",
        class = "zfbehavior_error_missing_well_counts"
      )
    }
    label <- paste0(label, "(", n_kept, "/", n_total, ")")
  }
  # !is_vehicle_control is FALSE (sorts first) for VC rows, so VC always
  # ends up first; ties among treated rows are broken by ascending
  # concentration.
  ord <- order(!is_vehicle_control, concentration)
  levels_ordered <- unique(label[ord])
  factor(label, levels = levels_ordered)
}

#' Sequential Blues gradient, one shade per distinct value, deterministic
#' ordering
#'
#' Generalizes the vehicle-control branch of the original
#' `set_embryo_color()`: a reversed `RColorBrewer::brewer.pal(9, "Blues")`
#' ramp (darkest first), one shade per distinct value of `x`. Unlike the
#' original (which ordered wells by `arrange(Is_Vehicle_Control, Dose)` --
#' a no-op for vehicle-control wells, since they share the same nominal
#' dose, leaving the actual shade assignment dependent on incidental input
#' row order), this orders `x` alphabetically/numerically (via `sort()`)
#' before assigning shades, so the same set of values always gets the same
#' shades regardless of input row order.
#'
#' @param x A vector (typically well IDs) to assign colors to.
#' @return A character vector of hex colors, same length/order as `x`.
#' @keywords internal
#' @noRd
sequential_blues_colors <- function(x) {
  ux <- sort(unique(x))
  pal <- rev(grDevices::colorRampPalette(
    RColorBrewer::brewer.pal(9, "Blues")
  )(length(ux)))
  names(pal) <- as.character(ux)
  unname(pal[as.character(x)])
}

#' Deterministic dose-group color mapping: vehicle-control blue, treated
#' doses on a sequential red gradient with the lowest dose forced to pure
#' red
#'
#' Faithfully reproduces (generalized to any number of dose levels) the
#' original `set_dose_color()`: vehicle control is always `"blue"`;
#' treated doses (ascending concentration) get a
#' `colorRampPalette(RColorBrewer::brewer.pal(9, "Reds"))` gradient from
#' pale to dark; then the LOWEST treated concentration's color is
#' overridden to pure `"red"` (reproducing the original's hard-coded
#' `color_a[[2]] <- "red"` "temp" fix), because the ramp's naturally
#' palest shade for the lowest dose does not stand out enough against a
#' white background. This makes vehicle-control, lowest, and highest dose
#' all visually distinct at a glance, by design.
#'
#' @param concentration Numeric concentration values.
#' @param is_vehicle_control Logical vector, `TRUE` for vehicle-control rows.
#' @return A character vector of hex colors, same length/order as the
#'   inputs.
#' @keywords internal
#' @noRd
dose_color_map <- function(concentration, is_vehicle_control) {
  out <- character(length(concentration))
  out[is_vehicle_control] <- "blue"
  treated <- !is_vehicle_control
  if (any(treated)) {
    treated_conc <- concentration[treated]
    ux <- sort(unique(treated_conc))
    pal <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(9, "Reds")
    )(length(ux))
    pal[1] <- "red"
    names(pal) <- as.character(ux)
    out[treated] <- unname(pal[as.character(treated_conc)])
  }
  out
}

#' Sequential red-family color mapping for individual wells within a dose
#'
#' Extends the same "sequential, popped-out endpoints" visual language as
#' `dose_color_map()` to individual, per-well lines within a dose group
#' (a view that did not exist in the original code -- the original only
#' ever plotted individual wells for vehicle control, never for treated
#' concentrations). Each dose gets its base color from `dose_color_map()`;
#' wells within that dose are then distinguished by shade (lighter to
#' darker) within that base color's family, rather than an unrelated
#' hue per well.
#'
#' @param well_id Character vector of well identifiers.
#' @param concentration Numeric concentration values (same length as
#'   `well_id`).
#' @param is_vehicle_control Logical vector, `TRUE` for vehicle-control
#'   rows (same length as `well_id`); these are handled by
#'   `sequential_blues_colors()`, not this function's red-family logic.
#' @return A character vector of hex colors, same length/order as the
#'   inputs.
#' @keywords internal
#' @noRd
sequential_well_colors_within_dose <- function(
  well_id,
  concentration,
  is_vehicle_control
) {
  out <- character(length(well_id))

  if (any(is_vehicle_control)) {
    out[is_vehicle_control] <- sequential_blues_colors(well_id[
      is_vehicle_control
    ])
  }

  treated <- !is_vehicle_control
  if (any(treated)) {
    dose_base <- dose_color_map(
      concentration[treated],
      rep(FALSE, sum(treated))
    )
    treated_df <- tibble::tibble(
      well_id = well_id[treated],
      concentration = concentration[treated],
      dose_base = dose_base
    )
    treated_out <- treated_df |>
      dplyr::group_by(.data$concentration) |>
      dplyr::mutate(
        well_shade = {
          base <- unique(.data$dose_base)
          n <- dplyr::n_distinct(.data$well_id)
          if (n == 1) {
            rep(base, dplyr::n())
          } else {
            # Blend from a pale tint of the base color up to (and
            # slightly past) the base color itself, so within a dose,
            # wells are distinguishable while remaining clearly in that
            # dose's color family. Wells are ranked alphabetically
            # (deterministic, independent of input row order).
            ramp <- grDevices::colorRampPalette(c("white", base))(n + 1)[-1]
            well_rank <- match(.data$well_id, sort(unique(.data$well_id)))
            ramp[well_rank]
          }
        }
      ) |>
      dplyr::ungroup()
    out[treated] <- treated_out$well_shade
  }

  out
}

#' Build a light/dark (or arbitrary) segment shading table for plotting
#'
#' Takes the same named-list segment format used by
#' `calculate_total_distance_endpoints()` and friends (e.g.
#' `list(L1 = 1:10, D1 = 11:20, L2 = 21:30, D2 = 31:50)`) and derives each
#' segment's ACTUAL time span (`min`/`max` of its `time_value` entries) for
#' shading, rather than assuming a fixed width. This fixes a bug in the
#' original plotting functions, which drew two fixed-width (typically
#' 10-minute) shading rectangles regardless of each segment's real
#' duration -- incorrect for datasets where segments have unequal
#' durations (e.g. a 20-minute `D2` alongside 10-minute `L1`/`D1`/`L2`).
#'
#' @param segments A named list mapping segment labels to `time_value`
#'   entries, as used by `calculate_total_distance_endpoints()`.
#' @param segment_style Optional named list, one entry per segment name,
#'   each `list(fill = <hex/color string>, alpha = <0-1>)`. If `NULL`
#'   (default), a simple convenience default is used, matching the original
#'   plotting functions: segment names starting with `"L"`
#'   (case-insensitive; intended for "light" phases) get a pale yellow
#'   shade (`fill = "yellow"`, `alpha = 0.2`); all other segment names are
#'   unshaded (`alpha = 0`). Since segment names are entirely
#'   caller-defined (e.g. `"Excitatory"`/`"Refractory"` do not match this
#'   pattern), this default is a convenience only -- pass `segment_style`
#'   explicitly to control shading for any segment scheme.
#' @return A tibble with columns `segment`, `xmin`, `xmax`, `fill`, `alpha`.
#' @keywords internal
#' @noRd
segments_to_shading_df <- function(segments, segment_style = NULL) {
  check_segments(segments)

  is_light_like <- function(nm) grepl("^L", nm, ignore.case = TRUE)

  purrr::imap_dfr(segments, function(times, nm) {
    style <- segment_style[[nm]]
    if (is.null(style)) {
      if (is_light_like(nm)) {
        style <- list(fill = "yellow", alpha = 0.2)
      } else {
        style <- list(fill = "yellow", alpha = 0)
      }
    }
    fill <- style$fill
    if (is.null(fill)) {
      fill <- "yellow"
    }
    alpha <- style$alpha
    if (is.null(alpha)) {
      alpha <- 0
    }
    tibble::tibble(
      segment = nm,
      xmin = min(times),
      xmax = max(times),
      fill = fill,
      alpha = alpha
    )
  })
}

#' Summarize raw responses over wells, per group and timepoint
#'
#' Computes the median and interquartile range (or the mean and 95%
#' confidence interval) of `response_value` over the wells of each group at
#' each timepoint. It is the summary drawn by [plot_raw_responses()]
#' (`level = "summary"`) and the plate reference pattern of
#' [calculate_plate_reference_pattern()]. Part of Module 2.
#'
#' @param d A tibble with `group_cols`, `time_value`, `response_value` and
#'   `keep_well`; only `keep_well = TRUE` rows (filter first).
#' @param group_cols Grouping columns besides `time_value`.
#' @param summary_type `"median"` (default; median, Q25, Q75) or `"mean"`
#'   (mean and 95% confidence interval).
#' @param missing_value_policy `"impute_zero"` (default; missing responses
#'   count as 0, as in the endpoints, see [replace_na_response_zero()]) or
#'   `"na_omit"` (missing responses ignored).
#' @param well_counts Optional [count_wells_by_dose()] result; adds
#'   `n_kept_wells` and `n_total_wells`. Every group must have a row.
#'
#' @return A tibble with `group_cols`, `time_value`, `n` (non-missing
#'   values), `center`, `lower`, `upper`, and the well counts if given.
#' @seealso [module2].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   plate_id = "P1",
#'   well_id = rep(c("A1", "A2"), each = 3),
#'   time_value = rep(1:3, 2),
#'   response_value = c(1, 2, 3, 2, 3, 4),
#'   keep_well = TRUE
#' )
#' summarize_raw_responses(d, group_cols = "plate_id")
summarize_raw_responses <- function(
  d,
  group_cols,
  summary_type = c("median", "mean"),
  missing_value_policy = c("impute_zero", "na_omit"),
  well_counts = NULL
) {
  summary_type <- match.arg(summary_type)
  missing_value_policy <- match.arg(missing_value_policy)

  required_cols <- c(group_cols, "time_value", "response_value", "keep_well")
  missing_cols <- setdiff(required_cols, colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        "summarize_raw_responses() requires columns that are missing.",
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", "))
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  check_no_excluded_wells(d, "summarize_raw_responses")

  if (missing_value_policy == "impute_zero") {
    d <- replace_na_response_zero(d)
  }

  group_syms <- rlang::syms(c(group_cols, "time_value"))
  d1 <- d |> dplyr::group_by(!!!group_syms)

  if (summary_type == "median") {
    result <- d1 |>
      dplyr::summarise(
        n = sum(!is.na(.data$response_value)),
        center = stats::median(.data$response_value, na.rm = TRUE),
        lower = stats::quantile(
          .data$response_value,
          probs = 0.25,
          na.rm = TRUE,
          names = FALSE
        ),
        upper = stats::quantile(
          .data$response_value,
          probs = 0.75,
          na.rm = TRUE,
          names = FALSE
        ),
        .groups = "drop"
      )
  } else {
    result <- d1 |>
      dplyr::summarise(
        n = sum(!is.na(.data$response_value)),
        mean_val = mean(.data$response_value, na.rm = TRUE),
        se = stats::sd(.data$response_value, na.rm = TRUE) / sqrt(.data$n),
        center = .data$mean_val,
        lower = .data$mean_val - 1.96 * .data$se,
        upper = .data$mean_val + 1.96 * .data$se,
        .groups = "drop"
      ) |>
      dplyr::select(-"mean_val", -"se")
  }

  if (!is.null(well_counts)) {
    result <- join_well_counts(result, well_counts)
  }

  result
}

#' Join well_counts onto a data frame, erroring if any row is unmatched
#'
#' Shared by `summarize_raw_responses()` and `plot_raw_responses()` so both
#' fail the same way if `well_counts` (from `count_wells_by_dose()`) does
#' not cover every group present in the data being summarized/plotted. The
#' join key is auto-detected as every column in `well_counts` OTHER THAN
#' `n_kept_wells`/`n_total_wells` (i.e. whatever `group_cols` was used to
#' build `well_counts`), rather than requiring the caller to re-specify it
#' -- this lets both call sites reuse the exact same `well_counts` table
#' even though they group by slightly different column sets internally.
#'
#' Validates `well_counts` before joining: rejects
#' duplicate join-key rows (which would silently duplicate `d`'s rows and
#' change downstream `n`/summary statistics), and rejects nonnumeric,
#' non-finite, negative, noninteger, or `n_kept_wells > n_total_wells`
#' count values. The join itself uses `relationship = "many-to-one"` as a
#' second, join-time guarantee that each row of `d` matches at most one
#' `well_counts` row.
#'
#' @param d A tibble to join well counts onto. Must contain every join-key
#'   column present in `well_counts`.
#' @param well_counts A tibble from [count_wells_by_dose()]: some set of
#'   group-defining columns, plus `n_kept_wells`, `n_total_wells`.
#' @return `d` with `n_kept_wells`/`n_total_wells` columns added.
#' @keywords internal
#' @noRd
join_well_counts <- function(d, well_counts) {
  join_cols <- setdiff(
    colnames(well_counts),
    c("n_kept_wells", "n_total_wells")
  )
  missing_in_well_counts <- setdiff(
    c("n_kept_wells", "n_total_wells"),
    colnames(well_counts)
  )
  if (length(missing_in_well_counts) > 0) {
    rlang::abort(
      c(
        "`well_counts` requires columns that are missing.",
        "x" = paste0(
          "Missing: ",
          paste(missing_in_well_counts, collapse = ", ")
        ),
        "i" = "Build `well_counts` with count_wells_by_dose()."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  missing_in_d <- setdiff(join_cols, colnames(d))
  if (length(missing_in_d) > 0) {
    rlang::abort(
      c(
        "`well_counts` has grouping column(s) not present in the data being joined onto.",
        "x" = paste0("Missing: ", paste(missing_in_d, collapse = ", ")),
        "i" = "count_wells_by_dose()'s group_cols must all also be present in the data passed to summarize_raw_responses()/plot_raw_responses()."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }

  well_counts_key <- well_counts |>
    dplyr::select(dplyr::all_of(c(
      join_cols,
      "n_kept_wells",
      "n_total_wells"
    )))

  # Reject duplicate join-key rows: a many-to-one join against a duplicated
  # key would silently duplicate d's rows (changing n/summary statistics
  # downstream), so this must be caught before the join, not discovered
  # afterward as an inflated row count.
  n_key_rows <- nrow(well_counts_key)
  n_distinct_keys <- well_counts_key |>
    dplyr::distinct(dplyr::across(dplyr::all_of(join_cols))) |>
    nrow()
  if (n_distinct_keys < n_key_rows) {
    dup_keys <- well_counts_key |>
      dplyr::count(dplyr::across(dplyr::all_of(join_cols))) |>
      dplyr::filter(.data$n > 1)
    rlang::abort(
      c(
        "`well_counts` must have exactly one row per join-key group.",
        "x" = paste0(
          nrow(dup_keys),
          " group(s) had more than one row for (",
          paste(join_cols, collapse = ", "),
          ")."
        ),
        "i" = "Rebuild `well_counts` with count_wells_by_dose(), which always returns one summarized row per group_cols combination."
      ),
      class = "zfbehavior_error_duplicate_well_counts"
    )
  }

  # Validate count values: nonnumeric/non-finite/negative/noninteger/
  # n_kept_wells > n_total_wells counts would otherwise reach downstream
  # label formatting (make_dose_label()) or plot legends silently.
  invalid_reasons <- character(0)
  for (col in c("n_kept_wells", "n_total_wells")) {
    vals <- well_counts_key[[col]]
    if (!is.numeric(vals)) {
      invalid_reasons <- c(
        invalid_reasons,
        paste0("`", col, "` must be numeric.")
      )
    } else {
      if (any(!is.finite(vals))) {
        invalid_reasons <- c(
          invalid_reasons,
          paste0("`", col, "` must be finite (no NA/NaN/Inf).")
        )
      }
      if (any(is.finite(vals) & (vals < 0 | vals != round(vals)))) {
        invalid_reasons <- c(
          invalid_reasons,
          paste0("`", col, "` must contain only non-negative integers.")
        )
      }
    }
  }
  if (
    is.numeric(well_counts_key$n_kept_wells) &&
      is.numeric(well_counts_key$n_total_wells) &&
      any(
        is.finite(well_counts_key$n_kept_wells) &
          is.finite(well_counts_key$n_total_wells) &
          well_counts_key$n_kept_wells > well_counts_key$n_total_wells
      )
  ) {
    invalid_reasons <- c(
      invalid_reasons,
      "`n_kept_wells` must never exceed `n_total_wells` in the same row."
    )
  }
  if (length(invalid_reasons) > 0) {
    rlang::abort(
      c(
        "`well_counts` contains invalid n_kept_wells/n_total_wells values.",
        stats::setNames(invalid_reasons, rep("x", length(invalid_reasons)))
      ),
      class = "zfbehavior_error_invalid_well_counts"
    )
  }

  joined <- d |>
    dplyr::left_join(
      well_counts_key,
      by = join_cols,
      relationship = "many-to-one"
    )

  n_unmatched <- sum(is.na(joined$n_total_wells))
  if (n_unmatched > 0) {
    rlang::abort(
      c(
        "Every row in the data must have a matching entry in `well_counts`.",
        "x" = paste0(
          n_unmatched,
          " row(s) had no matching `well_counts` entry for their (",
          paste(join_cols, collapse = ", "),
          ") group."
        ),
        "i" = "Rebuild `well_counts` with count_wells_by_dose() from the FULL (unfiltered) dataset, covering every group present here."
      ),
      class = "zfbehavior_error_missing_well_counts"
    )
  }

  joined
}

#' Resolve a single plot title from a pre-joined column, erroring if
#' ambiguous
#'
#' The title is typically a pre-built column such as chemical name + blind
#' code + plate name joined together. This expects a COLUMN NAME already present in
#' `d`. This is deliberately a single column, not a vector of columns to
#' join here: the caller may need to reformat/shorten the title text
#' (e.g. abbreviate a long chemical name) in ways this package cannot
#' anticipate, so title construction (concatenation, truncation, etc.) is
#' left entirely to the caller, typically via `dplyr::mutate()` or
#' `tidyr::unite()` before calling [plot_raw_responses()].
#'
#' @param d A tibble containing `title_col`.
#' @param title_col Single column name (character scalar), or `NULL`.
#' @param fn_name Name of the calling function, for error messages.
#' @return The single resolved title string, or `NULL` if `title_col` is
#'   `NULL`.
#' @keywords internal
#' @noRd
resolve_plot_title <- function(d, title_col, fn_name) {
  if (is.null(title_col)) {
    return(NULL)
  }
  if (!title_col %in% colnames(d)) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires `title_col` to exist in `d`."),
        "x" = paste0("Missing column: ", title_col)
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  values <- unique(d[[title_col]][!is.na(d[[title_col]])])
  if (length(values) != 1) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires `title_col` to have exactly one distinct (non-NA) value in `d`."
        ),
        "x" = paste0(
          "Found ",
          length(values),
          " distinct value(s) in column '",
          title_col,
          "'."
        ),
        "i" = "A title only makes sense for data already filtered to one plate/chemical-replicate -- use plot_raw_responses_pages() for several groups."
      ),
      class = "zfbehavior_error_ambiguous_title"
    )
  }
  as.character(values)
}

#' Wrap a plot title to a maximum line width
#'
#' @param title Character scalar or `NULL`.
#' @param width Positive integer line width (characters), or `NULL`/`Inf`
#'   for no wrapping.
#' @param fn_name Calling function name, for error messages.
#' @return The wrapped title (`NULL` stays `NULL`).
#' @keywords internal
#' @noRd
wrap_title <- function(title, width, fn_name) {
  if (is.null(width) || identical(width, Inf)) {
    return(title)
  }
  if (!is.numeric(width) || length(width) != 1 || !is.finite(width) || width < 1) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires `title_wrap_width` to be a positive number, Inf, or NULL."),
        "x" = paste0("Got: ", format(width))
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  if (is.null(title)) {
    return(NULL)
  }
  paste(stringr::str_wrap(title, width = width), collapse = "\n")
}

#' Plot raw response traces over time for one unit
#'
#' Plots `response_value` (e.g. distance moved) against time for one unit
#' (usually one plate). `level = "summary"` draws one median line (with
#' interquartile band) per dose group; `level = "well"` draws one line per
#' well, faceted by dose group. Light phases are shaded. Part of Module 2
#' (raw-response visualization; skill `raw-response-plots`); for many units
#' use [plot_raw_responses_pages()].
#'
#' Colours: vehicle control in blue, doses in a red gradient (darker for
#' higher doses, the lowest dose in pure red so it stands out); at `level = "well"` the wells of a group get shades of the
#' group colour. Lines of different plates are never joined, even when
#' wells share an ID.
#'
#' @param d A tibble with the columns of [raw_plot_input_cols()] (and
#'   optionally `concentration_unit`), only `keep_well = TRUE` rows.
#' @param segments Named list of phases and their `time_value`s, e.g.
#'   `list(L1 = 1:10, D1 = 11:20, L2 = 21:30, D2 = 31:50)`, used for shading.
#' @param level `"summary"` (default) or `"well"`.
#' @param summary_type `"median"` (default) or `"mean"` (see
#'   [summarize_raw_responses()]).
#' @param summary_group_cols Columns giving separate summary lines within a
#'   dose group. Default `"plate_id"` (one line per plate); `character(0)`
#'   pools all plates.
#' @param missing_value_policy `"impute_zero"` (default) or `"na_omit"` (see
#'   [summarize_raw_responses()]; at `level = "well"` missing values leave a
#'   gap, with a warning).
#' @param segment_style Optional named list of shading per segment, e.g.
#'   `list(L1 = list(fill = "yellow", alpha = 0.2))`. Default: segments
#'   whose name starts with `"L"` are shaded pale yellow, others not.
#' @param show_uncertainty_band Draw the band at `level = "summary"`.
#'   Default `TRUE`.
#' @param wells At `level = "well"`: `"vehicle_control"` (default) or
#'   `"all"` (slower to draw for many wells).
#' @param response_label Y-axis label. Default `"response value"`.
#' @param well_counts Optional [count_wells_by_dose()] result (grouped by
#'   at least `plate_id`, `is_vehicle_control`, `concentration`) from the
#'   full data; dose labels then show `"(kept/total)"`.
#' @param title_col Optional column holding the title (one value in `d`).
#' @param title_wrap_width Wrap the title at this many characters. Default
#'   `60`; `Inf` or `NULL` disables wrapping.
#'
#' @return A `ggplot` object.
#' @seealso [module2], [plot_raw_responses_pages()].
#'
#' @examples
#' d <- expand.grid(
#'   well_id = sprintf("A%02d", 1:8), time_value = 1:20,
#'   stringsAsFactors = FALSE
#' )
#' d$plate_id <- "P1"
#' d$is_vehicle_control <- d$well_id %in% sprintf("A%02d", 1:4)
#' d$concentration <- ifelse(d$is_vehicle_control, 0, 10)
#' d$keep_well <- TRUE
#' set.seed(1)
#' d$response_value <- stats::rpois(nrow(d), ifelse(d$time_value > 10, 20, 5))
#' plot_raw_responses(d, segments = list(L1 = 1:10, D1 = 11:20))
#' @export
plot_raw_responses <- function(
  d,
  segments,
  level = c("summary", "well"),
  summary_type = c("median", "mean"),
  summary_group_cols = "plate_id",
  missing_value_policy = c("impute_zero", "na_omit"),
  segment_style = NULL,
  show_uncertainty_band = TRUE,
  wells = c("vehicle_control", "all"),
  response_label = "response value",
  well_counts = NULL,
  title_col = NULL,
  title_wrap_width = 60
) {
  level <- match.arg(level)
  summary_type <- match.arg(summary_type)
  missing_value_policy <- match.arg(missing_value_policy)
  wells <- match.arg(wells)

  check_raw_plot_input(d, "plot_raw_responses")
  wrap_title(NULL, title_wrap_width, "plot_raw_responses")
  check_segments(segments)

  plot_title <- resolve_plot_title(d, title_col, "plot_raw_responses")

  if (!is.null(well_counts)) {
    d <- join_well_counts(d, well_counts)
    n_kept <- d$n_kept_wells
    n_total <- d$n_total_wells
  } else {
    n_kept <- NULL
    n_total <- NULL
  }

  concentration_unit <- if ("concentration_unit" %in% colnames(d)) {
    d$concentration_unit
  } else {
    NULL
  }
  d$dose_label <- make_dose_label(
    d$concentration,
    concentration_unit,
    d$is_vehicle_control,
    n_kept = n_kept,
    n_total = n_total
  )

  shading_df <- segments_to_shading_df(segments, segment_style)
  shading_layers <- purrr::pmap(
    shading_df,
    function(
      segment,
      xmin,
      xmax,
      fill,
      alpha
    ) {
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

  if (level == "well") {
    d_plot <- if (wells == "vehicle_control") {
      d |> dplyr::filter(.data$is_vehicle_control)
    } else {
      d
    }
    if (missing_value_policy == "impute_zero") {
      d_plot <- replace_na_response_zero(d_plot)
    } else {
      # missing_value_policy == "na_omit": leave response_value NA as-is.
      # ggplot2 draws a visible gap in the line/points at that timepoint
      # rather than a value -- useful for well-level QC, where a gap can
      # mean "the tracker lost this embryo" versus a zero, which could be
      # misread as "confirmed motionless." This is a real, deliberate
      # choice: the two policies show visually DIFFERENT traces for the
      # same well, not just a cosmetic difference (see
      # summarize_raw_responses() docs re: AUC math).
      #
      # Emit a classed package warning (rather than relying solely on
      # ggplot2's generic, delayed "removed N rows" message, which is not
      # package-specific, may be suppressed by report-generation tooling,
      # and does not report WHICH wells were affected) whenever rows are
      # actually dropped. na.rm = TRUE is then set on the geoms below to
      # avoid a duplicate, redundant generic ggplot2 warning.
      n_missing <- sum(is.na(d_plot$response_value))
      if (n_missing > 0) {
        affected_wells <- unique(
          d_plot$well_id[is.na(d_plot$response_value)]
        )
        rlang::warn(
          c(
            paste0(
              "plot_raw_responses(missing_value_policy = \"na_omit\"): ",
              n_missing,
              " response_value(s) are missing and will appear as gaps in the plotted line(s)."
            ),
            "i" = paste0(
              "Affected well(s): ",
              paste(affected_wells, collapse = ", ")
            )
          ),
          class = "zfbehavior_warning_missing_response_dropped"
        )
      }
    }

    # Individual well lines/colors are ALWAYS disambiguated by plate (a
    # compound plate_id+well_id key), regardless of how many plates are
    # present -- see the "Plate identity at level = 'well'" section above.
    # Two different physical larvae sharing the same well_id on different
    # plates (e.g. "A1" on two plates) must never share a `group` (which
    # would connect their points into one continuous, meaningless line) or
    # be assigned the same color.
    d_plot$plot_well_key <- paste(d_plot$plate_id, d_plot$well_id, sep = "@")

    well_levels <- d_plot |>
      dplyr::distinct(
        .data$plot_well_key,
        .data$concentration,
        .data$is_vehicle_control
      )
    well_colors <- sequential_well_colors_within_dose(
      well_levels$plot_well_key,
      well_levels$concentration,
      well_levels$is_vehicle_control
    )
    color_lookup <- rlang::set_names(well_colors, well_levels$plot_well_key)

    p <- ggplot2::ggplot()
    p <- p + shading_layers
    p <- p +
      ggplot2::geom_line(
        data = d_plot,
        mapping = ggplot2::aes(
          x = .data$time_value,
          y = .data$response_value,
          color = .data$plot_well_key,
          group = .data$plot_well_key
        ),
        inherit.aes = FALSE,
        # na.rm = TRUE: the na_omit package warning above already reports
        # dropped rows explicitly; this avoids a duplicate, redundant
        # generic ggplot2 "removed N rows" warning. impute_zero leaves no
        # NAs to begin with, so this is a no-op for that policy.
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        data = d_plot,
        mapping = ggplot2::aes(
          x = .data$time_value,
          y = .data$response_value,
          color = .data$plot_well_key,
          group = .data$plot_well_key
        ),
        size = 0.8,
        inherit.aes = FALSE,
        na.rm = TRUE
      ) +
      ggplot2::scale_color_manual(values = color_lookup, guide = "none") +
      ggplot2::facet_wrap(~dose_label, scales = "fixed") +
      ggplot2::labs(x = "time", y = response_label) +
      ggplot2::theme_bw()
  } else {
    group_cols <- union(
      c("dose_label", "concentration", "is_vehicle_control"),
      summary_group_cols
    )
    s <- summarize_raw_responses(
      d,
      group_cols = group_cols,
      summary_type = summary_type,
      missing_value_policy = missing_value_policy
    )

    # The plotted `group` aesthetic must match summarize_raw_responses()'s
    # OWN group_cols exactly (dose_label plus summary_group_cols), so a
    # per-plate (or otherwise per-summary_group_cols) statistic can never
    # be silently re-merged into one continuous line at render time -- the
    # `color`/`fill` aesthetic stays keyed on dose_label alone (same color
    # per dose, even when summary_group_cols keeps plates as separate
    # lines) since only ONE dose-based legend/color scheme is desired.
    plot_group_key_cols <- c("dose_label", summary_group_cols)
    s$plot_group_key <- if (length(summary_group_cols) > 0) {
      do.call(paste, c(as.list(s[plot_group_key_cols]), sep = "@"))
    } else {
      as.character(s$dose_label)
    }

    levels_df <- s |>
      dplyr::distinct(
        .data$dose_label,
        .data$concentration,
        .data$is_vehicle_control
      ) |>
      dplyr::arrange(.data$dose_label)
    color_lookup <- dose_color_map(
      levels_df$concentration,
      levels_df$is_vehicle_control
    )
    names(color_lookup) <- as.character(levels_df$dose_label)

    p <- ggplot2::ggplot()
    p <- p + shading_layers
    if (show_uncertainty_band) {
      p <- p +
        ggplot2::geom_ribbon(
          data = s,
          mapping = ggplot2::aes(
            x = .data$time_value,
            ymin = .data$lower,
            ymax = .data$upper,
            fill = .data$dose_label,
            group = .data$plot_group_key
          ),
          alpha = 0.2,
          inherit.aes = FALSE
        )
    }
    p <- p +
      ggplot2::geom_line(
        data = s,
        mapping = ggplot2::aes(
          x = .data$time_value,
          y = .data$center,
          color = .data$dose_label,
          group = .data$plot_group_key
        ),
        inherit.aes = FALSE
      ) +
      ggplot2::geom_point(
        data = s,
        mapping = ggplot2::aes(
          x = .data$time_value,
          y = .data$center,
          color = .data$dose_label,
          group = .data$plot_group_key
        ),
        size = 1,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_color_manual(values = color_lookup, name = "Dose") +
      ggplot2::labs(x = "time", y = response_label) +
      ggplot2::theme_bw()
    if (show_uncertainty_band) {
      p <- p + ggplot2::scale_fill_manual(values = color_lookup, guide = "none")
    }
  }

  if (!is.null(plot_title)) {
    p <- p + ggplot2::ggtitle(wrap_title(plot_title, title_wrap_width, "plot_raw_responses"))
  }

  p
}

#' One raw-response plot per group (internal)
#'
#' Splits `d` by `group_cols` and calls `plot_raw_responses()` once per
#' group; titles default to the group key. Used by
#' `plot_raw_responses_pages()`.
#'
#' @return A named list of `ggplot` objects.
#' @keywords internal
#' @noRd
plot_raw_responses_by_group <- function(
  d,
  group_cols,
  segments,
  level = c("summary", "well"),
  summary_type = c("median", "mean"),
  summary_group_cols = "plate_id",
  missing_value_policy = c("impute_zero", "na_omit"),
  segment_style = NULL,
  show_uncertainty_band = TRUE,
  wells = c("vehicle_control", "all"),
  response_label = "response value",
  well_counts = NULL,
  title_col = NULL,
  title_wrap_width = 60
) {
  check_raw_plot_input(d, "plot_raw_responses_by_group")

  missing_cols <- setdiff(group_cols, colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        "plot_raw_responses_by_group() requires `group_cols` that are missing from `d`.",
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", "))
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }

  grouped <- d |> dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  keys_df <- dplyr::group_keys(grouped)
  chunks <- dplyr::group_split(grouped, .keep = TRUE)

  names_vec <- keys_df |>
    tidyr::unite("key", dplyr::everything(), sep = "@") |>
    dplyr::pull("key")

  plots <- purrr::map(
    chunks,
    plot_raw_responses,
    segments = segments,
    level = level,
    summary_type = summary_type,
    summary_group_cols = summary_group_cols,
    missing_value_policy = missing_value_policy,
    segment_style = segment_style,
    show_uncertainty_band = show_uncertainty_band,
    wells = wells,
    response_label = response_label,
    well_counts = well_counts,
    title_col = title_col,
    title_wrap_width = title_wrap_width
  ) |>
    rlang::set_names(names_vec)

  if (is.null(title_col)) {
    # No caller-supplied title column: default every child plot's title to
    # its own generated group key, so a paginated/batched PDF never shows
    # several visually similar, unlabeled panels (see
    # plot_raw_responses_pages()). title_col, when supplied, already sets
    # a title inside plot_raw_responses() itself and is left untouched.
    plots <- purrr::map2(plots, names_vec, function(p, key) {
      p + ggplot2::ggtitle(wrap_title(key, title_wrap_width, "plot_raw_responses"))
    })
  }

  plots
}

#' Raw-response plots for many units, paginated
#'
#' Draws one [plot_raw_responses()] panel per unit (`group_cols`, e.g. one
#' plate) and arranges them in a grid per page for a multi-page PDF. Each
#' panel is titled with `title_col` or the unit key. Part of Module 2
#' (skills `raw-response-plots`, `interplate-vc-qc`; also used by
#' `intraplate-vc-qc`).
#'
#' The default grid suits `ggplot2::ggsave(width = 15, height = 10)`: 3 x 2
#' panels per page for `level = "summary"`, 2 x 2 for `level = "well"`.
#'
#' @inheritParams plot_raw_responses
#' @param d A tibble as for [plot_raw_responses()], with `group_cols`.
#' @param group_cols Columns defining one panel, e.g. `"plate_id"`.
#' @param nrow,ncol Panels per page, both or neither. Default `NULL` (see
#'   above).
#'
#' @return A paginated grob from `gridExtra::marrangeGrob()`; save with
#'   `ggplot2::ggsave()`.
#' @seealso [module2].
#' @export
plot_raw_responses_pages <- function(
  d,
  group_cols,
  segments,
  level = c("summary", "well"),
  summary_type = c("median", "mean"),
  summary_group_cols = "plate_id",
  missing_value_policy = c("impute_zero", "na_omit"),
  segment_style = NULL,
  show_uncertainty_band = TRUE,
  wells = c("vehicle_control", "all"),
  response_label = "response value",
  well_counts = NULL,
  title_col = NULL,
  title_wrap_width = 60,
  nrow = NULL,
  ncol = NULL
) {
  level <- match.arg(level)
  if (xor(is.null(nrow), is.null(ncol))) {
    rlang::abort(
      c(
        "plot_raw_responses_pages() requires `nrow` and `ncol` to be supplied together, or neither.",
        "i" = "Leave both NULL for the level-dependent default grid (summary: 3 x 2, well: 2 x 2)."
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  if (!is.null(nrow)) {
    is_valid_positive_int <- function(x) {
      is.numeric(x) &&
        length(x) == 1 &&
        is.finite(x) &&
        x > 0 &&
        x == round(x)
    }
    if (!is_valid_positive_int(nrow) || !is_valid_positive_int(ncol)) {
      rlang::abort(
        c(
          "plot_raw_responses_pages() requires `nrow`/`ncol` to be finite positive integer scalars.",
          "x" = paste0("Got nrow = ", nrow, ", ncol = ", ncol, ".")
        ),
        class = "zfbehavior_error_invalid_argument"
      )
    }
  }

  pls <- plot_raw_responses_by_group(
    d,
    group_cols = group_cols,
    segments = segments,
    level = level,
    summary_type = summary_type,
    summary_group_cols = summary_group_cols,
    missing_value_policy = missing_value_policy,
    segment_style = segment_style,
    show_uncertainty_band = show_uncertainty_band,
    wells = wells,
    response_label = response_label,
    well_counts = well_counts,
    title_col = title_col,
    title_wrap_width = title_wrap_width
  )

  if (is.null(nrow)) {
    if (level == "summary") {
      nrow <- 3
      ncol <- 2
    } else {
      nrow <- 2
      ncol <- 2
    }
  }
  gridExtra::marrangeGrob(pls, nrow = nrow, ncol = ncol, top = "")
}
