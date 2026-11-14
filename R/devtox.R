#' Columns needed for the developmental toxicity endpoint
#'
#' The canonical columns required by [calculate_devtox_endpoints()], besides
#' its `group_cols`. No time or response columns: the endpoint is per well.
#' Part of Module 3 (skill `devtox-endpoints`).
#'
#' @seealso [module3], [canonical-columns].
#' @return A character vector of required column names (excluding the
#'   caller-specified `group_cols`).
#' @export
#'
#' @examples
#' devtox_input_cols()
devtox_input_cols <- function() {
  c(
    "well_id",
    "concentration",
    "is_vehicle_control",
    "keep_well"
  )
}

#' Calculate the developmental toxicity endpoint (percent affected)
#'
#' Counts, per dose-response unit (`group_cols`) and concentration, the
#' larvae (`N`, wells present) and the affected larvae (`n_affected`, wells
#' with `keep_well = FALSE`: dead or malformed). Relies on the single-larva
#' well contract ([canonical-columns]). The input may have one row per
#' timepoint; it is reduced to wells. Part of Module 3 (skill
#' `devtox-endpoints`); BMCs are Module 5 ([run_bmc_curvep()] with
#' `n_in_col`, `n_col`).
#'
#' @section Per plate or pooled:
#' `group_cols = c("chemical_id", "plate_id")` gives one unit per plate.
#' A chemical column alone (e.g. `"DTXSID"`) pools the plates of a
#' chemical: wells at the same concentration are added up, and plates
#' tested at other concentrations add dose groups. To compare plates
#' instead, keep `plate_id` and draw one line per plate
#' ([plot_devtox_dose_response_pages()], `line_col`).
#'
#' @param d A tibble with the columns of [devtox_input_cols()],
#'   `group_cols`, and optionally `plate_id` (identifies wells across
#'   plates) and `concentration_unit`.
#' @param group_cols Columns defining one dose-response unit (no `NA`).
#' @param include_vehicle_control Keep the VC wells of each unit as a dose
#'   group (concentration 0). Default `TRUE`; drop these rows before BMC.
#'
#' @return A tibble, one row per unit x dose group, VC first: `group_cols`,
#'   `is_vehicle_control`, `concentration`, `concentration_unit` (if in
#'   `d`), `N`, `n_affected`, `pct_affected` (`100 * n_affected / N`) and
#'   `endpoint` (`"percent of affected"`). A unit with more than one
#'   concentration unit is an error.
#' @seealso [module3], [plot_devtox_dose_response_pages()].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   chemical_id = "chem1",
#'   plate_id = "plate1",
#'   well_id = paste0("W", 1:6),
#'   is_vehicle_control = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
#'   concentration = c(0, 0, 1, 1, 10, 10),
#'   concentration_unit = "uM",
#'   keep_well = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
#' )
#' calculate_devtox_endpoints(d, group_cols = c("chemical_id", "plate_id"))
calculate_devtox_endpoints <- function(
  d,
  group_cols,
  include_vehicle_control = TRUE
) {
  fn_name <- "calculate_devtox_endpoints"

  if (!is.character(group_cols) || length(group_cols) == 0) {
    rlang::abort(
      paste0(fn_name, "() requires `group_cols` to be a non-empty character vector."),
      class = "zfbehavior_error_invalid_argument"
    )
  }

  required_cols <- c(devtox_input_cols(), group_cols)
  missing_cols <- setdiff(unique(required_cols), colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires canonical columns that are missing."),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", ")),
        "i" = "See devtox_input_cols() and the group_cols argument."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }

  if (anyNA(d$keep_well)) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires `keep_well` to contain no NA values."),
        "x" = paste0("Found ", sum(is.na(d$keep_well)), " NA keep_well entrie(s)."),
        "i" = "keep_well == NA is not a confirmed classification -- resolve upstream."
      ),
      class = "zfbehavior_error_missing_keep_well"
    )
  }

  na_group <- vapply(group_cols, function(col) anyNA(d[[col]]), logical(1))
  if (any(na_group)) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires `group_cols` to contain no NA values."),
        "x" = paste0(
          "NA values in: ",
          paste(group_cols[na_group], collapse = ", ")
        ),
        "i" = "Choose a grouping column that is filled for every well (e.g. CHEMICAL_NAME when DTXSID is missing)."
      ),
      class = "zfbehavior_error_missing_group_cols"
    )
  }

  has_unit <- "concentration_unit" %in% colnames(d)
  well_key_cols <- intersect(c("plate_id", "well_id"), colnames(d))
  dedup_cols <- unique(c(
    group_cols,
    well_key_cols,
    "is_vehicle_control",
    "concentration",
    if (has_unit) "concentration_unit",
    "keep_well"
  ))
  d1 <- dplyr::distinct(d, dplyr::across(dplyr::all_of(dedup_cols)))

  dup_wells <- d1 |>
    dplyr::count(dplyr::across(dplyr::all_of(unique(c(group_cols, well_key_cols))))) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup_wells) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() found wells with more than one dose or keep_well value."),
        "x" = paste0(nrow(dup_wells), " well(s) are not well-level consistent."),
        "i" = "Each well must have one concentration, vehicle-control flag and keep_well value."
      ),
      class = "zfbehavior_error_inconsistent_well"
    )
  }

  if (!include_vehicle_control) {
    d1 <- dplyr::filter(d1, !.data$is_vehicle_control)
  }

  if (has_unit) {
    # The VC group's unit is not a dosing unit; only check treated wells.
    mixed <- d1 |>
      dplyr::filter(!.data$is_vehicle_control) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(c(group_cols, "concentration_unit")))) |>
      dplyr::count(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::filter(.data$n > 1)
    if (nrow(mixed) > 0) {
      rlang::abort(
        c(
          paste0(fn_name, "() found dose-response units mixing concentration units."),
          "x" = paste0(
            nrow(mixed),
            " unit(s) affected, e.g. ",
            paste(unlist(mixed[1, group_cols]), collapse = "@"),
            "."
          ),
          "i" = "Group per plate, or convert concentrations to one unit before pooling."
        ),
        class = "zfbehavior_error_mixed_concentration_units"
      )
    }
  }

  out_group_cols <- c(
    group_cols,
    "is_vehicle_control",
    "concentration",
    if (has_unit) "concentration_unit"
  )
  out <- d1 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(out_group_cols))) |>
    dplyr::summarise(
      N = dplyr::n(),
      n_affected = sum(!.data$keep_well),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct_affected = 100 * .data$n_affected / .data$N,
      endpoint = "percent of affected"
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols)),
      dplyr::desc(.data$is_vehicle_control),
      .data$concentration
    )

  if (has_unit && include_vehicle_control) {
    # A pooled unit may have VC rows with different (meaningless) units;
    # collapse them into one VC dose group carrying the treated unit.
    out <- collapse_vc_units(out, group_cols)
  }
  out
}

#' Collapse vehicle-control rows of a unit into one dose group
#'
#' Within one unit, VC wells from plates that recorded different
#' `concentration_unit` strings would otherwise form separate VC rows.
#' Merge them and label them with the unit's treated-well unit.
#' @keywords internal
#' @noRd
collapse_vc_units <- function(out, group_cols) {
  treated_unit <- out |>
    dplyr::filter(!.data$is_vehicle_control) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)), .data$concentration_unit) |>
    dplyr::rename(treated_unit = "concentration_unit")
  vc <- out |>
    dplyr::filter(.data$is_vehicle_control) |>
    dplyr::left_join(treated_unit, by = group_cols) |>
    dplyr::mutate(
      concentration_unit = dplyr::coalesce(.data$treated_unit, .data$concentration_unit)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      group_cols, "is_vehicle_control", "concentration", "concentration_unit"
    )))) |>
    dplyr::summarise(
      N = sum(.data$N),
      n_affected = sum(.data$n_affected),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct_affected = 100 * .data$n_affected / .data$N,
      endpoint = "percent of affected"
    )
  dplyr::bind_rows(vc, dplyr::filter(out, !.data$is_vehicle_control)) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols)),
      dplyr::desc(.data$is_vehicle_control),
      .data$concentration
    )
}

#' Dose-response plot of percent affected for one unit (internal)
#'
#' Points and line of percent affected per dose group, VC first, optional
#' BMC / score label. Used by `plot_devtox_dose_response_pages()`, which
#' documents the arguments.
#'
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
plot_devtox_dose_response <- function(
  d,
  bmc_data = NULL,
  line_col = NULL,
  batch_active_fraction_col = NULL,
  bmc_col = "BMC",
  bmcl_col = "BMCL",
  score_col = "score",
  title = NULL,
  title_wrap_width = 40
) {
  fn_name <- "plot_devtox_dose_response"
  title <- wrap_title(title, title_wrap_width, fn_name)
  check_bmc_columns(
    d,
    c("concentration", "pct_affected", "is_vehicle_control", line_col),
    fn_name,
    label = "d"
  )
  if (!is.null(bmc_data)) {
    check_bmc_columns(
      bmc_data,
      c(bmc_col, bmcl_col, score_col, batch_active_fraction_col),
      fn_name,
      label = "bmc_data"
    )
    if (nrow(bmc_data) > 1) {
      rlang::abort(
        c(
          paste0(fn_name, "() requires `bmc_data` to have at most one row."),
          "x" = paste0("Got ", nrow(bmc_data), " rows."),
          "i" = "Supply the BMC result of this one dose-response unit only."
        ),
        class = "zfbehavior_error_invalid_argument"
      )
    }
  }

  dd <- tibble::tibble(
    conc = d$concentration,
    resp = d$pct_affected,
    is_vc = d$is_vehicle_control,
    line = if (is.null(line_col)) "all" else as.character(d[[line_col]])
  )

  # Factor level identity is the exact numeric concentration; the
  # formatted string is only a display label (two concentrations that
  # format identically must not merge).
  conc_levels <- sort(unique(dd$conc[!dd$is_vc]))
  has_vc <- any(dd$is_vc)
  key_levels <- c(if (has_vc) "VC", as.character(conc_levels))
  display_labels <- c(
    if (has_vc) "VC",
    format(conc_levels, scientific = TRUE, digits = 2)
  )
  dd$conc_label <- factor(
    ifelse(dd$is_vc, "VC", as.character(dd$conc)),
    levels = key_levels
  )

  x_lab <- if ("concentration_unit" %in% colnames(d)) {
    units <- unique(stats::na.omit(d$concentration_unit[!d$is_vehicle_control]))
    if (length(units) == 1) units else ""
  } else {
    ""
  }

  p <- if (is.null(line_col)) {
    ggplot2::ggplot(
      dd,
      ggplot2::aes(x = .data$conc_label, y = .data$resp, group = .data$line)
    )
  } else {
    ggplot2::ggplot(
      dd,
      ggplot2::aes(
        x = .data$conc_label,
        y = .data$resp,
        group = .data$line,
        colour = .data$line
      )
    )
  }
  p <- p +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_line(alpha = 0.7)

  if (!is.null(bmc_data) && nrow(bmc_data) == 1) {
    bmc <- bmc_data[[bmc_col]]
    label <- if (is.na(bmc)) {
      NA_character_
    } else {
      paste0(
        "BMC: ", format(bmc, scientific = TRUE, digits = 3),
        "\nscore: ", round(bmc_data[[score_col]], 3)
      )
    }
    if (
      !is.na(label) &&
        !is.null(batch_active_fraction_col) &&
        isTRUE(bmc_data[[batch_active_fraction_col]] < 0.5)
    ) {
      label <- paste0(label, " &")
    }
    if (!is.na(label)) {
      annotation <- tibble::tibble(
        conc_label = factor(key_levels[1], levels = key_levels),
        resp = Inf,
        label = label
      )
      p <- p +
        ggplot2::geom_text(
          data = annotation,
          mapping = ggplot2::aes(x = .data$conc_label, y = .data$resp, label = .data$label),
          inherit.aes = FALSE,
          hjust = -0.1,
          vjust = 1.5,
          color = "blue",
          size = 3
        )
    }
  }

  p <- p +
    ggplot2::scale_y_continuous("percent affected", limits = c(0, 100)) +
    ggplot2::scale_x_discrete(x_lab, labels = display_labels, drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (!is.null(line_col)) {
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend(line_col))
  }
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }
  p
}

#' One devtox dose-response plot per group (internal)
#'
#' Splits `d` (and `bmc_data`) by `group_cols` and calls
#' `plot_devtox_dose_response()` once per group. Used by
#' `plot_devtox_dose_response_pages()`.
#'
#' @return A named list of `ggplot` objects (names: group values joined
#'   by `"@"`), in the order of `group_cols` (factor levels respected).
#' @keywords internal
#' @noRd
plot_devtox_dose_response_by_group <- function(
  d,
  group_cols,
  bmc_data = NULL,
  bmc_group_cols = NULL,
  line_col = NULL,
  title_col = NULL,
  batch_active_fraction_col = NULL,
  bmc_col = "BMC",
  bmcl_col = "BMCL",
  score_col = "score",
  title_wrap_width = 40
) {
  fn_name <- "plot_devtox_dose_response_pages"
  check_bmc_columns(d, c(group_cols, title_col), fn_name, label = "d")
  if (!is.null(bmc_data)) {
    if (is.null(bmc_group_cols)) {
      bmc_group_cols <- group_cols
    }
    if (length(bmc_group_cols) != length(group_cols)) {
      rlang::abort(
        c(
          paste0(fn_name, "() requires `bmc_group_cols` to be the same length as `group_cols`."),
          "x" = paste0(
            "group_cols has ", length(group_cols),
            " element(s); bmc_group_cols has ", length(bmc_group_cols), "."
          )
        ),
        class = "zfbehavior_error_mismatched_group_cols"
      )
    }
    check_bmc_columns(bmc_data, bmc_group_cols, fn_name, label = "bmc_data")
  }

  grouped <- dplyr::group_by(d, dplyr::across(dplyr::all_of(group_cols)))
  keys_df <- dplyr::group_keys(grouped)
  chunks <- dplyr::group_split(grouped, .keep = TRUE)
  names_vec <- keys_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::unite("key", dplyr::everything(), sep = "@") |>
    dplyr::pull("key")

  purrr::map(seq_along(chunks), function(i) {
    chunk <- chunks[[i]]
    bmc_chunk <- NULL
    if (!is.null(bmc_data)) {
      keep <- Reduce(`&`, purrr::map2(bmc_group_cols, group_cols, function(b, g) {
        as.character(bmc_data[[b]]) == as.character(keys_df[[g]][i])
      }))
      bmc_chunk <- bmc_data[keep %in% TRUE, , drop = FALSE]
      if (nrow(bmc_chunk) == 0) bmc_chunk <- NULL
    }
    title <- if (is.null(title_col)) {
      names_vec[i]
    } else {
      resolve_plot_title(chunk, title_col, fn_name)
    }
    plot_devtox_dose_response(
      chunk,
      bmc_data = bmc_chunk,
      line_col = line_col,
      batch_active_fraction_col = batch_active_fraction_col,
      bmc_col = bmc_col,
      bmcl_col = bmcl_col,
      score_col = score_col,
      title = title,
      title_wrap_width = title_wrap_width
    )
  }) |>
    rlang::set_names(names_vec)
}

#' Percent-affected dose-response plots for many units, paginated
#'
#' Draws one panel per unit (`group_cols`): points and line of percent
#' affected (0-100) per dose group, vehicle control first, doses evenly
#' spaced. With `bmc_data`, a BMC / score label is added (rows with
#' `BMC = NA` get none). Panels are arranged `nrow` x `ncol` per page for a
#' multi-page PDF. Panels
#' follow the order of `group_cols` (use a factor to control it). Part of
#' Module 3 (skill `devtox-endpoints`) and Module 5 (skill
#' `devtox-endpoints-bmc`). Typical layouts:
#' \itemize{
#'   \item One panel per plate: `group_cols = "plate_id"` on per-plate
#'     endpoints.
#'   \item One panel per chemical, one line per plate: `group_cols =
#'     "DTXSID"`, `line_col = "plate_id"` on per-plate endpoints.
#'   \item One panel per chemical, pooled: `group_cols = "DTXSID"` on
#'     endpoints calculated with `group_cols = "DTXSID"`.
#' }
#'
#' @param d [calculate_devtox_endpoints()] output: `group_cols`,
#'   `concentration`, `pct_affected`, `is_vehicle_control`, optionally
#'   `concentration_unit` (x-axis title) and `line_col`.
#' @param group_cols Columns defining one panel.
#' @param bmc_data Optional BMC table with `bmc_group_cols`, `bmc_col`,
#'   `bmcl_col`, `score_col` (BMC in the concentration unit, not log); at
#'   most one row per panel. Default `NULL` (no labels).
#' @param bmc_group_cols Columns of `bmc_data` matching `group_cols` (same
#'   length/order). Default `NULL` = `group_cols`.
#' @param title_col Optional column with one value per panel used as the
#'   panel title. Default `NULL`: the `group_cols` values joined by `"@"`.
#' @param line_col Optional column of `d` drawn as separate coloured lines,
#'   e.g. `"plate_id"` to compare the plates of one chemical. Default `NULL`.
#' @param batch_active_fraction_col Optional `bmc_data` column with the
#'   fraction of plates where the unit was active; `" &"` is added to the
#'   label below 0.5.
#' @param bmc_col,bmcl_col,score_col `bmc_data` columns. Defaults `"BMC"`,
#'   `"BMCL"`, `"score"`.
#' @param title_wrap_width Wrap panel titles at this many characters.
#'   Default `40`; `Inf` or `NULL` disables wrapping.
#' @param nrow,ncol Panels per page (rows, columns). Default `1` x `5`.
#'
#' @return A paginated grob from `gridExtra::marrangeGrob()`; save with
#'   `ggplot2::ggsave()`.
#' @seealso [module3], [module5].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   plate_id = rep(c("P1", "P2"), each = 9),
#'   well_id = rep(paste0("W", 1:9), 2),
#'   is_vehicle_control = rep(rep(c(TRUE, FALSE, FALSE), each = 3), 2),
#'   concentration = rep(rep(c(0, 1, 10), each = 3), 2),
#'   concentration_unit = "uM",
#'   keep_well = c(rep(TRUE, 5), FALSE, FALSE, FALSE, TRUE, rep(TRUE, 9))
#' )
#' e <- calculate_devtox_endpoints(d, group_cols = "plate_id")
#' bmc <- tibble::tibble(plate_id = "P1", BMC = 2.5, BMCL = 1.1, score = 0.9)
#' pages <- plot_devtox_dose_response_pages(
#'   e, "plate_id", bmc_data = bmc, ncol = 2
#' )
#' pages # draws the pages
plot_devtox_dose_response_pages <- function(
  d,
  group_cols,
  bmc_data = NULL,
  bmc_group_cols = NULL,
  line_col = NULL,
  title_col = NULL,
  batch_active_fraction_col = NULL,
  bmc_col = "BMC",
  bmcl_col = "BMCL",
  score_col = "score",
  title_wrap_width = 40,
  nrow = 1,
  ncol = 5
) {
  is_pos_int <- function(x) {
    is.numeric(x) && length(x) == 1 && is.finite(x) && x > 0 && x == round(x)
  }
  if (!is_pos_int(nrow) || !is_pos_int(ncol)) {
    rlang::abort(
      c(
        "plot_devtox_dose_response_pages() requires `nrow`/`ncol` to be positive integer scalars.",
        "x" = paste0("Got nrow = ", format(nrow), ", ncol = ", format(ncol), ".")
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  pls <- plot_devtox_dose_response_by_group(
    d,
    group_cols = group_cols,
    bmc_data = bmc_data,
    bmc_group_cols = bmc_group_cols,
    line_col = line_col,
    title_col = title_col,
    batch_active_fraction_col = batch_active_fraction_col,
    bmc_col = bmc_col,
    bmcl_col = bmcl_col,
    score_col = score_col,
    title_wrap_width = title_wrap_width
  )
  gridExtra::marrangeGrob(pls, nrow = nrow, ncol = ncol, top = "")
}
