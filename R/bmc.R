#' Module 5: Curvep-based BMC modeling
#'
#' Module 5 of the zfbehavior workflow fits Curvep concentration-response
#' curves (package Rcurvep) to behavioral and developmental toxicity
#' endpoints and reports a benchmark concentration (BMC) and a hit
#' confidence per curve. It is run by the skills `behavior-endpoints-bmc`
#' and `devtox-endpoints-bmc`.
#'
#' @section Workflow:
#' \enumerate{
#'   \item Choose a benchmark response (BMR) per endpoint. It is the Curvep
#'     baseline threshold (`TRSH`): responses below it count as no effect.
#'     \itemize{
#'       \item From vehicle-control (VC) variability:
#'         [estimate_bmr_from_control()], median + 3 MAD of the VC values
#'         (skill default for Distance Change/Shift).
#'       \item From the data with Rcurvep's knee-point method:
#'         [estimate_bmr_curvep()] suggests a value that a person reviews
#'         (skill default for Movement Similarity and Total Distance; an
#'         option for devtox percent affected).
#'       \item A known value, e.g. from a prior study (skill default for
#'         devtox percent affected).
#'     }
#'   \item Combine the BMRs into one table (`endpoint`, `bmr`, `RNGE`), where
#'     `RNGE` gives the direction: positive for an increase, negative for a
#'     decrease, both for Total Distance.
#'   \item Run [run_bmc_curvep()] and plot with
#'     [plot_behavior_dose_response_pages()] (one panel per curve, BMC
#'     labels), [plot_behavior_bmc_activity_pages()] (overview of hits) or,
#'     for devtox, [plot_devtox_dose_response_pages()].
#' }
#'
#' @section Value scale:
#' BMR and BMC use the plate-normalized value `endpoint_value_norm` of
#' [generate_behavior_endpoints()]; the functions have no default for
#' `value_col`, so the scale is always a deliberate choice. Statistics
#' (Module 4) use `endpoint_value_raw` instead: ANOVA compares doses within
#' one plate, whereas a BMR is shared across plates and chemicals and needs
#' the normalized scale. For Distance Change/Shift the two columns are equal
#' (no normalization); for Movement Similarity and Total Distance they are
#' not.
#'
#' @section Data prepared by the caller:
#' Curvep takes treated concentrations only, on the log10 molar scale:
#' remove vehicle-control rows and transform the concentration (e.g.
#' `log10(concentration / 1e6)` for micromolar) before calling
#' [estimate_bmr_curvep()] or [run_bmc_curvep()]. The `chemical` column is
#' one dose-response unit: chemical x plate for per-plate curves, or the
#' chemical when plates are pooled.
#'
#' @section Monte Carlo and run time:
#' Curvep bootstraps each curve (`n_samples`, default 1000), so
#' `hit_confidence` and the BMC confidence interval vary slightly with the
#' seed and the R version; set `seed` for reproducible runs. A full batch
#' can take a long time. Rcurvep respects the active `future` plan, so run
#' `future::plan(future::multisession, workers = n)` before
#' [run_bmc_curvep()] to use several cores; the package never sets a plan.
#'
#' @references
#' Hsieh J-H, Ryan K, Sedykh A, et al. (2019). Application of benchmark
#' concentration (BMC) analysis on zebrafish data: a new perspective for
#' quantifying toxicity in alternative animal models. Toxicological Sciences
#' 167(1):92-104. \doi{10.1093/toxsci/kfy258}
#'
#' @seealso Skills `behavior-endpoints-bmc` and `devtox-endpoints-bmc`
#'   (`system.file("skills", package = "zfbehavior")`); the Rcurvep package.
#' @name bmc
#' @aliases module5
NULL

#' Required-columns check for BMC-module input (internal)
#'
#' @param d A tibble.
#' @param required_cols Character vector of required column names.
#' @param fn_name Name of the calling function, for the error message.
#' @param label Which input `required_cols` describes, for the error
#'   message (e.g. `"d"`, `"bmr_table"`, `"bmc_data"`). Default `"d"`.
#' @keywords internal
#' @noRd
check_bmc_columns <- function(d, required_cols, fn_name, label = "d") {
  missing_cols <- setdiff(unique(required_cols), colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires columns that are missing",
          if (label == "d") "." else paste0(" from `", label, "`.")
        ),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", "))
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  invisible(TRUE)
}

#' Require an explicit `value_col` for BMC/BMR functions (internal)
#'
#' @keywords internal
#' @noRd
check_value_col_supplied_bmc <- function(value_col, fn_name) {
  if (
    is.null(value_col) || !is.character(value_col) || length(value_col) != 1
  ) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires an explicit `value_col`."),
        "i" = "generate_behavior_endpoints() returns THREE value columns: endpoint_value_raw, endpoint_value_transformed, endpoint_value_norm.",
        "i" = "Use \"endpoint_value_norm\" for BMC/BMR (see the Value scale section of ?bmc)."
      ),
      class = "zfbehavior_error_missing_value_col"
    )
  }
  invisible(TRUE)
}

#' Estimate a benchmark response (BMR) from vehicle-control variability
#'
#' Computes `bmr = median(VC) + n_mad * MAD(VC)` from the
#' vehicle-control (VC) values, once per `group_cols` combination. This is
#' the default BMR of the Distance Change/Shift endpoints in Module 5 (Curvep
#' BMC modeling; skill `behavior-endpoints-bmc`). The direction (`RNGE`) is
#' added by the caller when building the BMR table for [run_bmc_curvep()].
#'
#' @section Scope (`group_cols`):
#' `group_cols` decides which VC values form one distribution:
#' \itemize{
#'   \item `"endpoint"`: all VC wells of all plates, one BMR per endpoint
#'     (global; the default of the skill).
#'   \item `c("endpoint", "plate_id")`: one BMR per plate.
#'   \item `c("endpoint", <chemical column>)`: one BMR per chemical, from the
#'     VC wells of that chemical's plates.
#' }
#'
#' @section Plate medians (`plate_col`):
#' With `plate_col`, each plate's VC wells are first reduced to their
#' median, and the median and MAD are taken over these plate medians. The
#' spread then reflects only differences between plates, not between wells,
#' which usually gives a lower BMR. It needs at least 3 plates per group and
#' only makes sense for endpoints that are not normalized to their own
#' plate's VC (Distance Change/Shift); otherwise all plate medians are equal
#' and the function stops with an error.
#'
#' @param d A tibble with `group_cols`, `value_col`, `is_vc_col`, and
#'   `plate_col` if given.
#' @param group_cols Columns defining one BMR each (see Scope).
#' @param value_col Value column; use `"endpoint_value_norm"` (no default,
#'   see the Value scale section of [bmc]).
#' @param is_vc_col Logical vehicle-control column. Default
#'   `"is_vehicle_control"`.
#' @param plate_col Optional plate column to use plate medians (see Plate
#'   medians). Default `NULL`.
#' @param n_mad Number of MADs added to the median. Default `3`.
#'
#' @return A tibble with `group_cols`, `bmr`, `n_values` (VC wells, or plates
#'   with `plate_col`), `vc_median` and `vc_mad` ([stats::mad()], scaled to
#'   approximate the standard deviation).
#'
#' @seealso [bmc] (Module 5), [estimate_bmr_curvep()], [run_bmc_curvep()].
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   endpoint = rep("D1@both_auc", 6),
#'   endpoint_value_norm = c(10, 12, 11, 9, 100, 8),
#'   is_vehicle_control = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
#' )
#' estimate_bmr_from_control(d, group_cols = "endpoint", value_col = "endpoint_value_norm")
#'
#' # Per-plate VC medians instead of VC wells:
#' d2 <- tibble::tibble(
#'   endpoint = "D1@both_auc",
#'   plate_id = rep(c("p1", "p2", "p3"), each = 4),
#'   endpoint_value_norm = c(10, 12, 11, 9, 20, 22, 21, 19, 5, 6, 5, 7),
#'   is_vehicle_control = TRUE
#' )
#' estimate_bmr_from_control(
#'   d2, group_cols = "endpoint", value_col = "endpoint_value_norm",
#'   plate_col = "plate_id"
#' )
estimate_bmr_from_control <- function(
  d,
  group_cols,
  value_col = NULL,
  is_vc_col = "is_vehicle_control",
  plate_col = NULL,
  n_mad = 3
) {
  fn_name <- "estimate_bmr_from_control"
  check_value_col_supplied_bmc(value_col, fn_name)
  check_bmc_columns(d, c(group_cols, value_col, is_vc_col, plate_col), fn_name)
  if (!is.numeric(n_mad) || length(n_mad) != 1 || !is.finite(n_mad) || n_mad < 0) {
    rlang::abort(paste0(fn_name, "() requires `n_mad` to be one non-negative number."),
                 class = "zfbehavior_error_invalid_argument")
  }
  if (!is.null(plate_col) && plate_col %in% group_cols) {
    rlang::abort(
      c(paste0(fn_name, "() cannot use `plate_col` when the plate is also in `group_cols`."),
        "x" = "One plate gives one median, so its MAD is 0."),
      class = "zfbehavior_error_invalid_argument"
    )
  }

  vc <- d[d[[is_vc_col]] %in% TRUE, , drop = FALSE]
  vc <- vc[!is.na(vc[[value_col]]), , drop = FALSE]
  vc$.v <- vc[[value_col]]
  if (!is.null(plate_col)) {
    vc <- vc |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, plate_col)))) |>
      dplyr::summarise(.v = stats::median(.data$.v), .groups = "drop")
  }
  out <- vc |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_values = dplyr::n(),
      vc_median = stats::median(.data$.v),
      vc_mad = stats::mad(.data$.v),
      .groups = "drop"
    ) |>
    dplyr::mutate(bmr = .data$vc_median + n_mad * .data$vc_mad, .before = "n_values")

  all_groups <- dplyr::distinct(d, dplyr::across(dplyr::all_of(group_cols)))
  no_vc <- dplyr::anti_join(all_groups, out, by = group_cols)
  if (nrow(no_vc) > 0) {
    rlang::abort(
      c(paste0(fn_name, "() found group(s) without vehicle-control values."),
        "x" = paste0(nrow(no_vc), " group(s), e.g. ",
                     paste(unlist(no_vc[1, ]), collapse = " / "))),
      class = "zfbehavior_error_no_vehicle_control",
      data = no_vc
    )
  }
  if (!is.null(plate_col)) {
    few <- out$n_values < 3
    flat <- !few & out$vc_mad == 0
    if (any(few) || any(flat)) {
      rlang::abort(
        c(paste0(fn_name, "() cannot use plate medians here."),
          "x" = if (any(few)) paste0(sum(few), " group(s) have fewer than 3 plates."),
          "x" = if (any(flat)) paste0(sum(flat), " group(s) have identical plate medians (MAD 0), ",
                                       "e.g. endpoints normalized to their own plate's VC median."),
          "i" = "Use plate medians only for non-normalized endpoints (Distance Change/Shift) with >= 3 plates per group."),
        class = "zfbehavior_error_invalid_plate_medians"
      )
    }
  }
  out
}

#' Suggest a BMR with Rcurvep's knee-point method
#'
#' Runs Curvep over a series of baseline thresholds (`TRSH`) and applies
#' `Rcurvep::estimate_dataset_bmr()` to suggest a BMR per endpoint and
#' direction. Part of Module 5 (Curvep BMC modeling; skills
#' `behavior-endpoints-bmc` and `devtox-endpoints-bmc`). It does not choose
#' the BMR: review the suggestion (`estimate$outcome`, `plot(estimate)`),
#' then put the chosen value in the BMR table of [run_bmc_curvep()].
#'
#' Works on continuous responses (`value_col`; behavioral endpoints) or on
#' counts (`n_in_col` and `n_col`; devtox percent affected).
#'
#' @param d A tibble with `endpoint_col`, `chemical_col`, `conc_col`, and
#'   either `value_col` or both `n_in_col` and `n_col`. Treated
#'   concentrations only (see the Data prepared by the caller section of
#'   [bmc]).
#' @param conc_col Column with the log10 molar concentration.
#' @param rnge Direction (Curvep `RNGE`): `1e6` for an increase, `-1e6` for a
#'   decrease, `c(-1e6, 1e6)` for both (one suggestion per direction).
#' @param n_samples Bootstrap samples per curve (e.g. `10` for behavior,
#'   `100` for devtox).
#' @param endpoint_col,chemical_col Columns with the endpoint and the
#'   dose-response unit (e.g. chemical x plate). Defaults `"endpoint"`,
#'   `"chemical"`.
#' @param value_col Continuous response column; use `"endpoint_value_norm"`.
#' @param n_in_col,n_col Affected and total count columns (count data;
#'   give both and no `value_col`).
#' @param trsh_seq Thresholds to try, shared by all endpoints. Default
#'   `seq(5, 95, by = 5)`, for percent-scale responses. `NULL` derives one
#'   series per endpoint from the data: 19 steps from 0 to the 95th
#'   percentile of the absolute response (for other scales, e.g. Distance
#'   Change/Shift).
#' @param seed Optional random seed.
#' @param p1,p2 Optional first and last points for the knee-point line
#'   (passed to `Rcurvep::estimate_dataset_bmr()`).
#'
#' @return A list:
#'   \describe{
#'     \item{`sweep`}{the `rcurvep` object of all thresholds; save it to
#'       re-estimate (e.g. with `p1`/`p2`) without re-running Curvep.}
#'     \item{`estimate`}{the `rcurvep_bmr` result; `estimate$outcome` has one
#'       row per endpoint and direction with the suggested BMR (`bmr_exp`,
#'       `bmr_ori`) and a `qc` flag.}
#'     \item{`trsh_seq`}{a tibble (`endpoint`, `trsh_from`, `trsh_to`,
#'       `trsh_by`, `n_trsh`) of the thresholds used.}
#'   }
#'
#' @seealso [bmc] (Module 5), [estimate_bmr_from_control()],
#'   [run_bmc_curvep()].
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' d <- expand.grid(
#'   chemical = paste0("chem", 1:6),
#'   conc = log10(c(0.1, 1, 10, 100) / 1e6),
#'   rep = 1:4
#' )
#' d$endpoint <- "LD_spearman"
#' potency <- c(-7, -6, -5, -4, 0, 0)[as.integer(d$chemical)]
#' d$endpoint_value_norm <- stats::rnorm(nrow(d), sd = 8) -
#'   80 * (potency != 0) / (1 + 10^(potency - d$conc))
#' est <- suppressWarnings(estimate_bmr_curvep(
#'   d, conc_col = "conc", rnge = -1e6, n_samples = 10,
#'   value_col = "endpoint_value_norm", seed = 1
#' ))
#' est$estimate$outcome
#' }
#' @export
estimate_bmr_curvep <- function(
  d,
  conc_col,
  rnge,
  n_samples,
  endpoint_col = "endpoint",
  chemical_col = "chemical",
  value_col = NULL,
  n_in_col = NULL,
  n_col = NULL,
  trsh_seq = seq(5, 95, by = 5),
  seed = NULL,
  p1 = NULL,
  p2 = NULL
) {
  fn_name <- "estimate_bmr_curvep"
  counts <- !is.null(n_in_col) || !is.null(n_col)
  if (counts && (is.null(n_in_col) || is.null(n_col) || !is.null(value_col))) {
    rlang::abort(
      c(
        paste0(fn_name, "() needs either `value_col` (continuous) or both `n_in_col` and `n_col` (counts)."),
        "x" = "Got an incomplete or mixed set of response columns."
      ),
      class = "zfbehavior_error_incomplete_dichotomous_cols"
    )
  }
  if (!counts) {
    check_value_col_supplied_bmc(value_col, fn_name)
  }
  if (!is.numeric(rnge) || !length(rnge) %in% 1:2 || any(!is.finite(rnge)) ||
      any(rnge == 0) || anyDuplicated(sign(rnge))) {
    rlang::abort(
      paste0(fn_name, "() requires `rnge` to be one non-zero number, or one negative and one positive number."),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  if (!is.null(trsh_seq) &&
      (!is.numeric(trsh_seq) || length(trsh_seq) < 4 || any(!is.finite(trsh_seq)) || any(trsh_seq < 0))) {
    rlang::abort(
      paste0(fn_name, "() requires `trsh_seq` to be NULL or at least 4 non-negative numbers."),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  if (!is.numeric(n_samples) || length(n_samples) != 1 || n_samples < 1) {
    rlang::abort(paste0(fn_name, "() requires `n_samples` to be a positive number."),
                 class = "zfbehavior_error_invalid_argument")
  }
  required_cols <- c(endpoint_col, chemical_col, conc_col,
                     if (counts) c(n_in_col, n_col) else value_col)
  check_bmc_columns(d, required_cols, fn_name)

  rcurvep_input <- if (counts) {
    dplyr::transmute(
      d,
      endpoint = .data[[endpoint_col]],
      chemical = .data[[chemical_col]],
      conc = .data[[conc_col]],
      n_in = .data[[n_in_col]],
      N = .data[[n_col]]
    )
  } else {
    dplyr::transmute(
      d,
      endpoint = .data[[endpoint_col]],
      chemical = .data[[chemical_col]],
      conc = .data[[conc_col]],
      resp = .data[[value_col]]
    )
  }
  if (any(!is.finite(rcurvep_input$conc))) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires `conc_col` to be finite for every row."),
        "i" = "Remove vehicle-control rows (log10(0) = -Inf) before estimating a BMR."
      ),
      class = "zfbehavior_error_invalid_conc"
    )
  }

  run_sweep <- function(dd, trsh) {
    Rcurvep::combi_run_rcurvep(
      dd,
      TRSH = trsh,
      RNGE = rnge,
      n_samples = n_samples,
      seed = if (is.null(seed)) NA else seed,
      keep_sets = "act_set"
    )
  }
  endpoints <- unique(rcurvep_input$endpoint)
  if (is.null(trsh_seq)) {
    trsh_list <- lapply(endpoints, function(ep) {
      dd <- rcurvep_input[rcurvep_input$endpoint == ep, , drop = FALSE]
      resp <- if (counts) 100 * dd$n_in / dd$N else dd$resp
      q95 <- unname(stats::quantile(abs(resp), 0.95, na.rm = TRUE))
      if (!is.finite(q95) || q95 <= 0) {
        rlang::abort(
          c(paste0(fn_name, "() cannot derive a TRSH range from the data."),
            "x" = paste0("Endpoint ", ep, ": the 95th percentile of the absolute response is 0."),
            "i" = "Supply `trsh_seq`."),
          class = "zfbehavior_error_invalid_argument"
        )
      }
      unique(signif(seq(0, q95, length.out = 20)[-1], 3))
    })
    names(trsh_list) <- endpoints
    sweeps <- lapply(endpoints, function(ep) {
      run_sweep(rcurvep_input[rcurvep_input$endpoint == ep, , drop = FALSE], trsh_list[[ep]])
    })
    sweep <- sweeps[[1]]
    sweep$result$act_set <- dplyr::bind_rows(lapply(sweeps, function(x) x$result$act_set))
    sweep$config$TRSH <- sort(unique(unlist(trsh_list)))
  } else {
    trsh_list <- stats::setNames(rep(list(trsh_seq), length(endpoints)), endpoints)
    sweep <- run_sweep(rcurvep_input, trsh_seq)
  }
  estimate <- Rcurvep::estimate_dataset_bmr(sweep, p1 = p1, p2 = p2, plot = FALSE)
  trsh_tbl <- tibble::tibble(
    endpoint = names(trsh_list),
    trsh_from = unname(vapply(trsh_list, min, numeric(1))),
    trsh_to = unname(vapply(trsh_list, max, numeric(1))),
    trsh_by = unname(vapply(trsh_list, function(x) signif(stats::median(diff(x)), 3), numeric(1))),
    n_trsh = unname(lengths(trsh_list))
  )
  list(sweep = sweep, estimate = estimate, trsh_seq = trsh_tbl)
}

#' Fit Curvep curves and compute BMCs from a BMR table
#'
#' For each row of `bmr_table`, runs `Rcurvep::combi_run_rcurvep()` on the
#' matching data with the row's `bmr` as baseline threshold (`TRSH`) and its
#' `RNGE` as direction, then summarizes with
#' `Rcurvep::summarize_rcurvep_output()`. Part of Module 5 (Curvep BMC
#' modeling; skills `behavior-endpoints-bmc` and `devtox-endpoints-bmc`).
#'
#' Works on continuous responses (`value_col`; behavioral endpoints) or on
#' counts (`n_in_col` and `n_col`; devtox percent affected, from
#' [calculate_devtox_endpoints()]).
#'
#' @section BMR table:
#' One row per endpoint and direction. Two rows for one endpoint, one with a
#' negative and one with a positive `RNGE`, fit both directions (e.g. Total
#' Distance); the two results are told apart by `RNGE`. For a BMR per plate
#' or per chemical, add the key column(s) to both `d` and `bmr_table` and
#' name them in `bmr_key_cols`. A duplicate row (same keys and direction) is
#' an error.
#'
#' @param d A tibble with `endpoint_col`, `chemical_col`, `conc_col`,
#'   `bmr_key_cols`, and either `value_col` or both `n_in_col` and `n_col`.
#'   Treated concentrations only (see the Data prepared by the caller
#'   section of [bmc]).
#' @param bmr_table A tibble with `endpoint_col`, `bmr_col`, `rnge_col`, and
#'   `bmr_key_cols` (see BMR table).
#' @param endpoint_col,chemical_col,conc_col Columns with the endpoint, the
#'   dose-response unit (e.g. chemical x plate), and the log10 molar
#'   concentration. Defaults `"endpoint"`, `"chemical"`, `"conc"`.
#' @param value_col Continuous response column; use `"endpoint_value_norm"`
#'   (no default, see the Value scale section of [bmc]).
#' @param n_in_col,n_col Affected and total count columns (count data; give
#'   both, `value_col` is then ignored).
#' @param bmr_col,rnge_col Columns of `bmr_table` with the BMR and the
#'   direction. Defaults `"bmr"`, `"RNGE"`.
#' @param bmr_key_cols Optional extra key columns (e.g. `"plate_id"`) present
#'   in both `d` and `bmr_table`. Default `NULL` (BMR per endpoint).
#' @param unmatched What to do with data that has no BMR row: `"error"`
#'   (default) or `"drop"` (listed in `unmatched_in_d`).
#' @param n_samples Bootstrap samples per curve. Default `1000`.
#' @param seed Optional random seed (see the Monte Carlo section of [bmc]).
#' @param inactivate Passed to `Rcurvep::summarize_rcurvep_output()`.
#'   Default `"INVERSE"`: curves flagged as inverse are not hits.
#' @param keep_sets Rcurvep result sets to keep. Default
#'   `c("act_set", "resp_set")`.
#'
#' @return A list:
#'   \describe{
#'     \item{`raw`}{named list of `rcurvep` objects, one per `bmr_table` row
#'       with data (names: endpoint, plus `bmr_key_cols` values, plus `"@up"`
#'       / `"@down"` when both directions are fitted).}
#'     \item{`act_summary`}{tibble with one row per curve: `endpoint`,
#'       `chemical`, `RNGE`, `hit_confidence`, `POD_med` (log10 molar BMC),
#'       `POD_cil`, `POD_ciu`, and other Curvep columns.}
#'     \item{`unmatched_in_d`}{tibble of keys in `d` without a BMR row.}
#'   }
#'
#' @seealso [bmc] (Module 5), [estimate_bmr_from_control()],
#'   [estimate_bmr_curvep()], [plot_behavior_dose_response_pages()],
#'   [plot_behavior_bmc_activity_pages()].
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' d <- expand.grid(
#'   chemical = c("A", "B"),
#'   conc = log10(c(0.1, 1, 10, 100) / 1e6),
#'   rep = 1:4
#' )
#' d$endpoint <- "D1@both_auc"
#' d$endpoint_value_norm <- stats::rnorm(nrow(d), sd = 5) +
#'   50 * (d$chemical == "A" & d$conc > -6)
#' bmr <- tibble::tibble(endpoint = "D1@both_auc", bmr = 20, RNGE = 1e6)
#' res <- run_bmc_curvep(
#'   d, bmr, value_col = "endpoint_value_norm", n_samples = 50, seed = 1
#' )
#' res$act_summary[, c("chemical", "hit_confidence", "POD_med")]
#' }
#' @export
run_bmc_curvep <- function(
  d,
  bmr_table,
  endpoint_col = "endpoint",
  chemical_col = "chemical",
  conc_col = "conc",
  value_col = NULL,
  n_in_col = NULL,
  n_col = NULL,
  bmr_col = "bmr",
  rnge_col = "RNGE",
  bmr_key_cols = NULL,
  unmatched = c("error", "drop"),
  n_samples = 1000,
  seed = NULL,
  inactivate = "INVERSE",
  keep_sets = c("act_set", "resp_set")
) {
  fn_name <- "run_bmc_curvep"
  unmatched <- match.arg(unmatched)

  dichotomous <- !is.null(n_in_col) || !is.null(n_col)
  if (dichotomous && (is.null(n_in_col) || is.null(n_col))) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires BOTH `n_in_col` and `n_col` to switch to dichotomous mode."
        ),
        "x" = paste0(
          "Supplied only: ",
          if (is.null(n_in_col)) "n_col" else "n_in_col"
        ),
        "i" = "Leave both NULL for continuous mode (using value_col), or supply both for dichotomous mode."
      ),
      class = "zfbehavior_error_incomplete_dichotomous_cols"
    )
  }
  if (!dichotomous) {
    check_value_col_supplied_bmc(value_col, fn_name)
  }

  required_cols <- c(
    endpoint_col,
    chemical_col,
    conc_col,
    bmr_key_cols,
    if (dichotomous) c(n_in_col, n_col) else value_col
  )
  check_bmc_columns(d, required_cols, fn_name, label = "d")

  required_bmr_cols <- c(endpoint_col, bmr_col, rnge_col, bmr_key_cols)
  check_bmc_columns(bmr_table, required_bmr_cols, fn_name, label = "bmr_table")

  rcurvep_input <- if (dichotomous) {
    d |>
      dplyr::transmute(
        endpoint = .data[[endpoint_col]],
        chemical = .data[[chemical_col]],
        conc = .data[[conc_col]],
        n_in = .data[[n_in_col]],
        N = .data[[n_col]]
      )
  } else {
    d |>
      dplyr::transmute(
        endpoint = .data[[endpoint_col]],
        chemical = .data[[chemical_col]],
        conc = .data[[conc_col]],
        resp = .data[[value_col]]
      )
  }
  if (!is.null(bmr_key_cols)) {
    rcurvep_input[bmr_key_cols] <- d[bmr_key_cols]
  }

  # Structural checks, so malformed input fails here with a clear error
  # rather than inside Rcurvep.
  if (!is.numeric(rcurvep_input$conc) || any(!is.finite(rcurvep_input$conc))) {
    rlang::abort(
      paste0(fn_name, "() requires `conc_col` to be finite numeric for every row."),
      class = "zfbehavior_error_invalid_conc"
    )
  }
  if (dichotomous) {
    n_val <- rcurvep_input$N
    n_in_val <- rcurvep_input$n_in
    bad_n <- !is.numeric(n_val) |
      !is.finite(n_val) |
      n_val != round(n_val) |
      n_val <= 0
    bad_n_in <- !is.numeric(n_in_val) |
      !is.finite(n_in_val) |
      n_in_val != round(n_in_val) |
      n_in_val < 0 |
      n_in_val > n_val
    if (any(bad_n) || any(bad_n_in)) {
      rlang::abort(
        c(
          paste0(
            fn_name,
            "() requires `n_col` to be a positive integer and `n_in_col` an integer with 0 <= n_in <= N, for every row."
          ),
          "x" = paste0(
            sum(bad_n | bad_n_in, na.rm = TRUE),
            " invalid row(s) found."
          )
        ),
        class = "zfbehavior_error_invalid_dichotomous_counts"
      )
    }
  }

  bmr_key_syms <- rlang::syms(bmr_key_cols)
  bmr_clean <- bmr_table |>
    dplyr::transmute(
      endpoint = .data[[endpoint_col]],
      bmr = .data[[bmr_col]],
      RNGE = .data[[rnge_col]],
      !!!stats::setNames(bmr_key_syms, bmr_key_cols)
    )

  join_cols <- c("endpoint", bmr_key_cols)
  # Up to two rows (one per direction, RNGE sign) per join key.
  dup_bmr <- bmr_clean |>
    dplyr::mutate(.dir = sign(.data$RNGE)) |>
    dplyr::count(dplyr::across(dplyr::all_of(c(join_cols, ".dir")))) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup_bmr) > 0) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires `bmr_table` to have at most one row per (",
          paste(join_cols, collapse = ", "),
          ") combination and direction (RNGE sign)."
        ),
        "x" = paste0(nrow(dup_bmr), " duplicated combination(s) found."),
        "i" = "For a BMR per plate or chemical, name the key column(s) in `bmr_key_cols` (see ?run_bmc_curvep)."
      ),
      class = "zfbehavior_error_duplicate_bmr_endpoint",
      data = dup_bmr
    )
  }

  # Endpoint (+ bmr_key_cols) combinations present in `d` but absent from
  # `bmr_table`: policy governed by `unmatched` (see @param unmatched).
  d_keys <- rcurvep_input |>
    dplyr::distinct(dplyr::across(dplyr::all_of(join_cols)))
  bmr_keys <- bmr_clean |>
    dplyr::distinct(dplyr::across(dplyr::all_of(join_cols)))
  unmatched_in_d <- dplyr::anti_join(d_keys, bmr_keys, by = join_cols)

  if (nrow(unmatched_in_d) > 0 && unmatched == "error") {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() found key combination(s) in `d` with no matching row in `bmr_table`."
        ),
        "x" = paste0(nrow(unmatched_in_d), " unmatched combination(s) found."),
        "i" = 'Pass `unmatched = "drop"` to silently exclude these (recorded in the result\'s `unmatched_in_d` element), or add the missing rows to `bmr_table`.'
      ),
      class = "zfbehavior_error_unmatched_bmr_endpoint",
      data = unmatched_in_d
    )
  }

  # bmr_table rows without data in `d` are skipped (nothing to fit).
  nested <- bmr_clean |>
    dplyr::nest_join(
      rcurvep_input,
      by = join_cols,
      keep = TRUE,
      name = "endpoint_data"
    ) |>
    dplyr::filter(purrr::map_int(.data$endpoint_data, nrow) > 0)

  seed_arg <- if (is.null(seed)) NA else seed

  result_names <- if (is.null(bmr_key_cols)) {
    nested$endpoint
  } else {
    nested |>
      tidyr::unite("zfbehavior_key", dplyr::all_of(join_cols), sep = "@") |>
      dplyr::pull("zfbehavior_key")
  }
  two_dir <- duplicated(result_names) | duplicated(result_names, fromLast = TRUE)
  result_names[two_dir] <- paste0(result_names[two_dir],
                                  ifelse(nested$RNGE[two_dir] > 0, "@up", "@down"))

  raw_results <- purrr::pmap(
    nested,
    function(...) {
      row <- list(...)
      # Rcurvep needs an exact column set: drop the join-only key columns.
      endpoint_data <- row$endpoint_data |>
        dplyr::select(-dplyr::any_of(bmr_key_cols))
      Rcurvep::combi_run_rcurvep(
        endpoint_data,
        TRSH = row$bmr,
        RNGE = row$RNGE,
        n_samples = n_samples,
        seed = seed_arg,
        keep_sets = keep_sets
      )
    }
  )
  names(raw_results) <- result_names

  act_summary <- purrr::map_df(
    raw_results,
    function(x) {
      Rcurvep::summarize_rcurvep_output(x, inactivate = inactivate)$act_summary
    }
  )

  list(raw = raw_results, act_summary = act_summary, unmatched_in_d = unmatched_in_d)
}

#' Dose-response plot of behavioral endpoints for one unit (plate or chemical)
#'
#' Boxplot + jittered points of one dose-response unit (typically one plate,
#' or one chemical pooled across plates), one row per endpoint (facets,
#' free y scale), dose groups on the x axis with the vehicle control first
#' (`"VC"`). Takes [generate_behavior_endpoints()] output directly, usually
#' filtered to one endpoint type (e.g. total distance). Pages of many units
#' are made with [plot_behavior_dose_response_pages()].
#'
#' Without `bmc_data` (default) only the responses are drawn; supplying it
#' (e.g. from [run_bmc_curvep()], back-transformed to the concentration unit)
#' adds a BMC / score label per endpoint. No back-transform or score
#' threshold is applied here: a row with `BMC = NA` draws no label.
#'
#' @param d [generate_behavior_endpoints()] output for one unit: columns
#'   `endpoint_col`, `concentration`, `is_vehicle_control`, `value_col`,
#'   optionally `concentration_unit` (x-axis title), and `point_colour_col`
#'   if supplied.
#' @param bmc_data Optional tibble with `endpoint_col`, `bmc_col`,
#'   `score_col` (and `batch_active_fraction_col` if supplied), at most one
#'   row per endpoint. Default `NULL` (no labels).
#' @param value_col Response column. Default `"endpoint_value_norm"` (the
#'   plate-normalized value).
#' @param endpoint_col Column naming the endpoint (one facet row each).
#'   Default `"endpoint"`.
#' @param endpoint_order Optional character vector giving the row order of
#'   endpoints; default the order of first appearance in `d` (or factor
#'   levels).
#' @param point_colour_col Optional column used to colour the points, e.g.
#'   `"plate_id"` when several plates are pooled into one unit. Default
#'   `NULL` (one colour). Boxes are always one per dose group.
#' @param y_label Y-axis label. Default `"normalized response"`.
#' @param batch_active_fraction_col Optional column in `bmc_data` giving the
#'   fraction (`0`-`1`) of plates in which the endpoint was active; `" &"`
#'   is appended to the label when `< 0.5`.
#' @param bmc_col,score_col Columns in `bmc_data`. Defaults `"BMC"`,
#'   `"score"`.
#' @param direction_col Optional column in `bmc_data` with `"up"` or
#'   `"down"` (e.g. Total Distance, fitted in both directions); the label
#'   then starts with an up or down arrow. Default `NULL`.
#' @param title Optional plot title.
#' @param title_wrap_width Wrap the title at this many characters. Default
#'   `40`; `Inf` or `NULL` disables wrapping.
#' @param facet_label_wrap_width Wrap endpoint (facet) labels at this many
#'   characters. Default `30`.
#'
#' @return A single `ggplot` object.
#' @keywords internal
#' @noRd
plot_behavior_dose_response <- function(
  d,
  bmc_data = NULL,
  value_col = "endpoint_value_norm",
  endpoint_col = "endpoint",
  endpoint_order = NULL,
  point_colour_col = NULL,
  y_label = "normalized response",
  batch_active_fraction_col = NULL,
  bmc_col = "BMC",
  score_col = "score",
  direction_col = NULL,
  title = NULL,
  title_wrap_width = 40,
  facet_label_wrap_width = 30
) {
  fn_name <- "plot_behavior_dose_response"
  check_bmc_columns(
    d,
    c(endpoint_col, "concentration", "is_vehicle_control", value_col, point_colour_col),
    fn_name,
    label = "d"
  )
  title <- wrap_title(title, title_wrap_width, fn_name)

  ep <- as.character(d[[endpoint_col]])
  ep_levels <- if (!is.null(endpoint_order)) {
    c(endpoint_order[endpoint_order %in% ep], setdiff(unique(ep), endpoint_order))
  } else if (is.factor(d[[endpoint_col]])) {
    intersect(levels(d[[endpoint_col]]), ep)
  } else {
    unique(ep)
  }

  dd <- tibble::tibble(
    endpoint = factor(ep, levels = ep_levels),
    conc = d$concentration,
    is_vc = d$is_vehicle_control,
    value = d[[value_col]]
  )
  if (!is.null(point_colour_col)) {
    dd$point_colour <- as.factor(d[[point_colour_col]])
  }

  # Factor level identity is the exact numeric concentration; the formatted
  # string is only the display label.
  conc_levels <- sort(unique(dd$conc[!dd$is_vc]))
  has_vc <- any(dd$is_vc)
  key_levels <- c(if (has_vc) "VC", as.character(conc_levels))
  display_labels <- c(if (has_vc) "VC", format(conc_levels, scientific = TRUE, digits = 2))
  dd$conc_label <- factor(ifelse(dd$is_vc, "VC", as.character(dd$conc)), levels = key_levels)

  x_lab <- if ("concentration_unit" %in% colnames(d)) {
    units <- unique(stats::na.omit(d$concentration_unit[!d$is_vehicle_control]))
    if (length(units) == 1) units else ""
  } else {
    ""
  }

  p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data$conc_label, y = .data$value)) +
    ggplot2::geom_boxplot(outlier.shape = NA, width = 0.5, alpha = 0.7, na.rm = TRUE)
  p <- if (is.null(point_colour_col)) {
    p + ggplot2::geom_jitter(size = 1, alpha = 0.7, width = 0.2, height = 0, na.rm = TRUE)
  } else {
    p +
      ggplot2::geom_jitter(
        ggplot2::aes(colour = .data$point_colour),
        size = 1, alpha = 0.8, width = 0.2, height = 0, na.rm = TRUE
      ) +
      ggplot2::guides(colour = ggplot2::guide_legend(point_colour_col))
  }

  if (!is.null(bmc_data)) {
    check_bmc_columns(
      bmc_data,
      c(endpoint_col, bmc_col, score_col, batch_active_fraction_col, direction_col),
      fn_name,
      label = "bmc_data"
    )
    ann <- tibble::tibble(
      endpoint = as.character(bmc_data[[endpoint_col]]),
      BMC = bmc_data[[bmc_col]],
      score = bmc_data[[score_col]],
      baf = if (is.null(batch_active_fraction_col)) NA_real_ else bmc_data[[batch_active_fraction_col]],
      dir = if (is.null(direction_col)) NA_character_ else as.character(bmc_data[[direction_col]])
    )
    ann <- ann[ann$endpoint %in% ep_levels & !is.na(ann$BMC), , drop = FALSE]
    if (anyDuplicated(ann$endpoint)) {
      rlang::abort(
        paste0(fn_name, "() requires at most one `bmc_data` row per endpoint."),
        class = "zfbehavior_error_invalid_argument"
      )
    }
    if (nrow(ann) > 0) {
      arrow <- ifelse(ann$dir %in% "up", "\u2191 ", ifelse(ann$dir %in% "down", "\u2193 ", ""))
      ann$label <- paste0(
        arrow, "BMC: ", format(ann$BMC, scientific = TRUE, digits = 3),
        "\nscore: ", round(ann$score, 3),
        ifelse(!is.na(ann$baf) & ann$baf < 0.5, " &", "")
      )
      ann$endpoint <- factor(ann$endpoint, levels = ep_levels)
      ann$conc_label <- factor(key_levels[1], levels = key_levels)
      ann$value <- Inf
      p <- p +
        ggplot2::geom_text(
          data = ann,
          mapping = ggplot2::aes(x = .data$conc_label, y = .data$value, label = .data$label),
          inherit.aes = FALSE,
          hjust = -0.1,
          vjust = 1.5,
          color = "blue",
          size = 3
        )
    }
  }

  p <- p +
    ggplot2::scale_x_discrete(x_lab, labels = display_labels, drop = FALSE) +
    ggplot2::scale_y_continuous(y_label) +
    ggplot2::facet_wrap(
      ~endpoint,
      ncol = 1,
      scales = "free_y",
      labeller = ggplot2::labeller(
        endpoint = function(x) stringr::str_wrap(x, width = facet_label_wrap_width)
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }
  p
}

#' One behavioral dose-response plot per group (internal)
#'
#' Splits `d` (and `bmc_data`) by `group_cols` and calls
#' `plot_behavior_dose_response()` once per group. Used by
#' [plot_behavior_dose_response_pages()].
#'
#' @return A named list of `ggplot` objects in `group_cols` order.
#' @keywords internal
#' @noRd
plot_behavior_dose_response_by_group <- function(
  d,
  group_cols,
  bmc_data = NULL,
  bmc_group_cols = NULL,
  title_col = NULL,
  ...
) {
  fn_name <- "plot_behavior_dose_response_pages"
  check_bmc_columns(d, c(group_cols, title_col), fn_name, label = "d")
  if (!is.null(bmc_data)) {
    if (is.null(bmc_group_cols)) bmc_group_cols <- group_cols
    if (length(bmc_group_cols) != length(group_cols)) {
      rlang::abort(
        c(
          paste0(fn_name, "() requires `bmc_group_cols` to be the same length as `group_cols`."),
          "x" = paste0("group_cols has ", length(group_cols), " element(s); bmc_group_cols has ",
                       length(bmc_group_cols), ".")
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
    title <- if (is.null(title_col)) names_vec[i] else resolve_plot_title(chunk, title_col, fn_name)
    plot_behavior_dose_response(chunk, bmc_data = bmc_chunk, title = title, ...)
  }) |>
    rlang::set_names(names_vec)
}

#' Behavioral dose-response plots for many units, paginated
#'
#' Draws one dose-response panel per unit (`group_cols`, e.g. one plate, or
#' one chemical with points coloured by plate): boxplots and points per dose
#' group, vehicle control first, one facet row per endpoint. With
#' `bmc_data`, each endpoint gets a BMC / score label. Panels are arranged
#' `nrow` x `ncol` per page for a multi-page PDF. Used in Module 3 (skill
#' `behavior-endpoints`) and Module 5 (skill `behavior-endpoints-bmc`).
#'
#' Panels follow the order of `group_cols` (use a factor to control it).
#' Make one PDF per endpoint type. The page height must grow with the number
#' of endpoint rows, otherwise the points are squashed: about 2.2 inches per
#' row plus 1.5 inches for titles and axes, at a width of about 15 inches for
#' 4 panels side by side, e.g.
#' `ggplot2::ggsave("total-distance.pdf", pages, width = 15, height = 1.5 +
#' 2.2 * n_rows, device = grDevices::cairo_pdf)` with `n_rows` the number of
#' endpoints plotted. `cairo_pdf` also handles non-ASCII labels.
#'
#' @param d [generate_behavior_endpoints()] output: `group_cols`,
#'   `endpoint_col`, `concentration`, `is_vehicle_control`, `value_col`, and
#'   optionally `concentration_unit` (x-axis title).
#' @param group_cols Columns defining one panel.
#' @param bmc_data Optional BMC table with `bmc_group_cols`, `endpoint_col`,
#'   `bmc_col`, `score_col`; at most one row per panel and endpoint. Rows
#'   with `BMC = NA` get no label. Default `NULL` (no labels).
#' @param bmc_group_cols Columns of `bmc_data` matching `group_cols`.
#'   Default `NULL` (same names).
#' @param title_col Optional column with the panel title. Default `NULL`:
#'   `group_cols` values joined by `"@"`.
#' @param value_col Response column. Default `"endpoint_value_norm"`.
#' @param endpoint_col Endpoint column. Default `"endpoint"`.
#' @param endpoint_order Optional endpoint (row) order. Default: order of
#'   appearance (or factor levels).
#' @param point_colour_col Optional column colouring the points (e.g.
#'   `"plate_id"` for pooled plates). Default `NULL`.
#' @param y_label Y-axis label. Default `"normalized response"`.
#' @param batch_active_fraction_col Optional `bmc_data` column with the
#'   fraction of plates where the endpoint was active; `" &"` is added to the
#'   label when it is below 0.5.
#' @param bmc_col,score_col `bmc_data` columns with the BMC (concentration
#'   unit, not log) and the score. Defaults `"BMC"`, `"score"`.
#' @param direction_col Optional `bmc_data` column with `"up"` or `"down"`;
#'   the label then starts with an arrow. Default `NULL`.
#' @param title_wrap_width,facet_label_wrap_width Wrap widths (characters)
#'   of panel titles and endpoint labels. Defaults `40`, `30`.
#' @param nrow,ncol Panels per page. Default `1` x `4`.
#'
#' @return A paginated grob from `gridExtra::marrangeGrob()`; save with
#'   `ggplot2::ggsave()`.
#'
#' @seealso [bmc] (Module 5), [run_bmc_curvep()].
#'
#' @examples
#' set.seed(1)
#' d <- tibble::tibble(
#'   plate_id = rep(c("P1", "P2"), each = 12),
#'   endpoint = "D1@both_auc",
#'   concentration = rep(rep(c(0, 1, 10), each = 4), 2),
#'   is_vehicle_control = concentration == 0,
#'   concentration_unit = "uM",
#'   endpoint_value_norm = c(rnorm(8, 10), rnorm(4, 40), rnorm(12, 10))
#' )
#' bmc <- tibble::tibble(
#'   plate_id = "P1", endpoint = "D1@both_auc", BMC = 3.2, score = 0.95
#' )
#' pages <- plot_behavior_dose_response_pages(
#'   d, "plate_id", bmc_data = bmc, ncol = 2
#' )
#' pages # draws the pages
#' @export
plot_behavior_dose_response_pages <- function(
  d,
  group_cols,
  bmc_data = NULL,
  bmc_group_cols = NULL,
  title_col = NULL,
  value_col = "endpoint_value_norm",
  endpoint_col = "endpoint",
  endpoint_order = NULL,
  point_colour_col = NULL,
  y_label = "normalized response",
  batch_active_fraction_col = NULL,
  bmc_col = "BMC",
  score_col = "score",
  direction_col = NULL,
  title_wrap_width = 40,
  facet_label_wrap_width = 30,
  nrow = 1,
  ncol = 4
) {
  is_pos_int <- function(x) {
    is.numeric(x) && length(x) == 1 && is.finite(x) && x > 0 && x == round(x)
  }
  if (!is_pos_int(nrow) || !is_pos_int(ncol)) {
    rlang::abort(
      c(
        "plot_behavior_dose_response_pages() requires `nrow`/`ncol` to be positive integer scalars.",
        "x" = paste0("Got nrow = ", format(nrow), ", ncol = ", format(ncol), ".")
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  pls <- plot_behavior_dose_response_by_group(
    d,
    group_cols = group_cols,
    bmc_data = bmc_data,
    bmc_group_cols = bmc_group_cols,
    title_col = title_col,
    value_col = value_col,
    endpoint_col = endpoint_col,
    endpoint_order = endpoint_order,
    point_colour_col = point_colour_col,
    y_label = y_label,
    batch_active_fraction_col = batch_active_fraction_col,
    bmc_col = bmc_col,
    score_col = score_col,
    direction_col = direction_col,
    title_wrap_width = title_wrap_width,
    facet_label_wrap_width = facet_label_wrap_width
  )
  gridExtra::marrangeGrob(pls, nrow = nrow, ncol = ncol, top = "")
}
