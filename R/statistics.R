#' Module 4: ANOVA / repeated-measures ANOVA hit calling
#'
#' Module 4 of the zfbehavior workflow tests, for each plate, whether the
#' treated larvae differ from the vehicle control (Konrad et al. 2026,
#' Sections 2.5-2.6). It is run by the skill `behavior-endpoints-anova`.
#'
#' @section Workflow:
#' \enumerate{
#'   \item [run_anova()]: per plate and endpoint, a one-way ANOVA of
#'     concentration with Dunnett's test of each concentration against the
#'     vehicle control.
#'   \item [run_rmanova()]: per plate, a repeated-measures ANOVA over the
#'     Distance Change phases (each larva measured in every phase, AR(1)
#'     correlation between phases), with Dunnett's tests.
#'   \item [run_anova_rmanova_hit_calling()]: a hit needs a significant
#'     overall test AND at least one significant Dunnett comparison.
#'   \item [plot_behavior_anova_activity_pages()]: overview of the hits.
#' }
#'
#' @section Value scale and transforms:
#' The tests use `endpoint_value_raw` of [generate_behavior_endpoints()]
#' (within one plate no normalization is needed); `value_col` has no
#' default so the scale is a deliberate choice. BMC (Module 5) uses
#' `endpoint_value_norm` instead. Before testing, endpoints are transformed
#' towards normality (Konrad et al. 2026, Section 2.5): Distance Change
#' (`both_auc`) with log10, Movement Similarity with the Fisher z transform;
#' Total Distance and Distance Shift need none. By default only Distance
#' Change and Movement Similarity (Spearman) are tested; see
#' `endpoint_transforms`.
#'
#' @section Monte Carlo:
#' Dunnett p-values are computed by numerical integration with a random
#' component, so they vary slightly between runs; set a seed
#' (`set.seed()`) for reproducible results.
#'
#' @references
#' Konrad KS, Allen-Moyer K, Ellis L, et al. (2026). Power calculations for
#' larval zebrafish in light-dark transition test for developmental
#' neurotoxicity. NeuroToxicology 112:103371.
#' \doi{10.1016/j.neuro.2025.103371}
#'
#' @seealso Skill `behavior-endpoints-anova`
#'   (`system.file("skills", "behavior-endpoints-anova", package =
#'   "zfbehavior")`).
#' @name statistics
#' @aliases module4
NULL

#' Required-columns check for statistics-module input
#'
#' @param d A tibble.
#' @param required_cols Character vector of required column names.
#' @param fn_name Name of the calling function, for the error message.
#' @keywords internal
#' @noRd
check_statistics_input <- function(d, required_cols, fn_name) {
  missing_cols <- setdiff(unique(required_cols), colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires columns that are missing."),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", "))
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  check_no_excluded_wells(d, fn_name)
  invisible(TRUE)
}

#' Validate `value_col` is not `NULL` (no silent default)
#'
#' `run_anova()`/`run_rmanova()` require an explicit `value_col` (see the
#' Value scale section of `?statistics`).
#'
#' @keywords internal
#' @noRd
check_value_col_supplied <- function(value_col, fn_name) {
  if (is.null(value_col) || !is.character(value_col) || length(value_col) != 1) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires an explicit `value_col`."),
        "i" = "generate_behavior_endpoints() returns THREE value columns: endpoint_value_raw, endpoint_value_transformed, endpoint_value_norm.",
        "i" = "Use \"endpoint_value_raw\" for ANOVA/RMANOVA (see the Value scale section of ?statistics)."
      ),
      class = "zfbehavior_error_missing_value_col"
    )
  }
  invisible(TRUE)
}

#' Validate and identify the vehicle-control reference for Dunnett's test
#'
#' Guarantees a treated concentration cannot silently become Dunnett's
#' control reference. Requires:
#' \itemize{
#'   \item `is_vc` contains no `NA` (an unknown/unconfirmed control status
#'     is never treated as "OK").
#'   \item Exactly one distinct `conc_unit` label among `is_vc == TRUE`
#'     rows (a single, unambiguous control group).
#'   \item That label does not also appear among `is_vc == FALSE` rows
#'     (a treated dose cannot share the control's dose-label key).
#'   \item At least one FINITE control observation and at least one
#'     treated (`is_vc == FALSE`) dose level remain.
#' }
#'
#' @param conc_unit The `conc_unit` factor built by
#'   `build_conc_unit_factor()`.
#' @param is_vc Logical vehicle-control flag, same length as `conc_unit`.
#' @param value Numeric response value, same length as `conc_unit` (used
#'   to check at least one finite control observation exists).
#' @param fn_name Name of the calling function, for error messages.
#' @param group_key A one-row tibble (or `NULL`) identifying the current
#'   model group, included in the error for easier debugging.
#' @return The single control-group label (character scalar), for passing
#'   explicitly to `DescTools::DunnettTest(control = )` /
#'   `multcomp::contrMat(base = )`.
#' @keywords internal
#' @noRd
check_vc_reference <- function(conc_unit, is_vc, value, fn_name, group_key = NULL) {
  key_txt <- if (!is.null(group_key)) {
    paste0(" [", paste(names(group_key), unlist(group_key), sep = "=", collapse = ", "), "]")
  } else {
    ""
  }

  if (anyNA(is_vc)) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires a non-missing vehicle-control flag for every row."),
        "x" = paste0("Found ", sum(is.na(is_vc)), " NA value(s) in the control flag.", key_txt)
      ),
      class = "zfbehavior_error_invalid_vc_flag"
    )
  }

  vc_labels <- unique(as.character(conc_unit[is_vc]))
  if (length(vc_labels) == 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires at least one vehicle-control row per model group."),
        "x" = paste0("No `is_vc == TRUE` rows found.", key_txt)
      ),
      class = "zfbehavior_error_missing_vc_reference"
    )
  }
  if (length(vc_labels) > 1) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires exactly one vehicle-control dose key per model group."),
        "x" = paste0(
          "Found ", length(vc_labels), " distinct control dose key(s): ",
          paste(vc_labels, collapse = ", "), ".", key_txt
        )
      ),
      class = "zfbehavior_error_multiple_vc_references"
    )
  }
  vc_label <- vc_labels[[1]]

  treated_labels <- unique(as.character(conc_unit[!is_vc]))
  if (vc_label %in% treated_labels) {
    rlang::abort(
      c(
        paste0(fn_name, "() found treated row(s) sharing the vehicle-control's dose key."),
        "x" = paste0("Shared dose key: \"", vc_label, "\".", key_txt),
        "i" = "A dose key must not contain both control and treated rows."
      ),
      class = "zfbehavior_error_ambiguous_vc_reference"
    )
  }
  if (length(treated_labels) == 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires at least one treated dose level per model group."),
        "x" = paste0("Only the vehicle-control dose key (\"", vc_label, "\") is present.", key_txt)
      ),
      class = "zfbehavior_error_no_treated_dose"
    )
  }

  n_finite_vc <- sum(is.finite(value[is_vc]))
  if (n_finite_vc == 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires at least one finite vehicle-control observation."),
        "x" = paste0("All vehicle-control `value` entries are missing/non-finite.", key_txt)
      ),
      class = "zfbehavior_error_no_finite_vc_observation"
    )
  }

  vc_label
}

#' Build an ordered vehicle-control-first, ascending-concentration
#' `conc_unit` factor
#'
#' Each dose group's label is
#' `"<concentration><unit>"` (vehicle control included, e.g. `"0uM"`, NOT
#' `"VC"` -- unlike `make_dose_label()` in `R/raw-plots.R`, which is a
#' plotting label and uses `"VC"`), ordered with the vehicle-control group
#' first, then ascending concentration.
#'
#' \strong{This is a plain (unordered) `factor`}, not an R `ordered`
#' factor, despite the "ordered" wording in this function's name -- its
#' level ORDER is meaningful (vehicle-control first, matching
#' `DescTools::DunnettTest()`'s `control = levels(g)[1]`
#' default), but no `ordered`-class polynomial contrasts are intended or
#' applied. `run_anova()`/`run_rmanova()` additionally pass the control
#' label explicitly (via `check_vc_reference()`) to `DunnettTest(control
#' = )`/`contrMat(base = )` rather than relying on factor-level-1
#' ordering alone.
#'
#' @param concentration Numeric concentration values.
#' @param concentration_unit Character concentration units, or `NULL` if
#'   not available (in which case no unit suffix is added).
#' @param is_vehicle_control Logical vector, `TRUE` for vehicle-control
#'   rows.
#' @return A factor with levels in the required order (vehicle-control
#'   first, then ascending concentration) -- NOT an R `ordered` factor.
#' @keywords internal
#' @noRd
build_conc_unit_factor <- function(
  concentration,
  concentration_unit,
  is_vehicle_control
) {
  if (is.null(concentration_unit)) {
    concentration_unit <- rep("", length(concentration))
  }
  label <- paste0(concentration, concentration_unit)
  ord <- order(!is_vehicle_control, concentration)
  levels_ordered <- unique(label[ord])
  factor(label, levels = levels_ordered)
}

#' Resolve a `conc_unit_col` argument to the unit vector to use (or `NULL`)
#'
#' Concentration-unit policy: `conc_unit_col = NULL` is the ONLY valid "intentionally
#' unitless" signal. Any non-`NULL` name must actually exist in `d` (a
#' misspelled column name is a caller error, not silently-unitless data),
#' and every value within the (already split-by-group) vector must be
#' non-missing and non-empty.
#'
#' @param d A tibble.
#' @param conc_unit_col Column name, or `NULL`.
#' @param fn_name Name of the calling function, for error messages.
#' @return The unit character vector (`d[[conc_unit_col]]`), or `NULL` if
#'   `conc_unit_col` is `NULL`.
#' @keywords internal
#' @noRd
resolve_conc_unit_col <- function(d, conc_unit_col, fn_name) {
  if (is.null(conc_unit_col)) {
    return(NULL)
  }
  if (!conc_unit_col %in% colnames(d)) {
    rlang::abort(
      c(
        paste0(fn_name, "() was given a `conc_unit_col` that does not exist in `d`."),
        "x" = paste0("Column \"", conc_unit_col, "\" not found."),
        "i" = "Pass `conc_unit_col = NULL` if concentration is intentionally unitless."
      ),
      class = "zfbehavior_error_missing_conc_unit_col"
    )
  }
  d[[conc_unit_col]]
}

#' Validate a (per-group) concentration-unit vector has no missing/empty
#' values
#'
#' @keywords internal
#' @noRd
check_conc_unit_values <- function(conc_unit_vals, fn_name, group_key = NULL) {
  if (is.null(conc_unit_vals)) {
    return(invisible(TRUE))
  }
  key_txt <- if (!is.null(group_key)) {
    paste0(" [", paste(names(group_key), unlist(group_key), sep = "=", collapse = ", "), "]")
  } else {
    ""
  }
  bad <- is.na(conc_unit_vals) | conc_unit_vals == ""
  if (any(bad)) {
    rlang::abort(
      c(
        paste0(fn_name, "() found missing or empty concentration-unit value(s) within a model group."),
        "x" = paste0("Found ", sum(bad), " missing/empty unit value(s).", key_txt)
      ),
      class = "zfbehavior_error_invalid_conc_unit_value"
    )
  }
  invisible(TRUE)
}

#' Validate an `endpoint_transforms` mapping's structure
#'
#' Requires a named list
#' with unique, non-missing, non-empty pattern names, and each element a
#' single supported `transform_for_normality()` method string (rejects
#' `NULL`, multivalued, or unsupported method entries) -- rather than
#' allowing e.g. a `NULL` method to silently fall through to
#' `transform_for_normality()`'s first formal default (`"log10"`).
#'
#' @keywords internal
#' @noRd
validate_endpoint_transforms_mapping <- function(endpoint_transforms, fn_name) {
  if (
    !is.list(endpoint_transforms) ||
      is.null(names(endpoint_transforms)) ||
      any(names(endpoint_transforms) == "") ||
      anyNA(names(endpoint_transforms))
  ) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires `endpoint_transforms` to be a named list."),
        "i" = 'e.g. list(both_auc = "log10", spearman = "fisher_z")'
      ),
      class = "zfbehavior_error_invalid_endpoint_transforms"
    )
  }
  if (anyDuplicated(names(endpoint_transforms)) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires unique `endpoint_transforms` pattern names."),
        "x" = paste0(
          "Duplicated pattern name(s): ",
          paste(unique(names(endpoint_transforms)[duplicated(names(endpoint_transforms))]), collapse = ", ")
        )
      ),
      class = "zfbehavior_error_invalid_endpoint_transforms"
    )
  }

  supported_methods <- c("log10", "fisher_z", "none")
  bad <- vapply(
    endpoint_transforms,
    function(m) is.null(m) || !is.character(m) || length(m) != 1 || !(m %in% supported_methods),
    logical(1)
  )
  if (any(bad)) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires each `endpoint_transforms` value to be exactly one supported method."),
        "x" = paste0(
          "Invalid method for pattern(s): ",
          paste(names(endpoint_transforms)[bad], collapse = ", ")
        ),
        "i" = paste0("Supported methods: ", paste(supported_methods, collapse = ", "), ".")
      ),
      class = "zfbehavior_error_invalid_endpoint_transforms"
    )
  }
  invisible(TRUE)
}

#' Warn (do not block) on a transform's input domain immediately before
#' applying it
#'
#' `log10(0)`/`log10(<0)` silently produce `-Inf`/`NaN`; Fisher-Z at
#' exactly `1`/`-1` silently produces `Inf`. Movement Similarity
#' legitimately reaching exactly `1` is an intentional, documented
#' possible endpoint value elsewhere in this package (see
#' `cal_mean_similarity()`, `R/endpoints.R`), so a domain violation here
#' is not always a caller mistake -- this is deliberately a WARNING, not
#' a blocking error: the transform is applied as-is (producing whatever
#' `-Inf`/`NaN`/`Inf` value `log10()`/the Fisher-Z formula naturally
#' would), and the caller is informed which pattern/how many rows were
#' affected, rather than the analysis being halted outright. Any
#' resulting non-finite value that would break `aov()`/`gls()` is still
#' caught separately, as a hard error, by
#' `check_finite_model_values()` immediately before fitting -- that
#' guard is NOT relaxed by this one.
#'
#' @keywords internal
#' @noRd
check_transform_domain <- function(x, method, pattern, fn_name) {
  if (method == "log10") {
    bad <- is.finite(x) & x <= 0
    if (any(bad)) {
      rlang::warn(
        c(
          paste0(fn_name, "() found endpoint value(s) outside log10's valid domain (x > 0)."),
          "x" = paste0(sum(bad), " value(s) <= 0 for pattern \"", pattern, "\"."),
          "i" = "log10(0) = -Inf and log10(<0) = NaN; the transform is applied as-is and these values will pass through unless caught downstream."
        ),
        class = "zfbehavior_warning_transform_domain"
      )
    }
  } else if (method == "fisher_z") {
    bad <- is.finite(x) & abs(x) >= 1
    if (any(bad)) {
      rlang::warn(
        c(
          paste0(fn_name, "() found endpoint value(s) outside the Fisher-Z transform's valid domain (-1 < x < 1)."),
          "x" = paste0(sum(bad), " value(s) with |x| >= 1 for pattern \"", pattern, "\"."),
          "i" = "Fisher-Z at |x| == 1 is +/-Inf; this is a legitimate possible Movement Similarity value elsewhere in this package, so it is surfaced as a warning rather than blocked -- the transform is applied as-is."
        ),
        class = "zfbehavior_warning_transform_domain"
      )
    }
  }
  invisible(TRUE)
}

#' Warn (do not block) if a transform's OUTPUT is non-finite immediately
#' after applying it
#'
#' Companion to `check_transform_domain()` -- same warn-don't-block
#' philosophy: a non-finite transformed value is surfaced to the caller,
#' but does not itself halt `apply_endpoint_transforms()`. If the value
#' is genuinely unusable, `check_finite_model_values()` still hard-errors
#' immediately before `aov()`/`gls()` is fit.
#'
#' @keywords internal
#' @noRd
check_transform_output_finite <- function(x, pattern, fn_name) {
  bad <- !is.finite(x)
  if (any(bad)) {
    rlang::warn(
      c(
        paste0(fn_name, "() produced non-finite value(s) after transforming pattern \"", pattern, "\"."),
        "x" = paste0(sum(bad), " non-finite (NA/NaN/Inf) value(s) after transformation.")
      ),
      class = "zfbehavior_warning_non_finite_transform_output"
    )
  }
  invisible(TRUE)
}

#' Filter to, and transform, rows matching an endpoint-transform mapping
#'
#' `endpoint_transforms` is a named list
#' where each NAME is a fixed-string pattern matched against
#' `endpoint_col` (via `grepl(fixed = TRUE)`), and each VALUE is the
#' `transform_for_normality()` `method` to apply to matching rows. Rows
#' matching more than one pattern raise an error (ambiguous, ergo not
#' silently resolved). `endpoint_transforms = NULL` skips this step
#' entirely -- `d` is used as-is, with no filtering or transformation
#' (the caller is then responsible for any pre-filtering/
#' pre-transformation, e.g. via `transform_for_normality()` directly).
#'
#' @section Zero-match handling (`unmatched`):
#' Rows matching NO `endpoint_transforms` pattern are handled per the
#' `unmatched` argument:
#' \describe{
#'   \item{`"drop"`}{(Default.) Silently excluded from the returned data.}
#'   \item{`"error"`}{Raises a classed error naming the affected
#'     `endpoint_col` values -- use this when every row is expected to
#'     match, so an unexpectedly-unmatched endpoint (e.g. a rename or a
#'     newly added endpoint type) is caught immediately.}
#'   \item{`"keep"`}{Unmatched rows are kept UNTRANSFORMED (their
#'     `value_col` is left as-is) -- use this when some endpoints
#'     legitimately require no transform and are not worth listing
#'     explicitly in `endpoint_transforms`.}
#' }
#' If, after applying `unmatched`, zero rows remain, a classed
#' `zfbehavior_error_no_analyzable_rows` error is raised (there is
#' nothing left to analyze).
#'
#' @section Transform domain/output validation is a warning, not a blocking error:
#' Before applying `log10`/`fisher_z`, out-of-domain input values (e.g.
#' `log10(x <= 0)`, `fisher_z(|x| >= 1)`) and non-finite transformed
#' output are surfaced via a classed `rlang::warn()`
#' (`zfbehavior_warning_transform_domain`/
#' `zfbehavior_warning_non_finite_transform_output`) -- NOT a blocking
#' error. The transform is applied as-is regardless (producing whatever
#' `-Inf`/`NaN`/`Inf` value naturally results), since some boundary
#' values are legitimate, expected endpoint values elsewhere in this
#' package (e.g. Movement Similarity exactly `1`), not necessarily a
#' caller mistake. If a resulting non-finite value would actually break
#' `aov()`/`gls()`, that is caught separately -- as a hard,
#' non-warning-suppressible error -- by `check_finite_model_values()`
#' immediately before the model is fit; this function's own warnings are
#' purely informational, not a substitute for that final guard.
#'
#' @param d A tibble.
#' @param endpoint_col Column name identifying each row's endpoint.
#' @param value_col Column name to transform in place.
#' @param endpoint_transforms Named list (pattern = transform method), or
#'   `NULL`.
#' @param fn_name Name of the calling function, for error messages.
#' @param unmatched One of `"drop"` (default), `"error"`, or `"keep"` --
#'   see `@section Zero-match handling` above.
#' @return `d`, filtered and with `value_col` transformed, per
#'   `endpoint_transforms`.
#' @keywords internal
#' @noRd
apply_endpoint_transforms <- function(
  d,
  endpoint_col,
  value_col,
  endpoint_transforms,
  fn_name,
  unmatched = c("drop", "error", "keep")
) {
  unmatched <- match.arg(unmatched)
  if (is.null(endpoint_transforms)) {
    return(d)
  }
  validate_endpoint_transforms_mapping(endpoint_transforms, fn_name)

  patterns <- names(endpoint_transforms)
  match_mat <- vapply(
    patterns,
    function(p) grepl(p, d[[endpoint_col]], fixed = TRUE),
    logical(nrow(d))
  )
  if (is.null(dim(match_mat))) {
    # nrow(d) == 1: vapply drops to a plain vector; restore matrix shape.
    match_mat <- matrix(match_mat, nrow = 1, dimnames = list(NULL, patterns))
  }
  n_matches <- rowSums(match_mat)

  if (any(n_matches > 1)) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() found endpoint value(s) matching more than one `endpoint_transforms` pattern."
        ),
        "x" = "Patterns must be non-overlapping (each endpoint should match at most one).",
        "i" = paste0("Patterns given: ", paste(patterns, collapse = ", "))
      ),
      class = "zfbehavior_error_ambiguous_endpoint_transform"
    )
  }

  no_match <- n_matches == 0
  if (any(no_match)) {
    if (unmatched == "error") {
      rlang::abort(
        c(
          paste0(fn_name, "() found endpoint value(s) matching no `endpoint_transforms` pattern."),
          "x" = paste0(
            sum(no_match), " row(s), endpoint value(s): ",
            paste(unique(d[[endpoint_col]][no_match]), collapse = ", ")
          ),
          "i" = 'Pass `unmatched = "drop"` to silently exclude these rows, or `unmatched = "keep"` to keep them untransformed.'
        ),
        class = "zfbehavior_error_unmatched_endpoint"
      )
    } else if (unmatched == "drop") {
      d <- d[!no_match, , drop = FALSE]
      match_mat <- match_mat[!no_match, , drop = FALSE]
      n_matches <- n_matches[!no_match]
    }
    # unmatched == "keep": rows are kept, left untransformed below.
  }

  if (nrow(d) == 0) {
    rlang::abort(
      paste0(fn_name, "() has no analyzable rows remaining after endpoint-transform filtering."),
      class = "zfbehavior_error_no_analyzable_rows"
    )
  }

  match_idx <- apply(match_mat, 1, function(row) {
    w <- which(row)
    if (length(w) == 0) NA_integer_ else w[1]
  })

  for (i in seq_along(patterns)) {
    rows <- !is.na(match_idx) & match_idx == i
    if (any(rows)) {
      method <- endpoint_transforms[[i]]
      check_transform_domain(d[[value_col]][rows], method, patterns[i], fn_name)
      transformed <- transform_for_normality(d[[value_col]][rows], method = method)
      check_transform_output_finite(transformed, patterns[i], fn_name)
      d[[value_col]][rows] <- transformed
    }
  }
  d
}

#' Validate model-input values are numeric and finite immediately before fitting
#'
#' Even after `apply_endpoint_transforms()`
#' (which validates only the rows it actually transforms), a caller's raw
#' or untransformed `value_col` could still contain non-finite values
#' (e.g. `endpoint_transforms = NULL`, or `unmatched = "keep"` rows).
#' This is a final guard immediately before `aov()`/`gls()`.
#'
#' @keywords internal
#' @noRd
check_finite_model_values <- function(value, fn_name, group_key = NULL) {
  key_txt <- if (!is.null(group_key)) {
    paste0(" [", paste(names(group_key), unlist(group_key), sep = "=", collapse = ", "), "]")
  } else {
    ""
  }
  if (!is.numeric(value)) {
    rlang::abort(
      paste0(fn_name, "() requires a numeric `value_col`.", key_txt),
      class = "zfbehavior_error_invalid_input"
    )
  }
  bad <- !is.finite(value)
  if (any(bad)) {
    rlang::abort(
      c(
        paste0(fn_name, "() found non-finite (NA/NaN/Inf) `value_col` entrie(s) reaching the model."),
        "x" = paste0("Found ", sum(bad), " non-finite value(s).", key_txt)
      ),
      class = "zfbehavior_error_non_finite_model_input"
    )
  }
  invisible(TRUE)
}

#' Validate a vector of p-values, allowing `NA` (non-estimable) but
#' rejecting NaN/Inf/out-of-range as errors
#'
#' A p-value must be a real probability
#' (`[0, 1]`) or genuinely missing (`NA`, meaning "not estimable/not
#' computed") -- `NaN`/`Inf`/negative/`>1` values are not legitimate
#' missingness, they indicate an upstream computation error and must be
#' rejected loudly rather than silently propagating into hit-calling
#' arithmetic.
#'
#' @param p Numeric vector of p-values (may contain `NA`).
#' @param what A short description of the p-value's source, for the error
#'   message (e.g. `"Dunnett post-hoc p-value"`).
#' @param fn_name Name of the calling function, for error messages.
#' @keywords internal
#' @noRd
check_valid_pvalues <- function(p, what, fn_name) {
  # NOTE: is.na(NaN) is TRUE in base R, so NaN must be excluded from "p"
  # BEFORE filtering by is.na(), not treated as if it were legitimate
  # missingness -- otherwise is.nan() below would always see zero NaN
  # values (they'd already have been silently filtered out as "NA").
  is_nan <- is.nan(p)
  non_na <- p[!is.na(p) & !is_nan]
  bad <- is.infinite(non_na) | non_na < 0 | non_na > 1
  if (any(is_nan) || any(bad)) {
    rlang::abort(
      c(
        paste0(fn_name, "() found invalid ", what, "(s)."),
        "x" = paste0(sum(is_nan) + sum(bad), " value(s) are NaN, infinite, negative, or greater than 1."),
        "i" = "A p-value must be NA (non-estimable) or a finite number in [0, 1]."
      ),
      class = "zfbehavior_error_invalid_pvalue"
    )
  }
  invisible(TRUE)
}

#' Validate `alpha` is a finite, non-missing numeric scalar strictly in (0, 1)
#'
#' Checks `is.na()`/`is.finite()` explicitly first, so every invalid
#' `alpha` value reaches the SAME classed
#' `zfbehavior_error_invalid_alpha` error.
#'
#' @keywords internal
#' @noRd
check_valid_alpha <- function(alpha, fn_name) {
  invalid <- !is.numeric(alpha) ||
    length(alpha) != 1 ||
    is.na(alpha) ||
    !is.finite(alpha) ||
    alpha <= 0 ||
    alpha >= 1
  if (invalid) {
    rlang::abort(
      paste0(fn_name, "() requires `alpha` to be a single finite number in (0, 1)."),
      class = "zfbehavior_error_invalid_alpha"
    )
  }
  invisible(TRUE)
}

#' Clean a `summary(aov(...))` object into a tidy one-row tibble
#' @keywords internal
#' @noRd
clean_anova_summary <- function(a) {
  tab <- a[[1]]
  tibble::tibble(
    df_1 = tab$Df[1],
    df_2 = tab$Df[2],
    F_value = tab$`F value`[1],
    p_value = tab$`Pr(>F)`[1]
  )
}

#' Clean a `DescTools::DunnettTest()` result into a tidy tibble
#' @keywords internal
#' @noRd
clean_dunnett_result <- function(dt) {
  tab <- as.data.frame(dt[[1]])
  tab <- tibble::rownames_to_column(tab, var = "pairwise_comp")
  tab <- tibble::as_tibble(tab)
  dplyr::rename(
    tab,
    lwr_ci = "lwr.ci",
    upr_ci = "upr.ci",
    p_value = "pval"
  )
}

#' Clean a `summary(multcomp::glht(...))` result into a tidy tibble
#' @keywords internal
#' @noRd
clean_glht_result <- function(s) {
  test <- s$test
  tibble::tibble(
    pairwise_comp = names(test$coefficients),
    coef = as.numeric(test$coefficients),
    sigma = as.numeric(test$sigma),
    zstat = as.numeric(test$tstat),
    p_value = as.numeric(test$pvalues)
  )
}

#' Minimum group-size / factor-level guardrail shared by run_anova()/run_rmanova()
#' @keywords internal
#' @noRd
check_min_levels <- function(f, min_levels, what, fn_name, group_key = NULL) {
  n <- nlevels(droplevels(f))
  if (n < min_levels) {
    key_txt <- if (!is.null(group_key)) {
      paste0(" [", paste(names(group_key), unlist(group_key), sep = "=", collapse = ", "), "]")
    } else {
      ""
    }
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires at least ",
          min_levels,
          " distinct ",
          what,
          " level(s) within a group."
        ),
        "x" = paste0("Found ", n, ".", key_txt)
      ),
      class = "zfbehavior_error_insufficient_levels"
    )
  }
  invisible(TRUE)
}

#' Attach a lightweight `call_params` attribute recording the
#' configuration that produced a result
#'
#' Enough for a caller/reviewer to
#' audit what configuration produced a given result without re-deriving
#' it from context or re-reading the calling script.
#'
#' @keywords internal
#' @noRd
with_call_params <- function(result, params) {
  attr(result, "call_params") <- params
  result
}

#' One-way ANOVA with Dunnett's test, per group
#'
#' For each group (`group_cols`, usually plate x endpoint), fits a one-way
#' ANOVA of the (transformed) endpoint on concentration and compares each
#' concentration with the vehicle control by Dunnett's test
#' ([DescTools::DunnettTest()]). Part of Module 4 (skill
#' `behavior-endpoints-anova`).
#'
#' Grouping by chemical instead of plate pools plates as if all wells were
#' independent (no plate effect); keep `plate_id` in `group_cols` when plates
#' may differ. Each group must contain one endpoint.
#'
#' @param d Output of [generate_behavior_endpoints()] (kept wells only) with
#'   `group_cols`, `endpoint_col`, `value_col`, `conc_col`, `is_vc_col` and
#'   `keep_well`.
#' @param group_cols Columns defining one test, e.g.
#'   `c("plate_id", "endpoint")`.
#' @param endpoint_col Endpoint column. Default `"endpoint"`.
#' @param value_col Value column; use `"endpoint_value_raw"` (no default,
#'   see [module4]).
#' @param conc_col Concentration column. Default `"concentration"`.
#' @param conc_unit_col Concentration-unit column, or `NULL` if there is
#'   none. Default `"concentration_unit"`.
#' @param is_vc_col Logical vehicle-control column. Default
#'   `"is_vehicle_control"`. Each group needs vehicle-control wells.
#' @param endpoint_transforms Named list: endpoint-name pattern (fixed
#'   string) = transform (`"log10"`, `"fisher_z"`, `"none"`). Only endpoints
#'   matching a pattern are tested (see `unmatched_endpoints`); `NULL` tests
#'   all endpoints untransformed. Default
#'   `list(both_auc = "log10", spearman = "fisher_z")`.
#' @param unmatched_endpoints Endpoints matching no pattern: `"drop"`
#'   (default), `"error"`, or `"keep"` (tested untransformed).
#'
#' @return A list of two tibbles:
#'   \describe{
#'     \item{`anova`}{one row per group: `group_cols`, `df_1`, `df_2`,
#'       `F_value`, `p_value`.}
#'     \item{`dunn`}{one row per group x concentration: `group_cols`,
#'       `pairwise_comp`, `diff`, `lwr_ci`, `upr_ci`, `p_value`.}
#'   }
#'   The arguments are kept in attribute `call_params`.
#'
#' @seealso [module4], [run_anova_rmanova_hit_calling()].
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   plate_id = "P1",
#'   endpoint = "D1@both_auc",
#'   concentration = rep(c(0, 1, 10, 100), each = 8),
#'   concentration_unit = "uM",
#'   keep_well = TRUE
#' )
#' d$is_vehicle_control <- d$concentration == 0
#' d$endpoint_value_raw <- 10^stats::rnorm(32, 2 + 0.3 * (d$concentration >= 10), 0.2)
#' res <- run_anova(
#'   d, group_cols = c("plate_id", "endpoint"), value_col = "endpoint_value_raw"
#' )
#' res$anova
#' res$dunn
#' run_anova_rmanova_hit_calling(
#'   res, NULL, anova_group_cols = c("plate_id", "endpoint")
#' )
#' @export
run_anova <- function(
  d,
  group_cols,
  endpoint_col = "endpoint",
  value_col = NULL,
  conc_col = "concentration",
  conc_unit_col = "concentration_unit",
  is_vc_col = "is_vehicle_control",
  endpoint_transforms = list(both_auc = "log10", spearman = "fisher_z"),
  unmatched_endpoints = c("drop", "error", "keep")
) {
  fn_name <- "run_anova"
  unmatched_endpoints <- match.arg(unmatched_endpoints)
  check_value_col_supplied(value_col, fn_name)

  required_cols <- c(
    group_cols,
    endpoint_col,
    value_col,
    conc_col,
    is_vc_col,
    "keep_well"
  )
  check_statistics_input(d, required_cols, fn_name)

  d <- apply_endpoint_transforms(
    d,
    endpoint_col,
    value_col,
    endpoint_transforms,
    fn_name,
    unmatched = unmatched_endpoints
  )

  # conc_unit_col existence/non-NULL validated once up front; per-group
  # missing/empty-value checks happen inside fit_one() via check_conc_unit_values().
  resolve_conc_unit_col(d, conc_unit_col, fn_name)
  group_syms <- rlang::syms(group_cols)
  # dplyr::group_modify() drops grouping columns from `sub` (they're only
  # available via `key`) -- if endpoint_col IS one of group_cols, grouping
  # itself already guarantees exactly one distinct value per group, and
  # sub[[endpoint_col]] would not exist to check. Only check explicitly
  # when endpoint_col is NOT part of group_cols (the case the review is
  # actually concerned about: a caller omitting it from the key).
  endpoint_col_in_group <- endpoint_col %in% group_cols

  fit_one <- function(sub, key) {
    if (!endpoint_col_in_group && dplyr::n_distinct(sub[[endpoint_col]]) != 1) {
      rlang::abort(
        c(
          paste0(fn_name, "() requires exactly one distinct `endpoint_col` value within each model group."),
          "x" = paste0(
            "Found ", dplyr::n_distinct(sub[[endpoint_col]]), " distinct endpoint value(s): ",
            paste(unique(sub[[endpoint_col]]), collapse = ", "),
            " [", paste(names(key), unlist(key), sep = "=", collapse = ", "), "]"
          ),
          "i" = "Add `endpoint_col` to `group_cols` if it is not already there."
        ),
        class = "zfbehavior_error_mixed_endpoints_in_group"
      )
    }

    unit_vals <- if (!is.null(conc_unit_col)) sub[[conc_unit_col]] else NULL
    check_conc_unit_values(unit_vals, fn_name, key)

    conc_unit <- build_conc_unit_factor(
      sub[[conc_col]],
      unit_vals,
      sub[[is_vc_col]]
    )
    vc_label <- check_vc_reference(conc_unit, sub[[is_vc_col]], sub[[value_col]], fn_name, key)
    check_min_levels(conc_unit, 2, "conc_unit", fn_name, key)

    value <- sub[[value_col]]
    check_finite_model_values(value, fn_name, key)

    a <- summary(stats::aov(value ~ conc_unit))
    anova_tbl <- clean_anova_summary(a)

    dt <- DescTools::DunnettTest(value, conc_unit, control = vc_label)
    dunn_tbl <- clean_dunnett_result(dt)
    check_valid_pvalues(dunn_tbl$p_value, "Dunnett post-hoc p-value", fn_name)

    tibble::tibble(anova = list(anova_tbl), dunn = list(dunn_tbl))
  }

  results <- d |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::group_modify(fit_one) |>
    dplyr::ungroup()

  anova_out <- results |>
    dplyr::select(!!!group_syms, "anova") |>
    tidyr::unnest("anova")

  dunn_out <- results |>
    dplyr::select(!!!group_syms, "dunn") |>
    tidyr::unnest("dunn")

  with_call_params(
    list(anova = anova_out, dunn = dunn_out),
    list(
      group_cols = group_cols,
      value_col = value_col,
      endpoint_transforms = endpoint_transforms
    )
  )
}

#' Validate per-subject repeated-measures structure before fitting RMANOVA
#'
#' Requires, within one model group:
#' \itemize{
#'   \item No duplicate subject x phase rows.
#'   \item Every subject observes the COMPLETE set of phase levels used
#'     anywhere in the group (rejects, rather than silently allows, a
#'     subject missing an intermediate -- or any -- phase; a deliberate,
#'     stricter-than-necessary policy).
#'   \item Concentration, unit, and vehicle-control status do not vary
#'     within a subject's own rows (a subject's treatment must be
#'     constant across its repeated phase observations).
#' }
#'
#' @param is_vc_col Name of the vehicle-control flag column in `sub` (NOT
#'   the fixed `.zfbehavior_*` name -- this is the caller's original
#'   `is_vc_col`), checked for invariance SEPARATELY from
#'   `.zfbehavior_conc_unit`, since a VC-flag flip need not always change
#'   the derived dose-key label (e.g. if concentration/unit stay the
#'   same but only the flag itself is wrong).
#' @keywords internal
#' @noRd
check_rmanova_subject_structure <- function(sub, fn_name, is_vc_col, group_key = NULL) {
  key_txt <- if (!is.null(group_key)) {
    paste0(" [", paste(names(group_key), unlist(group_key), sep = "=", collapse = ", "), "]")
  } else {
    ""
  }

  dup_counts <- sub |>
    dplyr::count(.data$.zfbehavior_subject, .data$.zfbehavior_phase)
  dup <- dup_counts[dup_counts$n > 1, , drop = FALSE]
  if (nrow(dup) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() found duplicate subject x phase row(s)."),
        "x" = paste0(nrow(dup), " duplicated subject x phase combination(s).", key_txt),
        "i" = "Exactly one row per subject x phase is required."
      ),
      class = "zfbehavior_error_duplicate_subject_phase"
    )
  }

  full_phases <- levels(droplevels(sub$.zfbehavior_phase))
  per_subject_n <- sub |>
    dplyr::count(.data$.zfbehavior_subject) |>
    dplyr::filter(.data$n != length(full_phases))
  if (nrow(per_subject_n) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() found subject(s) missing one or more phases used elsewhere in this model group."),
        "x" = paste0(
          nrow(per_subject_n), " subject(s) do not observe all ", length(full_phases),
          " phase(s) (", paste(full_phases, collapse = ", "), ").", key_txt
        ),
        "i" = "Every subject must have exactly one row per phase used anywhere in its group; incomplete trajectories are rejected, not silently compressed."
      ),
      class = "zfbehavior_error_incomplete_subject_phases"
    )
  }

  invariance_check <- sub |>
    dplyr::group_by(.data$.zfbehavior_subject) |>
    dplyr::summarise(
      n_conc_unit = dplyr::n_distinct(.data$.zfbehavior_conc_unit),
      n_vc_flag = dplyr::n_distinct(.data[[is_vc_col]]),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_conc_unit > 1 | .data$n_vc_flag > 1)
  if (nrow(invariance_check) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() found subject(s) whose dose/unit/control status changes across phases."),
        "x" = paste0(nrow(invariance_check), " subject(s) are not treatment-invariant.", key_txt),
        "i" = "A subject's concentration, unit, and vehicle-control status must be constant across its own repeated phase rows."
      ),
      class = "zfbehavior_error_non_invariant_subject_treatment"
    )
  }

  invisible(TRUE)
}

#' Repeated-measures ANOVA over phases, with Dunnett's tests
#'
#' For each group (usually one plate), fits generalized least squares
#' models ([nlme::gls()]) of the (log10) Distance Change over the phases,
#' with an AR(1) correlation between the phases of each larva (plate x
#' well), in the order `phase_levels`:
#' \itemize{
#'   \item full: `value ~ concentration + phase + concentration:phase`
#'     (the dose effect may differ between phases);
#'   \item reduced: `value ~ concentration + phase` (one dose effect over
#'     all phases).
#' }
#' Dunnett's tests ([multcomp::glht()]) compare each concentration with the
#' vehicle control in the reduced model and within each phase. Part of
#' Module 4 (skill `behavior-endpoints-anova`).
#'
#' Every larva must have exactly one row per phase, and its concentration
#' and vehicle-control status must be the same in all phases. Results
#' differ from the original scripts, which ordered phases by endpoint name
#' and could merge phases with similar names (e.g. D2 and D2-ext).
#'
#' @section Dunnett tests within a phase:
#' `full_dunn` fits one model per phase. With one observation per larva the
#' AR(1) term has no effect there, and p-values use the normal (z)
#' approximation, as in the original method.
#'
#' @inheritParams run_anova
#' @param phase_col Phase column. Default `"segment"`.
#' @param phase_levels Phases in time order, e.g.
#'   `c("L1", "D1", "L2", "D2", "D2-ext")` (required; at least 2).
#' @param plate_id_col,well_id_col Columns identifying a larva. Defaults
#'   `"plate_id"`, `"well_id"`.
#' @param endpoint_transforms As in [run_anova()]; default
#'   `list(both_auc = "log10")` (Distance Change only; Movement Similarity
#'   has one value over all phases).
#'
#' @return A list of four tibbles:
#'   \describe{
#'     \item{`rmanova`}{one row per group x model (`"full"`, `"reduced"`) x
#'       term: `numDF`, `F_value`, `p_value`, `residual_var`.}
#'     \item{`rmanova_simple`}{one row per group: `p_value_interaction_full`
#'       (concentration x phase, full model) and `p_value_conc_unit_reduced`
#'       (concentration, reduced model), used for hit calling.}
#'     \item{`reduced_dunn`}{Dunnett's tests of the reduced model:
#'       `pairwise_comp`, `coef`, `sigma`, `zstat`, `p_value`.}
#'     \item{`full_dunn`}{Dunnett's tests within each `phase`.}
#'   }
#'   The arguments are kept in attribute `call_params`.
#'
#' @seealso [module4], [run_anova_rmanova_hit_calling()].
#'
#' @examples
#' set.seed(1)
#' d <- expand.grid(
#'   well_id = sprintf("W%02d", 1:24), segment = c("L1", "D1", "L2"),
#'   stringsAsFactors = FALSE
#' )
#' d$plate_id <- "P1"
#' d$endpoint <- paste0(d$segment, "@both_auc")
#' d$concentration <- rep(c(0, 1, 10), each = 8)[match(d$well_id, sprintf("W%02d", 1:24))]
#' d$concentration_unit <- "uM"
#' d$is_vehicle_control <- d$concentration == 0
#' d$keep_well <- TRUE
#' d$endpoint_value_raw <- 10^stats::rnorm(nrow(d), 2 + 0.3 * (d$concentration == 10), 0.2)
#' res <- run_rmanova(
#'   d, group_cols = "plate_id", phase_levels = c("L1", "D1", "L2"),
#'   value_col = "endpoint_value_raw"
#' )
#' res$rmanova_simple
#' @export
run_rmanova <- function(
  d,
  group_cols,
  phase_levels,
  phase_col = "segment",
  plate_id_col = "plate_id",
  well_id_col = "well_id",
  endpoint_col = "endpoint",
  value_col = NULL,
  conc_col = "concentration",
  conc_unit_col = "concentration_unit",
  is_vc_col = "is_vehicle_control",
  endpoint_transforms = list(both_auc = "log10"),
  unmatched_endpoints = c("drop", "error", "keep")
) {
  fn_name <- "run_rmanova"
  unmatched_endpoints <- match.arg(unmatched_endpoints)
  check_value_col_supplied(value_col, fn_name)

  if (!is.character(phase_levels) || length(phase_levels) < 2 || anyNA(phase_levels)) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires `phase_levels` to be a character vector of at least 2 non-missing phase labels, in true temporal order."),
        "i" = 'e.g. phase_levels = c("L1", "D1", "L2", "D2", "D2-ext")'
      ),
      class = "zfbehavior_error_invalid_phase_levels"
    )
  }
  if (anyDuplicated(phase_levels) > 0) {
    rlang::abort(
      paste0(fn_name, "() requires `phase_levels` to contain no duplicates."),
      class = "zfbehavior_error_invalid_phase_levels"
    )
  }

  required_cols <- c(
    group_cols,
    plate_id_col,
    well_id_col,
    phase_col,
    endpoint_col,
    value_col,
    conc_col,
    is_vc_col,
    "keep_well"
  )
  check_statistics_input(d, required_cols, fn_name)

  d <- apply_endpoint_transforms(
    d,
    endpoint_col,
    value_col,
    endpoint_transforms,
    fn_name,
    unmatched = unmatched_endpoints
  )

  # Checked AFTER endpoint-transform filtering, not before: rows outside
  # run_rmanova()'s endpoint scope (e.g. Movement Similarity, which has no
  # phase structure) may legitimately carry an NA/out-of-scope phase_col
  # value and are dropped by apply_endpoint_transforms() before ever
  # reaching this RMANOVA-specific phase check.
  unmatched_phase <- setdiff(unique(as.character(d[[phase_col]])), phase_levels)
  if (length(unmatched_phase) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() found `phase_col` value(s) not present in `phase_levels`."),
        "x" = paste0("Unrecognized phase value(s): ", paste(unmatched_phase, collapse = ", ")),
        "i" = "Every distinct `phase_col` value must appear in `phase_levels`."
      ),
      class = "zfbehavior_error_unrecognized_phase_value"
    )
  }

  resolve_conc_unit_col(d, conc_unit_col, fn_name)
  group_syms <- rlang::syms(group_cols)
  # NOTE: unlike run_anova(), run_rmanova() does NOT require exactly one
  # distinct `endpoint_col` value per model group -- this function's
  # entire purpose is to POOL multiple phase-labeled endpoint rows (e.g.
  # "L1@both_auc", "D1@both_auc", "L2@both_auc", ...) into one repeated-
  # measures model per group. `endpoint_transforms`'s pattern-matching
  # step is what constrains which endpoint CATEGORY reaches this function
  # (e.g. only "both_auc"-family endpoints); phase_col/phase_levels then
  # constrain the different phases within that one category. Requiring
  # endpoint_col uniqueness here would break RMANOVA's basic design.

  # Internal working columns (fixed names, independent of caller's column
  # naming) so gls()'s formula can reference stable symbols regardless of
  # what plate_id_col/well_id_col/value_col/phase_col/conc_col are named.
  d$.zfbehavior_subject <- as.integer(interaction(
    d[[plate_id_col]],
    d[[well_id_col]],
    drop = TRUE
  ))
  d$.zfbehavior_value <- d[[value_col]]
  d$.zfbehavior_phase <- factor(d[[phase_col]], levels = phase_levels)
  d$.zfbehavior_phase_index <- as.integer(d$.zfbehavior_phase)

  fit_full_reduced <- function(sub) {
    m_full <- nlme::gls(
      .zfbehavior_value ~
        .zfbehavior_conc_unit +
        .zfbehavior_phase +
        .zfbehavior_conc_unit:.zfbehavior_phase,
      data = sub,
      correlation = nlme::corAR1(form = ~ .zfbehavior_phase_index | .zfbehavior_subject),
      method = "REML"
    )
    m_full$data <- sub
    m_red <- nlme::gls(
      .zfbehavior_value ~ .zfbehavior_conc_unit + .zfbehavior_phase,
      data = sub,
      correlation = nlme::corAR1(form = ~ .zfbehavior_phase_index | .zfbehavior_subject),
      method = "REML"
    )
    m_red$data <- sub

    a_full <- as.data.frame(stats::anova(m_full, type = "marginal"))
    a_red <- as.data.frame(stats::anova(m_red, type = "marginal"))

    dunn_red <- summary(multcomp::glht(
      m_red,
      linfct = multcomp::mcp(.zfbehavior_conc_unit = "Dunnett")
    ))

    list(
      anova_full = a_full,
      anova_red = a_red,
      dunn_red = dunn_red,
      residual_full = nlme::getVarCov(m_full)[1, 1],
      residual_red = nlme::getVarCov(m_red)[1, 1]
    )
  }

  # Per-phase one-way GLS (not a contrast on the full model), as in the
  # original method (see ?run_rmanova). corAR1()
  # is included for structural/formula consistency with the full/reduced
  # models above, even though it is inert here (Phi mathematically forced
  # to 0, since each subject contributes exactly one row per phase-subset
  # fit) -- see the regression test confirming this below.
  fit_full_dunn_by_phase <- function(sub, phases) {
    purrr::map(phases, function(ph) {
      sub_ph <- sub[sub$.zfbehavior_phase == ph, , drop = FALSE]
      sub_ph <- sub_ph[order(sub_ph$.zfbehavior_subject), , drop = FALSE]
      m_ph <- nlme::gls(
        .zfbehavior_value ~ .zfbehavior_conc_unit,
        data = sub_ph,
        correlation = nlme::corAR1(form = ~ 1 | .zfbehavior_subject),
        method = "REML"
      )
      m_ph$data <- sub_ph
      clean_glht_result(summary(multcomp::glht(
        m_ph,
        linfct = multcomp::mcp(.zfbehavior_conc_unit = "Dunnett")
      )))
    })
  }

  fit_one <- function(sub, key) {
    unit_vals <- if (!is.null(conc_unit_col)) sub[[conc_unit_col]] else NULL
    check_conc_unit_values(unit_vals, fn_name, key)

    sub$.zfbehavior_conc_unit <- build_conc_unit_factor(
      sub[[conc_col]],
      unit_vals,
      sub[[is_vc_col]]
    )
    vc_label <- check_vc_reference(
      sub$.zfbehavior_conc_unit,
      sub[[is_vc_col]],
      sub$.zfbehavior_value,
      fn_name,
      key
    )
    check_min_levels(sub$.zfbehavior_conc_unit, 2, "conc_unit", fn_name, key)
    sub$.zfbehavior_phase <- droplevels(sub$.zfbehavior_phase)
    check_min_levels(sub$.zfbehavior_phase, 2, "phase", fn_name, key)
    check_finite_model_values(sub$.zfbehavior_value, fn_name, key)
    check_rmanova_subject_structure(sub, fn_name, is_vc_col, key)

    sub <- sub[
      order(sub$.zfbehavior_subject, sub$.zfbehavior_phase_index),
      ,
      drop = FALSE
    ]

    fit <- fit_full_reduced(sub)

    phases <- levels(sub$.zfbehavior_phase)
    full_dunn_list <- fit_full_dunn_by_phase(sub, phases)
    names(full_dunn_list) <- phases

    clean_term_names <- function(x) {
      x <- gsub(".zfbehavior_conc_unit", "conc_unit", x, fixed = TRUE)
      gsub(".zfbehavior_phase", "phase", x, fixed = TRUE)
    }

    rmanova_tbl <- dplyr::bind_rows(
      tibble::tibble(
        model = "full",
        term = clean_term_names(rownames(fit$anova_full)),
        numDF = fit$anova_full$numDF,
        F_value = fit$anova_full$`F-value`,
        p_value = fit$anova_full$`p-value`,
        residual_var = fit$residual_full
      ),
      tibble::tibble(
        model = "reduced",
        term = clean_term_names(rownames(fit$anova_red)),
        numDF = fit$anova_red$numDF,
        F_value = fit$anova_red$`F-value`,
        p_value = fit$anova_red$`p-value`,
        residual_var = fit$residual_red
      )
    )
    check_valid_pvalues(rmanova_tbl$p_value, "RMANOVA omnibus p-value", fn_name)

    interaction_row <- fit$anova_full[
      rownames(fit$anova_full) == ".zfbehavior_conc_unit:.zfbehavior_phase",
    ]
    conc_row_red <- fit$anova_red[
      rownames(fit$anova_red) == ".zfbehavior_conc_unit",
    ]
    rmanova_simple_tbl <- tibble::tibble(
      p_value_interaction_full = interaction_row$`p-value`,
      p_value_conc_unit_reduced = conc_row_red$`p-value`
    )

    reduced_dunn_tbl <- clean_glht_result(fit$dunn_red)
    check_valid_pvalues(reduced_dunn_tbl$p_value, "reduced-model Dunnett post-hoc p-value", fn_name)

    full_dunn_tbl <- purrr::imap(
      full_dunn_list,
      function(tbl, ph) dplyr::mutate(tbl, phase = ph, .before = 1)
    ) |>
      dplyr::bind_rows()
    check_valid_pvalues(full_dunn_tbl$p_value, "full-model per-phase Dunnett post-hoc p-value", fn_name)

    tibble::tibble(
      rmanova = list(rmanova_tbl),
      rmanova_simple = list(rmanova_simple_tbl),
      reduced_dunn = list(reduced_dunn_tbl),
      full_dunn = list(full_dunn_tbl)
    )
  }

  results <- d |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::group_modify(fit_one) |>
    dplyr::ungroup()

  with_call_params(
    list(
      rmanova = results |>
        dplyr::select(!!!group_syms, "rmanova") |>
        tidyr::unnest("rmanova"),
      rmanova_simple = results |>
        dplyr::select(!!!group_syms, "rmanova_simple") |>
        tidyr::unnest("rmanova_simple"),
      reduced_dunn = results |>
        dplyr::select(!!!group_syms, "reduced_dunn") |>
        tidyr::unnest("reduced_dunn"),
      full_dunn = results |>
        dplyr::select(!!!group_syms, "full_dunn") |>
        tidyr::unnest("full_dunn")
    ),
    list(
      group_cols = group_cols,
      value_col = value_col,
      endpoint_transforms = endpoint_transforms,
      phase_levels = phase_levels
    )
  )
}

#' Validate hit-calling `group_cols` against a result's provenance
#' (if available) or its component-table columns (fallback)
#'
#' Presence-only validation of
#' `anova_group_cols`/`rmanova_group_cols` would allow a caller to supply a
#' plausible-looking but WRONG grouping key, silently producing an
#' incorrect many-to-many join. If `result` carries a `call_params`
#' attribute (set by `run_anova()`/`run_rmanova()` via
#' `with_call_params()`), the supplied `group_cols` must be an EXACT set
#' match against the original grouping key used to produce `result` --
#' not merely a subset or superset. If no such attribute is present (e.g.
#' a hand-built list in a unit test), this falls back to a structural
#' check: every supplied column must actually exist in the relevant
#' component table.
#'
#' @keywords internal
#' @noRd
check_hit_calling_group_cols <- function(
  supplied_group_cols,
  result,
  component_name,
  fn_name
) {
  cp <- attr(result, "call_params")
  if (!is.null(cp) && !is.null(cp$group_cols)) {
    if (!setequal(supplied_group_cols, cp$group_cols)) {
      rlang::abort(
        c(
          paste0(
            fn_name,
            "() was given `group_cols` that do not match the columns originally used to produce this result."
          ),
          "x" = paste0("Supplied: ", paste(supplied_group_cols, collapse = ", ")),
          "x" = paste0("Original: ", paste(cp$group_cols, collapse = ", ")),
          "i" = "These must match exactly -- a mismatched key would silently produce an incorrect join."
        ),
        class = "zfbehavior_error_group_cols_mismatch"
      )
    }
  } else {
    missing_from_table <- setdiff(supplied_group_cols, colnames(result[[component_name]]))
    if (length(missing_from_table) > 0) {
      rlang::abort(
        c(
          paste0(fn_name, "() was given `group_cols` not present in `result$", component_name, "`."),
          "x" = paste0("Missing: ", paste(missing_from_table, collapse = ", "))
        ),
        class = "zfbehavior_error_group_cols_mismatch"
      )
    }
  }
  invisible(TRUE)
}

#' Assert a left_join() did not change the left-hand table's row count
#' (fan-out guard)
#' @keywords internal
#' @noRd
check_join_row_count_preserved <- function(n_before, n_after, fn_name, what) {
  if (n_after != n_before) {
    rlang::abort(
      c(
        paste0(fn_name, "() found a post-hoc join that changed the omnibus row count (", what, ")."),
        "x" = paste0("Before: ", n_before, " row(s); after: ", n_after, " row(s)."),
        "i" = "This indicates an under-specified or mismatched grouping key producing a many-to-many join."
      ),
      class = "zfbehavior_error_unexpected_join_fanout"
    )
  }
  invisible(TRUE)
}

#' Apply the hit-calling cascade to `run_anova()` output
#'
#' Internal helper for `run_anova_rmanova_hit_calling()` (see its Hit calling
#' section).
#'
#' @keywords internal
#' @noRd
hit_call_anova <- function(anova_result, group_cols, alpha) {
  fn_name <- "run_anova_rmanova_hit_calling"
  group_syms <- rlang::syms(group_cols)

  check_valid_pvalues(anova_result$anova$p_value, "ANOVA omnibus p-value", fn_name)
  check_valid_pvalues(anova_result$dunn$p_value, "Dunnett post-hoc p-value", fn_name)

  n_posthoc <- anova_result$dunn |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::summarise(
      n_posthoc_sig = sum(.data$p_value < alpha, na.rm = TRUE),
      n_posthoc_na = sum(is.na(.data$p_value)),
      .groups = "drop"
    )

  n_before <- nrow(anova_result$anova)
  joined <- anova_result$anova |>
    dplyr::left_join(n_posthoc, by = group_cols)
  check_join_row_count_preserved(n_before, nrow(joined), fn_name, "ANOVA post-hoc")

  joined |>
    dplyr::mutate(
      n_posthoc_sig = dplyr::coalesce(.data$n_posthoc_sig, 0L),
      n_posthoc_na = dplyr::coalesce(.data$n_posthoc_na, 0L),
      decision_state = dplyr::case_when(
        is.na(.data$p_value) ~ "indeterminate",
        .data$p_value >= alpha ~ "not_hit",
        .data$n_posthoc_sig > 0 ~ "hit",
        .data$n_posthoc_na > 0 ~ "indeterminate",
        TRUE ~ "not_hit"
      ),
      decision_source = ifelse(.data$decision_state == "hit", "anova", NA_character_),
      is_hit = dplyr::case_when(
        .data$decision_state == "hit" ~ TRUE,
        .data$decision_state == "not_hit" ~ FALSE,
        TRUE ~ NA
      ),
      hit_p_value = ifelse(.data$decision_state == "hit", .data$p_value, NA_real_),
      neg_log10_p_hit = -log10(.data$hit_p_value),
      test = "ANOVA"
    ) |>
    dplyr::select(
      !!!group_syms,
      "p_value",
      "n_posthoc_sig",
      "n_posthoc_na",
      "is_hit",
      "decision_state",
      "decision_source",
      "hit_p_value",
      "neg_log10_p_hit",
      "test"
    )
}

#' Apply the hit-calling cascade to `run_rmanova()` output
#'
#' Internal helper for `run_anova_rmanova_hit_calling()` (full model first,
#' reduced model as fallback).
#'
#' @keywords internal
#' @noRd
hit_call_rmanova <- function(rmanova_result, group_cols, alpha) {
  fn_name <- "run_anova_rmanova_hit_calling"
  group_syms <- rlang::syms(group_cols)

  check_valid_pvalues(
    rmanova_result$rmanova_simple$p_value_interaction_full,
    "RMANOVA interaction p-value",
    fn_name
  )
  check_valid_pvalues(
    rmanova_result$rmanova_simple$p_value_conc_unit_reduced,
    "RMANOVA reduced-model p-value",
    fn_name
  )
  check_valid_pvalues(rmanova_result$reduced_dunn$p_value, "reduced-model Dunnett post-hoc p-value", fn_name)
  check_valid_pvalues(rmanova_result$full_dunn$p_value, "full-model per-phase Dunnett post-hoc p-value", fn_name)

  n_posthoc_reduced <- rmanova_result$reduced_dunn |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::summarise(
      n_posthoc_sig_reduced = sum(.data$p_value < alpha, na.rm = TRUE),
      n_posthoc_na_reduced = sum(is.na(.data$p_value)),
      .groups = "drop"
    )

  n_posthoc_full <- rmanova_result$full_dunn |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::summarise(
      n_posthoc_sig_full = sum(.data$p_value < alpha, na.rm = TRUE),
      n_posthoc_na_full = sum(is.na(.data$p_value)),
      .groups = "drop"
    )

  n_before <- nrow(rmanova_result$rmanova_simple)
  joined <- rmanova_result$rmanova_simple |>
    dplyr::left_join(n_posthoc_reduced, by = group_cols) |>
    dplyr::left_join(n_posthoc_full, by = group_cols)
  check_join_row_count_preserved(n_before, nrow(joined), fn_name, "RMANOVA post-hoc")

  joined |>
    dplyr::mutate(
      n_posthoc_sig_reduced = dplyr::coalesce(.data$n_posthoc_sig_reduced, 0L),
      n_posthoc_na_reduced = dplyr::coalesce(.data$n_posthoc_na_reduced, 0L),
      n_posthoc_sig_full = dplyr::coalesce(.data$n_posthoc_sig_full, 0L),
      n_posthoc_na_full = dplyr::coalesce(.data$n_posthoc_na_full, 0L),
      decision_state = dplyr::case_when(
        is.na(.data$p_value_interaction_full) ~ "indeterminate",
        .data$p_value_interaction_full < alpha & .data$n_posthoc_sig_full > 0 ~ "hit",
        .data$p_value_interaction_full < alpha & .data$n_posthoc_na_full > 0 ~ "indeterminate",
        .data$p_value_interaction_full < alpha ~ "not_hit",
        is.na(.data$p_value_conc_unit_reduced) ~ "indeterminate",
        .data$p_value_conc_unit_reduced < alpha & .data$n_posthoc_sig_reduced > 0 ~ "hit",
        .data$p_value_conc_unit_reduced < alpha & .data$n_posthoc_na_reduced > 0 ~ "indeterminate",
        TRUE ~ "not_hit"
      ),
      decision_source = dplyr::case_when(
        .data$decision_state != "hit" ~ NA_character_,
        .data$p_value_interaction_full < alpha ~ "interaction",
        TRUE ~ "reduced"
      ),
      is_hit = dplyr::case_when(
        .data$decision_state == "hit" ~ TRUE,
        .data$decision_state == "not_hit" ~ FALSE,
        TRUE ~ NA
      ),
      hit_p_value = dplyr::case_when(
        .data$decision_state != "hit" ~ NA_real_,
        .data$decision_source == "interaction" ~ .data$p_value_interaction_full,
        TRUE ~ .data$p_value_conc_unit_reduced
      ),
      neg_log10_p_hit = -log10(.data$hit_p_value),
      test = "RMANOVA"
    ) |>
    dplyr::select(
      !!!group_syms,
      "p_value_interaction_full",
      "p_value_conc_unit_reduced",
      "n_posthoc_sig_reduced",
      "n_posthoc_sig_full",
      "is_hit",
      "decision_state",
      "decision_source",
      "hit_p_value",
      "neg_log10_p_hit",
      "test"
    )
}

#' Call hits from ANOVA and repeated-measures ANOVA results
#'
#' Turns the results of [run_anova()] and/or [run_rmanova()] into one hit
#' decision per group. No models are refitted. Part of Module 4 (skill
#' `behavior-endpoints-anova`).
#'
#' @section Hit calling:
#' A significant overall test alone can arise without any concentration
#' differing from the vehicle control, so a hit needs both:
#' \itemize{
#'   \item ANOVA: overall `p_value < alpha` and at least one Dunnett
#'     p-value `< alpha`.
#'   \item Repeated-measures ANOVA: first the full model, concentration x
#'     phase `p < alpha` and at least one within-phase Dunnett p-value
#'     `< alpha` (hit "via interaction"); otherwise the reduced model,
#'     concentration `p < alpha` and at least one Dunnett p-value `< alpha`
#'     (hit "via reduced").
#' }
#' A missing p-value that would decide the outcome gives `"indeterminate"`
#' instead of a hit or non-hit. `hit_p_value` is the p-value of the overall
#' test that gave the hit, and `neg_log10_p_hit` its `-log10`.
#'
#' @param anova_result Output of [run_anova()], or `NULL`.
#' @param rmanova_result Output of [run_rmanova()], or `NULL`. At least one
#'   of the two is required.
#' @param anova_group_cols,rmanova_group_cols The `group_cols` used for
#'   `anova_result` (e.g. `c("plate_id", "endpoint")`) and `rmanova_result`
#'   (e.g. `"plate_id"`); must match those recorded in the results.
#' @param alpha Significance level for all tests. Default `0.05`.
#'
#' @return A tibble, one row per group and test: group columns, `test`
#'   (`"ANOVA"`, `"RMANOVA"`), `is_hit` (`TRUE`, `FALSE`, `NA`),
#'   `decision_state` (`"hit"`, `"not_hit"`, `"indeterminate"`),
#'   `decision_source` (`"anova"`, `"interaction"`, `"reduced"`, `NA`),
#'   `hit_p_value`, `neg_log10_p_hit`, and the p-values and numbers of
#'   significant Dunnett tests behind the decision.
#'
#' @seealso [module4], [plot_behavior_anova_activity_pages()]; example in
#'   [run_anova()].
#' @export
run_anova_rmanova_hit_calling <- function(
  anova_result,
  rmanova_result,
  anova_group_cols = NULL,
  rmanova_group_cols = NULL,
  alpha = 0.05
) {
  fn_name <- "run_anova_rmanova_hit_calling"

  if (is.null(anova_result) && is.null(rmanova_result)) {
    rlang::abort(
      paste0(
        fn_name,
        "() requires at least one of `anova_result`/`rmanova_result` to be non-NULL."
      ),
      class = "zfbehavior_error_no_results_supplied"
    )
  }
  if (!is.null(anova_result) && is.null(anova_group_cols)) {
    rlang::abort(
      paste0(
        fn_name,
        "() requires `anova_group_cols` when `anova_result` is supplied."
      ),
      class = "zfbehavior_error_missing_group_cols"
    )
  }
  if (!is.null(rmanova_result) && is.null(rmanova_group_cols)) {
    rlang::abort(
      paste0(
        fn_name,
        "() requires `rmanova_group_cols` when `rmanova_result` is supplied."
      ),
      class = "zfbehavior_error_missing_group_cols"
    )
  }
  check_valid_alpha(alpha, fn_name)

  if (!is.null(anova_result)) {
    check_hit_calling_group_cols(anova_group_cols, anova_result, "anova", fn_name)
  }
  if (!is.null(rmanova_result)) {
    check_hit_calling_group_cols(rmanova_group_cols, rmanova_result, "rmanova_simple", fn_name)
  }

  anova_hits <- if (!is.null(anova_result)) {
    hit_call_anova(anova_result, anova_group_cols, alpha)
  } else {
    NULL
  }
  rmanova_hits <- if (!is.null(rmanova_result)) {
    hit_call_rmanova(rmanova_result, rmanova_group_cols, alpha)
  } else {
    NULL
  }

  with_call_params(
    dplyr::bind_rows(anova_hits, rmanova_hits),
    list(
      anova_group_cols = anova_group_cols,
      rmanova_group_cols = rmanova_group_cols,
      alpha = alpha
    )
  )
}
