# Internal helpers shared across zfbehavior's import/validation functions.
#
# None of these are exported. They exist so common argument-validation and
# "leaf check aborts on a missing/invalid argument" patterns are implemented
# once, rather than re-implemented slightly differently in every function
# that needs them.

#' Abort if any of `cols` is not a column of `d` (leaf-check convention)
#'
#' Internal "leaf" checks (e.g. `check_plate_row_counts()`,
#' `check_id_correspondence()`) abort immediately if a caller-referenced
#' column does not exist -- this is programmer error, not a scientific QC
#' finding. Higher-level validators (e.g. `validate_behavior_data()`)
#' preflight their own required columns before calling any leaf check, so
#' this abort should never actually propagate out of them.
#'
#' @param d A tibble.
#' @param cols Character vector of column names that must exist in `d`.
#' @param fn_name Name of the calling function, for the error message.
#' @param arg_desc Short description of what `cols` represents, for the
#'   error message (e.g. `"plate_col and id_col"`).
#' @keywords internal
#' @noRd
check_columns_exist <- function(d, cols, fn_name, arg_desc = "column(s)") {
  missing_cols <- setdiff(unique(cols), colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires ", arg_desc, " to be present in `d`."),
        "x" = paste0("Not found: ", paste(missing_cols, collapse = ", "))
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  invisible(TRUE)
}

#' Abort unless `x` is `NULL` or a single, finite, non-negative, integer-like number
#'
#' Used to validate `expected_n`/`expected_vc_wells`/`expected_dose_wells`/
#' `expected_row_count`-style arguments before they are used to drive a
#' scientific check, so `NA`, vectors, characters, or non-integer counts
#' fail loudly as an invalid-argument error rather than silently disabling
#' the check or producing a confusing low-level error downstream.
#'
#' @param x Value to check.
#' @param arg_name Name of the argument, for the error message.
#' @param fn_name Name of the calling function, for the error message.
#' @param allow_null Whether `NULL` is an acceptable value (default `TRUE`).
#' @keywords internal
#' @noRd
check_scalar_nonneg_count <- function(x, arg_name, fn_name, allow_null = TRUE) {
  if (is.null(x)) {
    if (allow_null) {
      return(invisible(TRUE))
    }
    rlang::abort(
      c(
        paste0(fn_name, "() requires `", arg_name, "` to be supplied (not NULL)."),
        "i" = "Expected a single non-negative, finite, integer-like number."
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  ok <- is.numeric(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    is.finite(x) &&
    x >= 0 &&
    x == round(x)
  if (!ok) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires `",
          arg_name,
          "` to be a single non-negative, finite, integer-like number",
          if (allow_null) ", or NULL." else "."
        ),
        "x" = paste0("Got: ", paste(utils::capture.output(print(x)), collapse = " "))
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  invisible(TRUE)
}

#' Abort unless `x` is a fully, uniquely named list of valid scalar counts
#'
#' Used for arguments like `expected_n_distinct` (a named list mapping
#' column name -> expected distinct-value count).
#'
#' @param x The list to check (`list()`/`character(0)`-length is allowed:
#'   an empty list means "no checks requested", not an error).
#' @param arg_name Name of the argument, for the error message.
#' @param fn_name Name of the calling function, for the error message.
#' @keywords internal
#' @noRd
check_named_list_of_counts <- function(x, arg_name, fn_name) {
  if (length(x) == 0) {
    return(invisible(TRUE))
  }
  nms <- names(x)
  if (is.null(nms) || any(is.na(nms)) || any(nms == "")) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires every element of `",
          arg_name,
          "` to have a non-empty name."
        ),
        "i" = paste0(
          "Example: list(CHEMICAL_NAME = 123) maps column name to expected count."
        )
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  if (any(duplicated(nms))) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires `", arg_name, "` names to be unique."),
        "x" = paste0(
          "Duplicated name(s): ",
          paste(unique(nms[duplicated(nms)]), collapse = ", ")
        )
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  for (i in seq_along(x)) {
    check_scalar_nonneg_count(
      x[[i]],
      paste0(arg_name, "[[\"", nms[i], "\"]]"),
      fn_name,
      allow_null = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate a `column_mapping` before it is used to rename data
#'
#' Checks (all before any renaming is attempted):
#' \itemize{
#'   \item Every value is one non-missing, non-empty character string (not
#'     empty, not multivalued, not `NA`).
#'   \item Names (canonical role names) are unique.
#'   \item Values (source column names) are not duplicated -- the same
#'     source column cannot be mapped to two different canonical roles.
#' }
#'
#' @param column_mapping A named list (already coerced via `as.list()`).
#' @param fn_name Name of the calling function, for the error message.
#' @keywords internal
#' @noRd
check_mapping_values <- function(column_mapping, fn_name) {
  nms <- names(column_mapping)
  if (is.null(nms) || any(is.na(nms)) || any(nms == "")) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires every column_mapping element to have a non-empty name."
        ),
        "i" = "Names must be canonical role names (see canonical_behavior_roles())."
      ),
      class = "zfbehavior_error_invalid_mapping"
    )
  }
  if (any(duplicated(nms))) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires column_mapping names (canonical roles) to be unique."
        ),
        "x" = paste0(
          "Duplicated role name(s): ",
          paste(unique(nms[duplicated(nms)]), collapse = ", ")
        )
      ),
      class = "zfbehavior_error_invalid_mapping"
    )
  }

  bad_roles <- character(0)
  for (i in seq_along(column_mapping)) {
    v <- column_mapping[[i]]
    if (!is.character(v) || length(v) != 1 || is.na(v) || !nzchar(v)) {
      bad_roles <- c(bad_roles, nms[i])
    }
  }
  if (length(bad_roles) > 0) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires every column_mapping value to be a single, non-missing, non-empty character string."
        ),
        "x" = paste0(
          "Invalid value(s) for role(s): ",
          paste(bad_roles, collapse = ", ")
        ),
        "i" = "Empty vectors, NA, and multi-element character vectors are not valid mapping values."
      ),
      class = "zfbehavior_error_invalid_mapping"
    )
  }

  values <- vapply(column_mapping, identity, character(1))
  dup_values <- unique(values[duplicated(values)])
  if (length(dup_values) > 0) {
    rlang::abort(
      c(
        paste0(
          fn_name,
          "() requires column_mapping to reference each source column at most once."
        ),
        "x" = paste0(
          "Source column(s) mapped to more than one role: ",
          paste(dup_values, collapse = ", ")
        )
      ),
      class = "zfbehavior_error_invalid_mapping"
    )
  }

  invisible(TRUE)
}

#' Resolve a "target" count from a vector of observed counts, or flag ambiguity
#'
#' If `expected` is supplied, it is returned directly as the target (no
#' inference). Otherwise, the mode (most common value) of `counts` is used
#' as the target -- but if more than one value is tied for the maximum
#' frequency, the target is ambiguous and must not be silently resolved by
#' picking one arbitrarily (e.g. `which.max()`'s first-hit behavior).
#'
#' @param counts A numeric/integer vector of observed counts.
#' @param expected Optional scalar expected count (already validated by
#'   `check_scalar_nonneg_count()`).
#'
#' @return A list with `target` (integer, or `NA_integer_` if ambiguous or
#'   `counts` is empty), `ambiguous` (logical), and (if ambiguous) `modes`
#'   (the tied candidate values).
#' @keywords internal
#' @noRd
resolve_target_count <- function(counts, expected = NULL) {
  if (!is.null(expected)) {
    return(list(target = as.integer(expected), ambiguous = FALSE))
  }
  if (length(counts) == 0) {
    return(list(target = NA_integer_, ambiguous = FALSE))
  }
  tab <- table(counts)
  max_freq <- max(tab)
  modes <- as.integer(names(tab)[tab == max_freq])
  if (length(modes) > 1) {
    return(list(target = NA_integer_, ambiguous = TRUE, modes = sort(modes)))
  }
  list(target = modes[1], ambiguous = FALSE)
}
