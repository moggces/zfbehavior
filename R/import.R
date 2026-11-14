#' Module 1: data import and consistency checks
#'
#' Module 1 of the zfbehavior workflow reads vendor tracking files into one
#' canonical table, checks it per plate and across plates, and fixes the
#' well contract that all later modules rely on. It is run by the skill
#' `dataset-import-qc`.
#'
#' @section Workflow:
#' \enumerate{
#'   \item Map vendor columns to the canonical roles and read each file with
#'     [read_behavior_data()] (strict parsing). Derive a role after import
#'     if needed, e.g. `time_value` with [parse_binning_to_time()].
#'   \item Check each plate with [validate_behavior_data()] (types, unique
#'     plate x well x time rows, complete trajectories, replicate counts,
#'     rows per plate).
#'   \item Curate wells so the single-larva well contract holds (see below).
#'   \item Combine the plates and check identifiers across the whole dataset
#'     with [validate_combined_dataset()].
#'   \item Before computing endpoints, set missing responses of kept wells to
#'     zero with [replace_na_response_zero()].
#' }
#'
#' @section Canonical columns:
#' Every function works on these column roles; vendor names are mapped to
#' them at import ([canonical_behavior_roles()] lists them,
#' [required_canonical_roles()] gives the required ones):
#' \describe{
#'   \item{`plate_id`}{plate identifier}
#'   \item{`well_id`}{well identifier within a plate}
#'   \item{`chemical_id`}{chemical (treatment) identifier}
#'   \item{`is_vehicle_control`}{logical, `TRUE` for vehicle-control wells}
#'   \item{`concentration`}{numeric concentration (0 for vehicle control)}
#'   \item{`concentration_unit`}{optional: unit of `concentration`}
#'   \item{`time_value`}{numeric time (e.g. minutes), one row per timepoint}
#'   \item{`response_value`}{numeric response (e.g. distance moved)}
#'   \item{`keep_well`}{logical, `TRUE` if the larva is analyzed (see below)}
#' }
#' Other input columns are kept unchanged. Each later step needs only some
#' of these columns; [behavior_endpoint_input_cols()],
#' [raw_plot_input_cols()], [interplate_qc_input_cols()] and
#' [devtox_input_cols()] list them.
#'
#' @section Single-larva well contract and `keep_well`:
#' Every well in the imported dataset must hold exactly one larva:
#' \itemize{
#'   \item `keep_well = TRUE`: the larva is analyzed.
#'   \item `keep_well = FALSE`: the larva was dead or malformed (or has
#'     another condition of the larva itself that affects behavior, e.g. no
#'     swim bladder). It is left out of behavior endpoints but counted as
#'     affected in developmental toxicity, and in all denominators.
#' }
#' Wells with no larva, more than one larva, or a technical failure (e.g. a
#' dosing error) must be removed from the data, not marked
#' `keep_well = FALSE`. Then the wells present in a dose group equal the
#' larvae assayed, and the number of wells may differ between dose groups.
#' `keep_well` is a judgment made by the lab or the scoring step; the
#' package checks that it is a complete logical column but cannot check
#' that it is right. The `dataset-import-qc` skill guides this curation and
#' records it.
#'
#' @seealso Skill `dataset-import-qc`
#'   (`system.file("skills", "dataset-import-qc", package = "zfbehavior")`).
#' @name canonical-columns
#' @aliases module1
NULL

#' Internal canonical-schema table (single source for the role accessors)
#'
#' @return A tibble with columns `role`, `required` (logical), and
#'   `description`.
#' @keywords internal
#' @noRd
canonical_schema_table <- function() {
  tibble::tibble(
    role = c(
      "plate_id",
      "well_id",
      "chemical_id",
      "is_vehicle_control",
      "concentration",
      "concentration_unit",
      "time_value",
      "response_value",
      "keep_well"
    ),
    required = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      TRUE,
      TRUE,
      TRUE
    ),
    description = c(
      "plate/experiment identifier",
      "well identifier within a plate",
      "chemical/treatment identifier",
      "logical, TRUE for vehicle control wells",
      "numeric concentration (0 for vehicle control)",
      "units for concentration",
      "numeric time (e.g. minutes), one row per timepoint",
      "numeric behavioral response (e.g. distance moved)",
      "logical, TRUE if well/embryo is valid and should be analyzed"
    )
  )
}

#' Canonical column roles
#'
#' Lists the canonical column roles that [read_behavior_data()] maps vendor
#' columns to and that all functions use. Part of Module 1 (data import;
#' skill `dataset-import-qc`); see [canonical-columns] for the roles and the
#' meaning of `keep_well`.
#'
#' @return A named character vector: names are the roles, values say
#'   whether the role is required and what it holds.
#' @seealso [required_canonical_roles()], [read_behavior_data()].
#' @export
#'
#' @examples
#' canonical_behavior_roles()
canonical_behavior_roles <- function() {
  schema <- canonical_schema_table()
  out <- paste0(
    ifelse(schema$required, "required: ", "optional: "),
    schema$description
  )
  stats::setNames(out, schema$role)
}

#' Required canonical column roles
#'
#' The roles of [canonical_behavior_roles()] that [read_behavior_data()]
#' requires in `column_mapping` (unless listed in `optional_roles`). Part of
#' Module 1 (data import; skill `dataset-import-qc`).
#'
#' @return A character vector of role names.
#' @seealso [canonical-columns]
#' @export
#'
#' @examples
#' required_canonical_roles()
required_canonical_roles <- function() {
  schema <- canonical_schema_table()
  schema$role[schema$required]
}

#' Read behavioral tracking data with an explicit column mapping
#'
#' Reads one CSV, TSV or Excel file of tracking data and renames the vendor
#' columns to the canonical roles given in `column_mapping`. Columns are
#' never guessed: every required role must be mapped. Part of Module 1 (data
#' import; skill `dataset-import-qc`).
#'
#' @section Strict parsing:
#' Any parse or type-coercion problem reported by the reader (e.g. text in a
#' numeric column) stops the import with an error listing the file, row,
#' column and value. Such a value would otherwise become `NA` and look like
#' an ordinary missing response. For multi-file imports, give explicit
#' column types (`excel_col_types` or `delimited_col_types`): automatic type
#' guessing can differ between files, e.g. a mostly empty comment column
#' guessed as logical turns its few text values into `NA`.
#'
#' @section Deriving a role after import:
#' A required role that has no input column can be listed in
#' `optional_roles` and added after import, before validation. For example,
#' a text time-bin column:
#' ```r
#' d <- read_behavior_data(path, mapping, optional_roles = "time_value")
#' d$time_value <- parse_binning_to_time(d$BINNING)
#' ```
#'
#' @param path Path to a `.csv`, `.tsv`, `.xlsx` or `.xls` file.
#' @param column_mapping Named list or character vector: canonical role =
#'   input column name, e.g. `list(plate_id = "PLATE_NAME", ...)`. Each
#'   input column may be used once.
#' @param sheet Excel sheet name or index. Default `1`.
#' @param delimited_col_types Optional `col_types` for
#'   [readr::read_csv()] / [readr::read_tsv()]. Default `NULL` (guessed).
#' @param excel_col_types Optional `col_types` for [readxl::read_excel()],
#'   e.g. `c("text", "numeric", ...)`. Default `NULL` (guessed).
#' @param optional_roles Required roles that will be added after import (see
#'   Deriving a role after import). Default `NULL`.
#'
#' @return A tibble with mapped columns renamed to their roles and all other
#'   columns unchanged.
#'
#' @seealso [canonical-columns] (roles and `keep_well`),
#'   [validate_behavior_data()], [parse_binning_to_time()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mapping <- list(
#'   plate_id = "PLATE_NAME",
#'   well_id = "WELL_POSITION",
#'   chemical_id = "BIOBIDE_ID",
#'   is_vehicle_control = "IS_VEHICLE",
#'   concentration = "CONCENTRATION",
#'   concentration_unit = "CONCENTRATION_UNIT",
#'   time_value = "time_end",
#'   response_value = "DISTANCE_MOVED",
#'   keep_well = "KEEP_WELL_DATA"
#' )
#' d <- read_behavior_data("my_data.xlsx", column_mapping = mapping)
#' }
read_behavior_data <- function(
  path,
  column_mapping,
  sheet = 1,
  delimited_col_types = NULL,
  excel_col_types = NULL,
  optional_roles = NULL
) {
  if (!file.exists(path)) {
    rlang::abort(
      c(
        "Cannot read behavioral data.",
        "x" = paste0("File does not exist: ", path)
      ),
      class = "zfbehavior_error_file_not_found"
    )
  }

  column_mapping <- as.list(column_mapping)
  check_mapping_values(column_mapping, "read_behavior_data")

  bad_optional_roles <- setdiff(optional_roles, required_canonical_roles())
  if (length(bad_optional_roles) > 0) {
    rlang::abort(
      c(
        "read_behavior_data() requires optional_roles to be a subset of required_canonical_roles().",
        "x" = paste0(
          "Not a required role: ",
          paste(bad_optional_roles, collapse = ", ")
        ),
        "i" = "optional_roles marks a required role that is added after import (see ?read_behavior_data)."
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }

  required_roles <- setdiff(required_canonical_roles(), optional_roles)
  missing_roles <- setdiff(required_roles, names(column_mapping))

  if (length(missing_roles) > 0) {
    rlang::abort(
      c(
        "column_mapping is missing required canonical role(s).",
        "x" = paste0(
          "Missing role(s): ",
          paste(missing_roles, collapse = ", ")
        ),
        "i" = "Every required role must be explicitly mapped to an input column name.",
        "i" = "See canonical_behavior_roles() for the full list of roles and their meaning.",
        "i" = "If a role must be derived AFTER import (e.g. from a text time-bin column), list it in optional_roles instead."
      ),
      class = "zfbehavior_error_missing_role_mapping"
    )
  }

  unknown_roles <- setdiff(
    names(column_mapping),
    names(canonical_behavior_roles())
  )
  if (length(unknown_roles) > 0) {
    rlang::abort(
      c(
        "column_mapping contains unrecognized canonical role name(s).",
        "x" = paste0(
          "Unknown role(s): ",
          paste(unknown_roles, collapse = ", ")
        ),
        "i" = "See canonical_behavior_roles() for valid role names."
      ),
      class = "zfbehavior_error_unknown_role"
    )
  }

  ext <- tolower(tools::file_ext(path))

  parse_problems <- NULL
  d <- switch(
    ext,
    "csv" = ,
    "tsv" = {
      reader <- if (ext == "csv") readr::read_csv else readr::read_tsv
      # readr's own "one or more parsing issues" warning is redundant with
      # the structured readr::problems() check just below (which we
      # convert into a hard, informative abort) -- suppress it here so it
      # is not also surfaced independently.
      out <- withCallingHandlers(
        reader(path, col_types = delimited_col_types, show_col_types = FALSE),
        warning = function(w) {
          if (inherits(w, "vroom_parse_issue")) {
            invokeRestart("muffleWarning")
          }
        }
      )
      parse_problems <- readr::problems(out)
      out
    },
    "xlsx" = ,
    "xls" = {
      excel_warnings <- list()
      out <- withCallingHandlers(
        readxl::read_excel(path, sheet = sheet, col_types = excel_col_types),
        warning = function(w) {
          excel_warnings[[length(excel_warnings) + 1]] <<- w
          invokeRestart("muffleWarning")
        }
      )
      if (length(excel_warnings) > 0) {
        parse_problems <- tibble::tibble(
          file = basename(path),
          message = vapply(excel_warnings, conditionMessage, character(1))
        )
      }
      out
    },
    rlang::abort(
      c(
        "Unsupported file type.",
        "x" = paste0("File extension not supported: .", ext),
        "i" = "Supported types: .csv, .tsv, .xlsx, .xls"
      ),
      class = "zfbehavior_error_unsupported_filetype"
    )
  )
  d <- tibble::as_tibble(d)

  if (!is.null(parse_problems) && nrow(parse_problems) > 0) {
    problem_lines <- utils::head(
      apply(parse_problems, 1, function(r) paste(names(r), r, sep = "=", collapse = ", ")),
      20
    )
    rlang::abort(
      c(
        paste0(
          "read_behavior_data() detected ",
          nrow(parse_problems),
          " parse/coercion problem(s) while reading ",
          basename(path),
          "."
        ),
        stats::setNames(problem_lines, rep("x", length(problem_lines))),
        "i" = "A malformed token can otherwise be silently parsed as NA and later mistaken for an ordinary missing value.",
        "i" = "Fix the source file, or adjust delimited_col_types/excel_col_types so the affected column's declared type matches its real content."
      ),
      class = "zfbehavior_error_parse_problems"
    )
  }

  input_cols_used <- unlist(column_mapping, use.names = FALSE)
  missing_input_cols <- setdiff(input_cols_used, colnames(d))
  if (length(missing_input_cols) > 0) {
    rlang::abort(
      c(
        "column_mapping references column(s) not found in the input file.",
        "x" = paste0(
          "Not found in ",
          basename(path),
          ": ",
          paste(missing_input_cols, collapse = ", ")
        ),
        "i" = paste0("Available columns: ", paste(colnames(d), collapse = ", "))
      ),
      class = "zfbehavior_error_column_not_found"
    )
  }

  rename_vec <- unlist(column_mapping)
  # rlang::set_names expects new = old; dplyr::rename expects new_name = old_name
  d <- dplyr::rename(d, !!!rename_vec)

  d
}

#' Replace missing response values with zero
#'
#' Sets `NA` responses to `0`. Tracking can miss a live larva for a
#' timepoint; treating that as no movement keeps every well on the same time
#' grid, which the endpoint calculations require (area under the curve and
#' movement similarity). Part of Module 1 (data import; skill
#' `dataset-import-qc`); the endpoint functions stop with an error if
#' `response_value` still has `NA`.
#'
#' @param d A tibble.
#' @param response_col Numeric column to fill. Default `"response_value"`.
#'
#' @return `d` with `NA` and `NaN` in `response_col` replaced by `0` (type
#'   kept).
#'
#' @seealso [canonical-columns], [generate_behavior_endpoints()].
#' @export
#'
#' @examples
#' d <- tibble::tibble(response_value = c(1, NA, 3))
#' replace_na_response_zero(d)
replace_na_response_zero <- function(d, response_col = "response_value") {
  if (!response_col %in% colnames(d)) {
    rlang::abort(
      c(
        "replace_na_response_zero() requires `response_col` to be a column in `d`.",
        "x" = paste0("Column not found: ", response_col)
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  x <- d[[response_col]]
  if (!is.numeric(x)) {
    rlang::abort(
      c(
        paste0(
          "replace_na_response_zero() requires `",
          response_col,
          "` to be numeric."
        ),
        "x" = paste0("Column class: ", paste(class(x), collapse = "/")),
        "i" = "Zero-imputation is only meaningful for a numeric response column."
      ),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  x[is.na(x)] <- if (is.integer(x)) 0L else 0
  d[[response_col]] <- x
  d
}
