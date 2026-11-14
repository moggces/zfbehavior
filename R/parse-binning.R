#' Convert text time bins to minutes
#'
#' Converts time-bin text such as `"Start-0:01:00"` or `"0:09:00-0:10:00"`
#' (`h:mm:ss`, as exported by some tracking systems) to the bin end time in
#' minutes. Use it when the input has no numeric time column: import with
#' `optional_roles = "time_value"` in [read_behavior_data()], then set
#' `time_value` with this function. Part of Module 1 (data import; skill
#' `dataset-import-qc`).
#'
#' @param binning Character vector of time-bin strings.
#'
#' @return Numeric bin end times in minutes; strings that do not match the
#'   format give `NA` with a warning.
#'
#' @seealso [canonical-columns] (Module 1).
#' @export
#'
#' @examples
#' parse_binning_to_time(c("Start-0:01:00", "0:01:00-0:02:00", "0:09:00-0:10:00"))
parse_binning_to_time <- function(binning) {
  if (!is.character(binning)) {
    rlang::abort(
      c(
        "parse_binning_to_time() requires a character vector.",
        "x" = paste0("Got class: ", paste(class(binning), collapse = "/"))
      ),
      class = "zfbehavior_error_invalid_input"
    )
  }

  hour_part <- stringr::str_replace(binning, ".*-([0-9]{1,2}):.*", "\\1")
  minute_part <- stringr::str_replace(binning, ".*:([0-9]{1,2}):00$", "\\1")

  looks_valid <- stringr::str_detect(
    binning,
    "^(Start|[0-9]{1,2}:[0-9]{2}:[0-9]{2})-[0-9]{1,2}:[0-9]{2}:[0-9]{2}$"
  )

  hour_num <- suppressWarnings(as.numeric(hour_part))
  minute_num <- suppressWarnings(as.numeric(minute_part))

  time_value <- minute_num + hour_num * 60
  time_value[!looks_valid] <- NA_real_

  n_failed <- sum(is.na(time_value) & !is.na(binning))
  if (n_failed > 0) {
    rlang::warn(
      c(
        paste0(
          n_failed,
          " entr",
          ifelse(n_failed == 1, "y", "ies"),
          " could not be parsed as BINNING-style time strings."
        ),
        "i" = "Unparsed entries were set to NA. Expected format like '0:01:00-0:02:00'."
      ),
      class = "zfbehavior_warning_binning_parse_failed"
    )
  }

  time_value
}
