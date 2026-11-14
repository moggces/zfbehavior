test_that("parse_binning_to_time() parses Start-prefixed and normal bins", {
  x <- c("Start-0:01:00", "0:01:00-0:02:00", "0:09:00-0:10:00")
  expect_equal(parse_binning_to_time(x), c(1, 2, 10))
})

test_that("parse_binning_to_time() handles hour rollover", {
  expect_equal(parse_binning_to_time("0:59:00-1:00:00"), 60)
  expect_equal(parse_binning_to_time("1:00:00-1:01:00"), 61)
})

test_that("parse_binning_to_time() returns NA with a warning for unparseable entries", {
  expect_warning(
    result <- parse_binning_to_time(c("0:01:00-0:02:00", "garbage")),
    class = "zfbehavior_warning_binning_parse_failed"
  )
  expect_equal(result, c(2, NA))
})

test_that("parse_binning_to_time() passes NA through without warning", {
  expect_no_warning(result <- parse_binning_to_time(c("0:01:00-0:02:00", NA)))
  expect_equal(result, c(2, NA))
})

test_that("parse_binning_to_time() errors on non-character input", {
  expect_error(
    parse_binning_to_time(1:3),
    class = "zfbehavior_error_invalid_input"
  )
})

test_that("parse_binning_to_time() matches real BINNING values from the fixture", {
  raw <- zf_fixture_raw()
  sample_binning <- raw %>%
    dplyr::distinct(.data$BINNING, .data$time_end) %>%
    dplyr::arrange(.data$time_end)

  parsed <- parse_binning_to_time(sample_binning$BINNING)
  expect_equal(parsed, sample_binning$time_end)
})
