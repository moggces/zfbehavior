# Optional private data (original analysis outputs) used by a few
# regression tests. Not shipped with the package. The location is the
# environment variable ZFBEHAVIOR_PRIVATE_DIR, or `private/raw` (or `raw`)
# in the source tree when it exists. Tests that need it skip with a clear
# message when it is missing.

zf_private_dir <- function() {
  dir <- Sys.getenv("ZFBEHAVIOR_PRIVATE_DIR", "")
  if (!nzchar(dir)) {
    candidates <- c(
      test_path("..", "..", "private", "raw"),
      test_path("..", "..", "raw")
    )
    found <- candidates[dir.exists(file.path(candidates, "data"))]
    if (length(found) > 0) dir <- found[1]
  }
  dir
}

# Path to a file under the private data directory, or skip the test.
zf_private_file <- function(...) {
  dir <- zf_private_dir()
  path <- if (nzchar(dir)) file.path(dir, ...) else ""
  testthat::skip_if(
    !nzchar(path) || !file.exists(path),
    paste0(
      "private data not available (set ZFBEHAVIOR_PRIVATE_DIR): ",
      file.path(...)
    )
  )
  path
}
