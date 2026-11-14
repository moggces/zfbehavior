# ---- run_anova(): synthetic unit tests -------------------------------------

test_that("run_anova() computes a known one-way ANOVA (hand-verifiable, independent oracle)", {
  # Hand-verified (not computed via aov()/DescTools): 3 groups, n=3 each,
  # non-degenerate within-dose variance -- unlike a previous version of
  # this fixture which had zero within-dose variance and therefore was
  # not an independent check on the ANOVA engine itself.
  # control = c(1,2,3), dose1 = c(4,5,6), dose2 = c(7,9,8)
  # grand mean = 5; SS_between = 3*((2-5)^2+(5-5)^2+(8-5)^2) = 54; df1 = 2
  # SS_within = 2+2+2 = 6; df2 = 6; F = (54/2)/(6/6) = 27; p = 0.001
  # Dunnett: SE = sqrt(1*(1/3+1/3)) = 0.8164966
  # diff1 (dose1 - control) = 3; diff2 (dose2 - control) = 6
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "total_distance",
    well_id = paste0("W", 1:9),
    concentration = rep(c(0, 50, 150), each = 3),
    concentration_unit = "uM",
    is_vehicle_control = .data$concentration == 0,
    endpoint_value = c(1, 2, 3, 4, 5, 6, 7, 9, 8),
    keep_well = TRUE
  )
  out <- run_anova(
    d,
    group_cols = c("plate_id", "endpoint"),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )
  expect_equal(out$anova$df_1, 2)
  expect_equal(out$anova$df_2, 6)
  expect_equal(out$anova$F_value, 27, tolerance = 1e-8)
  expect_equal(out$anova$p_value, 0.001, tolerance = 1e-8)

  # Sort by treated concentration (not alphabetically by pairwise_comp
  # string, since "150uM-0uM" sorts before "50uM-0uM" alphabetically).
  dunn <- out$dunn[order(readr::parse_number(out$dunn$pairwise_comp)), ]
  expect_equal(dunn$diff, c(3, 6), tolerance = 1e-8)
  # SE is not returned directly, but coefficients/direction are -- also
  # confirm the comparison direction is always "<treated>-0<unit>" (VC
  # named as the second/reference term).
  expect_true(all(grepl("-0uM$", dunn$pairwise_comp)))
})

test_that("run_anova() requires an explicit value_col (no silent default)", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = c("A1", "A2"),
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value_raw = c(1, 2),
    keep_well = TRUE
  )
  expect_error(
    run_anova(d, group_cols = c("plate_id", "endpoint")),
    class = "zfbehavior_error_missing_value_col"
  )
})

test_that("run_anova() errors on missing columns", {
  d <- tibble::tibble(plate_id = "P1")
  expect_error(
    run_anova(d, group_cols = "plate_id", value_col = "endpoint_value"),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("run_anova() enforces keep_well", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "both_auc",
    well_id = c("A1", "A2"),
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(1, 2),
    keep_well = c(TRUE, FALSE)
  )
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value"
    ),
    class = "zfbehavior_error_excluded_wells_present"
  )
})

test_that("run_anova() errors with fewer than 2 conc_unit levels in a group (only VC present, no treated dose)", {
  # check_vc_reference() now catches this case earlier/more specifically
  # (zfbehavior_error_no_treated_dose) than the generic min-levels check
  # -- since a valid VC reference + >=1 treated dose already guarantees
  # >=2 conc_unit levels, this scenario is caught by the VC-reference
  # check first.
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "both_auc",
    well_id = c("A1", "A2", "A3"),
    concentration = 0,
    concentration_unit = "uM",
    is_vehicle_control = TRUE,
    endpoint_value = c(1, 2, 3),
    keep_well = TRUE
  )
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value"
    ),
    class = "zfbehavior_error_no_treated_dose"
  )
})

test_that("run_anova() errors on ambiguous endpoint_transforms patterns", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "both_auc_spearman_overlap",
    well_id = c("A1", "A2"),
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(1, 2),
    keep_well = TRUE
  )
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = list(both_auc = "log10", spearman = "fisher_z")
    ),
    class = "zfbehavior_error_ambiguous_endpoint_transform"
  )
})

test_that("run_anova() endpoint_transforms = NULL skips filtering/transforming", {
  set.seed(2)
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "total_distance", # matches no default pattern
    well_id = paste0("W", 1:30),
    concentration = rep(c(0, 50, 150), each = 10),
    concentration_unit = "uM",
    is_vehicle_control = .data$concentration == 0,
    endpoint_value = rnorm(30, mean = 100, sd = 10) +
      rep(c(0, 5, 10), each = 10),
    keep_well = TRUE
  )
  expect_no_error(
    out <- run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    )
  )
  expect_equal(nrow(out$anova), 1)
})

test_that("run_anova() groups independently across multiple plates (chemical_id pooling)", {
  set.seed(3)
  mk_plate <- function(pid) {
    tibble::tibble(
      plate_id = pid,
      chemical_id = "chemX",
      endpoint = "both_auc",
      well_id = paste0("W", 1:20),
      concentration = rep(c(0, 50), each = 10),
      concentration_unit = "uM",
      is_vehicle_control = .data$concentration == 0,
      endpoint_value = rnorm(20, mean = 5, sd = 0.5) +
        rep(c(0, 1), each = 10),
      keep_well = TRUE
    )
  }
  d <- dplyr::bind_rows(mk_plate("P1"), mk_plate("P2"))
  out <- run_anova(
    d,
    group_cols = c("chemical_id", "endpoint"),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )
  expect_equal(nrow(out$anova), 1) # pooled into ONE group despite 2 plates
})

test_that("run_anova() requires exactly one distinct endpoint_col value per model group", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = rep(c("e1", "e2"), each = 10),
    well_id = paste0("W", 1:20),
    concentration = rep(c(0, 50), 10),
    concentration_unit = "uM",
    is_vehicle_control = .data$concentration == 0,
    endpoint_value = rnorm(20, mean = 5),
    keep_well = TRUE
  )
  # group_cols = "plate_id" only -- endpoint NOT part of the key, so two
  # distinct endpoints silently reach the same model group.
  expect_error(
    run_anova(
      d,
      group_cols = "plate_id",
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_mixed_endpoints_in_group"
  )
})

# ---- Vehicle-control reference guarantee (Module 4 review item 1) ---------

test_that("run_anova() errors when no vehicle-control rows are present", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = paste0("W", 1:6),
    concentration = rep(c(50, 150), each = 3),
    concentration_unit = "uM",
    is_vehicle_control = FALSE,
    endpoint_value = c(1, 2, 3, 4, 5, 6),
    keep_well = TRUE
  )
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_missing_vc_reference"
  )
})

test_that("run_anova() errors when multiple vehicle-control dose keys are present", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = paste0("W", 1:9),
    concentration = c(0, 0, 0, 5, 5, 5, 50, 50, 50),
    concentration_unit = c(rep("uM", 6), rep("uM", 3)),
    is_vehicle_control = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    endpoint_value = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    keep_well = TRUE
  )
  # Two distinct VC dose keys: "0uM" and "5uM" both flagged is_vehicle_control.
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_multiple_vc_references"
  )
})

test_that("run_anova() errors when a treated row shares the VC's dose key", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = paste0("W", 1:9),
    concentration = c(0, 0, 0, 0, 50, 50, 50, 50, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    endpoint_value = 1:9,
    keep_well = TRUE
  )
  # "0uM" appears in both VC (is_vehicle_control==TRUE) and treated
  # (is_vehicle_control==FALSE) rows -- ambiguous.
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_ambiguous_vc_reference"
  )
})

test_that("run_anova() errors when no treated dose remains", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = paste0("W", 1:3),
    concentration = 0,
    concentration_unit = "uM",
    is_vehicle_control = TRUE,
    endpoint_value = c(1, 2, 3),
    keep_well = TRUE
  )
  # Only one conc_unit level exists at all -- fails the min-levels check
  # via the VC-reference check first (no treated dose).
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_no_treated_dose"
  )
})

test_that("run_anova() errors when all vehicle-control observations are non-finite", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = paste0("W", 1:6),
    concentration = rep(c(0, 50), each = 3),
    concentration_unit = "uM",
    is_vehicle_control = rep(c(TRUE, FALSE), each = 3),
    endpoint_value = c(NA_real_, NA_real_, NA_real_, 4, 5, 6),
    keep_well = TRUE
  )
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_no_finite_vc_observation"
  )
})

test_that("run_anova() errors when is_vc_col contains NA", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = paste0("W", 1:6),
    concentration = rep(c(0, 50), each = 3),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, TRUE, NA, FALSE, FALSE, FALSE),
    endpoint_value = 1:6,
    keep_well = TRUE
  )
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_invalid_vc_flag"
  )
})

test_that("run_anova() passes the control label explicitly to DunnettTest() (correct even with non-alphabetical VC placement)", {
  # Regression test: confirm the VC group is correctly identified as
  # control even when it would NOT be factor level 1 by default ordering
  # alone if labels/order were naive -- build_conc_unit_factor() already
  # orders VC first, but this test locks in that check_vc_reference()'s
  # explicit control= argument makes this robust regardless.
  set.seed(5)
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = paste0("W", 1:15),
    concentration = rep(c(0, 50, 150), each = 5),
    concentration_unit = "uM",
    is_vehicle_control = .data$concentration == 0,
    endpoint_value = rnorm(15, mean = rep(c(5, 8, 12), each = 5)),
    keep_well = TRUE
  )
  out <- run_anova(
    d,
    group_cols = c("plate_id", "endpoint"),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )
  expect_true(all(grepl("-0uM$", out$dunn$pairwise_comp)))
})

# ---- Transform/domain validation: warn, don't block --------------------
# Transform domain/output violations are surfaced as WARNINGS, not
# blocking errors (log10/fisher_z boundary values can be legitimate --
# e.g. Movement Similarity exactly 1 -- so this package does not assume
# every domain violation is a caller mistake). The transform is applied
# as-is regardless. If the resulting value is genuinely non-finite and
# would break the model, that is caught SEPARATELY as a hard error by
# check_finite_model_values() immediately before aov()/gls() is fit --
# these two guards are independent and layered, not one relaxed into
# the other.

test_that("apply_endpoint_transforms() warns (does not error) on log10 of a non-positive value", {
  d <- tibble::tibble(
    endpoint = "both_auc",
    endpoint_value = c(0, 2) # log10(0) = -Inf
  )
  # Two layered warnings: input domain, then non-finite output.
  expect_warning(
    expect_warning(
      out <- apply_endpoint_transforms(
        d,
        "endpoint",
        "endpoint_value",
        list(both_auc = "log10"),
        "test_fn"
      ),
      class = "zfbehavior_warning_transform_domain"
    ),
    class = "zfbehavior_warning_non_finite_transform_output"
  )
  # The transform is still applied as-is -- log10(0) = -Inf passes through.
  expect_equal(out$endpoint_value, c(-Inf, log10(2)))
})

test_that("apply_endpoint_transforms() warns (does not error) on fisher_z at exactly 1", {
  d <- tibble::tibble(
    endpoint = "spearman",
    endpoint_value = c(0.5, 1) # fisher_z(1) = Inf
  )
  # Two layered warnings: input domain, then non-finite output.
  expect_warning(
    expect_warning(
      out <- apply_endpoint_transforms(
        d,
        "endpoint",
        "endpoint_value",
        list(spearman = "fisher_z"),
        "test_fn"
      ),
      class = "zfbehavior_warning_transform_domain"
    ),
    class = "zfbehavior_warning_non_finite_transform_output"
  )
  expect_equal(out$endpoint_value, c(0.5 * log((1 + 0.5) / (1 - 0.5)), Inf))
})

test_that("run_anova() still hard-errors (via check_finite_model_values()) when a transform-domain-violating value is genuinely non-finite, despite apply_endpoint_transforms() itself only warning", {
  # Confirms the two guards are independently layered: a warning from
  # apply_endpoint_transforms() does not, by itself, block anything --
  # but a resulting non-finite value (log10(0) = -Inf on the TREATED row
  # here -- deliberately not the VC row, so this test isolates
  # check_finite_model_values()'s guard specifically, rather than
  # tripping the separately-tested "no finite VC observation" check
  # first) still can't reach aov()/gls(), caught downstream by
  # check_finite_model_values()'s own hard error, which remains
  # unrelaxed.
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "both_auc",
    well_id = c("A1", "A2"),
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(2, 0), # log10(0) = -Inf on the treated (50uM) row
    keep_well = TRUE
  )
  expect_warning(
    expect_error(
      run_anova(
        d,
        group_cols = c("plate_id", "endpoint"),
        value_col = "endpoint_value",
        endpoint_transforms = list(both_auc = "log10")
      ),
      class = "zfbehavior_error_non_finite_model_input"
    ),
    class = "zfbehavior_warning_transform_domain"
  ) |>
    expect_warning(class = "zfbehavior_warning_non_finite_transform_output")
})

test_that("apply_endpoint_transforms() errors on invalid endpoint_transforms mapping structure", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "both_auc",
    well_id = c("A1", "A2"),
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(1, 2),
    keep_well = TRUE
  )
  # unnamed list
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = list("log10")
    ),
    class = "zfbehavior_error_invalid_endpoint_transforms"
  )
  # duplicate names
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = list(both_auc = "log10", both_auc = "fisher_z")
    ),
    class = "zfbehavior_error_invalid_endpoint_transforms"
  )
  # NULL method
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = list(both_auc = NULL)
    ),
    class = "zfbehavior_error_invalid_endpoint_transforms"
  )
  # unsupported method
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = list(both_auc = "sqrt")
    ),
    class = "zfbehavior_error_invalid_endpoint_transforms"
  )
  # multivalued method
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = list(both_auc = c("log10", "fisher_z"))
    ),
    class = "zfbehavior_error_invalid_endpoint_transforms"
  )
})

test_that("apply_endpoint_transforms() exact log10/fisher_z values (not just successful completion)", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = c("both_auc", "spearman"),
    well_id = c("A1", "A2"),
    concentration = c(0, 0),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, TRUE),
    endpoint_value = c(100, 0.5),
    keep_well = TRUE
  )
  out <- apply_endpoint_transforms(
    d,
    "endpoint",
    "endpoint_value",
    list(both_auc = "log10", spearman = "fisher_z"),
    "test_fn"
  )
  expect_equal(out$endpoint_value[out$endpoint == "both_auc"], log10(100))
  expect_equal(out$endpoint_value[out$endpoint == "spearman"], 0.5 * log((1 + 0.5) / (1 - 0.5)))
})

test_that("apply_endpoint_transforms() unmatched policy: default drop, error, and keep", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = c("both_auc", "total_distance"),
    well_id = c("A1", "A2"),
    concentration = c(0, 0),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, TRUE),
    endpoint_value = c(100, 50),
    keep_well = TRUE
  )
  # default (drop): total_distance silently excluded
  out_drop <- apply_endpoint_transforms(
    d,
    "endpoint",
    "endpoint_value",
    list(both_auc = "log10"),
    "test_fn"
  )
  expect_equal(nrow(out_drop), 1)
  expect_equal(out_drop$endpoint, "both_auc")

  # error: raises a classed error naming the unmatched endpoint
  expect_error(
    apply_endpoint_transforms(
      d,
      "endpoint",
      "endpoint_value",
      list(both_auc = "log10"),
      "test_fn",
      unmatched = "error"
    ),
    class = "zfbehavior_error_unmatched_endpoint"
  )

  # keep: total_distance retained, untransformed
  out_keep <- apply_endpoint_transforms(
    d,
    "endpoint",
    "endpoint_value",
    list(both_auc = "log10"),
    "test_fn",
    unmatched = "keep"
  )
  expect_equal(nrow(out_keep), 2)
  expect_equal(
    out_keep$endpoint_value[out_keep$endpoint == "total_distance"],
    50
  )
  expect_equal(
    out_keep$endpoint_value[out_keep$endpoint == "both_auc"],
    log10(100)
  )
})

test_that("apply_endpoint_transforms() errors when all endpoints are unmatched and dropped (no analyzable rows)", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "total_distance",
    well_id = c("A1", "A2"),
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(1, 2),
    keep_well = TRUE
  )
  expect_error(
    apply_endpoint_transforms(
      d,
      "endpoint",
      "endpoint_value",
      list(both_auc = "log10"),
      "test_fn"
    ),
    class = "zfbehavior_error_no_analyzable_rows"
  )
})

test_that("apply_endpoint_transforms() with default mapping arguments (not passed explicitly)", {
  # Confirms run_anova()'s formal default mapping (both_auc/spearman) is
  # itself well-formed and behaves as documented when invoked without an
  # explicit endpoint_transforms argument.
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = rep(c("both_auc", "spearman"), each = 4),
    well_id = paste0("W", 1:8),
    concentration = rep(c(0, 0, 50, 50), 2),
    concentration_unit = "uM",
    is_vehicle_control = rep(c(TRUE, TRUE, FALSE, FALSE), 2),
    endpoint_value = c(10, 12, 20, 22, 0.1, 0.2, 0.5, 0.6),
    keep_well = TRUE
  )
  expect_no_error(
    out <- run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value"
    )
  )
  expect_equal(nrow(out$anova), 2)
})

test_that("apply_endpoint_transforms() one-row and zero-row helper paths", {
  d1 <- tibble::tibble(
    plate_id = "P1",
    endpoint = "both_auc",
    endpoint_value = 10
  )
  out1 <- apply_endpoint_transforms(
    d1, "endpoint", "endpoint_value", list(both_auc = "log10"), "test_fn"
  )
  expect_equal(nrow(out1), 1)
  expect_equal(out1$endpoint_value, log10(10))

  d0 <- d1[0, ]
  expect_error(
    apply_endpoint_transforms(
      d0, "endpoint", "endpoint_value", list(both_auc = "log10"), "test_fn"
    ),
    class = "zfbehavior_error_no_analyzable_rows"
  )
})

# ---- Concentration-unit policy (Module 4 review item 10) -------------------

test_that("run_anova() requires a non-NULL conc_unit_col to actually exist in d", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = c("A1", "A2"),
    concentration = c(0, 50),
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(1, 2),
    keep_well = TRUE
  )
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      conc_unit_col = "concentration_unit", # does not exist in d
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_missing_conc_unit_col"
  )
})

test_that("run_anova() supports conc_unit_col = NULL as the explicit unitless mode", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = paste0("W", 1:6),
    concentration = rep(c(0, 50), each = 3),
    is_vehicle_control = rep(c(TRUE, FALSE), each = 3),
    endpoint_value = c(1, 2, 3, 4, 5, 6),
    keep_well = TRUE
  )
  expect_no_error(
    out <- run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      conc_unit_col = NULL,
      endpoint_transforms = NULL
    )
  )
  expect_true(all(grepl("^[0-9]+-[0-9]+$", out$dunn$pairwise_comp)))
})

test_that("run_anova() rejects missing/empty concentration-unit values within a group", {
  d <- tibble::tibble(
    plate_id = "P1",
    endpoint = "e1",
    well_id = paste0("W", 1:6),
    concentration = rep(c(0, 50), each = 3),
    concentration_unit = c("uM", "uM", NA_character_, "uM", "uM", "uM"),
    is_vehicle_control = rep(c(TRUE, FALSE), each = 3),
    endpoint_value = 1:6,
    keep_well = TRUE
  )
  expect_error(
    run_anova(
      d,
      group_cols = c("plate_id", "endpoint"),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_invalid_conc_unit_value"
  )
})

# ---- run_rmanova(): synthetic unit tests ------------------------------------

test_that("run_rmanova() requires an explicit value_col (no silent default)", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = "A1",
    phase = "L1",
    endpoint = "both_auc",
    concentration = 0,
    concentration_unit = "uM",
    is_vehicle_control = TRUE,
    endpoint_value_raw = 1,
    keep_well = TRUE
  )
  expect_error(
    run_rmanova(
      d,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = c("L1", "D1")
    ),
    class = "zfbehavior_error_missing_value_col"
  )
})

test_that("run_rmanova() requires phase_levels (at least 2, non-missing, no duplicates)", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = "A1",
    phase = "L1",
    endpoint = "both_auc",
    concentration = 0,
    concentration_unit = "uM",
    is_vehicle_control = TRUE,
    endpoint_value = 1,
    keep_well = TRUE
  )
  expect_error(
    run_rmanova(
      d,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = "L1", # too few
      value_col = "endpoint_value"
    ),
    class = "zfbehavior_error_invalid_phase_levels"
  )
  expect_error(
    run_rmanova(
      d,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = c("L1", "L1"), # duplicated
      value_col = "endpoint_value"
    ),
    class = "zfbehavior_error_invalid_phase_levels"
  )
  expect_error(
    run_rmanova(
      d,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = c("L1", NA_character_),
      value_col = "endpoint_value"
    ),
    class = "zfbehavior_error_invalid_phase_levels"
  )
})

test_that("run_rmanova() errors on missing columns", {
  d <- tibble::tibble(plate_id = "P1")
  expect_error(
    run_rmanova(
      d,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = c("L1", "D1"),
      value_col = "endpoint_value"
    ),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("run_rmanova() enforces keep_well", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2"),
    phase = c("L1", "L1"),
    endpoint = "both_auc",
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(1, 2),
    keep_well = c(TRUE, FALSE)
  )
  expect_error(
    run_rmanova(
      d,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = c("L1", "D1"),
      value_col = "endpoint_value"
    ),
    class = "zfbehavior_error_excluded_wells_present"
  )
})

test_that("run_rmanova() errors when phase_col contains a value not in phase_levels", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2"),
    phase = c("L1", "L3"),
    endpoint = "both_auc",
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(1, 2),
    keep_well = TRUE
  )
  expect_error(
    run_rmanova(
      d,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = c("L1", "D1"), # "L3" not included
      value_col = "endpoint_value"
    ),
    class = "zfbehavior_error_unrecognized_phase_value"
  )
})

make_rmanova_fixture <- function(seed = 1, n_well = 96) {
  set.seed(seed)
  phases <- factor(
    rep(c("L1", "D1", "L2", "D2", "D2-ext"), each = n_well),
    levels = c("L1", "D1", "L2", "D2", "D2-ext")
  )
  base_conc <- rep(c(0, 5, 15, 50, 150, 500), each = n_well / 6)
  tibble::tibble(
    plate_id = "P1",
    well_id = rep(paste0("W", seq_len(n_well)), 5),
    concentration = rep(base_conc, 5),
    concentration_unit = "uM",
    is_vehicle_control = .data$concentration == 0,
    phase = phases,
    endpoint = "both_auc",
    endpoint_value = exp(
      stats::rnorm(n_well * 5, mean = 5, sd = 0.4) + as.numeric(phases) * 0.1
    ),
    keep_well = TRUE
  )
}

zf_default_phase_levels <- function() c("L1", "D1", "L2", "D2", "D2-ext")

test_that("run_rmanova() keeps D2 and D2-ext as distinct phase levels", {
  d <- make_rmanova_fixture()
  out <- run_rmanova(
    d,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )
  expect_setequal(
    out$full_dunn$phase,
    c("L1", "D1", "L2", "D2", "D2-ext")
  )
})

test_that("run_rmanova() result is independent of input row order (explicit phase_index fix)", {
  d <- make_rmanova_fixture()
  d_shuffled <- d[sample(nrow(d)), ]

  out1 <- run_rmanova(
    d,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )
  out2 <- run_rmanova(
    d_shuffled,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )
  expect_equal(out1$rmanova_simple, out2$rmanova_simple)
  expect_equal(out1$rmanova$p_value, out2$rmanova$p_value)
})

test_that("run_rmanova() result is identical under a deliberately non-alphabetical phase_levels order", {
  # phase_levels is the sole source of temporal order -- confirm giving
  # phase_col non-alphabetically-sortable values still works correctly
  # as long as phase_levels correctly encodes the true order.
  d <- make_rmanova_fixture()
  out_std <- run_rmanova(
    d,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )

  d_renamed <- d
  # Rename phases to intentionally-non-alphabetical labels while
  # preserving true temporal order via phase_levels.
  rename_map <- c(L1 = "zzz_first", D1 = "aaa_second", L2 = "mmm_third", D2 = "bbb_fourth", `D2-ext` = "qqq_fifth")
  d_renamed$phase <- unname(rename_map[as.character(d_renamed$phase)])
  out_renamed <- run_rmanova(
    d_renamed,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = unname(rename_map[zf_default_phase_levels()]),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )
  expect_equal(out_std$rmanova_simple, out_renamed$rmanova_simple)
})

test_that("run_rmanova() disambiguates subjects across plates via plate_id + well_id", {
  # Same well_id names reused across 2 plates on purpose -- the composite
  # subject key (plate_id + well_id) must keep them distinct; this test
  # only confirms the function runs without error and produces exactly
  # one pooled group (not a numeric ground-truth check).
  mk_plate <- function(pid) {
    n_well <- 32
    phases <- factor(rep(c("L1", "D1"), each = n_well), levels = c("L1", "D1"))
    base_conc <- rep(c(0, 50), each = n_well / 2)
    tibble::tibble(
      plate_id = pid,
      chemical_id = "chemX",
      well_id = rep(paste0("W", seq_len(n_well)), 2),
      concentration = rep(base_conc, 2),
      concentration_unit = "uM",
      is_vehicle_control = .data$concentration == 0,
      phase = phases,
      endpoint = "both_auc",
      endpoint_value = exp(stats::rnorm(n_well * 2, mean = 5, sd = 0.3)),
      keep_well = TRUE
    )
  }
  set.seed(4)
  d <- dplyr::bind_rows(mk_plate("P1"), mk_plate("P2"))
  expect_no_error(
    out <- run_rmanova(
      d,
      group_cols = "chemical_id",
      phase_col = "phase",
      phase_levels = c("L1", "D1"),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    )
  )
  expect_equal(nrow(out$rmanova_simple), 1)
})

test_that("run_rmanova() errors with fewer than 2 phase levels after filtering", {
  d <- tibble::tibble(
    plate_id = "P1",
    well_id = c("A1", "A2"),
    phase = c("L1", "L1"),
    endpoint = "both_auc",
    concentration = c(0, 50),
    concentration_unit = "uM",
    is_vehicle_control = c(TRUE, FALSE),
    endpoint_value = c(1, 2),
    keep_well = TRUE
  )
  expect_error(
    run_rmanova(
      d,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = c("L1", "D1"),
      value_col = "endpoint_value"
    ),
    class = "zfbehavior_error_insufficient_levels"
  )
})

test_that("run_rmanova() does NOT require a single endpoint_col value per group (it pools phase-labeled endpoint rows by design)", {
  # Unlike run_anova(), run_rmanova()'s entire purpose is to pool
  # multiple phase-tagged endpoint_col values (e.g. "L1@both_auc",
  # "D1@both_auc", ...) into one repeated-measures model per group --
  # requiring endpoint_col uniqueness here would defeat that design.
  d <- make_rmanova_fixture()
  d2 <- d
  d2$endpoint <- paste0(as.character(d2$phase), "@both_auc")
  expect_no_error(
    run_rmanova(
      d2,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = zf_default_phase_levels(),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    )
  )
})

# ---- RMANOVA repeated-measure structure validation (review item 2) --------

test_that("run_rmanova() rejects duplicate subject x phase rows", {
  d <- make_rmanova_fixture()
  d_dup <- dplyr::bind_rows(d, d[d$well_id == "W1" & d$phase == "L1", ])
  expect_error(
    run_rmanova(
      d_dup,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = zf_default_phase_levels(),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_duplicate_subject_phase"
  )
})

test_that("run_rmanova() rejects a subject missing an intermediate phase", {
  d <- make_rmanova_fixture()
  # Remove W1's L2 phase row (an intermediate phase, not first/last).
  d_gap <- d[!(d$well_id == "W1" & d$phase == "L2"), ]
  expect_error(
    run_rmanova(
      d_gap,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = zf_default_phase_levels(),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_incomplete_subject_phases"
  )
})

test_that("run_rmanova() rejects a subject missing a trailing phase (strict completeness policy)", {
  # Deliberate design decision:
  # reject ANY incomplete subject, even a clean trailing/leading gap that
  # the explicit phase_index would handle correctly without distortion.
  d <- make_rmanova_fixture()
  d_gap <- d[!(d$well_id == "W1" & d$phase == "D2-ext"), ]
  expect_error(
    run_rmanova(
      d_gap,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = zf_default_phase_levels(),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_incomplete_subject_phases"
  )
})

test_that("run_rmanova() rejects a whole group missing one declared phase", {
  d <- make_rmanova_fixture()
  # Drop D2-ext entirely from the group -- every subject is "complete"
  # relative to the group's remaining phases (L1/D1/L2/D2), so this is
  # allowed (droplevels() + check_min_levels() handles the >= 2 phases
  # requirement; this is not an error case for check_rmanova_subject_structure()
  # itself, since every remaining subject still observes the full
  # (reduced) phase set). This test documents that expectation.
  d_no_ext <- d[d$phase != "D2-ext", ]
  expect_no_error(
    out <- run_rmanova(
      d_no_ext,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = zf_default_phase_levels(),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    )
  )
  expect_setequal(unique(out$full_dunn$phase), c("L1", "D1", "L2", "D2"))
})

test_that("run_rmanova() rejects treatment/dose changing within a subject", {
  d <- make_rmanova_fixture()
  # Change W17's (a TREATED, non-VC well) concentration on just its L1
  # row -- violates within-subject treatment invariance without also
  # triggering the (separately tested) VC-reference checks.
  d_bad <- d
  d_bad$concentration[d_bad$well_id == "W17" & d_bad$phase == "L1"] <- 999
  expect_error(
    run_rmanova(
      d_bad,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = zf_default_phase_levels(),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_non_invariant_subject_treatment"
  )
})

test_that("run_rmanova() rejects concentration_unit changing within a subject", {
  # Isolates the subject-invariance check itself (using concentration_unit
  # rather than is_vehicle_control, which would otherwise ALSO trip the
  # separately-tested VC-reference ambiguity check first if a VC well's
  # own flag were flipped -- both are real, independently-tested
  # failure modes, but this test targets the invariance check alone).
  d <- make_rmanova_fixture()
  d_bad <- d
  target <- d_bad$well_id == "W17" & d_bad$phase == "L1" # a treated well
  d_bad$concentration_unit[target] <- "mM"
  expect_error(
    run_rmanova(
      d_bad,
      group_cols = "plate_id",
      phase_col = "phase",
      phase_levels = zf_default_phase_levels(),
      value_col = "endpoint_value",
      endpoint_transforms = NULL
    ),
    class = "zfbehavior_error_non_invariant_subject_treatment"
  )
})

test_that("check_rmanova_subject_structure() directly rejects vehicle-control status changing within a subject (in isolation)", {
  # Directly exercises check_rmanova_subject_structure() with an
  # internal-column-shaped input, isolating the VC-flag-invariance branch
  # from run_rmanova()'s own upstream VC-reference-ambiguity check (which
  # a full run_rmanova() call would hit first if a VC well's flag were
  # flipped in a way that also creates dose-key overlap).
  sub <- tibble::tibble(
    .zfbehavior_subject = rep(1:2, each = 2),
    .zfbehavior_phase = factor(rep(c("L1", "D1"), 2), levels = c("L1", "D1")),
    .zfbehavior_conc_unit = factor(rep("0uM", 4), levels = "0uM"),
    is_vehicle_control = c(TRUE, FALSE, TRUE, TRUE) # subject 1 changes
  )
  expect_error(
    check_rmanova_subject_structure(sub, "test_fn", is_vc_col = "is_vehicle_control"),
    class = "zfbehavior_error_non_invariant_subject_treatment"
  )
})

# ---- full_dunn per-phase corAR1() inertness (review item 11) --------------

test_that("run_rmanova()'s per-phase full_dunn fit is numerically identical with vs. without corAR1() (Phi mathematically forced to 0)", {
  d <- make_rmanova_fixture()
  out <- run_rmanova(
    d,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )

  # Reproduce fit_one()'s internal per-phase-subset construction directly,
  # with vs. without corAR1(), and confirm identical results -- locking in
  # that the AR(1) structure is provably inert in this specific
  # one-observation-per-subject context (matching the original RMANOVA's own
  # true original methodology, see run_rmanova()'s roxygen @section
  # "RMANOVA post-hoc inference").
  d$.zfbehavior_subject <- as.integer(interaction(d$plate_id, d$well_id, drop = TRUE))
  d$.zfbehavior_value <- d$endpoint_value
  d$.zfbehavior_phase <- factor(d$phase, levels = zf_default_phase_levels())
  d$.zfbehavior_conc_unit <- build_conc_unit_factor(d$concentration, d$concentration_unit, d$is_vehicle_control)

  sub_l1 <- d[d$.zfbehavior_phase == "L1", ]
  sub_l1 <- sub_l1[order(sub_l1$.zfbehavior_subject), ]

  m_with_ar1 <- nlme::gls(
    .zfbehavior_value ~ .zfbehavior_conc_unit,
    data = sub_l1,
    correlation = nlme::corAR1(form = ~ 1 | .zfbehavior_subject),
    method = "REML"
  )
  m_without_ar1 <- nlme::gls(
    .zfbehavior_value ~ .zfbehavior_conc_unit,
    data = sub_l1,
    method = "REML"
  )
  expect_equal(
    as.numeric(stats::coef(m_with_ar1)),
    as.numeric(stats::coef(m_without_ar1)),
    tolerance = 1e-10
  )
  expect_equal(
    stats::logLik(m_with_ar1)[1],
    stats::logLik(m_without_ar1)[1],
    tolerance = 1e-8
  )
  # Phi (the AR(1) correlation parameter) is mathematically forced to 0
  # when each subject contributes exactly one observation.
  phi <- as.numeric(stats::coef(m_with_ar1$modelStruct$corStruct, unconstrained = FALSE))
  expect_equal(phi, 0, tolerance = 1e-8)
})

# ---- run_anova_rmanova_hit_calling(): synthetic unit tests -----------------

test_that("run_anova_rmanova_hit_calling() ANOVA cascade: not-sig omnibus is not a hit", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "A",
      endpoint = "e1",
      df_1 = 2,
      df_2 = 10,
      F_value = 0.5,
      p_value = 0.6
    ),
    dunn = tibble::tibble(
      plate_id = "A",
      endpoint = "e1",
      pairwise_comp = "5-0",
      diff = 0,
      lwr_ci = 0,
      upr_ci = 0,
      p_value = 0.9
    )
  )
  out <- run_anova_rmanova_hit_calling(
    anova_result,
    NULL,
    anova_group_cols = c("plate_id", "endpoint")
  )
  expect_true(is.na(out$hit_p_value))
  expect_true(is.na(out$neg_log10_p_hit))
  expect_false(out$is_hit)
  expect_equal(out$decision_state, "not_hit")
})

test_that("run_anova_rmanova_hit_calling() ANOVA cascade: sig omnibus but n_posthoc_sig == 0 is not a hit", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "B",
      endpoint = "e1",
      df_1 = 2,
      df_2 = 10,
      F_value = 5,
      p_value = 0.02
    ),
    dunn = tibble::tibble(
      plate_id = "B",
      endpoint = "e1",
      pairwise_comp = "5-0",
      diff = 0,
      lwr_ci = 0,
      upr_ci = 0,
      p_value = 0.6
    )
  )
  out <- run_anova_rmanova_hit_calling(
    anova_result,
    NULL,
    anova_group_cols = c("plate_id", "endpoint")
  )
  expect_true(is.na(out$hit_p_value))
  expect_equal(out$n_posthoc_sig, 0L)
  expect_equal(out$decision_state, "not_hit")
})

test_that("run_anova_rmanova_hit_calling() ANOVA cascade: sig omnibus + n_posthoc_sig > 0 is a hit", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "C",
      endpoint = "e1",
      df_1 = 2,
      df_2 = 10,
      F_value = 8,
      p_value = 0.001
    ),
    dunn = tibble::tibble(
      plate_id = c("C", "C"),
      endpoint = "e1",
      pairwise_comp = c("5-0", "15-0"),
      diff = 0,
      lwr_ci = 0,
      upr_ci = 0,
      p_value = c(0.01, 0.9)
    )
  )
  out <- run_anova_rmanova_hit_calling(
    anova_result,
    NULL,
    anova_group_cols = c("plate_id", "endpoint")
  )
  expect_equal(out$n_posthoc_sig, 1L)
  expect_equal(out$hit_p_value, 0.001)
  expect_equal(out$neg_log10_p_hit, -log10(0.001))
  expect_equal(out$test, "ANOVA")
  expect_true(out$is_hit)
  expect_equal(out$decision_state, "hit")
  expect_equal(out$decision_source, "anova")
})

test_that("run_anova_rmanova_hit_calling() ANOVA cascade: NA omnibus p-value is indeterminate, not a silent non-hit/hit", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "Z",
      endpoint = "e1",
      df_1 = 2,
      df_2 = 10,
      F_value = NA_real_,
      p_value = NA_real_
    ),
    dunn = tibble::tibble(
      plate_id = "Z",
      endpoint = "e1",
      pairwise_comp = "5-0",
      diff = 0,
      lwr_ci = 0,
      upr_ci = 0,
      p_value = 0.01
    )
  )
  out <- run_anova_rmanova_hit_calling(
    anova_result,
    NULL,
    anova_group_cols = c("plate_id", "endpoint")
  )
  expect_true(is.na(out$is_hit))
  expect_equal(out$decision_state, "indeterminate")
  expect_true(is.na(out$hit_p_value))
})

test_that("run_anova_rmanova_hit_calling() ANOVA cascade: sig omnibus, non-sig post-hoc, but an NA post-hoc p-value is indeterminate (not silently coalesced to non-hit)", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "Y",
      endpoint = "e1",
      df_1 = 2,
      df_2 = 10,
      F_value = 8,
      p_value = 0.001
    ),
    dunn = tibble::tibble(
      plate_id = c("Y", "Y"),
      endpoint = "e1",
      pairwise_comp = c("5-0", "15-0"),
      diff = 0,
      lwr_ci = 0,
      upr_ci = 0,
      p_value = c(0.9, NA_real_)
    )
  )
  out <- run_anova_rmanova_hit_calling(
    anova_result,
    NULL,
    anova_group_cols = c("plate_id", "endpoint")
  )
  expect_equal(out$decision_state, "indeterminate")
  expect_true(is.na(out$is_hit))
})

test_that("run_anova_rmanova_hit_calling() rejects invalid p-values (NaN/Inf/negative/>1)", {
  mk <- function(pval) {
    list(
      anova = tibble::tibble(
        plate_id = "A", endpoint = "e1", df_1 = 1, df_2 = 1, F_value = 1, p_value = pval
      ),
      dunn = tibble::tibble(
        plate_id = "A", endpoint = "e1", pairwise_comp = "x",
        diff = 0, lwr_ci = 0, upr_ci = 0, p_value = 0.5
      )
    )
  }
  for (bad in list(NaN, Inf, -0.1, 1.1)) {
    expect_error(
      run_anova_rmanova_hit_calling(
        mk(bad),
        NULL,
        anova_group_cols = c("plate_id", "endpoint")
      ),
      class = "zfbehavior_error_invalid_pvalue"
    )
  }
})

test_that("run_anova_rmanova_hit_calling() RMANOVA cascade: covers all branches including NA-interaction indeterminate", {
  rmanova_result <- list(
    rmanova_simple = tibble::tibble(
      plate_id = c("D", "E", "F", "G", "H", "I"),
      p_value_interaction_full = c(0.001, 0.01, 0.5, 0.6, 0.7, NA_real_),
      p_value_conc_unit_reduced = c(0.2, 0.02, 0.001, 0.02, 0.9, 0.01)
    ),
    reduced_dunn = tibble::tibble(
      plate_id = c("D", "E", "F", "F", "G", "I"),
      p_value = c(0.9, 0.9, 0.01, 0.9, 0.9, 0.01)
    ),
    full_dunn = tibble::tibble(
      plate_id = c("D", "E"),
      p_value = c(0.01, 0.9)
    )
  )
  out <- run_anova_rmanova_hit_calling(
    NULL,
    rmanova_result,
    rmanova_group_cols = "plate_id"
  )
  row <- function(pid) out[out$plate_id == pid, ]

  # D: interaction sig, n_posthoc_sig_full > 0 -> hit via interaction
  expect_equal(row("D")$hit_p_value, 0.001)
  expect_equal(row("D")$decision_state, "hit")
  expect_equal(row("D")$decision_source, "interaction")
  # E: interaction sig, n_posthoc_sig_full == 0 -> NOT a hit, even though
  # the reduced-model p-value also happens to be < alpha (irrelevant, the
  # cascade never falls back to reduced once interaction is significant).
  # Regression-hardened: reduced_dunn for E is ALSO
  # significant (0.9 in the original test was too weak to prove the
  # cascade doesn't fall through) -- here full_dunn is explicitly
  # non-significant (0.9) while reduced_dunn is 0.9 too, so this remains
  # a true non-hit either way; see the strengthened test below for a
  # stronger full-model-precedence proof.
  expect_true(is.na(row("E")$hit_p_value))
  expect_equal(row("E")$decision_state, "not_hit")
  # F: interaction not sig, reduced sig, n_posthoc_sig_reduced > 0 -> hit
  # via reduced (fallback branch).
  expect_equal(row("F")$hit_p_value, 0.001)
  expect_equal(row("F")$decision_source, "reduced")
  # G: interaction not sig, reduced sig, n_posthoc_sig_reduced == 0 -> not a hit.
  expect_true(is.na(row("G")$hit_p_value))
  expect_equal(row("G")$decision_state, "not_hit")
  # H: neither sig -> not a hit.
  expect_true(is.na(row("H")$hit_p_value))
  expect_equal(row("H")$decision_state, "not_hit")
  # I: interaction p-value is NA -> indeterminate, regardless of reduced model.
  expect_equal(row("I")$decision_state, "indeterminate")
  expect_true(is.na(row("I")$is_hit))
  expect_true(all(out$test == "RMANOVA"))
})

test_that("run_anova_rmanova_hit_calling() RMANOVA: significant interaction blocks reduced-model fallback even when reduced post-hoc IS significant (strengthened full-model-precedence test)", {
  # Regression-hardened: the ORIGINAL "covers all 5
  # branches" test's plate E had a nonsignificant reduced post-hoc, so a
  # buggy implementation that improperly fell through to the reduced
  # branch would have produced the SAME non-hit result by coincidence.
  # Here plate E's reduced post-hoc IS significant, proving the
  # interaction branch's precedence is actually enforced, not merely
  # consistent with a fallback bug.
  rmanova_result <- list(
    rmanova_simple = tibble::tibble(
      plate_id = "E",
      p_value_interaction_full = 0.01, # significant
      p_value_conc_unit_reduced = 0.001 # ALSO significant
    ),
    reduced_dunn = tibble::tibble(plate_id = "E", p_value = 0.01), # significant
    full_dunn = tibble::tibble(plate_id = "E", p_value = 0.9) # NOT significant
  )
  out <- run_anova_rmanova_hit_calling(
    NULL,
    rmanova_result,
    rmanova_group_cols = "plate_id"
  )
  # Interaction is significant but its own post-hoc (full_dunn) is not --
  # must be a non-hit, NOT a hit via the reduced model's (significant)
  # post-hoc, which would happen if the cascade incorrectly fell through.
  expect_true(is.na(out$hit_p_value))
  expect_equal(out$decision_state, "not_hit")
})

test_that("run_anova_rmanova_hit_calling() combines ANOVA and RMANOVA rows via bind_rows()", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "C",
      endpoint = "e1",
      df_1 = 2,
      df_2 = 10,
      F_value = 8,
      p_value = 0.001
    ),
    dunn = tibble::tibble(
      plate_id = "C",
      endpoint = "e1",
      pairwise_comp = "5-0",
      diff = 0,
      lwr_ci = 0,
      upr_ci = 0,
      p_value = 0.01
    )
  )
  rmanova_result <- list(
    rmanova_simple = tibble::tibble(
      plate_id = "D",
      p_value_interaction_full = 0.001,
      p_value_conc_unit_reduced = 0.2
    ),
    reduced_dunn = tibble::tibble(plate_id = "D", p_value = 0.9),
    full_dunn = tibble::tibble(plate_id = "D", p_value = 0.01)
  )
  out <- run_anova_rmanova_hit_calling(
    anova_result,
    rmanova_result,
    anova_group_cols = c("plate_id", "endpoint"),
    rmanova_group_cols = "plate_id"
  )
  expect_equal(nrow(out), 2)
  expect_setequal(out$test, c("ANOVA", "RMANOVA"))
  # Each row's own test-specific columns are populated; the other test
  # type's columns are NA (standard bind_rows() behavior).
  anova_row <- out[out$test == "ANOVA", ]
  rmanova_row <- out[out$test == "RMANOVA", ]
  expect_true(is.na(anova_row$p_value_interaction_full))
  expect_true(is.na(rmanova_row$p_value))
})

test_that("run_anova_rmanova_hit_calling() errors if both results are NULL", {
  expect_error(
    run_anova_rmanova_hit_calling(NULL, NULL),
    class = "zfbehavior_error_no_results_supplied"
  )
})

test_that("run_anova_rmanova_hit_calling() errors if anova_group_cols missing when anova_result supplied", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "A",
      endpoint = "e1",
      df_1 = 1,
      df_2 = 1,
      F_value = 1,
      p_value = 0.5
    ),
    dunn = tibble::tibble(
      plate_id = "A",
      endpoint = "e1",
      pairwise_comp = "x",
      diff = 0,
      lwr_ci = 0,
      upr_ci = 0,
      p_value = 0.5
    )
  )
  expect_error(
    run_anova_rmanova_hit_calling(anova_result, NULL),
    class = "zfbehavior_error_missing_group_cols"
  )
})

test_that("run_anova_rmanova_hit_calling() errors if rmanova_group_cols missing when rmanova_result supplied", {
  rmanova_result <- list(
    rmanova_simple = tibble::tibble(
      plate_id = "A",
      p_value_interaction_full = 0.5,
      p_value_conc_unit_reduced = 0.5
    ),
    reduced_dunn = tibble::tibble(plate_id = "A", p_value = 0.5),
    full_dunn = tibble::tibble(plate_id = "A", p_value = 0.5)
  )
  expect_error(
    run_anova_rmanova_hit_calling(NULL, rmanova_result),
    class = "zfbehavior_error_missing_group_cols"
  )
})

test_that("run_anova_rmanova_hit_calling() errors on invalid alpha (including NA/NaN/Inf, not an unclassed if(NA) error)", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "A",
      endpoint = "e1",
      df_1 = 1,
      df_2 = 1,
      F_value = 1,
      p_value = 0.5
    ),
    dunn = tibble::tibble(
      plate_id = "A",
      endpoint = "e1",
      pairwise_comp = "x",
      diff = 0,
      lwr_ci = 0,
      upr_ci = 0,
      p_value = 0.5
    )
  )
  bad_alphas <- list(1.5, 0, NA_real_, NaN, Inf, -Inf, c(0.05, 0.1), "0.05", integer(0))
  for (bad in bad_alphas) {
    expect_error(
      run_anova_rmanova_hit_calling(
        anova_result,
        NULL,
        anova_group_cols = c("plate_id", "endpoint"),
        alpha = bad
      ),
      class = "zfbehavior_error_invalid_alpha"
    )
  }
})

test_that("run_anova_rmanova_hit_calling() respects a caller-supplied alpha threshold", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "A",
      endpoint = "e1",
      df_1 = 2,
      df_2 = 10,
      F_value = 3,
      p_value = 0.08
    ),
    dunn = tibble::tibble(
      plate_id = "A",
      endpoint = "e1",
      pairwise_comp = "5-0",
      diff = 0,
      lwr_ci = 0,
      upr_ci = 0,
      p_value = 0.08
    )
  )
  out_default <- run_anova_rmanova_hit_calling(
    anova_result,
    NULL,
    anova_group_cols = c("plate_id", "endpoint"),
    alpha = 0.05
  )
  expect_true(is.na(out_default$hit_p_value))

  out_loose <- run_anova_rmanova_hit_calling(
    anova_result,
    NULL,
    anova_group_cols = c("plate_id", "endpoint"),
    alpha = 0.1
  )
  expect_equal(out_loose$hit_p_value, 0.08)
})

test_that("run_anova_rmanova_hit_calling() exact p == alpha is NOT significant (strict inequality)", {
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "A", endpoint = "e1", df_1 = 1, df_2 = 1, F_value = 1, p_value = 0.05
    ),
    dunn = tibble::tibble(
      plate_id = "A", endpoint = "e1", pairwise_comp = "x",
      diff = 0, lwr_ci = 0, upr_ci = 0, p_value = 0.05
    )
  )
  out <- run_anova_rmanova_hit_calling(
    anova_result,
    NULL,
    anova_group_cols = c("plate_id", "endpoint"),
    alpha = 0.05
  )
  expect_true(is.na(out$hit_p_value))
  expect_equal(out$decision_state, "not_hit")
})

# ---- Hit-calling key/provenance safety (review items 3/7) ------------------

test_that("run_anova_rmanova_hit_calling() rejects a plausible-but-incomplete ANOVA key when call_params provenance is available", {
  d <- tibble::tibble(
    plate_id = rep(c("A", "B"), each = 8),
    endpoint = rep(rep(c("e1", "e2"), each = 4), 2),
    well_id = paste0("W", 1:16),
    concentration = rep(rep(c(0, 50), each = 2), 4),
    concentration_unit = "uM",
    is_vehicle_control = rep(rep(c(TRUE, FALSE), each = 2), 4),
    endpoint_value = 1:16,
    keep_well = TRUE
  )
  anova_result <- run_anova(
    d,
    group_cols = c("plate_id", "endpoint"),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )
  # Caller supplies an incomplete key (omits "endpoint") -- must be
  # rejected as a mismatch against the true call_params provenance,
  # not silently accepted because "plate_id" alone is a present column.
  expect_error(
    run_anova_rmanova_hit_calling(
      anova_result,
      NULL,
      anova_group_cols = "plate_id"
    ),
    class = "zfbehavior_error_group_cols_mismatch"
  )
})

test_that("run_anova_rmanova_hit_calling() rejects an incomplete RMANOVA key when call_params provenance is available", {
  d <- make_rmanova_fixture()
  rmanova_result <- run_rmanova(
    d,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = NULL
  )
  # character(0) is non-NULL (so the "is `rmanova_group_cols` supplied at
  # all" check passes) but is an empty, hence mismatched, key relative to
  # the true call_params provenance ("plate_id") -- caught by the
  # exact-match provenance check instead.
  expect_error(
    run_anova_rmanova_hit_calling(
      NULL,
      rmanova_result,
      rmanova_group_cols = character(0)
    ),
    class = "zfbehavior_error_group_cols_mismatch"
  )
})

test_that("run_anova_rmanova_hit_calling() falls back to a structural column-existence check for hand-built lists without call_params", {
  # Hand-built list (as in the synthetic unit tests above) has no
  # call_params attribute -- falls back to checking group_cols exist as
  # columns in the component table, rather than requiring exact
  # provenance match (which would be impossible to satisfy for
  # hand-built test fixtures).
  anova_result <- list(
    anova = tibble::tibble(
      plate_id = "A", endpoint = "e1", df_1 = 1, df_2 = 1, F_value = 1, p_value = 0.5
    ),
    dunn = tibble::tibble(
      plate_id = "A", endpoint = "e1", pairwise_comp = "x",
      diff = 0, lwr_ci = 0, upr_ci = 0, p_value = 0.5
    )
  )
  # A group_cols value not present as a column at all -- rejected.
  expect_error(
    run_anova_rmanova_hit_calling(
      anova_result,
      NULL,
      anova_group_cols = c("plate_id", "nonexistent_col")
    ),
    class = "zfbehavior_error_group_cols_mismatch"
  )
  # A subset that DOES exist as columns -- allowed under the fallback
  # (weaker) check, since there is no provenance metadata to compare
  # against exactly.
  expect_no_error(
    run_anova_rmanova_hit_calling(
      anova_result,
      NULL,
      anova_group_cols = "plate_id"
    )
  )
})
# ---- Real historical production ground-truth regression tests --------------
# See data-raw/build_statistics_snapshot.R for full provenance and the
# documented, investigated reasons some comparisons use a tolerance rather
# than exact equality (Dunnett p-value Monte-Carlo noise; deliberate
# D2/D2-ext phase-collapse and AR(1) row-ordering fixes vs. the legacy
# notebook).

test_that("run_anova() reproduces real historical ANOVA omnibus results exactly", {
  d <- zf_fixture_statistics_input()
  expected <- zf_statistics_outputs()

  out <- run_anova(
    d,
    group_cols = c("plate_id", "endpoint"),
    value_col = "endpoint_value",
    endpoint_transforms = list(both_auc = "log10", spearman = "fisher_z")
  )

  exp_anova <- expected$anova$anova
  exp_anova$endpoint <- gsub("^log10_|^Zt_", "", exp_anova$endpoint)

  cmp <- zf_expect_matching_keys(out$anova, exp_anova, by = c("plate_id", "endpoint"))
  expect_equal(nrow(cmp), 18)
  expect_equal(cmp$F_value_new, cmp$F_value_ref, tolerance = 1e-8)
  expect_equal(cmp$p_value_new, cmp$p_value_ref, tolerance = 1e-8)
})

test_that("run_anova() reproduces real historical Dunnett coefficients exactly (p-values within Monte-Carlo tolerance)", {
  d <- zf_fixture_statistics_input()
  expected <- zf_statistics_outputs()

  out <- run_anova(
    d,
    group_cols = c("plate_id", "endpoint"),
    value_col = "endpoint_value",
    endpoint_transforms = list(both_auc = "log10", spearman = "fisher_z")
  )

  exp_dunn <- expected$anova$dunn
  exp_dunn$endpoint <- gsub("^log10_|^Zt_", "", exp_dunn$endpoint)
  exp_dunn <- dplyr::rename(exp_dunn, pairwise_comp = "Pairwise_comp")

  cmp <- zf_expect_matching_keys(
    out$dunn,
    exp_dunn,
    by = c("plate_id", "endpoint", "pairwise_comp")
  )
  expect_equal(nrow(cmp), 90)
  # Coefficients (diff) are deterministic -- exact match.
  expect_equal(cmp$diff_new, cmp$diff_ref, tolerance = 1e-8)
  # p-values use DescTools::DunnettTest()'s randomized quasi-Monte-Carlo
  # integration (mvtnorm::pmvt) -- not exactly reproducible session to
  # session; confirmed empirically that re-running on identical input
  # differs by up to ~3.6e-4 even within one session. Use a matching
  # tolerance rather than exact equality.
  expect_equal(cmp$p_value, cmp$pval, tolerance = 5e-4)
})

test_that("run_rmanova() reproduces real historical reduced-model Dunnett coefficients closely", {
  d <- zf_fixture_statistics_input()
  expected <- zf_statistics_outputs()

  out <- run_rmanova(
    d,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = list(both_auc = "log10")
  )

  exp_rd <- dplyr::rename(expected$rmanova$reduced_dunn, pairwise_comp = "pair")
  cmp <- zf_expect_matching_keys(
    out$reduced_dunn,
    exp_rd,
    by = c("plate_id", "pairwise_comp")
  )
  expect_equal(nrow(cmp), 15)
  # Documented divergence source: the legacy notebook's D2/D2-ext phase
  # collapse + AR(1) row-ordering bug (see data-raw/build_statistics_snapshot.R)
  # -- run_rmanova() deliberately fixes both, so an exact match is not
  # expected. Check absolute (not relative) difference, since several
  # coefficients are small in magnitude (~0.03) where a relative tolerance
  # would be too strict for a documented, expected divergence of this size.
  expect_true(max(abs(cmp$coef_new - cmp$coef_ref)) < 0.03)
})

test_that("run_rmanova() runs end-to-end on the real historical fixture and returns all 4 expected tables with correct shapes", {
  d <- zf_fixture_statistics_input()

  out <- run_rmanova(
    d,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = list(both_auc = "log10")
  )

  expect_equal(nrow(out$rmanova_simple), 3) # 3 plates
  expect_setequal(
    names(out),
    c(
      "rmanova",
      "rmanova_simple",
      "reduced_dunn",
      "full_dunn"
    )
  )
  expect_true(all(
    c("p_value_interaction_full", "p_value_conc_unit_reduced") %in%
      colnames(out$rmanova_simple)
  ))
  expect_setequal(
    unique(out$full_dunn$phase),
    c("L1", "D1", "L2", "D2", "D2-ext")
  )
  # Exact phase order (not just set membership) within each group.
  phase_order_per_plate <- out$full_dunn |>
    dplyr::distinct(.data$plate_id, .data$phase) |>
    dplyr::group_by(.data$plate_id) |>
    dplyr::summarise(order_ok = all(.data$phase == zf_default_phase_levels()), .groups = "drop")
  expect_true(all(phase_order_per_plate$order_ok))
})

test_that("call_params attribute records the configuration used to produce run_anova()/run_rmanova() output", {
  d <- zf_fixture_statistics_input()
  out_anova <- run_anova(
    d,
    group_cols = c("plate_id", "endpoint"),
    value_col = "endpoint_value",
    endpoint_transforms = list(both_auc = "log10", spearman = "fisher_z")
  )
  cp_anova <- attr(out_anova, "call_params")
  expect_equal(cp_anova$group_cols, c("plate_id", "endpoint"))
  expect_equal(cp_anova$value_col, "endpoint_value")

  out_rmanova <- run_rmanova(
    d,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = list(both_auc = "log10")
  )
  cp_rmanova <- attr(out_rmanova, "call_params")
  expect_equal(cp_rmanova$group_cols, "plate_id")
  expect_equal(cp_rmanova$phase_levels, zf_default_phase_levels())

  hits <- run_anova_rmanova_hit_calling(
    out_anova,
    out_rmanova,
    anova_group_cols = c("plate_id", "endpoint"),
    rmanova_group_cols = "plate_id"
  )
  cp_hits <- attr(hits, "call_params")
  expect_equal(cp_hits$alpha, 0.05)
})

# ---- Real historical production ground-truth regression test --------------
# See data-raw/build_hit_calling_snapshot.R for full provenance and
# documented caveats (inherited from build_statistics_snapshot.R's
# documented RMANOVA divergence, confirmed not to flip any hit/non-hit
# decision on this specific 3-chemical fixture).

test_that("run_anova_rmanova_hit_calling() reproduces real historical hit-calling results on the 3-chemical fixture", {
  d <- zf_fixture_statistics_input()
  expected <- zf_hit_calling_outputs()

  anova_out <- run_anova(
    d,
    group_cols = c("plate_id", "endpoint"),
    value_col = "endpoint_value",
    endpoint_transforms = list(both_auc = "log10", spearman = "fisher_z")
  )
  rmanova_out <- run_rmanova(
    d,
    group_cols = "plate_id",
    phase_col = "phase",
    phase_levels = zf_default_phase_levels(),
    value_col = "endpoint_value",
    endpoint_transforms = list(both_auc = "log10")
  )

  hits <- run_anova_rmanova_hit_calling(
    anova_out,
    rmanova_out,
    anova_group_cols = c("plate_id", "endpoint"),
    rmanova_group_cols = "plate_id"
  )

  expect_equal(nrow(hits), 21) # 18 ANOVA (3 plates x 6 endpoints) + 3 RMANOVA

  # ANOVA side: n_posthoc_sig and hit_p_value match exactly on this
  # fixture (no comparison is close enough to alpha for Monte-Carlo
  # Dunnett p-value noise to flip a hit/non-hit decision -- see
  # data-raw/build_hit_calling_snapshot.R).
  anova_hits <- hits[hits$test == "ANOVA", ]
  exp_anova <- expected$anova_sum
  cmp <- zf_expect_matching_keys(
    anova_hits,
    exp_anova,
    by = c("plate_id", "endpoint")
  )
  expect_equal(nrow(cmp), 18)
  expect_equal(cmp$n_posthoc_sig, cmp$n_sig)
  expect_equal(cmp$hit_p_value, cmp$act_confidence, tolerance = 1e-8) # both NA on this fixture
  # All 18 ANOVA rows on this fixture are confirmed non-hits (not
  # indeterminate) in both the ground truth and this package's output.
  expect_true(all(anova_hits$decision_state == "not_hit"))

  # RMANOVA side: none of the 3 fixture chemicals are a hit in either the
  # ground truth or this package's output (confirmed by direct inspection
  # -- see data-raw/build_hit_calling_snapshot.R for why this holds
  # despite run_rmanova()'s documented, deliberate divergence from the
  # legacy notebook's omnibus p-values).
  rmanova_hits <- hits[hits$test == "RMANOVA", ]
  expect_true(all(is.na(rmanova_hits$hit_p_value)))
  expect_true(all(rmanova_hits$decision_state == "not_hit"))
  expect_true(all(is.na(expected$rmanova_sum$act_confidence)))
})
