# Shared test fixtures and helpers for zfbehavior tests.
#
# The fixture data (the package example data,
# inst/extdata/zf_behavior_example.rds, ?zfbehavior_example) is a subset of real
# zebrafish LDTT tracking data (3 chemicals, full 16 wells/dose, all 50
# timepoints), derived per data-raw/build_fixture.R. See
# fixtures/legacy_expected_outputs.rds (data-raw/build_legacy_snapshot.R)
# for regression-test reference values computed with the original,
# unmodified original analysis functions, and
# fixtures/authoritative_endpoint_outputs.rds
# (data-raw/build_authoritative_snapshot.R) for a stronger check against
# REAL historical production output (Distance Change/Shift and Movement
# Similarity only; Total Distance was never run in production).

#' Assert exact key-set equality between two data frames before comparing
#' values (regression-test hardening)
#'
#' A previous convention across this test suite's endpoint regression tests
#' inner-joined new vs. reference output, asserted the joined row count
#' equalled BOTH inputs' row counts, then compared values -- but this can
#' pass even when new output has a genuinely wrong key set: e.g. new keys
#' `{A, A}` (a duplicated key) joined against expected keys `{A, B}` (a
#' missing key) still produces exactly 2 joined rows, matching neither
#' input's row count check in a way that would actually fail.
#'
#' This helper makes the review's recommended fix explicit and reusable:
#' assert key uniqueness independently on both sides, assert the complete
#' key sets are IDENTICAL (via anti-joins, with informative failure
#' messages naming any missing/extra keys), and only then return the
#' key-sorted inner join for value comparison.
#'
#' @param new_df,ref_df Data frames to compare.
#' @param by Character vector of column names forming the complete key.
#' @return The inner join of `new_df`/`ref_df` on `by`, sorted by `by`,
#'   with `suffix = c("_new", "_ref")` (only after all key-equality
#'   assertions pass).
zf_expect_matching_keys <- function(new_df, ref_df, by) {
  new_dup <- new_df |> dplyr::count(dplyr::across(dplyr::all_of(by))) |> dplyr::filter(.data$n > 1)
  ref_dup <- ref_df |> dplyr::count(dplyr::across(dplyr::all_of(by))) |> dplyr::filter(.data$n > 1)
  testthat::expect_equal(
    nrow(new_dup),
    0L,
    info = paste0("new_df has duplicate keys: ", nrow(new_dup), " duplicated key(s)")
  )
  testthat::expect_equal(
    nrow(ref_dup),
    0L,
    info = paste0("ref_df has duplicate keys: ", nrow(ref_dup), " duplicated key(s)")
  )

  missing_from_new <- dplyr::anti_join(ref_df, new_df, by = by)
  extra_in_new <- dplyr::anti_join(new_df, ref_df, by = by)
  testthat::expect_equal(
    nrow(missing_from_new),
    0L,
    info = paste0(
      nrow(missing_from_new),
      " key(s) present in ref_df but missing from new_df."
    )
  )
  testthat::expect_equal(
    nrow(extra_in_new),
    0L,
    info = paste0(
      nrow(extra_in_new),
      " key(s) present in new_df but not in ref_df (unexpected/extra rows)."
    )
  )

  dplyr::inner_join(new_df, ref_df, by = by, suffix = c("_new", "_ref")) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(by)))
}

#' The vendor-specific -> canonical column mapping for the test fixture
zf_fixture_column_mapping <- function() {
  list(
    plate_id = "PLATE_NAME",
    well_id = "WELL_POSITION",
    chemical_id = "BIOBIDE_ID",
    is_vehicle_control = "IS_VEHICLE",
    concentration = "CONCENTRATION",
    concentration_unit = "CONCENTRATION_UNIT",
    time_value = "time_end",
    response_value = "DISTANCE_MOVED",
    keep_well = "KEEP_WELL_DATA"
  )
}

#' Path to the example data shipped in inst/extdata
zf_example_path <- function() {
  path <- system.file("extdata", "zf_behavior_example.rds", package = "zfbehavior")
  if (!nzchar(path)) stop("zf_behavior_example.rds not found; is zfbehavior installed or loaded?")
  path
}

#' Load the raw (vendor-schema) fixture data frame
zf_fixture_raw <- function() {
  readRDS(zf_example_path())
}

#' Load the fixture already renamed to canonical columns (no other changes)
zf_fixture_canonical <- function() {
  mapping <- zf_fixture_column_mapping()
  rename_vec <- unlist(mapping)
  dplyr::rename(zf_fixture_raw(), !!!rename_vec)
}

#' Load the fixture, canonicalized, filtered to keep_well, NA responses -> 0
#'
#' This matches the preprocessing applied in the original
#' original endpoint script before endpoint calculation, and is
#' the standard input used across endpoint/normalization regression tests.
#' Uses the exported replace_na_response_zero() (rather than an inline
#' ifelse()) so tests exercise the real function callers are expected to
#' use.
zf_fixture_ready <- function() {
  zf_fixture_canonical() %>%
    dplyr::filter(.data$keep_well) %>%
    replace_na_response_zero()
}

#' Standard L1/D1/L2/D2/D2-ext segments matching the fixture's 50 timepoints
zf_fixture_segments <- function() {
  list(L1 = 1:10, D1 = 11:20, L2 = 21:30, D2 = 31:40, `D2-ext` = 41:50)
}

#' Load the legacy (original-code) reference outputs for regression tests
zf_legacy_outputs <- function() {
  readRDS(test_path("fixtures", "legacy_expected_outputs.rds"))
}

#' Load real historical production endpoint outputs for regression tests
#'
#' See data-raw/build_authoritative_snapshot.R. Stronger than
#' zf_legacy_outputs(): this is real, previously-published production data
#' (Distance Change/Shift and Movement Similarity only), not merely
#' re-computed by us from the original functions.
zf_authoritative_outputs <- function() {
  readRDS(test_path("fixtures", "authoritative_endpoint_outputs.rds"))
}

#' Load legacy get_vc_distrib()/get_vc_denom_outlier() reference outputs
#'
#' See data-raw/build_endpoint_qc_snapshot.R. Contains dcs_norm (Distance
#' Change/Shift, normalized, from the NEW package functions -- the exact
#' input used to generate the reference outputs below), vc_distrib
#' (original get_vc_distrib() output), and vc_denom_outlier (original
#' get_vc_denom_outlier() output).
zf_endpoint_qc_outputs <- function() {
  readRDS(test_path("fixtures", "endpoint_qc_expected_outputs.rds"))
}

#' Load the inter-plate QC fixture: real Lab-A vehicle-control data
#'
#' See data-raw/build_interplate_qc_fixture.R for full provenance. 37
#' plates' worth of vehicle-control-only, basic-QC-passed, canonical-column
#' data derived from the published inter-laboratory dataset (Truong et al.
#' 2026, Lab-A). Real published ground truth: Truong et al. (2026) reports
#' exactly 1 of these 37 plates was excluded by inter-plate QC, and the user
#' confirmed (interactive session) it is `plate_name == "B11_sqPlate23"`.
zf_interplate_qc_fixture <- function() {
  readRDS(test_path("fixtures", "interplate_qc_fixture.rds"))
}

#' Load the zero-variance-wells fixture: 8 real fully-missing-data wells
#'
#' See data-raw/build_zero_variance_wells_fixture.R for full provenance.
#' 8 full plates (basic-QC-filtered, canonical-column) from
#' the original study data, each containing exactly one well with
#' response_value == NA at every timepoint (a real production data-quality
#' edge case: these 8 wells were NOT flagged dead/malformed by upstream
#' QC, unlike 258 other wells with the identical all-missing pattern).
#' Used to confirm calculate_movement_similarity_endpoints() treats these
#' wells as similarity = 0 (not an error), per the documented decision in
#' cal_mean_similarity() (R/endpoints.R).
zf_zero_variance_wells_fixture <- function() {
  readRDS(test_path("fixtures", "zero_variance_wells_fixture.rds"))
}

#' Load real historical production ANOVA/RMANOVA outputs for regression tests
#'
#' See data-raw/build_statistics_snapshot.R for full provenance and
#' IMPORTANT documented caveats about which parts are expected to match
#' exactly vs. only approximately (Dunnett p-value Monte-Carlo noise;
#' deliberate D2/D2-ext phase-collapse and AR(1) row-ordering fixes in
#' run_rmanova() vs. the legacy notebook).
zf_statistics_outputs <- function() {
  readRDS(test_path("fixtures", "statistics_expected_outputs.rds"))
}

#' Load real historical production hit-calling outputs for regression tests
#'
#' See data-raw/build_hit_calling_snapshot.R for full provenance and
#' IMPORTANT documented caveats: this is the same 3-chemical fixture used
#' by zf_statistics_outputs(), one step further downstream (the `anova2`/
#' `rmanova2` hit-calling blocks of the original ANOVA script,
#' renamed: `n_sig` -> `n_posthoc_sig*`, `act_confidence` ->
#' `neg_log10_p_hit`/`hit_p_value`).
zf_hit_calling_outputs <- function() {
  readRDS(test_path("fixtures", "hit_calling_expected_outputs.rds"))
}

#' Build the long-form, canonical-column input for run_anova()/run_rmanova()
#' regression tests, from the SAME 3-chemical fixture used everywhere else
#' in this test suite (zf_fixture_ready()).
#'
#' Combines calculate_distance_change_shift_endpoints() (reshaped to
#' `"<segment>@<auc_type>"` long-form endpoint names, e.g. `"D1@both_auc"`,
#' matching the historical endp.rds naming) and
#' calculate_movement_similarity_endpoints() (`"LD_spearman"`) output,
#' re-attaches concentration, and rebuilds `plate_id` as the SAME
#' `CHEMICAL_NAME@NIH_BLINDED_ID@BIOBIDE_ID` composite key used in the
#' real historical ANOVA script output (see
#' data-raw/build_statistics_snapshot.R) -- NOT the raw PLATE_NAME -- so
#' results can be joined directly against zf_statistics_outputs().
#'
#' Also derives a `phase` factor column (`c("L1", "D1", "L2", "D2",
#' "D2-ext")`, ordered) from the `both_auc`-family endpoint names, for
#' run_rmanova()'s `phase_col` argument (NA for `LD_spearman` rows, which
#' have no phase).
zf_fixture_statistics_input <- function() {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()

  plate_id_lookup <- d %>%
    dplyr::distinct(
      .data$plate_id,
      .data$CHEMICAL_NAME,
      .data$NIH_BLINDED_ID,
      .data$chemical_id
    ) %>%
    tidyr::unite(
      composite_plate_id,
      c("CHEMICAL_NAME", "NIH_BLINDED_ID", "chemical_id"),
      sep = "@"
    )

  conc_lookup <- dplyr::distinct(
    d,
    .data$plate_id,
    .data$well_id,
    .data$concentration,
    .data$concentration_unit
  )

  dcs <- calculate_distance_change_shift_endpoints(d, segments)
  sim <- calculate_movement_similarity_endpoints(
    d,
    list(LD = 1:50),
    metric = "spearman"
  )

  dcs_long <- dcs %>%
    tidyr::pivot_longer(
      cols = c("up_auc", "down_auc", "both_auc"),
      names_to = "auc_type",
      values_to = "endpoint_value"
    ) %>%
    dplyr::mutate(endpoint = paste0(.data$endpoint, "@", .data$auc_type)) %>%
    dplyr::select(
      "plate_id",
      "well_id",
      "is_vehicle_control",
      "endpoint",
      "endpoint_value"
    )

  sim_long <- sim %>%
    dplyr::mutate(endpoint = "LD_spearman") %>%
    dplyr::select(
      "plate_id",
      "well_id",
      "is_vehicle_control",
      "endpoint",
      "endpoint_value"
    )

  dplyr::bind_rows(dcs_long, sim_long) %>%
    dplyr::left_join(conc_lookup, by = c("plate_id", "well_id")) %>%
    dplyr::left_join(plate_id_lookup, by = "plate_id") %>%
    dplyr::mutate(
      plate_id = .data$composite_plate_id,
      keep_well = TRUE,
      phase = sub("@both_auc$|@up_auc$|@down_auc$", "", .data$endpoint),
      phase = ifelse(grepl("@", .data$endpoint), .data$phase, NA_character_),
      phase = factor(.data$phase, levels = c("L1", "D1", "L2", "D2", "D2-ext"))
    ) %>%
    dplyr::select(-"composite_plate_id")
}

#' Load the raw (vendor-schema) BMC fixture data frame
#'
#' See data-raw/build_bmc_fixture.R for full provenance: 3 chemicals
#' (PE-4188/PE-4215/PE-4276), each on its own plate, chosen SPECIFICALLY
#' because 2 of the 3 (PE-4215 Loperamide hydrochloride, PE-4276
#' Methylene blue trihydrate) are REAL, STRONG active chemicals in the
#' historical BMC ground truth (unlike the zf_fixture_raw() fixture used
#' elsewhere, whose 3 chemicals are all non-hits/borderline).
zf_bmc_fixture_raw <- function() {
  readRDS(test_path("fixtures", "bmc_behavior_fixture.rds"))
}

#' Load the BMC fixture, canonicalized, filtered to keep_well, NA -> 0
#'
#' Same preprocessing as zf_fixture_ready(), applied to the separate BMC
#' fixture (zf_bmc_fixture_raw()).
zf_bmc_fixture_ready <- function() {
  mapping <- zf_fixture_column_mapping()
  rename_vec <- unlist(mapping)
  zf_bmc_fixture_raw() %>%
    dplyr::rename(!!!rename_vec) %>%
    dplyr::filter(.data$keep_well) %>%
    replace_na_response_zero()
}

#' Build the long-form input for run_bmc_curvep() regression tests, from
#' the BMC-specific 3-chemical fixture (zf_bmc_fixture_ready())
#'
#' Combines calculate_distance_change_shift_endpoints() (reshaped to
#' `"<segment>@<auc_type>"` long-form endpoint names) AND
#' calculate_movement_similarity_endpoints() (both `"LD_spearman"` and
#' `"LD_pearson"`, matching all 17 endpoints present in the real
#' historical `bmr_used` ground truth -- an earlier version of this
#' helper built `LD_spearman` only, silently leaving `LD_pearson`
#' unavailable for any BMC regression test/example built from it) with
#' normalize_none()/normalize_ratio_to_control_percent(), and rebuilds
#' `chemical` as the BMC-specific composite key
#' `CHEMICAL_NAME@BIOBIDE_ID@PLATE_NAME` (NOT the ANOVA module's
#' `CHEMICAL_NAME@NIH_BLINDED_ID@BIOBIDE_ID` -- see the `@section` note
#' in the BMC module notes for why these differ), matching the real historical BMC
#' ground truth's `chemical` column exactly (see
#' data-raw/build_bmc_snapshot.R). Filters out vehicle-control rows and
#' log10-molar-transforms concentration, matching
#' the original BMC script's
#' `filter(conc != 0) %>% mutate(conc = log10(conc/1000000))` step.
zf_bmc_fixture_input <- function() {
  d <- zf_bmc_fixture_ready()
  segments <- zf_fixture_segments()

  chem_lookup <- d %>%
    dplyr::distinct(.data$plate_id, .data$CHEMICAL_NAME, .data$chemical_id) %>%
    tidyr::unite(
      chemical,
      c("CHEMICAL_NAME", "chemical_id", "plate_id"),
      sep = "@",
      remove = FALSE
    )

  conc_lookup <- dplyr::distinct(
    d,
    .data$plate_id,
    .data$well_id,
    .data$concentration
  )

  dcs <- calculate_distance_change_shift_endpoints(d, segments)

  dcs_long <- dcs %>%
    tidyr::pivot_longer(
      cols = c("up_auc", "down_auc", "both_auc"),
      names_to = "auc_type",
      values_to = "endpoint_value"
    ) %>%
    dplyr::mutate(endpoint = paste0(.data$endpoint, "@", .data$auc_type)) %>%
    dplyr::select(
      "plate_id",
      "well_id",
      "is_vehicle_control",
      "endpoint",
      "endpoint_value"
    )

  sim_spearman <- calculate_movement_similarity_endpoints(
    d,
    list(LD = 1:50),
    metric = "spearman"
  ) %>%
    dplyr::mutate(endpoint = "LD_spearman")

  sim_pearson <- calculate_movement_similarity_endpoints(
    d,
    list(LD = 1:50),
    metric = "pearson"
  ) %>%
    dplyr::mutate(endpoint = "LD_pearson")

  sim_long <- dplyr::bind_rows(sim_spearman, sim_pearson) %>%
    dplyr::select(
      "plate_id",
      "well_id",
      "is_vehicle_control",
      "endpoint",
      "endpoint_value"
    )

  auc_ready <- dcs_long %>%
    dplyr::left_join(conc_lookup, by = c("plate_id", "well_id")) %>%
    dplyr::left_join(chem_lookup, by = "plate_id")
  auc_normed <- normalize_none(auc_ready)

  sim_ready <- sim_long %>%
    dplyr::left_join(conc_lookup, by = c("plate_id", "well_id")) %>%
    dplyr::left_join(chem_lookup, by = "plate_id")
  sim_normed <- normalize_ratio_to_control_percent(sim_ready)

  dplyr::bind_rows(auc_normed, sim_normed) %>%
    dplyr::filter(!.data$is_vehicle_control) %>%
    dplyr::transmute(
      chemical = .data$chemical,
      endpoint = .data$endpoint,
      conc = log10(.data$concentration / 1e6),
      endpoint_value_norm = .data$endpoint_value_norm
    )
}

#' Load real historical production BMC outputs for regression tests
#'
#' See data-raw/build_bmc_snapshot.R for full provenance and IMPORTANT
#' documented caveats: `bmr_used` (the GLOBAL, all-112-plate BMR table)
#' is kept in full (not subset to the 3 fixture chemicals, since BMR is
#' inherently a dataset-wide quantity); `act_summary` is subset to the 3
#' BMC fixture chemicals (PE-4188/PE-4215/PE-4276). `POD_med`/
#' `hit_confidence` in `act_summary` are bootstrap-simulation-derived
#' (`Rcurvep::combi_run_rcurvep(n_samples = 1000)`) and subject to
#' R-version-dependent Monte-Carlo noise (same category as the
#' documented `DescTools::DunnettTest()`/`multcomp::glht()` p-value
#' noise) -- confirmed empirically these do NOT always match bit-exactly
#' even with the same `seed`, though usually closely (see
#' tests/testthat/test-bmc.R for the specific tolerances used).
zf_bmc_outputs <- function() {
  readRDS(test_path("fixtures", "bmc_expected_outputs.rds"))
}

#' Load the raw (vendor-schema) devtox fixture data frame
#'
#' See data-raw/build_devtox_fixture.R for full provenance: 3 chemicals
#' (PE-4188/PE-4202/PE-4205), each on its own plate, chosen SPECIFICALLY
#' to contain NO real multi-larva wells (unlike zf_bmc_fixture_raw(),
#' whose PE-4215/Plate_28 does contain one) -- so this fixture's
#' regression test is not confounded by calculate_devtox_endpoints()'s
#' documented multi-larva-well exclusion (a deliberate departure from
#' historical ground truth).
zf_devtox_fixture_raw <- function() {
  readRDS(test_path("fixtures", "devtox_behavior_fixture.rds"))
}

#' Build calculate_devtox_endpoints() input from the raw devtox fixture
#'
#' Reproduces the original devtox script's well-level
#' pre-processing directly on the vendor-schema fixture (NOT
#' zf_fixture_ready(), since devtox uses the RAW `KEEP_WELL_DATA`/
#' `DEVELOPMENTAL_DEFFECTS` columns before any canonical-column renaming
#' or NA-response-zero-imputation -- those steps are specific to the
#' timepoint-based behavioral endpoints and not applicable here):
#' `CONCENTRATION` forced to 0 for vehicle-control rows, reduced to
#' distinct well-level rows, and filtered to
#' `KEEP_WELL_DATA | (!KEEP_WELL_DATA & !is.na(DEVELOPMENTAL_DEFFECTS))`
#' (drop true "no embryo"/multi-larva-annotated-as-excluded wells --
#' though this fixture, by construction, contains none of the latter).
#' Renames to the canonical columns `calculate_devtox_endpoints()`
#' expects (`well_id`, `is_vehicle_control`, `concentration`,
#' `keep_well`), plus `chemical_id`/`plate_id` for `chemical_group_cols`.
zf_devtox_fixture_ready <- function() {
  raw <- zf_devtox_fixture_raw()
  raw %>%
    dplyr::mutate(
      CONCENTRATION = ifelse(.data$IS_VEHICLE, 0, .data$CONCENTRATION)
    ) %>%
    dplyr::distinct(dplyr::across(c(
      DTXSID:HOURS_POST_EMBRYONIC,
      COMMENTS:DEVELOPMENTAL_DEFFECTS
    ))) %>%
    dplyr::filter(
      .data$KEEP_WELL_DATA |
        (!.data$KEEP_WELL_DATA & !is.na(.data$DEVELOPMENTAL_DEFFECTS))
    ) %>%
    dplyr::rename(
      well_id = "WELL_POSITION",
      plate_id = "PLATE_NAME",
      chemical_id = "BIOBIDE_ID",
      is_vehicle_control = "IS_VEHICLE",
      concentration = "CONCENTRATION",
      keep_well = "KEEP_WELL_DATA"
    )
}

#' Load real historical production devtox BMC outputs for regression tests
#'
#' See data-raw/build_devtox_snapshot.R for full provenance and
#' IMPORTANT documented caveats: `act_summary` is subset to the 3 devtox
#' fixture chemicals (PE-4188/PE-4202/PE-4205); `bmr_devtox` is the fixed
#' scalar BMR (`25`) confirmed to have been used for this "v1" (per-plate)
#' historical run (the RAW Rcurvep knee-point suggestion -- the
#' script's separate `bmr_exp <- 30` override never executed for this
#' ground truth, since it is conditioned on `run_version == "v2"`; see
#' R/devtox.R's `@section BMR for this endpoint` note).
zf_devtox_outputs <- function() {
  readRDS(test_path("fixtures", "devtox_expected_outputs.rds"))
}
