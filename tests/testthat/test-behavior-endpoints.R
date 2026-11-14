# ---- generate_behavior_endpoints() ---------------------------------------------------

test_that("generate_behavior_endpoints() reproduces real production output exactly (Distance Change/Shift + Movement Similarity, both metrics)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  auth <- zf_authoritative_outputs()

  out <- generate_behavior_endpoints(
    d,
    auc_segments = segments,
    similarity_segments = list(LD = 1:50),
    similarity_metrics = c("spearman", "pearson")
  )

  dcs <- out |>
    dplyr::filter(.data$endpoint_type == "distance_change_shift") |>
    dplyr::rename(endpoint_value = "endpoint_value_raw")
  cmp_dcs <- zf_expect_matching_keys(
    dcs,
    auth$distance_change_shift,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )
  expect_equal(cmp_dcs$endpoint_value_new, cmp_dcs$endpoint_value_ref, tolerance = 1e-8)
  expect_equal(cmp_dcs$endpoint_value_norm_new, cmp_dcs$endpoint_value_norm_ref, tolerance = 1e-8)

  sim <- out |>
    dplyr::filter(.data$endpoint_type == "movement_similarity") |>
    dplyr::rename(endpoint_value = "endpoint_value_raw")
  cmp_sim <- zf_expect_matching_keys(
    sim,
    auth$movement_similarity,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )
  expect_equal(cmp_sim$endpoint_value_new, cmp_sim$endpoint_value_ref, tolerance = 1e-8)
  expect_equal(cmp_sim$endpoint_value_norm_new, cmp_sim$endpoint_value_norm_ref, tolerance = 1e-8)
})

test_that("generate_behavior_endpoints() Total Distance matches the isolated, bug-free oracle exactly (raw AND normalized)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  legacy <- zf_legacy_outputs()

  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  td <- out |>
    dplyr::filter(.data$endpoint_type == "total_distance") |>
    dplyr::rename(endpoint_value = "endpoint_value_raw")

  legacy_td <- legacy$total_distance_raw |>
    dplyr::rename(well_id = "embryo_id", is_vehicle_control = "is_VC")
  cmp <- zf_expect_matching_keys(
    td,
    legacy_td,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )
  expect_equal(cmp$endpoint_value_new, cmp$endpoint_value_ref, tolerance = 1e-8)

  legacy_norm <- legacy$total_distance_norm_auc |>
    dplyr::rename(well_id = "embryo_id", is_vehicle_control = "is_VC")
  cmp2 <- zf_expect_matching_keys(
    td,
    legacy_norm,
    by = c("plate_id", "well_id", "is_vehicle_control", "endpoint")
  )
  expect_equal(cmp2$endpoint_value_norm_new, cmp2$endpoint_value_norm_ref, tolerance = 1e-8)
})

test_that("generate_behavior_endpoints() output has the documented canonical schema columns", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  expected_cols <- c(
    "plate_id", "well_id", "chemical_id", "concentration",
    "concentration_unit", "keep_well", "is_vehicle_control",
    "endpoint_type", "segment", "component", "metric",
    "normalization_method", "is_default_normalization", "endpoint",
    "endpoint_value_raw", "endpoint_value_transformed", "denom",
    "endpoint_value_norm"
  )
  expect_true(all(expected_cols %in% colnames(out)))
})

test_that("generate_behavior_endpoints() preserves chemical/concentration/unit identifiers unchanged", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  source_lookup <- d |>
    dplyr::distinct(.data$plate_id, .data$well_id, .data$chemical_id, .data$concentration, .data$concentration_unit)
  result_lookup <- out |>
    dplyr::distinct(.data$plate_id, .data$well_id, .data$chemical_id, .data$concentration, .data$concentration_unit)

  cmp <- zf_expect_matching_keys(
    result_lookup,
    source_lookup,
    by = c("plate_id", "well_id", "chemical_id", "concentration", "concentration_unit")
  )
  expect_equal(nrow(cmp), nrow(source_lookup))
})

test_that("generate_behavior_endpoints() preserves a caller-supplied extra_id_cols identifier", {
  d <- zf_fixture_ready() |>
    dplyr::mutate(experiment_id = "EXP1")
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(
    d,
    segments,
    list(LD = 1:50),
    extra_id_cols = "experiment_id"
  )
  expect_true("experiment_id" %in% colnames(out))
  expect_true(all(out$experiment_id == "EXP1"))
})

test_that("generate_behavior_endpoints() records segment/component/metric provenance correctly per endpoint type", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50), similarity_metrics = "spearman")

  td <- out |> dplyr::filter(.data$endpoint_type == "total_distance")
  expect_true(all(is.na(td$component)))
  expect_true(all(is.na(td$metric)))
  expect_true(all(td$segment %in% names(segments)))

  dcs <- out |> dplyr::filter(.data$endpoint_type == "distance_change_shift")
  expect_true(all(dcs$component %in% c("up", "down", "both")))
  expect_true(all(is.na(dcs$metric)))

  sim <- out |> dplyr::filter(.data$endpoint_type == "movement_similarity")
  expect_true(all(is.na(sim$component)))
  expect_equal(unique(sim$metric), "spearman")
  expect_equal(unique(sim$segment), "LD")
})

test_that("generate_behavior_endpoints() legacy-style `endpoint` string label matches this package's existing convention", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50), similarity_metrics = c("spearman", "pearson"))

  expect_true("L1" %in% out$endpoint[out$endpoint_type == "total_distance"])
  expect_true("D1@up_auc" %in% out$endpoint[out$endpoint_type == "distance_change_shift"])
  expect_true("LD_spearman" %in% out$endpoint[out$endpoint_type == "movement_similarity"])
  expect_true("LD_pearson" %in% out$endpoint[out$endpoint_type == "movement_similarity"])
})

test_that("generate_behavior_endpoints() enforces the approved default normalization pairing by default", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  expect_true(all(out$is_default_normalization))
  expect_equal(
    unique(out$normalization_method[out$endpoint_type == "total_distance"]),
    "log_diff"
  )
  expect_equal(
    unique(out$normalization_method[out$endpoint_type == "distance_change_shift"]),
    "none"
  )
  expect_equal(
    unique(out$normalization_method[out$endpoint_type == "movement_similarity"]),
    "ratio"
  )
})

test_that("generate_behavior_endpoints() records an explicit override as non-default", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(
    d,
    segments,
    list(LD = 1:50),
    normalization = list(total_distance = "log_ratio")
  )

  td <- out |> dplyr::filter(.data$endpoint_type == "total_distance")
  expect_equal(unique(td$normalization_method), "log_ratio")
  expect_true(all(!td$is_default_normalization))

  other <- out |> dplyr::filter(.data$endpoint_type != "total_distance")
  expect_true(all(other$is_default_normalization))
})

test_that("generate_behavior_endpoints() keeps endpoint_value_raw and endpoint_value_transformed separate and correctly related for log-scale normalization", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  td <- out |> dplyr::filter(.data$endpoint_type == "total_distance")
  expect_equal(td$endpoint_value_transformed, log10(td$endpoint_value_raw + 1))

  # Distance Change/Shift uses normalize_none(): raw == transformed exactly.
  dcs <- out |> dplyr::filter(.data$endpoint_type == "distance_change_shift")
  expect_equal(dcs$endpoint_value_raw, dcs$endpoint_value_transformed)
})

test_that("generate_behavior_endpoints() errors on an unknown normalization endpoint-type name", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  expect_error(
    generate_behavior_endpoints(d, segments, list(LD = 1:50), normalization = list(bogus_type = "none")),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("generate_behavior_endpoints() errors on an unsupported normalization method value", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  expect_error(
    generate_behavior_endpoints(d, segments, list(LD = 1:50), normalization = list(total_distance = "bogus_method")),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("generate_behavior_endpoints() errors on an unsupported similarity_metrics value", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  expect_error(
    generate_behavior_endpoints(d, segments, list(LD = 1:50), similarity_metrics = "bogus_metric"),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("generate_behavior_endpoints() errors when extra_id_cols collides with a reserved output column", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  expect_error(
    generate_behavior_endpoints(d, segments, list(LD = 1:50), extra_id_cols = "endpoint_type"),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("generate_behavior_endpoints() errors when a required identifier column is missing from input", {
  d <- zf_fixture_ready() |> dplyr::select(-"chemical_id")
  segments <- zf_fixture_segments()
  expect_error(
    generate_behavior_endpoints(d, segments, list(LD = 1:50)),
    class = "zfbehavior_error_missing_columns"
  )
})

test_that("generate_behavior_endpoints() errors when an identifier column varies within a well's own trajectory", {
  d <- zf_fixture_ready()
  d$chemical_id[1] <- "DIFFERENT_CHEMICAL"
  segments <- zf_fixture_segments()
  expect_error(
    generate_behavior_endpoints(d, segments, list(LD = 1:50)),
    class = "zfbehavior_error_non_invariant_identifiers"
  )
})

# ---- validate_behavior_endpoints() ---------------------------------------------

test_that("validate_behavior_endpoints() passes on real generate_behavior_endpoints() output", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50), similarity_metrics = c("spearman", "pearson"))

  res <- validate_behavior_endpoints(out, source_d = d)
  expect_s3_class(res, "zfbehavior_validation_result")
  expect_true(res$ok)
})

test_that("validate_behavior_endpoints() flags missing required columns", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  res <- validate_behavior_endpoints(out |> dplyr::select(-"endpoint_type"))
  expect_false(res$ok)
  expect_equal(
    res$checks$status[res$checks$code == "required_columns_present"],
    "failed"
  )
})

test_that("validate_behavior_endpoints() flags duplicate endpoint keys", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  out_dup <- dplyr::bind_rows(out, out[1, ])
  res <- validate_behavior_endpoints(out_dup)
  expect_false(res$ok)
  expect_equal(
    res$checks$status[res$checks$code == "unique_endpoint_key"],
    "failed"
  )
  expect_equal(nrow(res$diagnostics$duplicate_endpoint_keys), 1)
})

test_that("validate_behavior_endpoints() flags missing endpoint-type coverage for a well", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  out_missing <- out |>
    dplyr::filter(
      !(.data$plate_id == "Plate_01" &
        .data$well_id == "A1" &
        .data$endpoint_type == "total_distance")
    )
  res <- validate_behavior_endpoints(out_missing)
  expect_false(res$ok)
  expect_equal(
    res$checks$status[res$checks$code == "endpoint_coverage"],
    "failed"
  )
})

test_that("validate_behavior_endpoints() skips coverage/identifier checks (status 'skipped') when optional args are not supplied", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  res <- validate_behavior_endpoints(out)
  expect_equal(
    res$checks$status[res$checks$code == "identifier_preservation"],
    "skipped"
  )
})

test_that("validate_behavior_endpoints() flags a source well missing from the endpoint result (identifier_preservation)", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  out_dropped <- out |>
    dplyr::filter(!(.data$plate_id == "Plate_01" & .data$well_id == "A1"))
  res <- validate_behavior_endpoints(out_dropped, source_d = d)
  expect_false(res$ok)
  expect_equal(
    res$checks$status[res$checks$code == "identifier_preservation"],
    "failed"
  )
})

test_that("validate_behavior_endpoints() flags an inconsistent is_default_normalization/normalization_method pairing", {
  d <- zf_fixture_ready()
  segments <- zf_fixture_segments()
  out <- generate_behavior_endpoints(d, segments, list(LD = 1:50))

  out_bad <- out
  out_bad$is_default_normalization[1] <- !out_bad$is_default_normalization[1]
  res <- validate_behavior_endpoints(out_bad)
  expect_false(res$ok)
  expect_equal(
    res$checks$status[res$checks$code == "normalization_provenance_consistent"],
    "failed"
  )
})
