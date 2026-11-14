#' Example light-dark transition test data (3 plates)
#'
#' Tracking data of three 96-well plates of a zebrafish developmental
#' neurotoxicity study (light-dark transition test, 120 hours post
#' fertilization, Biobide), one chemical per plate: Sulfisoxazole
#' (Plate_01), Pyrasulfotole (Plate_47) and 2-ethylhexyl
#' 2,3,4,5-tetrabromobenzoate (TBB, Plate_62). Each plate has 16
#' vehicle-control (DMSO) wells and 5 concentrations x 16 wells, recorded
#' for 50 one-minute bins (L1 1-10, D1 11-20, L2 21-30, D2 31-50). It is
#' stored as a file in the vendor's column format, as it would be imported,
#' and is used by the examples, the tests and `vignette("methods")`.
#'
#' Read it with
#' `readRDS(system.file("extdata", "zf_behavior_example.rds", package = "zfbehavior"))`.
#'
#' @format A tibble with 14,400 rows (3 plates x 96 wells x 50 timepoints)
#'   and 24 vendor columns, including:
#' \describe{
#'   \item{`PLATE_NAME`, `WELL_POSITION`}{plate and well (canonical
#'     `plate_id`, `well_id`).}
#'   \item{`BIOBIDE_ID`, `CHEMICAL_NAME`, `DTXSID`, `NIH_BLINDED_ID`}{chemical
#'     identifiers (`BIOBIDE_ID` as `chemical_id`).}
#'   \item{`IS_VEHICLE`, `CONCENTRATION`, `CONCENTRATION_UNIT`}{vehicle-control
#'     flag and concentration in uM.}
#'   \item{`BINNING`, `PHASE`, `time_end`}{time bin as text, light/dark
#'     phase, and bin end in minutes (`time_value`).}
#'   \item{`DISTANCE_MOVED`}{distance moved per bin in mm
#'     (`response_value`); missing when tracking failed.}
#'   \item{`KEEP_WELL_DATA`}{`FALSE` for wells not to analyze (`keep_well`).}
#'   \item{`COMMENTS`}{the lab's well notes, e.g. `"dead"`,
#'     `"no swim bladder"`. Five wells of Plate_62 are `"empty"` or
#'     `"2 larvae"`: under the single-larva well contract
#'     ([canonical-columns]) they must be removed at import, which
#'     `vignette("methods")` shows.}
#' }
#' @source National Toxicology Program zebrafish developmental neurotoxicity
#'   study; subset of three plates.
#' @seealso [module1], [read_behavior_data()].
#' @name zfbehavior_example
#' @keywords datasets
NULL
