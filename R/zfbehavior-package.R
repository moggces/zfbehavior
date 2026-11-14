#' zfbehavior: analysis of zebrafish larval light-dark behavior and
#' developmental toxicity
#'
#' zfbehavior analyzes zebrafish larval light-dark transition test (LDTT)
#' tracking data and developmental toxicity, from vendor files to hit calls
#' and benchmark concentrations (BMC). The workflow has five modules; each is
#' run by one or more Agent Skills that guide a coding agent (or a person)
#' through one run folder per step, with a rendered Quarto report.
#'
#' @section Modules:
#' \itemize{
#'   \item [module1] Data import and consistency checks. Skill
#'     `dataset-import-qc`.
#'   \item [module2] Raw-response visualization and inter-plate VC QC.
#'     Skills `raw-response-plots`, `interplate-vc-qc`.
#'   \item [module3] Endpoint calculation, normalization, and intra-plate VC
#'     QC. Skills `behavior-endpoints`, `intraplate-vc-qc`,
#'     `devtox-endpoints`.
#'   \item [module4] ANOVA / repeated-measures ANOVA hit calling. Skill
#'     `behavior-endpoints-anova`.
#'   \item [module5] Curvep-based BMC modeling of behavioral and
#'     developmental toxicity endpoints. Skills `behavior-endpoints-bmc`,
#'     `devtox-endpoints-bmc`.
#' }
#'
#' @section Agent Skills:
#' The skills are installed with the package:
#' `system.file("skills", package = "zfbehavior")`. Start with
#' `CONVENTIONS.md` there (run folders, manifests, the order of the skills),
#' then each skill's `SKILL.md`. Rendering the reports needs Quarto.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL
