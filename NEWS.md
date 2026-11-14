# zfbehavior 0.1.0

First public release.

* Five analysis modules for zebrafish larval light-dark transition test and
  developmental toxicity data: data import and consistency checks;
  raw-response visualization and inter-plate vehicle-control QC; endpoint
  calculation, normalization and intra-plate vehicle-control QC;
  ANOVA/repeated-measures ANOVA hit calling; Curvep-based BMC modeling.
  Overview help pages `?module1` to `?module5`.
* Nine Agent Skills in `system.file("skills", package = "zfbehavior")`, one
  run folder per step with a parameter file, a Quarto report and a manifest.
* Example data `zf_behavior_example.rds` (3 plates) in `inst/extdata`.
* Vignettes "Getting started with the agent skills" and "Methods and
  modules".
