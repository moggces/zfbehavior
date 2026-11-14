#' Activity overview plots of behavioral hits
#'
#' The two activity overviews share one layout:
#' \itemize{
#'   \item [plot_behavior_anova_activity_pages()] (Module 4): ANOVA and
#'     repeated-measures ANOVA hits; point size = p-value class.
#'   \item [plot_behavior_bmc_activity_pages()] (Module 5): Curvep BMC
#'     hits; point size = hit-confidence class, optional BMC labels.
#' }
#'
#' @section Layout:
#' Facet rows = chemicals (wrapped labels); y = plate (replicate) label
#' within each chemical; facet columns = endpoint families, in the order
#' total distance, distance shift (up), distance shift (down), distance
#' change, then one movement-similarity column per metric (e.g.
#' "movement similarity (spearman)" next to "movement similarity
#' (pearson)"); x = the segment (phase or similarity window) of the
#' endpoint, in `segment_levels` order. Columns and x positions are derived
#' from the endpoint-table columns `endpoint_type`, `segment`, `component`
#' and `metric` (as in [generate_behavior_endpoints()] output), never by
#' parsing endpoint names. Points are drawn
#' only for rows with a size class (hits); plates without hits stay as
#' empty rows. Colours encode the family (all movement-similarity columns
#' share one colour); the column strips name the family and metric.
#'
#' @section Input ("activity table"):
#' One row per plate x endpoint with `chemical_col`, `plate_label_col`,
#' `endpoint_type` (`total_distance`, `distance_change_shift`,
#' `movement_similarity`), `segment`, `component` (`up`/`down`/`both` for
#' `distance_change_shift`) and `metric` (for `movement_similarity`), plus
#' the score column of the plot. Any table with these columns can be
#' plotted, e.g. a user's own summary across metrics (set `metric` to e.g.
#' `"spearman or pearson"` for a combined column).
#'
#' @seealso [module4], [module5].
#' @name activity-plots
NULL

activity_family_colours <- c(
  total = "#a6d854",
  up = "#fc8d62",
  down = "#66c2a5",
  both = "#8da0cb",
  similarity = "#e78ac3"
)

#' Order of segments on the x axis (internal)
#' @noRd
activity_segment_levels <- function(segments, segment_levels, extra = character(0)) {
  present <- unique(as.character(segments[!is.na(segments)]))
  if (is.null(segment_levels)) {
    segment_levels <- c(intersect(c("L1", "D1", "L2", "D2", "D2-ext", "LD"), present),
                        setdiff(present, c("L1", "D1", "L2", "D2", "D2-ext", "LD")))
  } else {
    missing <- setdiff(setdiff(present, extra), segment_levels)
    if (length(missing) > 0) {
      rlang::abort(
        c(
          "`segment_levels` does not include every segment in the data.",
          "x" = paste0("Missing: ", paste(missing, collapse = ", "))
        ),
        class = "zfbehavior_error_invalid_argument"
      )
    }
  }
  unique(c(segment_levels, extra))
}

#' Standardize an activity table to family / column / x (internal)
#' @noRd
standardize_activity_table <- function(d, chemical_col, plate_label_col, fn_name) {
  required <- c(chemical_col, plate_label_col, "endpoint_type", "segment", "component", "metric")
  missing_cols <- setdiff(required, colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        paste0(fn_name, "() requires columns that are missing."),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", ")),
        "i" = "Join endpoint_type/segment/component/metric from the behavior-endpoints table (distinct by endpoint)."
      ),
      class = "zfbehavior_error_missing_columns"
    )
  }
  et <- as.character(d$endpoint_type)
  comp <- as.character(d$component)
  met <- as.character(d$metric)
  family <- dplyr::case_when(
    et == "total_distance" ~ "total",
    et == "distance_change_shift" & comp %in% c("up", "down", "both") ~ comp,
    et == "movement_similarity" & !is.na(met) & nzchar(met) ~ "similarity",
    TRUE ~ NA_character_
  )
  bad <- is.na(family)
  if (any(bad)) {
    ex <- unique(paste0(et[bad], "/", comp[bad], "/", met[bad]))
    rlang::abort(
      c(
        paste0(fn_name, "() found rows whose endpoint family cannot be determined."),
        "x" = paste0("endpoint_type/component/metric: ", paste(utils::head(ex, 5), collapse = ", ")),
        "i" = "distance_change_shift needs component up/down/both; movement_similarity needs a metric."
      ),
      class = "zfbehavior_error_invalid_input"
    )
  }
  if (anyNA(d$segment)) {
    rlang::abort(paste0(fn_name, "() requires a non-missing `segment` for every row."),
                 class = "zfbehavior_error_invalid_input")
  }
  for (cc in c(chemical_col, plate_label_col)) {
    if (anyNA(d[[cc]])) {
      rlang::abort(paste0(fn_name, "() requires `", cc, "` without missing values."),
                   class = "zfbehavior_error_invalid_input")
    }
  }
  col_label <- dplyr::case_when(
    family == "total" ~ "total distance",
    family == "up" ~ "distance shift (up)",
    family == "down" ~ "distance shift (down)",
    family == "both" ~ "distance change",
    TRUE ~ paste0("movement similarity (", met, ")")
  )
  sim_cols <- unique(col_label[family == "similarity"])
  col_levels <- c("total distance", "distance shift (up)", "distance shift (down)",
                  "distance change", sim_cols)
  tibble::tibble(
    .chemical = as.character(d[[chemical_col]]),
    .plate = as.character(d[[plate_label_col]]),
    .family = family,
    .column = factor(col_label, levels = intersect(col_levels, unique(col_label))),
    .segment = as.character(d$segment)
  )
}

#' Sort chemicals and plates (internal)
#' @noRd
activity_order <- function(std, order) {
  chems <- unique(std$.chemical)
  if (order == "alphabetical") {
    chems <- chems[base::order(tolower(chems), chems, method = "radix")]
  }
  plates <- unique(std[, c(".chemical", ".plate")])
  plates$.ci <- match(plates$.chemical, chems)
  plates <- if (order == "alphabetical") {
    plates[base::order(plates$.ci, tolower(plates$.plate), plates$.plate, method = "radix"), , drop = FALSE]
  } else {
    plates[base::order(plates$.ci, method = "radix"), , drop = FALSE]
  }
  list(chemicals = chems, plates = unique(plates$.plate))
}

#' One activity page (internal engine)
#'
#' @param std Standardized table plus `.x` (factor), `.size` (factor of
#'   size classes, `NA` = not drawn) and optional `.label`.
#' @noRd
plot_activity_grid <- function(
  std,
  chem_levels,
  plate_levels,
  size_values,
  size_title,
  chemical_wrap_width = 30,
  column_wrap_width = 16,
  base_size = 14,
  label = FALSE
) {
  std$.chemical <- factor(std$.chemical, levels = chem_levels)
  std$.plate <- factor(std$.plate, levels = rev(plate_levels))
  std$.family <- factor(std$.family, levels = names(activity_family_colours))
  hits <- std[!is.na(std$.size), , drop = FALSE]

  p <- ggplot2::ggplot(std, ggplot2::aes(x = .data$.x, y = .data$.plate)) +
    # Invisible layer: trains every plate / segment so non-hit rows stay visible.
    ggplot2::geom_blank() +
    ggplot2::geom_point(
      data = hits,
      ggplot2::aes(size = .data$.size, colour = .data$.family)
    ) +
    # Invisible points of every size class: the size legend is complete and
    # identical on every page, even on a page without hits.
    ggplot2::geom_point(
      data = data.frame(.x = std$.x[1], .plate = std$.plate[1],
                        .chemical = std$.chemical[1], .column = std$.column[1],
                        .size = factor(names(size_values), levels = names(size_values))),
      ggplot2::aes(size = .data$.size),
      alpha = 0, show.legend = TRUE
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$.chemical),
      cols = ggplot2::vars(.data$.column),
      scales = "free",
      space = "free",
      drop = TRUE,
      labeller = ggplot2::labeller(
        .chemical = ggplot2::label_wrap_gen(chemical_wrap_width),
        .column = ggplot2::label_wrap_gen(column_wrap_width)
      )
    ) +
    ggplot2::scale_x_discrete(NULL, drop = TRUE) +
    ggplot2::scale_y_discrete("plate", drop = TRUE) +
    ggplot2::scale_size_manual(size_title, values = size_values, drop = FALSE) +
    ggplot2::scale_colour_manual(values = activity_family_colours, guide = "none") +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
      strip.clip = "off",
      legend.position = "top"
    ) +
    # Legend keys drawn opaque black: ggplot2 >= 3.5 otherwise shows a key only
    # for classes present in a layer's data (the full-legend layer is invisible).
    ggplot2::guides(size = ggplot2::guide_legend(
      nrow = 1, title.position = "left",
      override.aes = list(alpha = 1, colour = "black")
    ))

  if (label && nrow(hits) > 0 && ".label" %in% names(hits)) {
    lab <- hits[!is.na(hits$.label) & nzchar(hits$.label), , drop = FALSE]
    if (nrow(lab) > 0) {
      p <- p + ggplot2::geom_text(
        data = lab,
        ggplot2::aes(label = .data$.label),
        colour = "black", size = base_size / 4.5, hjust = 0, nudge_x = 0.15
      )
    }
  }
  p
}

check_pages_args <- function(n_per_page, page_height, fn_name) {
  if (!is.null(page_height) && (!is.numeric(page_height) || length(page_height) != 1 ||
                                !is.finite(page_height) || page_height <= 0)) {
    rlang::abort(paste0(fn_name, "() requires `page_height` to be NULL or one positive number (inches)."),
                 class = "zfbehavior_error_invalid_argument")
  }
  if (!is.numeric(n_per_page) || length(n_per_page) != 1 || !is.finite(n_per_page) ||
      n_per_page < 1 || n_per_page != round(n_per_page)) {
    rlang::abort(
      paste0(fn_name, "() requires `n_per_page` to be a finite positive integer scalar."),
      class = "zfbehavior_error_invalid_argument"
    )
  }
  invisible(TRUE)
}

#' Split sorted chemicals into pages of at most `n_per_page` plate rows
#'
#' A chemical's plates are never split; a chemical with more plates than
#' `n_per_page` gets a page of its own.
#' @noRd
activity_batches <- function(chemicals, n_plates, n_per_page) {
  batches <- list()
  cur <- character(0)
  used <- 0
  for (ch in chemicals) {
    n <- n_plates[[ch]]
    if (length(cur) > 0 && used + n > n_per_page) {
      batches[[length(batches) + 1]] <- cur
      cur <- character(0)
      used <- 0
    }
    cur <- c(cur, ch)
    used <- used + n
  }
  if (length(cur) > 0) batches[[length(batches) + 1]] <- cur
  batches
}

#' Pad a page gtable to the height of the fullest page (internal)
#'
#' Facet panels with `space = "free"` get `null` heights proportional to
#' their number of plate rows, so adding a bottom spacer of the missing
#' `null` units (plus the missing panel spacings) gives every plate row the
#' same absolute height on every page.
#' @noRd
pad_activity_gtable <- function(g, full_null, full_panels, spacing_pt) {
  is_null <- grid::unitType(g$heights) == "null"
  page_null <- sum(as.numeric(g$heights)[is_null])
  n_panels <- sum(is_null)
  extra_null <- full_null - page_null
  extra_pt <- (full_panels - n_panels) * spacing_pt
  if (extra_null > 1e-9) g$heights <- grid::unit.c(g$heights, grid::unit(extra_null, "null"))
  if (extra_pt > 1e-9) g$heights <- grid::unit.c(g$heights, grid::unit(extra_pt, "pt"))
  g
}

#' Does each chemical's (wrapped) label fit its panel? (internal)
#'
#' Measures the right strip labels and the panel heights of the padded page
#' gtables at the given page height (in inches).
#' @noRd
activity_label_fit <- function(gts, batches, n_plates, full_null, page_height) {
  grDevices::pdf(NULL, width = 10, height = page_height)
  on.exit(grDevices::dev.off(), add = TRUE)
  rows <- lapply(seq_along(gts), function(k) {
    g <- gts[[k]]
    is_null <- grid::unitType(g$heights) == "null"
    fixed_in <- sum(grid::convertHeight(g$heights[!is_null], "in", valueOnly = TRUE))
    # marrangeGrob adds an (empty) top title row of about one line.
    in_per_null <- max(page_height - fixed_in - 0.25, 0) / full_null
    strips <- g$grobs[grep("^strip-r", g$layout$name)]
    label_in <- vapply(strips, function(sg) {
      st <- sg$grobs[[1]]
      tx <- st$children[[length(st$children)]]
      grid::convertHeight(sum(tx$heights), "in", valueOnly = TRUE)
    }, 1)
    chems <- batches[[k]]
    np <- unname(n_plates[chems])
    tibble::tibble(
      chemical = chems,
      page = k,
      plates = np,
      # A panel of n plate rows spans n + 0.2 null units (discrete expansion).
      panel_height_in = round((np + 0.2) * in_per_null, 3),
      label_height_in = round(label_in, 3)
    )
  })
  out <- dplyr::bind_rows(rows)
  out$fits <- out$label_height_in <= out$panel_height_in + 1e-6
  out
}

#' Paginate standardized activity rows (internal)
#' @noRd
activity_pages <- function(std, order, n_per_page, page_height = NULL,
                           chemical_wrap_width = 30, base_size = 14, ...) {
  ord <- activity_order(std, order)
  plates <- unique(std[, c(".chemical", ".plate")])
  n_plates <- table(plates$.chemical)[ord$chemicals]
  n_plates <- stats::setNames(as.integer(n_plates), ord$chemicals)
  batches <- activity_batches(ord$chemicals, n_plates, n_per_page)
  pls <- lapply(batches, function(chems) {
    s <- std[std$.chemical %in% chems, , drop = FALSE]
    plot_activity_grid(
      s, chem_levels = chems,
      plate_levels = ord$plates[ord$plates %in% s$.plate],
      chemical_wrap_width = chemical_wrap_width, base_size = base_size,
      ...
    )
  })
  gts <- lapply(pls, ggplot2::ggplotGrob)
  null_of <- function(g) sum(as.numeric(g$heights)[grid::unitType(g$heights) == "null"])
  panels_of <- function(g) sum(grid::unitType(g$heights) == "null")
  full_null <- max(vapply(gts, null_of, 1))
  full_panels <- max(vapply(gts, panels_of, 1))
  spacing_pt <- 5.5
  gts <- lapply(gts, pad_activity_gtable, full_null = full_null,
                full_panels = full_panels, spacing_pt = spacing_pt)

  out <- gridExtra::marrangeGrob(gts, nrow = 1, ncol = 1, top = "")
  # The page ggplots, for inspection/tests (marrangeGrob keeps only gtables).
  attr(out, "plots") <- unname(pls)
  attr(out, "page_plate_rows") <- vapply(batches, function(ch) sum(n_plates[ch]), 1L)

  if (!is.null(page_height)) {
    attr(out, "label_fit") <- activity_label_fit(gts, batches, n_plates, full_null, page_height)
  }
  out
}

#' p-value classes of ANOVA/RM-ANOVA hits
#'
#' @param p Numeric p-values (`NA` for non-hits).
#' @return A factor with levels `"p < 0.05"`, `"p < 0.01"`, `"p < 0.001"`,
#'   `"p < 1e-5"`, `"p < 1e-10"`; `NA` where `p` is `NA` or `>= 0.05`.
#' @noRd
anova_p_class <- function(p) {
  lv <- c("p < 0.05", "p < 0.01", "p < 0.001", "p < 1e-5", "p < 1e-10")
  cls <- cut(p, breaks = c(-Inf, 1e-10, 1e-5, 0.001, 0.01, 0.05), right = FALSE,
             labels = rev(lv))
  factor(as.character(cls), levels = lv)
}

#' ANOVA/RM-ANOVA hit overview for many chemicals, paginated
#'
#' Module 4 overview plot (skill `behavior-endpoints-anova`). Plots the hits
#' of [run_anova_rmanova_hit_calling()] for many chemicals
#' in the shared activity layout (see `?"activity-plots"`): facet rows =
#' chemicals (sorted), y = plate labels (the tested units), columns =
#' endpoint families (one movement-similarity column per metric), x =
#' segments. ANOVA hits are drawn at their segment; RM-ANOVA hits (one per
#' plate, pooled over the distance-change segments) at an extra x position
#' `rm_label` in the distance-change column. Point size = p-value class of
#' the test that decided the hit (`hit_p_value`; legend "p < 0.05" ...
#' "p < 1e-10", the same on every page); non-hits are not drawn.
#'
#' @param hits Output of [run_anova_rmanova_hit_calling()] with
#'   `chemical_col` and `plate_label_col` joined on, and -- for ANOVA rows --
#'   `endpoint_type`, `segment`, `component`, `metric` joined from the
#'   behavior-endpoints table by `endpoint`. RM-ANOVA rows (`test ==
#'   "RMANOVA"`) may have these missing; they are placed in the distance
#'   change column at `rm_label`.
#' @param chemical_col Column with the chemical label (facet rows; pages are
#'   split by it).
#' @param plate_label_col Column with the plate label (y axis); unique per
#'   plate within a chemical.
#' @param segment_levels Order of segments (phases, similarity windows) on
#'   the x axis, e.g. the run's segment order `c("L1", "D1", "L2", "D2",
#'   "D2-ext", "LD")`. Default `NULL`: L1, D1, L2, D2, D2-ext, LD first, then
#'   other segments in order of appearance.
#' @param order `"alphabetical"` (default; chemicals case-insensitively,
#'   plates within chemical) or `"as_given"` (order of appearance).
#' @param n_per_page Plate rows per page (the tested unit sets the page).
#'   Whole chemicals are placed on a page until the next one would exceed
#'   `n_per_page` plate rows (a chemical is never split; one with more plates
#'   gets a page of its own). Every page is padded at the bottom to the plate
#'   rows of the fullest page, so a plate row has the same height on every
#'   page at a fixed PDF height. Default `20`.
#' @param page_height Optional PDF page height in inches (the height the
#'   pages will be drawn at). If given, attribute `"label_fit"` reports per
#'   chemical whether its wrapped label fits its panel (`fits`); if not, use
#'   a larger page height or `chemical_wrap_width`. Default `NULL`.
#' @param rm_label X label of RM-ANOVA hits. Default `"all phases (RM)"`.
#' @param chemical_wrap_width,column_wrap_width Wrap widths of the chemical
#'   (row) and column strip labels. Defaults `30`, `16`.
#' @param base_size Base font size. Default `14`.
#'
#' @return A paginated grob from `gridExtra::marrangeGrob()` (one page per
#'   batch of chemicals); save with `ggplot2::ggsave()` or draw page by page.
#'   Attributes: `"plots"` (the page `ggplot`s, unpadded),
#'   `"page_plate_rows"` (plate rows per page), and `"label_fit"` (if
#'   `page_height` is given).
#' @seealso [module4], [activity-plots].
#' @export
#'
#' @examples
#' hits <- tibble::tibble(
#'   chem = c("Chem B", "Chem B", "Chem A", "Chem A", "Chem A"),
#'   plate = c("P2", "P2", "P1", "P1", "P1"),
#'   test = c("ANOVA", "ANOVA", "ANOVA", "ANOVA", "RMANOVA"),
#'   endpoint_type = c("distance_change_shift", "movement_similarity",
#'                     "distance_change_shift", "movement_similarity", NA),
#'   segment = c("D1", "LD", "L1", "LD", NA),
#'   component = c("both", NA, "both", NA, NA),
#'   metric = c(NA, "spearman", NA, "spearman", NA),
#'   hit_p_value = c(0.003, NA, 1e-6, 0.04, 0.02)
#' )
#' plot_behavior_anova_activity_pages(hits, "chem", "plate")
plot_behavior_anova_activity_pages <- function(
  hits,
  chemical_col,
  plate_label_col,
  segment_levels = NULL,
  order = c("alphabetical", "as_given"),
  n_per_page = 20,
  page_height = NULL,
  rm_label = "all phases (RM)",
  chemical_wrap_width = 30,
  column_wrap_width = 16,
  base_size = 14
) {
  fn_name <- "plot_behavior_anova_activity_pages"
  order <- match.arg(order)
  check_pages_args(n_per_page, page_height, fn_name)
  missing_cols <- setdiff(c("test", "hit_p_value"), colnames(hits))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(paste0(fn_name, "() requires columns that are missing."),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", "))),
      class = "zfbehavior_error_missing_columns"
    )
  }
  for (cc in c("endpoint_type", "segment", "component", "metric")) {
    if (!cc %in% colnames(hits)) hits[[cc]] <- NA_character_
  }
  rm <- hits$test == "RMANOVA"
  hits$endpoint_type[rm] <- "distance_change_shift"
  hits$component[rm] <- "both"
  hits$segment[rm] <- rm_label
  hits$metric[rm] <- NA_character_

  std <- standardize_activity_table(hits, chemical_col, plate_label_col, fn_name)
  x_levels <- activity_segment_levels(std$.segment, segment_levels, extra = if (any(rm)) rm_label else character(0))
  std$.x <- factor(std$.segment, levels = x_levels)
  std$.size <- anova_p_class(hits$hit_p_value)

  activity_pages(
    std, order, n_per_page, page_height = page_height,
    size_values = stats::setNames(c(1.5, 2.5, 3.5, 4.5, 5.5),
                                  levels(std$.size)),
    size_title = "hit p-value",
    chemical_wrap_width = chemical_wrap_width,
    column_wrap_width = column_wrap_width,
    base_size = base_size
  )
}

#' Hit-confidence classes of BMC activity (internal)
#' @noRd
bmc_confidence_class <- function(conf, hit_threshold) {
  inner <- seq(0.1, 0.9, by = 0.1)
  br <- c(hit_threshold, inner[inner > hit_threshold + 1e-9], 1)
  fmt <- function(x) as.character(round(x, 2))
  lv <- paste0(fmt(utils::head(br, -1)), "-", fmt(br[-1]))
  cls <- cut(conf, breaks = br, labels = lv, right = TRUE, include.lowest = FALSE)
  factor(as.character(cls), levels = lv)
}

#' BMC activity overview for many chemicals, paginated
#'
#' Module 5 (Curvep BMC modeling) overview plot. Plots behavioral Curvep BMC
#' activity ([run_bmc_curvep()] `act_summary`) for many chemicals in the shared activity layout (see
#' `?"activity-plots"`), matching [plot_behavior_anova_activity_pages()]:
#' facet rows = chemicals (sorted), y = plate (or
#' pooled replicate) labels, columns = endpoint families (one
#' movement-similarity column per metric), x = segments. Point size =
#' hit-confidence class (above `hit_threshold`, bins of 0.1 up to 1; legend
#' the same on every page); rows at or below `hit_threshold` are not drawn.
#' An optional label column (e.g. the formatted BMC) is written next to each
#' drawn point.
#'
#' @param d Activity table: `act_summary` rows with `chemical_col`,
#'   `plate_label_col`, `endpoint_type`, `segment`, `component`, `metric`
#'   (joined from the behavior-endpoints table by `endpoint`), and
#'   `hit_confidence_col`.
#' @inheritParams plot_behavior_anova_activity_pages
#' @param hit_confidence_col Column with the hit confidence (0-1). Default
#'   `"hit_confidence"`.
#' @param hit_threshold Rows with hit confidence `<= hit_threshold` are not
#'   drawn. Default `0.5`.
#' @param label_col Optional column with text written next to drawn points
#'   (e.g. `"BMC 1.2 uM"`). Default `NULL`.
#'
#' @return A paginated grob from `gridExtra::marrangeGrob()`, with the same
#'   attributes as [plot_behavior_anova_activity_pages()].
#' @seealso [bmc] (Module 5), [run_bmc_curvep()], [activity-plots], skill
#'   `behavior-endpoints-bmc`.
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   chem = c("Chem A", "Chem A", "Chem B"),
#'   plate = c("P1", "P1", "P2"),
#'   endpoint_type = c("distance_change_shift", "movement_similarity", "distance_change_shift"),
#'   segment = c("D1", "LD", "L1"),
#'   component = c("up", NA, "both"),
#'   metric = c(NA, "spearman", NA),
#'   hit_confidence = c(0.92, 0.3, 0.71),
#'   bmc_label = c("1.2", NA, "30")
#' )
#' plot_behavior_bmc_activity_pages(d, "chem", "plate", label_col = "bmc_label")
plot_behavior_bmc_activity_pages <- function(
  d,
  chemical_col,
  plate_label_col,
  hit_confidence_col = "hit_confidence",
  hit_threshold = 0.5,
  label_col = NULL,
  segment_levels = NULL,
  order = c("alphabetical", "as_given"),
  n_per_page = 20,
  page_height = NULL,
  chemical_wrap_width = 30,
  column_wrap_width = 16,
  base_size = 14
) {
  fn_name <- "plot_behavior_bmc_activity_pages"
  order <- match.arg(order)
  check_pages_args(n_per_page, page_height, fn_name)
  missing_cols <- setdiff(c(hit_confidence_col, label_col), colnames(d))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(paste0(fn_name, "() requires columns that are missing."),
        "x" = paste0("Missing: ", paste(missing_cols, collapse = ", "))),
      class = "zfbehavior_error_missing_columns"
    )
  }
  if (!is.numeric(hit_threshold) || length(hit_threshold) != 1 || !is.finite(hit_threshold) ||
      hit_threshold < 0 || hit_threshold >= 1) {
    rlang::abort(paste0(fn_name, "() requires `hit_threshold` in [0, 1)."),
                 class = "zfbehavior_error_invalid_argument")
  }
  std <- standardize_activity_table(d, chemical_col, plate_label_col, fn_name)
  std$.x <- factor(std$.segment, levels = activity_segment_levels(std$.segment, segment_levels))
  std$.size <- bmc_confidence_class(d[[hit_confidence_col]], hit_threshold)
  if (!is.null(label_col)) std$.label <- as.character(d[[label_col]])
  n_lv <- nlevels(std$.size)

  activity_pages(
    std, order, n_per_page, page_height = page_height,
    size_values = stats::setNames(seq(1.5, 5.5, length.out = n_lv), levels(std$.size)),
    size_title = "hit confidence",
    chemical_wrap_width = chemical_wrap_width,
    column_wrap_width = column_wrap_width,
    base_size = base_size,
    label = !is.null(label_col)
  )
}
