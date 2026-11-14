# Shared behavioral activity plots (R/activity-plots.R)

anova_hits_fixture <- function() {
  tibble::tibble(
    chem = c("chem b", "chem b", "Chem A", "Chem A", "Chem A", "Chem A", "Chem C"),
    plate = c("P2", "P2", "P1b", "P1b", "P1a", "P1a", "P3"),
    test = c("ANOVA", "ANOVA", "ANOVA", "ANOVA", "ANOVA", "RMANOVA", "ANOVA"),
    endpoint_type = c("distance_change_shift", "movement_similarity", "distance_change_shift",
                      "movement_similarity", "movement_similarity", NA, "distance_change_shift"),
    segment = c("D1", "LD", "L1", "LD", "LD", NA, "D2"),
    component = c("both", NA, "both", NA, NA, NA, "both"),
    metric = c(NA, "spearman", NA, "spearman", "pearson", NA, NA),
    hit_p_value = c(0.003, NA, 1e-6, 0.04, 1e-12, 0.02, NA)
  )
}

page_plot <- function(pages, i = 1) attr(pages, "plots")[[i]]

draw_ok <- function(pages) {
  for (pg in attr(pages, "plots")) {
    expect_no_warning(ggplot2::ggplot_build(pg))
  }
}

test_that("anova_p_class() bins p-values into fixed classes", {
  cls <- zfbehavior:::anova_p_class(c(0.04, 0.009, 5e-4, 1e-6, 1e-11, 0.05, NA))
  expect_identical(levels(cls), c("p < 0.05", "p < 0.01", "p < 0.001", "p < 1e-5", "p < 1e-10"))
  expect_identical(as.character(cls),
                   c("p < 0.05", "p < 0.01", "p < 0.001", "p < 1e-5", "p < 1e-10", NA, NA))
})

test_that("plot_behavior_anova_activity_pages(): columns per family/metric, RM position, size legend", {
  pages <- plot_behavior_anova_activity_pages(anova_hits_fixture(), "chem", "plate")
  expect_s3_class(pages, "arrangelist")
  expect_length(pages, 1)
  p <- page_plot(pages)
  b <- ggplot2::ggplot_build(p)
  expect_identical(
    levels(b$layout$layout$.column),
    c("distance change", "movement similarity (spearman)", "movement similarity (pearson)")
  )
  # Size legend is shown, with all p classes on every page.
  expect_false(identical(p$guides$guides$size, "none"))
  size_scale <- b$plot$scales$get_scales("size")
  expect_identical(size_scale$get_labels(),
                   c("p < 0.05", "p < 0.01", "p < 0.001", "p < 1e-5", "p < 1e-10"))
  # RM-ANOVA hit at its own x position in the distance-change column.
  pts <- p$layers[[2]]$data
  expect_identical(as.character(pts$.x[pts$.plate == "P1a" & pts$.column == "distance change"]),
                   "all phases (RM)")
  expect_identical(levels(pts$.x), c("L1", "D1", "D2", "LD", "all phases (RM)"))
  # Non-hits (NA p) not drawn: 5 drawn points of 7 rows.
  expect_identical(nrow(pts), 5L)
  draw_ok(pages)
})

# Legend keys (of the size legend) holding at least one visible point.
visible_legend_keys <- function(p) {
  g <- ggplot2::ggplotGrob(p)
  box <- g$grobs[[which(g$layout$name == "guide-box-top")]]
  visible <- function(x) {
    if (inherits(x, "points")) {
      a <- grDevices::col2rgb(x$gp$col, alpha = TRUE)["alpha", ]
      return(any(a > 0) && (is.null(x$gp$alpha) || x$gp$alpha > 0))
    }
    any(vapply(if (inherits(x, "gtable")) x$grobs else x$children, visible, TRUE))
  }
  n <- 0L
  walk <- function(x) {
    if (inherits(x, "gtable") && any(grepl("^key-", x$layout$name))) {
      # Group key grobs by cell (t, l) and count cells with a visible point.
      cells <- unique(x$layout[grepl("^key-", x$layout$name), c("t", "l")])
      for (i in seq_len(nrow(cells))) {
        idx <- which(grepl("^key-", x$layout$name) & x$layout$t == cells$t[i] & x$layout$l == cells$l[i])
        if (any(vapply(x$grobs[idx], visible, TRUE))) n <<- n + 1L
      }
      return(invisible())
    }
    for (k in if (inherits(x, "gtable")) x$grobs else x$children) walk(k)
  }
  walk(box)
  n
}

test_that("the size legend shows a visible key for every class, on every page", {
  # Fixture hits cover 4 of 5 p classes (no p < 0.001); one chemical per page.
  pages <- plot_behavior_anova_activity_pages(anova_hits_fixture(), "chem", "plate", n_per_page = 1)
  expect_length(pages, 3)
  for (p in attr(pages, "plots")) expect_identical(visible_legend_keys(p), 5L)
  d <- tibble::tibble(
    chem = "c1", plate = "p1", endpoint_type = "distance_change_shift", segment = "D1",
    component = "both", metric = NA, hit_confidence = 0.55
  )
  bp <- plot_behavior_bmc_activity_pages(d, "chem", "plate")
  expect_identical(visible_legend_keys(page_plot(bp)), 5L)
})

test_that("activity pages sort chemicals case-insensitively and plates within chemical", {
  # n_per_page = plate rows: Chem A (2 plates) + chem b (1) = 3 rows on page 1.
  pages <- plot_behavior_anova_activity_pages(anova_hits_fixture(), "chem", "plate", n_per_page = 3)
  expect_length(pages, 2)
  d1 <- page_plot(pages, 1)$data
  expect_identical(levels(d1$.chemical), c("Chem A", "chem b"))
  # y is reversed so the first plate is on top.
  expect_identical(rev(levels(d1$.plate)), c("P1a", "P1b", "P2"))
  expect_identical(levels(page_plot(pages, 2)$data$.chemical), "Chem C")

  as_given <- plot_behavior_anova_activity_pages(anova_hits_fixture(), "chem", "plate",
                                                 order = "as_given", n_per_page = 3)
  expect_identical(levels(page_plot(as_given, 1)$data$.chemical), c("chem b", "Chem A"))
})

test_that("activity pages keep plates without hits and honour segment_levels", {
  h <- anova_hits_fixture()
  pages <- plot_behavior_anova_activity_pages(h, "chem", "plate",
                                              segment_levels = c("D2", "D1", "L1", "LD"))
  p <- page_plot(pages)
  expect_true("P3" %in% levels(p$data$.plate))   # P3 has no hit
  expect_identical(levels(p$data$.x), c("D2", "D1", "L1", "LD", "all phases (RM)"))
  expect_error(
    plot_behavior_anova_activity_pages(h, "chem", "plate", segment_levels = c("L1", "LD")),
    class = "zfbehavior_error_invalid_argument"
  )
})

test_that("activity pages work for a single distance-change segment and several similarity windows", {
  h <- tibble::tibble(
    chem = "A", plate = "P1", test = "ANOVA",
    endpoint_type = c("distance_change_shift", "movement_similarity", "movement_similarity"),
    segment = c("D2", "L1D1", "L2D2"), component = c("both", NA, NA),
    metric = c(NA, "spearman", "spearman"), hit_p_value = c(0.01, 0.001, 0.2)
  )
  p <- page_plot(plot_behavior_anova_activity_pages(h, "chem", "plate"))
  expect_identical(levels(p$data$.x), c("D2", "L1D1", "L2D2"))
  draw_ok(plot_behavior_anova_activity_pages(h, "chem", "plate"))
})

test_that("activity pages validate input", {
  h <- anova_hits_fixture()
  expect_error(plot_behavior_anova_activity_pages(h, "nope", "plate"),
               class = "zfbehavior_error_missing_columns")
  expect_error(plot_behavior_anova_activity_pages(dplyr::select(h, -"hit_p_value"), "chem", "plate"),
               class = "zfbehavior_error_missing_columns")
  bad <- h
  bad$metric[2] <- NA
  expect_error(plot_behavior_anova_activity_pages(bad, "chem", "plate"),
               class = "zfbehavior_error_invalid_input")
  expect_error(plot_behavior_anova_activity_pages(h, "chem", "plate", n_per_page = 0),
               class = "zfbehavior_error_invalid_argument")
})

test_that("plot_behavior_bmc_activity_pages(): confidence classes, labels, same layout", {
  d <- tibble::tibble(
    chem = c("Chem B", "Chem A", "Chem A", "Chem A"),
    plate = c("P2", "P1", "P1", "P1"),
    endpoint_type = c("distance_change_shift", "distance_change_shift", "movement_similarity",
                      "total_distance"),
    segment = c("L1", "D1", "LD", "D1"),
    component = c("both", "up", NA, NA),
    metric = c(NA, NA, "pearson", NA),
    hit_confidence = c(0.71, 0.92, 0.3, 1),
    bmc = c("30", "1.2", NA, "5")
  )
  pages <- plot_behavior_bmc_activity_pages(d, "chem", "plate", label_col = "bmc")
  p <- page_plot(pages)
  b <- ggplot2::ggplot_build(p)
  expect_identical(levels(b$layout$layout$.column),
                   c("total distance", "distance shift (up)", "distance change",
                     "movement similarity (pearson)"))
  expect_identical(b$plot$scales$get_scales("size")$get_labels(),
                   c("0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
  pts <- p$layers[[2]]$data
  expect_identical(nrow(pts), 3L)  # 0.3 not drawn
  geoms <- unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
  expect_true("GeomText" %in% geoms)
  expect_identical(levels(p$data$.chemical), c("Chem A", "Chem B"))
  draw_ok(pages)

  cls <- zfbehavior:::bmc_confidence_class(c(0.65, 0.66, 1), 0.65)
  expect_identical(as.character(cls), c(NA, "0.65-0.7", "0.9-1"))
  expect_error(plot_behavior_bmc_activity_pages(d, "chem", "plate", hit_threshold = 1),
               class = "zfbehavior_error_invalid_argument")
  expect_error(plot_behavior_bmc_activity_pages(d, "chem", "plate", label_col = "x"),
               class = "zfbehavior_error_missing_columns")
})

# ---- paging by plate rows, padding, label fit -----------------------------

multi_plate_hits <- function() {
  # A: 3 plates, B: 1, C: 2, D: 4 plates.
  chem <- c(rep("A", 3), "B", rep("C", 2), rep("D", 4))
  tibble::tibble(
    chem = chem,
    plate = paste0(chem, "-p", ave(seq_along(chem), chem, FUN = seq_along)),
    test = "ANOVA",
    endpoint_type = "distance_change_shift", segment = "D1", component = "both",
    metric = NA_character_, hit_p_value = 0.01
  )
}

panel_null <- function(pages, i) {
  g <- pages[[i]]$grobs[[1]]
  sum(as.numeric(g$heights)[grid::unitType(g$heights) == "null"])
}

test_that("activity_batches() fills pages by plate rows without splitting a chemical", {
  b <- zfbehavior:::activity_batches(c("A", "B", "C", "D"), c(A = 3, B = 1, C = 2, D = 4), 4)
  expect_identical(b, list(c("A", "B"), "C", "D"))
  b2 <- zfbehavior:::activity_batches(c("A", "B"), c(A = 6, B = 1), 4)
  expect_identical(b2, list("A", "B"))   # oversized chemical alone on its page
})

test_that("n_per_page counts plate rows; every page padded to the same plate-row units", {
  pages <- plot_behavior_anova_activity_pages(multi_plate_hits(), "chem", "plate", n_per_page = 4)
  expect_identical(attr(pages, "page_plate_rows"), c(4L, 2L, 4L))
  plates_on <- function(i) as.character(unique(attr(pages, "plots")[[i]]$data$.plate))
  expect_setequal(plates_on(1), c("A-p1", "A-p2", "A-p3", "B-p1"))
  expect_setequal(plates_on(2), c("C-p1", "C-p2"))
  # Padded null units are equal on every page -> equal plate-row height.
  n <- vapply(seq_along(pages), function(i) panel_null(pages, i), 1)
  expect_equal(n, rep(n[1], 3))
  # Padding is blank: the short page's ggplot has only its own chemicals.
  expect_identical(levels(attr(pages, "plots")[[2]]$data$.chemical), "C")
})

test_that("label_fit flags long chemical labels at a small page height", {
  h <- multi_plate_hits()
  long <- "A chemical with a very long name that wraps over several lines in the strip"
  h$chem[h$chem == "B"] <- long
  small <- plot_behavior_anova_activity_pages(h, "chem", "plate", n_per_page = 4,
                                              page_height = 4, chemical_wrap_width = 15)
  lf <- attr(small, "label_fit")
  expect_s3_class(lf, "tbl_df")
  expect_identical(nrow(lf), 4L)
  expect_false(lf$fits[lf$chemical == long])
  expect_gt(lf$label_height_in[lf$chemical == long], lf$label_height_in[lf$chemical == "A"])
  big <- plot_behavior_anova_activity_pages(h, "chem", "plate", n_per_page = 4,
                                            page_height = 40, chemical_wrap_width = 15)
  expect_true(all(attr(big, "label_fit")$fits))
  expect_null(attr(plot_behavior_anova_activity_pages(h, "chem", "plate"), "label_fit"))
  expect_error(plot_behavior_anova_activity_pages(h, "chem", "plate", page_height = -1),
               class = "zfbehavior_error_invalid_argument")
})
