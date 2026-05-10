#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ape)
  library(dplyr)
  library(readxl)
})

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0) return(default)
  sub(prefix, "", hit[[1]], fixed = TRUE)
}

tree_file <- get_arg("tree", "results/phylo_genus/meliponini_genus_level_conservative.tre")
counts_file <- get_arg("counts-file", "data/article_counts_ai/meliponini_ai_studied_taxa_counts.xlsx")
counts_sheet <- get_arg("counts-sheet", "genus_ai_counts")
count_column <- get_arg("count-column", "n_articles_ai_studied_genus_combined")
output_dir <- get_arg("output-dir", "results/phylo_genus")
plot_prefix <- get_arg("plot-prefix", "meliponini_genus_phylo_branch_heatmap")
scale_mode <- get_arg("scale", "log1p")
basal_compression <- as.numeric(get_arg("basal-compression", "1.8"))
mark_regions <- tolower(get_arg("mark-regions", "true")) %in% c("true", "t", "1", "yes", "sim")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

tree <- read.tree(tree_file)
counts <- read_excel(counts_file, sheet = counts_sheet) |>
  mutate(genus = as.character(genus))

if (!count_column %in% names(counts)) {
  stop("Column not found in counts file: ", count_column)
}

compress_basal_branches <- function(phy, power = 1.8) {
  if (is.na(power) || power <= 0 || abs(power - 1) < .Machine$double.eps) {
    return(phy)
  }
  if (is.null(phy$edge.length) || any(!is.finite(phy$edge.length))) {
    return(phy)
  }
  depths <- node.depth.edgelength(phy)
  max_depth <- max(depths, na.rm = TRUE)
  if (!is.finite(max_depth) || max_depth <= 0) {
    return(phy)
  }
  transformed_depths <- max_depth * (depths / max_depth)^power
  phy$edge.length <- transformed_depths[phy$edge[, 2]] - transformed_depths[phy$edge[, 1]]
  phy$edge.length <- pmax(phy$edge.length, max_depth * 1e-6)
  phy
}

tree <- compress_basal_branches(tree, basal_compression)

genus_from_tip_label <- function(x) {
  x |>
    sub("^\\*", "", x = _) |>
    sub("\\s+.*$", "", x = _)
}

tip_data <- tibble(
  tip_index = seq_along(tree$tip.label),
  tip_label = tree$tip.label,
  genus = genus_from_tip_label(tree$tip.label),
  is_non_monophyletic_label = startsWith(tree$tip.label, "*")
) |>
  left_join(
    counts |> select(genus, subtribe = Subtribo, count = all_of(count_column)),
    by = "genus"
  ) |>
  mutate(
    count = ifelse(is.na(count), 0, as.numeric(count)),
    mapped_value = if (scale_mode == "log1p") log1p(count) else count,
    annotation_group = ifelse(is.na(subtribe), "Unassigned", subtribe)
  )

descendant_tips <- function(phy, node) {
  if (node <= Ntip(phy)) {
    return(node)
  }
  children <- phy$edge[phy$edge[, 1] == node, 2]
  unlist(lapply(children, descendant_tips, phy = phy), use.names = FALSE)
}

edge_descendants <- lapply(tree$edge[, 2], descendant_tips, phy = tree)
edge_values <- vapply(edge_descendants, function(idx) {
  mean(tip_data$mapped_value[idx], na.rm = TRUE)
}, numeric(1))

palette_fun <- colorRampPalette(c("#3132ff", "#00c8ff", "#00e676", "#fff000", "#ff6d00", "#e60000"))
pal <- palette_fun(256)

value_to_col <- function(x) {
  rng <- range(tip_data$mapped_value, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(pal[length(pal)], length(x)))
  idx <- floor((x - rng[1]) / diff(rng) * (length(pal) - 1)) + 1
  idx <- pmax(1, pmin(length(pal), idx))
  pal[idx]
}

edge_cols <- value_to_col(edge_values)
tip_cols <- value_to_col(tip_data$mapped_value)

if (scale_mode == "log1p") {
  legend_counts <- unique(c(0, 1, 5, 10, 25, 50, 100, max(tip_data$count, na.rm = TRUE)))
} else {
  legend_counts <- pretty(tip_data$count, n = 6)
}
legend_counts <- legend_counts[legend_counts >= min(tip_data$count) & legend_counts <= max(tip_data$count)]
if (!length(legend_counts)) legend_counts <- sort(unique(tip_data$count))
legend_values <- if (scale_mode == "log1p") log1p(legend_counts) else legend_counts

draw_plot <- function(file, device = c("png", "pdf")) {
  device <- match.arg(device)
  n_tip <- Ntip(tree)
  if (device == "png") {
    png(file, width = 3000, height = max(2200, 58 * n_tip), res = 220)
  } else {
    pdf(file, width = 12.5, height = max(8.5, 0.25 * n_tip))
  }
  on.exit(dev.off(), add = TRUE)

  max_depth <- max(node.depth.edgelength(tree))
  par(mar = c(5.5, 1, 4, 1))
  plot(
    tree,
    show.tip.label = TRUE,
    cex = 0.72,
    font = 3,
    edge.color = edge_cols,
    edge.width = 3,
    label.offset = max_depth * 0.02,
    no.margin = FALSE,
    x.lim = c(0, max_depth * 1.36)
  )

  last_plot <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  tip_y <- last_plot$yy[seq_len(Ntip(tree))]
  tip_x <- last_plot$xx[seq_len(Ntip(tree))]
  points(
    x = tip_x + max(node.depth.edgelength(tree)) * 0.012,
    y = tip_y,
    pch = 16,
    col = tip_cols,
    cex = 0.55
  )

  usr <- par("usr")

  if (mark_regions) {
    y_by_tip <- tibble(
      tip_index = seq_along(tree$tip.label),
      tip_label = tree$tip.label,
      y = tip_y,
      x = tip_x
    ) |>
      left_join(tip_data |> select(tip_index, annotation_group), by = "tip_index")

    region_styles <- tibble(
      annotation_group = c("Hypotrigonina", "Meliponina"),
      label = c("Hypotrigonina", "Meliponina"),
      color = c("#4d9221", "#c51b7d")
    )

    y_runs <- y_by_tip |>
      arrange(y) |>
      mutate(run_id = cumsum(annotation_group != lag(annotation_group, default = first(annotation_group)))) |>
      group_by(run_id, annotation_group) |>
      summarise(
        y0 = min(y) - 0.4,
        y1 = max(y) + 0.4,
        n_tips = n(),
        .groups = "drop"
      ) |>
      left_join(region_styles, by = "annotation_group")

    x_bracket <- max_depth * 1.17
    x_tick0 <- x_bracket - max_depth * 0.015

    for (i in seq_len(nrow(y_runs))) {
      run <- y_runs[i, ]
      segments(x_bracket, run$y0, x_bracket, run$y1, col = run$color, lwd = 4)
      segments(x_tick0, run$y0, x_bracket, run$y0, col = run$color, lwd = 4)
      segments(x_tick0, run$y1, x_bracket, run$y1, col = run$color, lwd = 4)
      text(
        x = x_bracket + max_depth * 0.02,
        y = mean(c(run$y0, run$y1)),
        labels = run$label,
        col = run$color,
        srt = 90,
        cex = 0.74,
        font = 2
      )
    }

    legend_x <- usr[1] + diff(usr[1:2]) * 0.36
    legend_y <- usr[3] + diff(usr[3:4]) * 0.075
    legend_step <- diff(usr[3:4]) * 0.03
    text(
      x = legend_x,
      y = legend_y + legend_step * 1.2,
      labels = "Subtribe",
      adj = 0,
      cex = 0.68,
      font = 2
    )
    for (i in seq_len(nrow(region_styles))) {
      y_leg <- legend_y - legend_step * (i - 1)
      segments(legend_x, y_leg, legend_x + diff(usr[1:2]) * 0.045, y_leg,
        col = region_styles$color[i], lwd = 4
      )
      text(
        x = legend_x + diff(usr[1:2]) * 0.052,
        y = y_leg,
        labels = region_styles$label[i],
        adj = 0,
        cex = 0.62
      )
    }
  }

  title(
    main = "Genetic and genomic studies by genus across the Meliponini phylogeny",
    sub = paste0(
      "Branches colored by the mean value of descendant genera; tips colored by genus-level counts; side brackets show subtribes",
      ifelse(scale_mode == "log1p", " (log1p color scale)", "")
    ),
    cex.main = 0.92,
    cex.sub = 0.62,
    line = 1.8
  )

  legend_x0 <- usr[1] + diff(usr[1:2]) * 0.02
  legend_x1 <- usr[1] + diff(usr[1:2]) * 0.30
  legend_y0 <- usr[3] + diff(usr[3:4]) * 0.045
  legend_y1 <- legend_y0 + diff(usr[3:4]) * 0.018
  x_seq <- seq(legend_x0, legend_x1, length.out = length(pal) + 1)
  rect(
    xleft = x_seq[-length(x_seq)],
    ybottom = legend_y0,
    xright = x_seq[-1],
    ytop = legend_y1,
    col = pal,
    border = NA
  )
  rect(legend_x0, legend_y0, legend_x1, legend_y1, border = "black", col = NA, lwd = 0.6)

  tick_x <- legend_x0 +
    (legend_values - min(tip_data$mapped_value, na.rm = TRUE)) /
      diff(range(tip_data$mapped_value, na.rm = TRUE)) *
      (legend_x1 - legend_x0)
  tick_x <- pmax(legend_x0, pmin(legend_x1, tick_x))
  segments(tick_x, legend_y0, tick_x, legend_y0 - diff(usr[3:4]) * 0.006, lwd = 0.6)
  text(tick_x, legend_y0 - diff(usr[3:4]) * 0.018, labels = legend_counts, cex = 0.56)
  text(
    x = (legend_x0 + legend_x1) / 2,
    y = legend_y1 + diff(usr[3:4]) * 0.018,
    labels = paste0("Number of articles", ifelse(scale_mode == "log1p", " (log1p)", "")),
    cex = 0.7,
    font = 2
  )
}

png_file <- file.path(output_dir, paste0(plot_prefix, ".png"))
pdf_file <- file.path(output_dir, paste0(plot_prefix, ".pdf"))
csv_file <- file.path(output_dir, paste0(plot_prefix, "_edge_data.csv"))

edge_data <- tibble(
  edge_id = seq_len(nrow(tree$edge)),
  parent = tree$edge[, 1],
  child = tree$edge[, 2],
  descendant_genera = vapply(edge_descendants, function(idx) {
    paste(tree$tip.label[idx], collapse = "; ")
  }, character(1)),
  edge_count_mean = vapply(edge_descendants, function(idx) {
    mean(tip_data$count[idx], na.rm = TRUE)
  }, numeric(1)),
  edge_mapped_value_mean = edge_values,
  edge_color = edge_cols,
  basal_compression = basal_compression
)

write.csv(edge_data, csv_file, row.names = FALSE)
draw_plot(png_file, "png")
draw_plot(pdf_file, "pdf")

message("Wrote branch heatmap PNG: ", png_file)
message("Wrote branch heatmap PDF: ", pdf_file)
message("Wrote edge data: ", csv_file)
