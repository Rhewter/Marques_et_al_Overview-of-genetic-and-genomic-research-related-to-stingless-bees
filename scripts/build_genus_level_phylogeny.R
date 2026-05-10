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

input_tree <- get_arg("input-tree", "data/phylo/meliponini_lepeco2024_opentree.tre")
counts_file <- get_arg("counts-file", "data/article_counts_ai/meliponini_ai_studied_taxa_counts.xlsx")
counts_sheet <- get_arg("counts-sheet", "genus_ai_counts")
output_dir <- get_arg("output-dir", "results/phylo_genus")
ultrametric <- get_arg("ultrametric", "grafen")
include_non_monophyletic <- tolower(get_arg("include-non-monophyletic", "false")) %in%
  c("true", "t", "1", "yes", "sim")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading tree: ", input_tree)
tree <- read.tree(input_tree)

clean_tip <- function(x) {
  x |>
    gsub("^'+|'+$", "", x = _) |>
    trimws()
}

tip_labels_clean <- clean_tip(tree$tip.label)
tip_genus <- sub("\\s+.*$", "", tip_labels_clean)

message("Reading genus counts: ", counts_file)
genus_counts <- read_excel(counts_file, sheet = counts_sheet) |>
  mutate(genus = as.character(genus))

valid_genera <- sort(unique(genus_counts$genus[!is.na(genus_counts$genus)]))

tip_map <- tibble(
  original_tip = tree$tip.label,
  clean_tip = tip_labels_clean,
  genus = tip_genus,
  in_reference_genus_list = genus %in% valid_genera
)

meliponini_tips <- tip_map |>
  filter(in_reference_genus_list)

if (nrow(meliponini_tips) == 0) {
  stop("No tree tips matched genera in the counts file.")
}

tree_meliponini <- keep.tip(tree, meliponini_tips$original_tip)
tip_map_meliponini <- tip_map |>
  filter(original_tip %in% tree_meliponini$tip.label)

genus_audit <- tip_map_meliponini |>
  group_by(genus) |>
  summarise(
    n_tree_tips = n(),
    tree_tips = paste(clean_tip, collapse = "; "),
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(
    is_monophyletic = if (n_tree_tips == 1) {
      TRUE
    } else {
      tips <- tip_map_meliponini$original_tip[tip_map_meliponini$genus == genus]
      isTRUE(is.monophyletic(tree_meliponini, tips))
    },
    included_in_genus_tree = is_monophyletic | include_non_monophyletic,
    exclusion_reason = ifelse(
      included_in_genus_tree,
      NA_character_,
      "excluded because genus is not monophyletic in the input tree"
    ),
    inclusion_mode = case_when(
      is_monophyletic ~ "single representative tip",
      include_non_monophyletic ~ "all original tips retained; starred label marks non-monophyly",
      TRUE ~ "excluded"
    )
  ) |>
  ungroup()

included_genera <- genus_audit |>
  filter(included_in_genus_tree) |>
  pull(genus)

descendant_tips <- function(phy, node) {
  if (node <= Ntip(phy)) {
    return(node)
  }
  children <- phy$edge[phy$edge[, 1] == node, 2]
  unlist(lapply(children, descendant_tips, phy = phy), use.names = FALSE)
}

genus_islands <- function(phy, tip_map, target_genus) {
  genus_by_tip <- tip_map$genus[match(phy$tip.label, tip_map$original_tip)]
  target_tip_idx <- which(genus_by_tip == target_genus)

  candidate_nodes <- c(target_tip_idx, seq.int(Ntip(phy) + 1, Ntip(phy) + phy$Nnode))
  candidate_sets <- lapply(candidate_nodes, function(node) {
    idx <- descendant_tips(phy, node)
    if (length(idx) > 0 && all(genus_by_tip[idx] == target_genus)) {
      sort(idx)
    } else {
      integer(0)
    }
  })
  candidate_sets <- candidate_sets[lengths(candidate_sets) > 0]

  is_subset_of_larger <- vapply(seq_along(candidate_sets), function(i) {
    any(vapply(seq_along(candidate_sets), function(j) {
      i != j &&
        length(candidate_sets[[j]]) > length(candidate_sets[[i]]) &&
        all(candidate_sets[[i]] %in% candidate_sets[[j]])
    }, logical(1)))
  }, logical(1))

  maximal_sets <- candidate_sets[!is_subset_of_larger]
  maximal_sets[order(vapply(maximal_sets, min, numeric(1)))]
}

monophyletic_representatives <- tip_map_meliponini |>
  filter(genus %in% included_genera, genus_audit$is_monophyletic[match(genus, genus_audit$genus)]) |>
  arrange(genus, clean_tip) |>
  group_by(genus) |>
  slice(1) |>
  ungroup() |>
  mutate(
    representative_mode = "single representative tip",
    display_label = genus
  )

non_monophyletic_genera <- genus_audit |>
  filter(include_non_monophyletic, included_in_genus_tree, !is_monophyletic) |>
  pull(genus)

non_monophyletic_representatives <- bind_rows(lapply(non_monophyletic_genera, function(target_genus) {
  islands <- genus_islands(tree_meliponini, tip_map_meliponini, target_genus)

  bind_rows(lapply(seq_along(islands), function(i) {
    island_tips <- tree_meliponini$tip.label[islands[[i]]]
    island_tip_map <- tip_map_meliponini |>
      filter(original_tip %in% island_tips) |>
      arrange(clean_tip)

    island_tip_map |>
      slice(1) |>
      mutate(
        representative_mode = "maximal same-genus island retained because genus is not monophyletic",
        display_label = paste0("*", genus),
        non_monophyletic_island_id = paste0(genus, "_", i),
        non_monophyletic_island_size = nrow(island_tip_map),
        non_monophyletic_island_tips = paste(island_tip_map$clean_tip, collapse = "; ")
      )
  }))
}))

if (nrow(monophyletic_representatives) > 0) {
  monophyletic_representatives <- monophyletic_representatives |>
    mutate(
      non_monophyletic_island_id = NA_character_,
      non_monophyletic_island_size = NA_integer_,
      non_monophyletic_island_tips = NA_character_
    )
}

representatives <- bind_rows(
  monophyletic_representatives,
  non_monophyletic_representatives
)

if (nrow(representatives) < 2) {
  stop("Fewer than two genera can be represented conservatively in the genus-level tree.")
}

genus_tree <- keep.tip(tree_meliponini, representatives$original_tip)

new_labels <- representatives$display_label[match(genus_tree$tip.label, representatives$original_tip)]
if (anyNA(new_labels)) {
  stop("Internal error while renaming representative tips.")
}
genus_tree$tip.label <- new_labels

if (ultrametric == "grafen") {
  genus_tree <- compute.brlen(genus_tree, method = "Grafen")
} else if (ultrametric == "chronos") {
  genus_tree <- chronos(genus_tree, quiet = TRUE)
} else if (ultrametric %in% c("none", "false", "FALSE", "no")) {
  # Keep original branch lengths from the representative-tip pruning.
} else {
  stop("--ultrametric must be one of: grafen, chronos, none")
}

genus_tree_file <- file.path(
  output_dir,
  if (include_non_monophyletic) {
    "meliponini_genus_level_with_polyphyletic.tre"
  } else {
    "meliponini_genus_level_conservative.tre"
  }
)
audit_file <- file.path(output_dir, "meliponini_genus_level_audit.csv")
metadata_file <- file.path(output_dir, "meliponini_genus_tree_tip_metadata.csv")

write.tree(genus_tree, file = genus_tree_file)

genus_audit <- genus_audit |>
  left_join(genus_counts, by = "genus")

representatives |>
  transmute(
    genus,
    display_label,
    representative_tip = clean_tip,
    original_tip_label = original_tip,
    representative_mode,
    non_monophyletic_island_id,
    non_monophyletic_island_size,
    non_monophyletic_island_tips
  ) |>
  left_join(genus_audit, by = "genus") |>
  write.csv(metadata_file, row.names = FALSE)

write.csv(genus_audit, audit_file, row.names = FALSE)

message("Wrote genus-level tree: ", genus_tree_file)
message("Wrote audit table: ", audit_file)
message("Wrote tip metadata: ", metadata_file)
message("Included genera: ", length(included_genera))
message("Included non-monophyletic genera: ", sum(!genus_audit$is_monophyletic & genus_audit$included_in_genus_tree))
message("Excluded non-monophyletic genera: ", sum(!genus_audit$included_in_genus_tree))
message("Ultrametric mode: ", ultrametric)
