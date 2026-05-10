#!/usr/bin/env Rscript

# Prepare article-ready outputs from the final time-covariate STM.
# The STM topics are estimated once with s(year). Biogeographic region and
# top-genus summaries are then analyzed as document-level covariates using the
# same topic proportions, so topic identities remain unchanged.

required_packages <- c(
  "dplyr",
  "ggplot2",
  "jsonlite",
  "readr",
  "readxl",
  "stringr",
  "tibble",
  "tidyr"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Install missing packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(jsonlite)
  library(readr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(tidyr)
})

parse_args <- function(args) {
  parsed <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (grepl("^--[^=]+=", arg)) {
      key <- sub("^--([^=]+)=.*$", "\\1", arg)
      parsed[[key]] <- sub("^--[^=]+=", "", arg)
      i <- i + 1
    } else if (grepl("^--", arg)) {
      key <- sub("^--", "", arg)
      if (i == length(args) || grepl("^--", args[[i + 1]])) {
        parsed[[key]] <- TRUE
        i <- i + 1
      } else {
        parsed[[key]] <- args[[i + 1]]
        i <- i + 2
      }
    } else {
      i <- i + 1
    }
  }
  parsed
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0 || identical(lhs, "")) rhs else lhs
}

percent_labels <- function(x) paste0(round(x * 100), "%")

normalize_metric <- function(x, higher_is_better = TRUE) {
  x <- as.numeric(x)
  if (!higher_is_better) {
    x <- -x
  }
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  if (isTRUE(all.equal(rng[[1]], rng[[2]]))) {
    return(rep(0.5, length(x)))
  }
  (x - rng[[1]]) / diff(rng)
}

normalize_region <- function(x) {
  dplyr::case_when(
    str_detect(x, regex("Neotropical", ignore_case = TRUE)) ~ "Neotropical",
    str_detect(x, regex("Afrotropical", ignore_case = TRUE)) ~ "Afrotropical",
    str_detect(x, regex("Indomalaia|Papu|Austr", ignore_case = TRUE)) ~ "Indo-Australasian",
    TRUE ~ NA_character_
  )
}

safe_topic_labels <- function(topic_labels) {
  n_topics <- nrow(topic_labels)
  if (n_topics == 15) {
    article_labels <- tibble::tribble(
      ~topic, ~article_label, ~short_label_article, ~broader_theme, ~rationale,
      1, "Nuclear marker development and genotyping", "Nuclear markers/genotyping", "Marker-based diversity and phylogeography", "Microsatellite, RAPD, ISSR, primer development, polymorphism, loci and marker-transferability terms define a marker-development/genotyping tradition.",
      2, "Mitochondrial/ribosomal marker assays and PCR-RFLP", "mt/rDNA marker assays", "Marker-based diversity and phylogeography", "Restriction, ITS, PCR, mtDNA, rDNA, NUMTs and spacer terms define a Sanger-era marker-assay tradition.",
      3, "Cytogenetics and cytogenomics", "Cytogenetics/cytogenomics", "Genome and cytogenome architecture", "Chromosome, karyotype, heterochromatin, fluorochrome staining, FISH, B chromosomes and repetitive DNA terms define this topic.",
      4, "Population structure, gene flow and phylogeography", "Population structure/phylogeography", "Marker-based diversity and phylogeography", "Population structure, gene flow, differentiation, diversity, fragmentation, dispersal and phylogeographic terms define the population-level application layer.",
      5, "Allozyme-era biochemical population markers", "Allozyme markers", "Marker-based diversity and phylogeography", "Isoenzymes, electrophoresis, dehydrogenase, hexokinase and enzyme-variant terms define an earlier biochemical-marker tradition.",
      6, "Gene expression, sex determination and caste differentiation", "Gene expression/caste", "Functional and developmental genomics", "Expression, transcriptome, sex, queens, workers, juvenile hormone, reproductive caste and differentially expressed genes define this topic.",
      7, "Mitogenomics and comparative genomics", "Mitogenomics/comparative genomics", "Genome and cytogenome architecture", "Complete mitogenomes, mitochondrial genome organization, rearrangements, heteroplasmy, PCGs and tRNAs define this topic.",
      8, "Microbiome, lactic acid bacteria and probiotics", "Microbiome/probiotics", "Microbiome and disease ecology", "Bacteria, gut, microbiota, lactic acid bacteria, Lactobacillus, LAB, probiotic, honey and bee-bread terms define this topic.",
      9, "Cytology, Malpighian tubules and NUMTs", "Cytology/NUMTs", "Cell biology and molecular systematics", "Malpighian tubules, larval instars, chromatin, cytology and NUMT terms define a specific mixed cell-biology/systematics topic.",
      10, "Viral pathogens and disease ecology", "Viral pathogens", "Microbiome and disease ecology", "Virus, viral disease, DWV, BQCV, pathogen spillover, managed/wild colonies and disease ecology terms define this topic.",
      11, "DNA barcoding, COI and phylogeography", "DNA barcoding/COI", "Marker-based diversity and phylogeography", "Barcoding, COI, morphology, phylogeography, phylogenetic relationships and taxonomic-identification terms define this topic.",
      12, "Reproductive biology, mating and kin structure", "Reproduction/mating", "Reproductive biology and kin structure", "Aggregations, mating, reproductive males, drones, relatedness and dispersal terms define this topic.",
      13, "Environmental exposure, detoxification and fungal symbionts", "Exposure/detoxification", "Environmental stress and symbiosis", "Exposure, insecticide, pesticide, detoxification, yeast and fungal symbiont terms define this topic.",
      14, "Genome size, cytometry and genome assembly", "Genome size/assembly", "Genome and cytogenome architecture", "Genome size, cytometry, assembly, genomics and transposable-element terms define this topic.",
      15, "Morphometrics and morphological differentiation", "Morphometrics", "Morphometrics and phenotypic differentiation", "Geometric morphometrics, morphometry, wing, subspecies and differentiation terms define this topic."
    )
  } else {
    article_labels <- topic_labels |>
      transmute(
        topic,
        article_label = short_label,
        short_label_article = short_label,
        broader_theme = "Unassigned",
        rationale = "Fallback label from STM FREX terms because no curated article labels are available for this K."
      )
  }

  topic_labels |>
    left_join(article_labels, by = "topic") |>
    mutate(
      compact_label = paste0("T", topic, ". ", short_label_article),
      plot_label = paste0("T", topic, ". ", article_label)
    )
}

read_jsonl <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(tibble())
  jsonlite::stream_in(file(path), verbose = FALSE) |>
    tibble::as_tibble()
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

stm_dir <- args[["stm-dir"]] %||% "results/article_final/stm_time_searchk"
output_dir <- args[["output-dir"]] %||% "results/article_final"
topic_labels_file <- args[["topic-labels-file"]] %||% NA_character_
genus_workbook <- args[["genus-workbook"]] %||% "Meliponini_genus_list.xlsx"
ai_classifications_file <- args[["ai-classifications"]] %||%
  "data/article_counts_ai/taxon_studied_ai_classifications.jsonl"
ai_genus_counts_file <- args[["ai-genus-counts"]] %||%
  "data/article_counts_ai/meliponini_ai_studied_genus_counts.csv"
frontier_window <- as.integer(args[["frontier-window"]] %||% 5L)
top_n_genera <- as.integer(args[["top-n-genera"]] %||% 10L)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

stm_fit <- readRDS(file.path(stm_dir, "stm_final_model.rds"))
model <- stm_fit$model
meta <- stm_fit$meta |> tibble::as_tibble()
n_topics <- stm_fit$best_k %||% ncol(model$theta)

topic_labels_raw <- readr::read_csv(file.path(stm_dir, "stm_topic_labels.csv"), show_col_types = FALSE)
if (!is.na(topic_labels_file) && nzchar(topic_labels_file)) {
  curated_topic_labels <- readr::read_csv(topic_labels_file, show_col_types = FALSE) |>
    mutate(topic = as.integer(topic))

  required_label_columns <- c("topic", "article_label", "short_label_article", "broader_theme", "rationale")
  missing_label_columns <- setdiff(required_label_columns, names(curated_topic_labels))
  if (length(missing_label_columns) > 0) {
    stop(
      "Curated topic label file is missing columns: ",
      paste(missing_label_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (!setequal(curated_topic_labels$topic, topic_labels_raw$topic)) {
    stop("Curated topic label file does not cover the same topic IDs as the STM output.", call. = FALSE)
  }

  topic_labels <- topic_labels_raw |>
    left_join(
      curated_topic_labels |>
        select(topic, article_label, short_label_article, broader_theme, rationale),
      by = "topic"
    ) |>
    mutate(
      compact_label = paste0("T", topic, ". ", short_label_article),
      plot_label = paste0("T", topic, ". ", article_label)
    )
} else {
  topic_labels <- safe_topic_labels(topic_labels_raw)
}
readr::write_csv(topic_labels, file.path(output_dir, "stm_topic_labels_article_named.csv"))

searchk_file <- file.path(stm_dir, "stm_searchk_results.csv")
if (file.exists(searchk_file)) {
  searchk_results <- readr::read_csv(searchk_file, show_col_types = FALSE) |>
    mutate(
      semantic_coherence = semcoh,
      exclusivity = exclus,
      heldout_likelihood = heldout,
      residual_dispersion = residual,
      lower_bound = bound,
      composite_score = composite_score
    )

  searchk_article <- searchk_results |>
    select(
      K,
      heldout_likelihood,
      residual_dispersion,
      semantic_coherence,
      exclusivity,
      lower_bound,
      composite_score
    )
  readr::write_csv(searchk_article, file.path(output_dir, "stm_searchk_results_article_named.csv"))

  best_k <- if ("selected_for_final_model" %in% names(searchk_results) &&
    any(searchk_results$selected_for_final_model %in% TRUE)) {
    searchk_results$K[which(searchk_results$selected_for_final_model %in% TRUE)[[1]]]
  } else {
    searchk_results$K[which.max(searchk_results$composite_score)]
  }
  composite_best_k <- searchk_results$K[which.max(searchk_results$composite_score)]
  searchk_long <- searchk_article |>
    pivot_longer(-K, names_to = "metric", values_to = "value") |>
    mutate(
      metric = recode(
        metric,
        heldout_likelihood = "Held-out likelihood",
        residual_dispersion = "Residual dispersion",
        semantic_coherence = "Semantic coherence",
        exclusivity = "Exclusivity",
        lower_bound = "Lower bound",
        composite_score = "Composite score"
      )
    )

  p_searchk <- ggplot(searchk_long, aes(x = K, y = value)) +
    geom_line(linewidth = 0.45, color = "#2f4858") +
    geom_point(size = 1.9, color = "#2f4858") +
    geom_vline(xintercept = best_k, linetype = "dashed", color = "#b23a48") +
    facet_wrap(~ metric, scales = "free_y") +
    labs(
      title = "Diagnostics for selecting the number of topics",
      subtitle = if (best_k == composite_best_k) {
        paste("K selected by the composite score:", best_k)
      } else {
        paste("Final K:", best_k, "| best composite-score K:", composite_best_k)
      },
      x = "Number of topics (K)",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggsave(file.path(output_dir, "stm_searchk_diagnostics_article_labels.png"), p_searchk, width = 10, height = 7, dpi = 300)
  ggsave(file.path(output_dir, "stm_searchk_diagnostics_article_labels.pdf"), p_searchk, width = 10, height = 7)
}

theta <- as_tibble(model$theta, .name_repair = "minimal") |>
  setNames(paste0("topic_", seq_len(n_topics))) |>
  mutate(doc_index = row_number(), .before = 1) |>
  bind_cols(meta |> select(doc_id, title, year, journal, doi))

topic_long <- theta |>
  pivot_longer(starts_with("topic_"), names_to = "topic_id", values_to = "prevalence") |>
  mutate(topic = as.integer(sub("^topic_", "", topic_id))) |>
  left_join(topic_labels |> select(topic, article_label, short_label_article, compact_label, plot_label, broader_theme), by = "topic")

topic_year <- topic_long |>
  group_by(year, topic, article_label, short_label_article, compact_label, plot_label, broader_theme) |>
  summarise(mean_prevalence = mean(prevalence), n_documents = n(), .groups = "drop") |>
  complete(
    year = seq(min(meta$year), max(meta$year)),
    topic = seq_len(n_topics),
    fill = list(mean_prevalence = 0, n_documents = 0)
  ) |>
  left_join(topic_labels |> select(topic, article_label, short_label_article, compact_label, plot_label, broader_theme), by = "topic") |>
  mutate(
    article_label = coalesce(article_label.x, article_label.y),
    short_label_article = coalesce(short_label_article.x, short_label_article.y),
    compact_label = coalesce(compact_label.x, compact_label.y),
    plot_label = coalesce(plot_label.x, plot_label.y),
    broader_theme = coalesce(broader_theme.x, broader_theme.y)
  ) |>
  select(year, topic, article_label, short_label_article, compact_label, plot_label, broader_theme, mean_prevalence, n_documents) |>
  arrange(topic, year) |>
  group_by(topic) |>
  mutate(
    prevalence_smooth = as.numeric(stats::filter(mean_prevalence, rep(1 / 3, 3), sides = 2)),
    prevalence_smooth = if_else(is.na(prevalence_smooth), mean_prevalence, prevalence_smooth)
  ) |>
  ungroup()

readr::write_csv(topic_year, file.path(output_dir, "stm_topic_year_prevalence_article_named.csv"))

recent_start <- max(meta$year) - frontier_window + 1L
frontier_topics <- topic_year |>
  filter(year >= recent_start) |>
  group_by(topic, article_label, short_label_article, compact_label, broader_theme) |>
  summarise(
    recent_mean_prevalence = mean(mean_prevalence),
    recent_slope = if (n_distinct(year) >= 2) coef(lm(mean_prevalence ~ year))[[2]] else NA_real_,
    current_prevalence = mean_prevalence[which.max(year)],
    current_year = max(year),
    .groups = "drop"
  ) |>
  arrange(desc(recent_slope), desc(current_prevalence)) |>
  mutate(frontier_rank = row_number(), .before = 1)
readr::write_csv(frontier_topics, file.path(output_dir, "stm_frontier_topics_article_named.csv"))

top_frontier_candidates <- frontier_topics |>
  filter(!is.na(recent_slope), recent_slope > 0)
top_frontier <- top_frontier_candidates |>
  slice_head(n = min(6, nrow(top_frontier_candidates)))

p_time <- topic_year |>
  mutate(
    topic_label = factor(str_wrap(plot_label, 42), levels = str_wrap(topic_labels$plot_label, 42)),
    frontier = topic %in% top_frontier$topic
  ) |>
  ggplot(aes(x = year, y = prevalence_smooth, group = topic)) +
  geom_line(aes(color = frontier), linewidth = 0.85, alpha = 0.9, show.legend = FALSE) +
  scale_color_manual(values = c(`FALSE` = "grey72", `TRUE` = "#0072B2")) +
  facet_wrap(~ topic_label, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = percent_labels) +
  labs(
    title = "Estimated topic prevalence over time",
    subtitle = "Highlighted lines indicate topics with the strongest positive recent slope",
    x = "Publication year",
    y = "Mean topic prevalence"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 9.5),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(output_dir, "stm_topics_over_time_article_labels.png"), p_time, width = 14, height = 10, dpi = 300)
ggsave(file.path(output_dir, "stm_topics_over_time_article_labels.pdf"), p_time, width = 14, height = 10)

p_frontier <- topic_year |>
  semi_join(top_frontier, by = "topic") |>
  mutate(topic_label = factor(str_wrap(compact_label, 28), levels = str_wrap(top_frontier$compact_label, 28))) |>
  ggplot(aes(x = year, y = prevalence_smooth, color = topic_label)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(labels = percent_labels) +
  labs(
    title = "Topics with the strongest positive recent slope",
    x = "Publication year",
    y = "Mean topic prevalence",
    color = "Topic"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 1))

ggsave(file.path(output_dir, "stm_frontier_topics_article_labels.png"), p_frontier, width = 11, height = 6.5, dpi = 300)
ggsave(file.path(output_dir, "stm_frontier_topics_article_labels.pdf"), p_frontier, width = 11, height = 6.5)

genus_map <- readxl::read_excel(genus_workbook, guess_max = 10000) |>
  transmute(
    genus = as.character(`Gênero`),
    table1_region_raw = as.character(`Região/fauna principal`),
    biogeographic_region = normalize_region(table1_region_raw),
    subtribe = as.character(Subtribo)
  ) |>
  filter(!is.na(genus), nzchar(genus)) |>
  distinct(genus, .keep_all = TRUE)

readr::write_csv(genus_map, file.path(output_dir, "meliponini_genus_biogeography_lepeco2024.csv"))

ai_classifications <- read_jsonl(ai_classifications_file)
studied_genera <- ai_classifications |>
  filter(isTRUE(count_as_studied) | count_as_studied == TRUE) |>
  mutate(
    genus = if_else(
      taxon_rank == "genus",
      as.character(taxon_name),
      word(as.character(taxon_name), 1)
    ),
    genus = str_squish(genus)
  ) |>
  filter(!is.na(article_id), !is.na(genus), nzchar(genus)) |>
  distinct(article_id = as.integer(article_id), genus) |>
  left_join(genus_map, by = "genus")

article_region <- studied_genera |>
  group_by(article_id) |>
  summarise(
    studied_genera = paste(sort(unique(genus)), collapse = "; "),
    n_studied_genera = n_distinct(genus),
    regions_detected = paste(sort(unique(na.omit(biogeographic_region))), collapse = "; "),
    n_regions_detected = n_distinct(na.omit(biogeographic_region)),
    unassigned_genera = paste(sort(unique(genus[is.na(biogeographic_region)])), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(
    single_biogeographic_region = case_when(
      n_regions_detected == 1 ~ regions_detected,
      TRUE ~ NA_character_
    ),
    article_region_assignment = case_when(
      n_regions_detected == 1 ~ regions_detected,
      n_regions_detected > 1 ~ "Multi-region article",
      TRUE ~ "No region assigned"
    )
  )

all_article_region <- theta |>
  select(doc_id, title, year, doi) |>
  left_join(article_region, by = c("doc_id" = "article_id")) |>
  mutate(
    studied_genera = coalesce(studied_genera, ""),
    n_studied_genera = coalesce(n_studied_genera, 0L),
    regions_detected = coalesce(regions_detected, ""),
    n_regions_detected = coalesce(n_regions_detected, 0L),
    unassigned_genera = coalesce(unassigned_genera, ""),
    article_region_assignment = coalesce(article_region_assignment, "No region assigned")
  )
readr::write_csv(all_article_region, file.path(output_dir, "article_biogeographic_region_assignments.csv"))

region_summary <- all_article_region |>
  count(article_region_assignment, name = "n_documents") |>
  arrange(desc(n_documents))
readr::write_csv(region_summary, file.path(output_dir, "article_biogeographic_region_summary.csv"))
readr::write_csv(region_summary, file.path(output_dir, "article_region_assignment_summary.csv"))

region_order <- c("Neotropical", "Indo-Australasian", "Afrotropical")
topic_region <- topic_long |>
  left_join(all_article_region |> select(doc_id, single_biogeographic_region), by = "doc_id") |>
  filter(single_biogeographic_region %in% region_order) |>
  group_by(single_biogeographic_region, topic, article_label, short_label_article, compact_label, broader_theme) |>
  summarise(
    mean_prevalence = mean(prevalence),
    n_documents = n_distinct(doc_id),
    .groups = "drop"
  ) |>
  group_by(topic) |>
  mutate(topic_mean = mean(mean_prevalence), prevalence_lift = mean_prevalence - topic_mean) |>
  ungroup()
readr::write_csv(topic_region, file.path(output_dir, "stm_topic_biogeographic_region_prevalence.csv"))

region_labels <- topic_region |>
  distinct(single_biogeographic_region, n_documents) |>
  group_by(single_biogeographic_region) |>
  summarise(n_documents = max(n_documents), .groups = "drop") |>
  mutate(region_label = paste0(single_biogeographic_region, "\n(n = ", n_documents, ")"))

p_region <- topic_region |>
  left_join(region_labels, by = "single_biogeographic_region") |>
  mutate(
    region_label = factor(region_label, levels = region_labels$region_label[match(region_order, region_labels$single_biogeographic_region)]),
    topic_label = factor(compact_label, levels = rev(topic_labels$compact_label))
  ) |>
  ggplot(aes(x = region_label, y = topic_label, fill = mean_prevalence)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(option = "C", labels = percent_labels) +
  labs(
    title = "Mean topic prevalence by biogeographic region",
    subtitle = "Regions follow the distribution categories in Lepeco et al. (2024), Table 1",
    x = "Biogeographic region",
    y = "Topic",
    fill = "Mean prevalence"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(file.path(output_dir, "stm_topics_by_biogeographic_region_article_labels.png"), p_region, width = 9.5, height = 8.5, dpi = 300)
ggsave(file.path(output_dir, "stm_topics_by_biogeographic_region_article_labels.pdf"), p_region, width = 9.5, height = 8.5)

ai_genus_counts <- readr::read_csv(ai_genus_counts_file, show_col_types = FALSE)
top_genera <- ai_genus_counts |>
  arrange(desc(n_articles_ai_studied_genus_combined), genus) |>
  slice_head(n = top_n_genera) |>
  select(genus, n_articles_ai_studied_genus_combined)
readr::write_csv(top_genera, file.path(output_dir, "top10_ai_studied_genera.csv"))

top_genus_articles <- studied_genera |>
  semi_join(top_genera, by = "genus") |>
  distinct(genus, doc_id = article_id)

topic_top_genus <- top_genus_articles |>
  left_join(topic_long, by = "doc_id", relationship = "many-to-many") |>
  group_by(genus, topic, article_label, short_label_article, compact_label, broader_theme) |>
  summarise(
    mean_prevalence = mean(prevalence, na.rm = TRUE),
    n_articles = n_distinct(doc_id),
    .groups = "drop"
  ) |>
  left_join(top_genera, by = "genus") |>
  arrange(desc(n_articles_ai_studied_genus_combined), genus, topic)
readr::write_csv(topic_top_genus, file.path(output_dir, "stm_topic_top10_ai_genus_prevalence.csv"))

genus_labels <- top_genera |>
  mutate(genus_label = paste0(genus, "\n(n = ", n_articles_ai_studied_genus_combined, ")"))

p_top_genus <- topic_top_genus |>
  left_join(genus_labels, by = c("genus", "n_articles_ai_studied_genus_combined")) |>
  mutate(
    genus_label = factor(genus_label, levels = genus_labels$genus_label),
    topic_label = factor(compact_label, levels = rev(topic_labels$compact_label))
  ) |>
  ggplot(aes(x = genus_label, y = topic_label, fill = mean_prevalence)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(option = "C", labels = percent_labels) +
  labs(
    title = "Mean topic prevalence for the ten most studied genera",
    subtitle = "Genera ranked by AI-assisted counts of articles that actually studied each taxon",
    x = "Genus",
    y = "Topic",
    fill = "Mean prevalence"
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1),
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(file.path(output_dir, "stm_topics_by_top10_ai_genera_article_labels.png"), p_top_genus, width = 12.5, height = 8.5, dpi = 300)
ggsave(file.path(output_dir, "stm_topics_by_top10_ai_genera_article_labels.pdf"), p_top_genus, width = 12.5, height = 8.5)

p_top_counts <- top_genera |>
  mutate(genus = factor(genus, levels = rev(genus))) |>
  ggplot(aes(x = n_articles_ai_studied_genus_combined, y = genus)) +
  geom_col(fill = "#0072B2", width = 0.72) +
  labs(
    title = "Ten most studied stingless bee genera",
    subtitle = "AI-assisted count of articles that studied each genus or species within it",
    x = "Number of articles",
    y = "Genus"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(output_dir, "top10_ai_studied_genera_counts.png"), p_top_counts, width = 7.5, height = 5.5, dpi = 300)
ggsave(file.path(output_dir, "top10_ai_studied_genera_counts.pdf"), p_top_counts, width = 7.5, height = 5.5)

message("Wrote final article outputs to: ", output_dir)
message("Best K from STM directory: ", n_topics)
