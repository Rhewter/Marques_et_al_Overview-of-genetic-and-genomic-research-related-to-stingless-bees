#!/usr/bin/env Rscript

# Summarize topic prevalence from an STM fitted with a categorical prevalence
# covariate, using article-ready topic labels.

required_packages <- c("dplyr", "ggplot2", "readr", "stringr", "tibble", "tidyr")
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
  library(readr)
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

load_topic_labels <- function(stm_dir, topic_labels_file = NA_character_) {
  raw <- readr::read_csv(file.path(stm_dir, "stm_topic_labels.csv"), show_col_types = FALSE) |>
    mutate(topic = as.integer(topic))

  if (!is.na(topic_labels_file) && nzchar(topic_labels_file) && file.exists(topic_labels_file)) {
    curated <- readr::read_csv(topic_labels_file, show_col_types = FALSE) |>
      mutate(topic = as.integer(topic))
    required <- c("topic", "article_label", "short_label_article", "broader_theme", "rationale")
    missing <- setdiff(required, names(curated))
    if (length(missing) > 0) {
      stop("Curated topic label file is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    if (!setequal(raw$topic, curated$topic)) {
      stop("Curated topic label file does not cover the same topic IDs as the STM output.", call. = FALSE)
    }

    return(raw |>
      left_join(curated |> select(all_of(required)), by = "topic") |>
      mutate(
        compact_label = paste0("T", topic, ". ", short_label_article),
        plot_label = paste0("T", topic, ". ", article_label)
      ))
  }

  raw |>
    transmute(
      topic,
      article_label = short_label,
      short_label_article = short_label,
      broader_theme = "Unassigned",
      rationale = "Fallback label from STM FREX terms.",
      compact_label = paste0("T", topic, ". ", short_label),
      plot_label = paste0("T", topic, ". ", short_label)
    )
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

stm_dir <- args[["stm-dir"]] %||% "results/article_final_merged_scopus_wos_articles_only/stm_region_k10"
output_dir <- args[["output-dir"]] %||% "results/article_final_merged_scopus_wos_articles_only"
topic_labels_file <- args[["topic-labels-file"]] %||% NA_character_
covariate <- args[["covariate"]] %||% "region_for_stm"
prefix <- args[["prefix"]] %||% paste0("stm_", covariate, "_covariate")
covariate_title <- args[["covariate-title"]] %||% covariate
keep_levels <- args[["keep-levels"]]
if (!is.null(keep_levels) && nzchar(keep_levels)) {
  keep_levels <- trimws(strsplit(keep_levels, ",", fixed = TRUE)[[1]])
  keep_levels <- keep_levels[nzchar(keep_levels)]
} else {
  keep_levels <- character()
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

stm_fit <- readRDS(file.path(stm_dir, "stm_final_model.rds"))
model <- stm_fit$model
meta <- stm_fit$meta |> tibble::as_tibble()
n_topics <- stm_fit$best_k %||% ncol(model$theta)

if (!covariate %in% names(meta)) {
  stop("Covariate not found in STM metadata: ", covariate, call. = FALSE)
}

topic_labels <- load_topic_labels(stm_dir, topic_labels_file)

cov_values <- meta[[covariate]]
if (!is.factor(cov_values)) {
  cov_values <- factor(cov_values)
}
cov_values <- droplevels(cov_values)
if (length(keep_levels) > 0) {
  missing_levels <- setdiff(keep_levels, levels(cov_values))
  if (length(missing_levels) > 0) {
    stop("Requested --keep-levels absent from covariate: ", paste(missing_levels, collapse = ", "), call. = FALSE)
  }
}
selected_levels <- if (length(keep_levels) > 0) keep_levels else levels(cov_values)

theta <- as_tibble(model$theta, .name_repair = "minimal") |>
  setNames(paste0("topic_", seq_len(n_topics))) |>
  mutate(
    doc_index = row_number(),
    covariate_level = cov_values,
    .before = 1
  )

covariate_counts <- theta |>
  distinct(doc_index, covariate_level) |>
  filter(as.character(covariate_level) %in% selected_levels) |>
  mutate(covariate_level = factor(as.character(covariate_level), levels = selected_levels)) |>
  count(covariate_level, name = "n_documents") |>
  arrange(covariate_level)

topic_covariate <- theta |>
  filter(as.character(covariate_level) %in% selected_levels) |>
  mutate(covariate_level = factor(as.character(covariate_level), levels = selected_levels)) |>
  pivot_longer(starts_with("topic_"), names_to = "topic_id", values_to = "prevalence") |>
  mutate(topic = as.integer(sub("^topic_", "", topic_id))) |>
  group_by(covariate_level, topic) |>
  summarise(
    mean_prevalence = mean(prevalence),
    median_prevalence = median(prevalence),
    n_documents = n(),
    .groups = "drop"
  ) |>
  group_by(topic) |>
  mutate(
    topic_mean = mean(mean_prevalence),
    prevalence_lift = mean_prevalence - topic_mean
  ) |>
  ungroup() |>
  left_join(
    topic_labels |> select(topic, article_label, short_label_article, compact_label, plot_label, broader_theme),
    by = "topic"
  ) |>
  arrange(covariate_level, topic)

readr::write_csv(topic_covariate, file.path(output_dir, paste0(prefix, "_topic_prevalence.csv")))
readr::write_csv(covariate_counts, file.path(output_dir, paste0(prefix, "_document_counts.csv")))

top_by_level <- topic_covariate |>
  group_by(covariate_level) |>
  arrange(desc(mean_prevalence), .by_group = TRUE) |>
  slice_head(n = 5) |>
  ungroup()
readr::write_csv(top_by_level, file.path(output_dir, paste0(prefix, "_top_topics_by_level.csv")))

level_labels <- covariate_counts |>
  mutate(
    covariate_label = paste0(as.character(covariate_level), "\n(n = ", n_documents, ")")
  )

p_heatmap <- topic_covariate |>
  left_join(level_labels, by = "covariate_level") |>
  mutate(
    covariate_label = factor(covariate_label, levels = level_labels$covariate_label),
    topic_label = factor(compact_label, levels = rev(topic_labels$compact_label))
  ) |>
  ggplot(aes(x = covariate_label, y = topic_label, fill = mean_prevalence)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(option = "C", labels = percent_labels) +
  labs(
    title = paste("Mean topic prevalence by", covariate_title),
    subtitle = "Topics were estimated in an STM fitted with this prevalence covariate",
    x = covariate_title,
    y = "Topic",
    fill = "Mean prevalence"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(size = 9.5),
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

width <- max(8.5, 1.45 * nrow(level_labels) + 4)
height <- max(7.5, 0.55 * n_topics + 3)
ggsave(file.path(output_dir, paste0(prefix, "_topic_prevalence_heatmap.png")), p_heatmap, width = width, height = height, dpi = 300)
ggsave(file.path(output_dir, paste0(prefix, "_topic_prevalence_heatmap.pdf")), p_heatmap, width = width, height = height)

message("Wrote covariate STM summaries to: ", output_dir)
message("Covariate: ", covariate, " | levels: ", paste(levels(cov_values), collapse = ", "))
