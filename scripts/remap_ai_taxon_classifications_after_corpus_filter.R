#!/usr/bin/env Rscript

# Remap AI taxon classifications after records are removed from a corpus.
# This avoids re-calling the API when the remaining documents are unchanged but
# article_id and candidate_id values have shifted.

required_packages <- c("dplyr", "jsonlite", "readr", "readxl", "stringr", "tibble", "tidyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
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

compact_unique <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(as.character(x))])
  paste(x, collapse = "; ")
}

read_jsonl <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(tibble())
  jsonlite::stream_in(file(path), verbose = FALSE) |>
    tibble::as_tibble()
}

write_jsonl <- function(rows, path) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  for (i in seq_len(nrow(rows))) {
    writeLines(jsonlite::toJSON(as.list(rows[i, ]), auto_unbox = TRUE, null = "null"), con)
  }
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

old_corpus <- args[["old-corpus"]] %||%
  "data/merged/merged_scopus_wos_genetics_genomics_corpus_20260503.csv"
new_corpus <- args[["new-corpus"]] %||%
  "data/merged/merged_scopus_wos_articles_only_genetics_genomics_corpus_20260503.csv"
candidate_workbook <- args[["candidate-workbook"]] %||%
  "data/article_counts_merged_scopus_wos_articles_only/meliponini_article_counts_by_genus.xlsx"
old_classifications_file <- args[["old-classifications"]] %||%
  "data/article_counts_ai_merged_scopus_wos/taxon_studied_ai_classifications.jsonl"
output_dir <- args[["output-dir"]] %||% "data/article_counts_ai_merged_scopus_wos_articles_only"

if (!file.exists(old_corpus)) stop("Old corpus not found: ", old_corpus, call. = FALSE)
if (!file.exists(new_corpus)) stop("New corpus not found: ", new_corpus, call. = FALSE)
if (!file.exists(candidate_workbook)) stop("Candidate workbook not found: ", candidate_workbook, call. = FALSE)
if (!file.exists(old_classifications_file)) {
  stop("Old classification checkpoint not found: ", old_classifications_file, call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

old_docs <- readr::read_csv(old_corpus, show_col_types = FALSE) |>
  transmute(old_article_id = as.integer(article_id), dedup_key)
new_docs <- readr::read_csv(new_corpus, show_col_types = FALSE) |>
  transmute(article_id = as.integer(article_id), dedup_key)

article_map <- new_docs |>
  left_join(old_docs, by = "dedup_key")

if (any(is.na(article_map$old_article_id))) {
  stop("Some new corpus records could not be mapped to the old corpus by dedup_key.", call. = FALSE)
}

species_matches <- readxl::read_excel(candidate_workbook, sheet = "species_article_matches") |>
  transmute(
    article_id = as.integer(article_id),
    taxon_rank = "species",
    taxon_name = scientific_name,
    genus = genus,
    candidate_source = compact_unique(match_type)
  ) |>
  distinct(article_id, taxon_rank, taxon_name, genus, .keep_all = TRUE)

genus_matches <- readxl::read_excel(candidate_workbook, sheet = "genus_name_article_matches") |>
  transmute(
    article_id = as.integer(article_id),
    taxon_rank = "genus",
    taxon_name = genus,
    genus = genus,
    candidate_source = "genus_name"
  ) |>
  distinct(article_id, taxon_rank, taxon_name, genus, .keep_all = TRUE)

new_candidates <- bind_rows(species_matches, genus_matches) |>
  distinct(article_id, taxon_rank, taxon_name, genus, .keep_all = TRUE) |>
  arrange(article_id, taxon_rank, taxon_name) |>
  mutate(candidate_id = row_number(), .before = 1) |>
  left_join(article_map |> select(article_id, old_article_id), by = "article_id")

old_classifications <- read_jsonl(old_classifications_file) |>
  transmute(
    old_article_id = as.integer(article_id),
    taxon_rank,
    taxon_name,
    study_relationship,
    count_as_studied = as.logical(count_as_studied),
    confidence,
    evidence,
    rationale
  ) |>
  distinct(old_article_id, taxon_rank, taxon_name, .keep_all = TRUE)

remapped <- new_candidates |>
  left_join(old_classifications, by = c("old_article_id", "taxon_rank", "taxon_name"))

missing <- remapped |>
  filter(is.na(study_relationship) | is.na(count_as_studied))

audit <- tibble(
  metric = c("new_candidates", "remapped_classifications", "missing_classifications"),
  value = c(nrow(new_candidates), nrow(remapped) - nrow(missing), nrow(missing))
)
readr::write_csv(audit, file.path(output_dir, "taxon_studied_ai_classification_remap_summary.csv"))

if (nrow(missing) > 0) {
  readr::write_csv(missing, file.path(output_dir, "taxon_studied_ai_classification_remap_missing.csv"))
  stop(
    "Missing remapped classifications: ", nrow(missing),
    ". See taxon_studied_ai_classification_remap_missing.csv",
    call. = FALSE
  )
}

output_rows <- remapped |>
  transmute(
    candidate_id = as.integer(candidate_id),
    article_id = as.integer(article_id),
    taxon_rank,
    taxon_name,
    study_relationship,
    count_as_studied,
    confidence,
    evidence,
    rationale
  )

write_jsonl(output_rows, file.path(output_dir, "taxon_studied_ai_classifications.jsonl"))

message("Remapped classifications written to: ", output_dir)
print(audit)
