#!/usr/bin/env Rscript

# Build article-level covariates derived from AI-studied genera:
# biogeographic region and subtribe.

required_packages <- c("dplyr", "jsonlite", "readr", "readxl", "stringr", "tibble")
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

safe_text <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  x
}

compact_unique <- function(x) {
  x <- sort(unique(safe_text(x)))
  x <- x[nzchar(x)]
  paste(x, collapse = "; ")
}

read_jsonl <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(tibble())
  jsonlite::stream_in(file(path), verbose = FALSE) |>
    tibble::as_tibble()
}

normalize_region <- function(x) {
  dplyr::case_when(
    str_detect(x, regex("Neotropical", ignore_case = TRUE)) ~ "Neotropical",
    str_detect(x, regex("Afrotropical", ignore_case = TRUE)) ~ "Afrotropical",
    str_detect(x, regex("Indomalaia|Papu|Austr", ignore_case = TRUE)) ~ "Indo-Australasian",
    TRUE ~ NA_character_
  )
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

corpus_file <- args[["corpus"]] %||%
  "data/merged/merged_scopus_wos_articles_only_genetics_genomics_corpus_20260503.csv"
ai_classifications_file <- args[["ai-classifications"]] %||%
  "data/article_counts_ai_merged_scopus_wos_articles_only/taxon_studied_ai_classifications.jsonl"
genus_workbook <- args[["genus-workbook"]] %||% "Meliponini_genus_list.xlsx"
output <- args[["output"]] %||%
  "data/merged/article_taxon_covariates_scopus_wos_articles_only_20260503.csv"

if (!file.exists(corpus_file)) stop("Corpus not found: ", corpus_file, call. = FALSE)
if (!file.exists(ai_classifications_file)) {
  stop("AI classifications not found: ", ai_classifications_file, call. = FALSE)
}
if (!file.exists(genus_workbook)) stop("Genus workbook not found: ", genus_workbook, call. = FALSE)

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

corpus <- readr::read_csv(corpus_file, show_col_types = FALSE) |>
  transmute(
    article_id = as.integer(article_id),
    title,
    year,
    doi
  )

genus_map <- readxl::read_excel(genus_workbook, guess_max = 10000) |>
  transmute(
    genus = as.character(`Gênero`),
    table1_region_raw = as.character(`Região/fauna principal`),
    biogeographic_region = normalize_region(table1_region_raw),
    subtribe = as.character(Subtribo)
  ) |>
  filter(!is.na(genus), nzchar(genus)) |>
  distinct(genus, .keep_all = TRUE)

ai_classifications <- read_jsonl(ai_classifications_file)

studied_genera <- ai_classifications |>
  filter(count_as_studied %in% TRUE) |>
  mutate(
    article_id = as.integer(article_id),
    genus = if_else(
      taxon_rank == "genus",
      as.character(taxon_name),
      stringr::word(as.character(taxon_name), 1)
    ),
    genus = str_squish(genus)
  ) |>
  filter(!is.na(article_id), !is.na(genus), nzchar(genus)) |>
  distinct(article_id, genus) |>
  left_join(genus_map, by = "genus")

article_covariates <- studied_genera |>
  group_by(article_id) |>
  summarise(
    studied_genera = compact_unique(genus),
    n_studied_genera = n_distinct(genus),
    regions_detected = compact_unique(na.omit(biogeographic_region)),
    n_regions_detected = n_distinct(na.omit(biogeographic_region)),
    subtribes_detected = compact_unique(na.omit(subtribe)),
    n_subtribes_detected = n_distinct(na.omit(subtribe)),
    unassigned_genera = compact_unique(genus[is.na(biogeographic_region) | is.na(subtribe)]),
    .groups = "drop"
  ) |>
  mutate(
    single_biogeographic_region = if_else(n_regions_detected == 1, regions_detected, NA_character_),
    article_region_assignment = case_when(
      n_regions_detected == 1 ~ regions_detected,
      n_regions_detected > 1 ~ "Multi-region article",
      TRUE ~ "No region assigned"
    ),
    region_for_stm = case_when(
      n_regions_detected == 1 ~ regions_detected,
      n_regions_detected > 1 ~ "Multi-region",
      TRUE ~ "Unassigned"
    ),
    single_subtribe = if_else(n_subtribes_detected == 1, subtribes_detected, NA_character_),
    article_subtribe_assignment = case_when(
      n_subtribes_detected == 1 ~ subtribes_detected,
      n_subtribes_detected > 1 ~ "Multi-subtribe article",
      TRUE ~ "No subtribe assigned"
    ),
    subtribe_for_stm = case_when(
      n_subtribes_detected == 1 ~ subtribes_detected,
      n_subtribes_detected > 1 ~ "Multi-subtribe",
      TRUE ~ "Unassigned"
    )
  )

out <- corpus |>
  left_join(article_covariates, by = "article_id") |>
  mutate(
    studied_genera = coalesce(studied_genera, ""),
    n_studied_genera = coalesce(n_studied_genera, 0L),
    regions_detected = coalesce(regions_detected, ""),
    n_regions_detected = coalesce(n_regions_detected, 0L),
    subtribes_detected = coalesce(subtribes_detected, ""),
    n_subtribes_detected = coalesce(n_subtribes_detected, 0L),
    unassigned_genera = coalesce(unassigned_genera, ""),
    article_region_assignment = coalesce(article_region_assignment, "No region assigned"),
    region_for_stm = coalesce(region_for_stm, "Unassigned"),
    article_subtribe_assignment = coalesce(article_subtribe_assignment, "No subtribe assigned"),
    subtribe_for_stm = coalesce(subtribe_for_stm, "Unassigned")
  )

readr::write_csv(out, output)
readr::write_csv(
  out |> count(region_for_stm, name = "n_documents") |> arrange(desc(n_documents)),
  sub("\\.csv$", "_region_summary.csv", output)
)
readr::write_csv(
  out |> count(subtribe_for_stm, name = "n_documents") |> arrange(desc(n_documents)),
  sub("\\.csv$", "_subtribe_summary.csv", output)
)

message("Wrote article covariates: ", output)
print(out |> count(region_for_stm, name = "n_documents") |> arrange(desc(n_documents)))
print(out |> count(subtribe_for_stm, name = "n_documents") |> arrange(desc(n_documents)))
