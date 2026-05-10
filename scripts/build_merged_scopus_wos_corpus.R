#!/usr/bin/env Rscript

# Builds a merged bibliographic corpus with the curated Scopus records followed
# by WoS-exclusive records that were manually confirmed as relevant.

required_packages <- c("bibliometrix", "dplyr", "readr", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(bibliometrix)
  library(dplyr)
  library(readr)
  library(stringr)
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

norm_doi <- function(x) {
  x <- tolower(str_squish(safe_text(x)))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:\\s*", "", x)
  x
}

norm_title <- function(x) {
  x <- iconv(safe_text(x), to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  str_squish(x)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

scopus_bib <- args$scopus %||%
  "data/scopus_export_May_2-2026_a80702bd-6141-4355-8ab3-3ee911a5ead3_Meliponini_genetics_and_genomics.bib"
wos_yes_csv <- args$wos %||%
  "data/wos/processed/wos_unique_not_in_scopus_genomics_ai_gpt52_relevance_yes_20260503.csv"
wos_exclude_file <- args[["wos-exclude-file"]] %||%
  "data/wos_excluded_non_article_records_20260503.csv"
output <- args$output %||%
  "data/merged/merged_scopus_wos_genetics_genomics_corpus_20260503.csv"

if (!file.exists(scopus_bib)) stop("Scopus BibTeX not found: ", scopus_bib, call. = FALSE)
if (!file.exists(wos_yes_csv)) stop("WoS CSV not found: ", wos_yes_csv, call. = FALSE)

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)

message("Reading Scopus corpus: ", scopus_bib)
scopus_bib_df <- bibliometrix::convert2df(scopus_bib, dbsource = "scopus", format = "bibtex")
scopus <- scopus_bib_df |>
  transmute(
    source_database = "Scopus",
    source_record_id = if ("EID" %in% names(scopus_bib_df)) safe_text(EID) else "",
    title = str_squish(safe_text(if ("TI_raw" %in% names(scopus_bib_df)) TI_raw else TI)),
    abstract = str_squish(safe_text(if ("AB_raw" %in% names(scopus_bib_df)) AB_raw else if ("AB" %in% names(scopus_bib_df)) AB else "")),
    author_keywords = str_squish(safe_text(if ("DE_raw" %in% names(scopus_bib_df)) DE_raw else if ("DE" %in% names(scopus_bib_df)) DE else "")),
    index_keywords = str_squish(safe_text(if ("ID_raw" %in% names(scopus_bib_df)) ID_raw else if ("ID" %in% names(scopus_bib_df)) ID else "")),
    year = suppressWarnings(as.integer(PY)),
    journal = str_squish(safe_text(if ("SO" %in% names(scopus_bib_df)) SO else "")),
    doi = norm_doi(if ("DI" %in% names(scopus_bib_df)) DI else ""),
    original_row = row_number()
  )

message("Reading confirmed WoS-exclusive records: ", wos_yes_csv)
wos_confirmed <- readr::read_csv(wos_yes_csv, show_col_types = FALSE)
wos_excluded <- tibble::tibble()
if (file.exists(wos_exclude_file)) {
  message("Reading WoS non-article exclusion list: ", wos_exclude_file)
  wos_excluded <- readr::read_csv(wos_exclude_file, show_col_types = FALSE) |>
    mutate(uid = safe_text(uid))
  wos_confirmed <- wos_confirmed |>
    filter(!safe_text(uid) %in% wos_excluded$uid)
}

wos <- wos_confirmed |>
  transmute(
    source_database = "Web of Science",
    source_record_id = safe_text(uid),
    title = str_squish(safe_text(title)),
    abstract = str_squish(safe_text(abstract)),
    author_keywords = str_squish(safe_text(keywords)),
    index_keywords = str_squish(safe_text(keywords_plus)),
    year = suppressWarnings(as.integer(year)),
    journal = str_squish(safe_text(journal)),
    doi = norm_doi(doi),
    original_row = row_number()
  )

merged_raw <- bind_rows(scopus, wos) |>
  mutate(
    doi_norm = norm_doi(doi),
    title_norm = norm_title(title),
    dedup_key = case_when(
      nzchar(doi_norm) ~ paste0("doi:", doi_norm),
      nzchar(title_norm) ~ paste0("title:", title_norm),
      TRUE ~ paste0("row:", row_number())
    )
  )

duplicates_after_merge <- merged_raw |> filter(duplicated(dedup_key))
merged <- merged_raw |>
  filter(!duplicated(dedup_key)) |>
  mutate(
    article_id = row_number(),
    text = str_squish(paste(title, abstract, author_keywords, index_keywords, sep = ". "))
  ) |>
  select(
    article_id, source_database, source_record_id, title, abstract,
    author_keywords, index_keywords, year, journal, doi, text,
    original_row, doi_norm, title_norm, dedup_key
  )

readr::write_csv(merged, output)

dup_file <- sub("\\.csv$", "_duplicates_removed.csv", output)
readr::write_csv(duplicates_after_merge, dup_file)

summary_file <- sub("\\.csv$", "_summary.csv", output)
summary <- tibble::tibble(
  metric = c(
    "scopus_records",
    "wos_confirmed_relevant_records_before_non_article_exclusion",
    "wos_excluded_non_article_records",
    "wos_confirmed_relevant_article_records",
    "merged_records_before_final_dedup",
    "duplicates_removed_after_merge",
    "merged_records_final"
  ),
  value = c(
    nrow(scopus),
    nrow(wos_confirmed) + nrow(wos_excluded),
    nrow(wos_excluded),
    nrow(wos),
    nrow(merged_raw),
    nrow(duplicates_after_merge),
    nrow(merged)
  )
)
readr::write_csv(summary, summary_file)

print(summary)
message("Merged corpus: ", output)
message("Duplicates removed after merge: ", dup_file)
message("Summary: ", summary_file)
