#!/usr/bin/env Rscript

# Compara registros deduplicados da Web of Science com a busca deduplicada da
# Scopus e calcula quantos artigos da WoS ja estavam presentes na Scopus.

required_packages <- character(0)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop("Instale os pacotes ausentes: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

parse_args <- function(args) {
  parsed <- list()
  i <- 1

  while (i <= length(args)) {
    arg <- args[[i]]

    if (grepl("^--[^=]+=", arg)) {
      key <- sub("^--([^=]+)=.*$", "\\1", arg)
      value <- sub("^--[^=]+=", "", arg)
      parsed[[key]] <- value
      i <- i + 1
    } else if (grepl("^--", arg)) {
      key <- sub("^", "", sub("^--", "", arg))
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
  if (is.null(lhs) || length(lhs) == 0 || identical(lhs, "")) {
    rhs
  } else {
    lhs
  }
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

if (isTRUE(args$help) || isTRUE(args$h)) {
  cat(
    paste0(
      "Uso:\n",
      "  Rscript scripts/compare_wos_scopus_overlap.R --wos=ARQUIVO --scopus=ARQUIVO [--output=ARQUIVO]\n\n",
      "Opcoes:\n",
      "  --wos=ARQUIVO       CSV deduplicado gerado por download_wos_meliponini.R.\n",
      "  --scopus=ARQUIVO    CSV deduplicado gerado por download_scopus_meliponini.R.\n",
      "  --output=ARQUIVO    CSV com a classificacao do pareamento. Opcional.\n"
    )
  )
  quit(status = 0)
}

wos_file <- args$wos
scopus_file <- args$scopus
output_file <- args$output

if (is.null(wos_file) || !file.exists(wos_file)) {
  stop("Informe um CSV WoS existente com --wos=ARQUIVO.", call. = FALSE)
}

if (is.null(scopus_file) || !file.exists(scopus_file)) {
  stop("Informe um CSV Scopus existente com --scopus=ARQUIVO.", call. = FALSE)
}

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

normalize_doi <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- tolower(trimws(x))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:\\s*", "", x)
  x
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit)) {
    NULL
  } else {
    hit
  }
}

wos <- utils::read.csv(wos_file, stringsAsFactors = FALSE, check.names = FALSE)
scopus <- utils::read.csv(scopus_file, stringsAsFactors = FALSE, check.names = FALSE)

wos_doi_col <- first_existing_col(wos, c("doi", "DI", "prism:doi"))
wos_title_col <- first_existing_col(wos, c("title", "TI", "dc:title"))
scopus_doi_col <- first_existing_col(scopus, c("prism:doi", "doi", "DI"))
scopus_title_col <- first_existing_col(scopus, c("dc:title", "title", "TI"))

if (is.null(wos_doi_col) && is.null(wos_title_col)) {
  stop("Nao encontrei DOI nem titulo no CSV WoS.", call. = FALSE)
}

if (is.null(scopus_doi_col) && is.null(scopus_title_col)) {
  stop("Nao encontrei DOI nem titulo no CSV Scopus.", call. = FALSE)
}

wos$doi_norm <- if (!is.null(wos_doi_col)) normalize_doi(wos[[wos_doi_col]]) else ""
wos$title_norm <- if (!is.null(wos_title_col)) normalize_text(wos[[wos_title_col]]) else ""
scopus$doi_norm <- if (!is.null(scopus_doi_col)) normalize_doi(scopus[[scopus_doi_col]]) else ""
scopus$title_norm <- if (!is.null(scopus_title_col)) normalize_text(scopus[[scopus_title_col]]) else ""

scopus_dois <- unique(scopus$doi_norm[nzchar(scopus$doi_norm)])
scopus_titles <- unique(scopus$title_norm[nzchar(scopus$title_norm)])

wos$duplicate_by_doi <- nzchar(wos$doi_norm) & wos$doi_norm %in% scopus_dois
wos$duplicate_by_title <- nzchar(wos$title_norm) & wos$title_norm %in% scopus_titles
wos$duplicate_in_scopus <- wos$duplicate_by_doi | wos$duplicate_by_title
wos$overlap_match_type <- ifelse(
  wos$duplicate_by_doi & wos$duplicate_by_title,
  "doi_and_title",
  ifelse(wos$duplicate_by_doi, "doi", ifelse(wos$duplicate_by_title, "title", "not_matched"))
)

summary <- data.frame(
  metric = c(
    "wos_total_deduplicated",
    "scopus_total_deduplicated",
    "wos_duplicates_in_scopus",
    "wos_unique_not_in_scopus",
    "wos_duplicates_by_doi",
    "wos_duplicates_by_title"
  ),
  value = c(
    nrow(wos),
    nrow(scopus),
    sum(wos$duplicate_in_scopus),
    sum(!wos$duplicate_in_scopus),
    sum(wos$duplicate_by_doi),
    sum(wos$duplicate_by_title)
  )
)

print(summary, row.names = FALSE)

if (!is.null(output_file) && nzchar(output_file)) {
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(wos, output_file, row.names = FALSE, fileEncoding = "UTF-8")
  message("Arquivo de pareamento salvo em: ", output_file)
}
