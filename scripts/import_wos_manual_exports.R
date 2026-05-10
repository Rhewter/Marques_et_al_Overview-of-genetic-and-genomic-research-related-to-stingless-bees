#!/usr/bin/env Rscript

# Importa exportacoes manuais da Web of Science em BibTeX, combina os lotes,
# deduplica internamente e, opcionalmente, compara com a busca Scopus.

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
      "  Rscript scripts/import_wos_manual_exports.R [opcoes]\n\n",
      "Opcoes:\n",
      "  --input-dir=PASTA       Pasta com arquivos .bib da WoS. Padrao: data/wos\n",
      "  --pattern=REGEX         Padrao dos arquivos. Padrao: \\\\.bib$\n",
      "  --output-dir=PASTA      Pasta de saida. Padrao: data/wos/processed\n",
      "  --scopus=ARQUIVO        CSV deduplicado da Scopus para comparar duplicatas. Opcional.\n"
    )
  )
  quit(status = 0)
}

input_dir <- args[["input-dir"]] %||% "data/wos"
pattern <- args$pattern %||% "\\.bib$"
output_dir <- args[["output-dir"]] %||% "data/wos/processed"
scopus_file <- args$scopus

if (!dir.exists(input_dir)) {
  stop("Pasta de entrada nao encontrada: ", input_dir, call. = FALSE)
}

files <- list.files(input_dir, pattern = pattern, full.names = TRUE)
files <- files[grepl("\\.bib$", files, ignore.case = TRUE)]
files <- sort(files)

if (length(files) == 0) {
  stop("Nenhum arquivo .bib encontrado em: ", input_dir, call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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

clean_bib_value <- function(x) {
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- gsub("\\\\&", "&", x, fixed = TRUE)
  x <- gsub("\\\\%", "%", x, fixed = TRUE)
  x <- gsub("\\{\\[\\}", "[", x)
  x <- gsub("\\{\\]\\}", "]", x)
  x <- gsub("[{}]", "", x)
  x
}

parse_bib_entries <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^\ufeff", "", lines)
  start <- grep("^@", lines)

  if (length(start) == 0) {
    return(data.frame())
  }

  end <- c(start[-1] - 1, length(lines))
  entries <- vector("list", length(start))

  for (i in seq_along(start)) {
    entry_lines <- lines[start[i]:end[i]]
    header <- entry_lines[[1]]
    key <- sub("^@[^\\{]+\\{\\s*([^,]+),.*$", "\\1", header)
    entry_type <- sub("^@([^\\{]+)\\{.*$", "\\1", header)

    fields <- list(
      bibtex_key = trimws(key),
      entry_type = trimws(entry_type),
      source_file = basename(path)
    )

    current_field <- NULL
    current_value <- character(0)

    flush_field <- function() {
      if (!is.null(current_field)) {
        value <- paste(current_value, collapse = " ")
        value <- sub(",?\\s*$", "", value)
        value <- sub("^\\s*[\\{\\\"]", "", value)
        value <- sub("[\\}\\\"]\\s*,?\\s*$", "", value)
        fields[[current_field]] <<- clean_bib_value(value)
      }
    }

    for (line in entry_lines[-1]) {
      if (grepl("^\\s*}\\s*$", line)) {
        next
      }

      field_match <- regexec("^\\s*([^=]+?)\\s*=\\s*(.*)$", line)
      field_parts <- regmatches(line, field_match)[[1]]

      if (length(field_parts) > 0) {
        flush_field()
        current_field <- trimws(field_parts[[2]])
        current_value <- field_parts[[3]]
      } else if (!is.null(current_field)) {
        current_value <- c(current_value, line)
      }
    }

    flush_field()
    entries[[i]] <- as.data.frame(fields, stringsAsFactors = FALSE)
  }

  all_names <- unique(unlist(lapply(entries, names)))
  entries <- lapply(entries, function(x) {
    missing <- setdiff(all_names, names(x))
    for (m in missing) {
      x[[m]] <- NA_character_
    }
    x[, all_names, drop = FALSE]
  })

  do.call(rbind, entries)
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)][1]
  if (is.na(hit)) {
    NULL
  } else {
    hit
  }
}

deduplicate_records <- function(df) {
  key <- rep(NA_character_, nrow(df))

  uid <- trimws(as.character(df$uid))
  use_uid <- !is.na(uid) & nzchar(uid)
  key[use_uid] <- paste0("uid:", uid[use_uid])

  doi <- normalize_doi(df$doi)
  use_doi <- is.na(key) & !is.na(doi) & nzchar(doi)
  key[use_doi] <- paste0("doi:", doi[use_doi])

  title <- normalize_text(df$title)
  use_title <- is.na(key) & !is.na(title) & nzchar(title)
  key[use_title] <- paste0("title:", title[use_title])

  key[is.na(key)] <- paste0("row:", seq_len(nrow(df))[is.na(key)])
  df$dedup_key <- key
  df[!duplicated(key), , drop = FALSE]
}

bind_rows_fill <- function(dfs) {
  dfs <- dfs[vapply(dfs, nrow, integer(1)) > 0]
  if (length(dfs) == 0) {
    return(data.frame())
  }

  all_names <- unique(unlist(lapply(dfs, names)))
  dfs <- lapply(dfs, function(x) {
    missing <- setdiff(all_names, names(x))
    for (m in missing) {
      x[[m]] <- NA_character_
    }
    x[, all_names, drop = FALSE]
  })

  do.call(rbind, dfs)
}

message("Arquivos BibTeX WoS encontrados: ", length(files))
for (file in files) {
  message(" - ", file)
}

raw <- bind_rows_fill(lapply(files, parse_bib_entries))

col_or_na <- function(df, name) {
  if (name %in% names(df)) {
    df[[name]]
  } else {
    rep(NA_character_, nrow(df))
  }
}

uid_values <- col_or_na(raw, "Unique-ID")
missing_uid <- is.na(uid_values) | !nzchar(trimws(uid_values))
uid_values[missing_uid] <- col_or_na(raw, "bibtex_key")[missing_uid]

wos <- data.frame(
  source_file = col_or_na(raw, "source_file"),
  bibtex_key = col_or_na(raw, "bibtex_key"),
  uid = uid_values,
  doi = col_or_na(raw, "DOI"),
  title = col_or_na(raw, "Title"),
  authors = col_or_na(raw, "Author"),
  journal = col_or_na(raw, "Journal"),
  year = col_or_na(raw, "Year"),
  volume = col_or_na(raw, "Volume"),
  number = col_or_na(raw, "Number"),
  pages = col_or_na(raw, "Pages"),
  article_number = col_or_na(raw, "Article-Number"),
  abstract = col_or_na(raw, "Abstract"),
  keywords = col_or_na(raw, "Keywords"),
  keywords_plus = col_or_na(raw, "Keywords-Plus"),
  document_type = col_or_na(raw, "Document-Type"),
  times_cited = col_or_na(raw, "Times-Cited"),
  issn = col_or_na(raw, "ISSN"),
  eissn = col_or_na(raw, "EISSN"),
  stringsAsFactors = FALSE
)

wos$doi_norm <- normalize_doi(wos$doi)
wos$title_norm <- normalize_text(wos$title)
wos_dedup <- deduplicate_records(wos)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
raw_csv <- file.path(output_dir, paste0("wos_manual_bibtex_raw_", timestamp, ".csv"))
dedup_csv <- file.path(output_dir, paste0("wos_manual_bibtex_deduplicated_", timestamp, ".csv"))
summary_csv <- file.path(output_dir, paste0("wos_manual_bibtex_summary_", timestamp, ".csv"))

utils::write.csv(wos, raw_csv, row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(wos_dedup, dedup_csv, row.names = FALSE, fileEncoding = "UTF-8")

summary_rows <- data.frame(
  metric = c("wos_raw_exported_records", "wos_deduplicated_records", "wos_internal_duplicates"),
  value = c(nrow(wos), nrow(wos_dedup), nrow(wos) - nrow(wos_dedup))
)

overlap_csv <- NA_character_

if (!is.null(scopus_file) && nzchar(scopus_file)) {
  if (!file.exists(scopus_file)) {
    stop("Arquivo Scopus nao encontrado: ", scopus_file, call. = FALSE)
  }

  scopus <- utils::read.csv(scopus_file, stringsAsFactors = FALSE, check.names = FALSE)
  scopus_doi_col <- first_existing_col(scopus, c("prism:doi", "doi", "DI"))
  scopus_title_col <- first_existing_col(scopus, c("dc:title", "title", "TI"))

  scopus$doi_norm <- if (!is.null(scopus_doi_col)) normalize_doi(scopus[[scopus_doi_col]]) else ""
  scopus$title_norm <- if (!is.null(scopus_title_col)) normalize_text(scopus[[scopus_title_col]]) else ""

  scopus_dois <- unique(scopus$doi_norm[nzchar(scopus$doi_norm)])
  scopus_titles <- unique(scopus$title_norm[nzchar(scopus$title_norm)])

  wos_dedup$duplicate_by_doi <- nzchar(wos_dedup$doi_norm) & wos_dedup$doi_norm %in% scopus_dois
  wos_dedup$duplicate_by_title <- nzchar(wos_dedup$title_norm) & wos_dedup$title_norm %in% scopus_titles
  wos_dedup$duplicate_in_scopus <- wos_dedup$duplicate_by_doi | wos_dedup$duplicate_by_title
  wos_dedup$overlap_match_type <- ifelse(
    wos_dedup$duplicate_by_doi & wos_dedup$duplicate_by_title,
    "doi_and_title",
    ifelse(wos_dedup$duplicate_by_doi, "doi", ifelse(wos_dedup$duplicate_by_title, "title", "not_matched"))
  )

  overlap_csv <- file.path(output_dir, paste0("wos_scopus_overlap_", timestamp, ".csv"))
  utils::write.csv(wos_dedup, overlap_csv, row.names = FALSE, fileEncoding = "UTF-8")

  summary_rows <- rbind(
    summary_rows,
    data.frame(
      metric = c(
        "scopus_deduplicated_records",
        "wos_duplicates_in_scopus",
        "wos_unique_not_in_scopus",
        "wos_duplicates_by_doi",
        "wos_duplicates_by_title",
        "wos_duplicates_by_doi_and_title"
      ),
      value = c(
        nrow(scopus),
        sum(wos_dedup$duplicate_in_scopus),
        sum(!wos_dedup$duplicate_in_scopus),
        sum(wos_dedup$duplicate_by_doi),
        sum(wos_dedup$duplicate_by_title),
        sum(wos_dedup$duplicate_by_doi & wos_dedup$duplicate_by_title)
      )
    )
  )
}

utils::write.csv(summary_rows, summary_csv, row.names = FALSE, fileEncoding = "UTF-8")

print(summary_rows, row.names = FALSE)
message("CSV WoS bruto: ", raw_csv)
message("CSV WoS deduplicado: ", dedup_csv)
message("Resumo: ", summary_csv)
if (!is.na(overlap_csv)) {
  message("Comparacao WoS x Scopus: ", overlap_csv)
}
