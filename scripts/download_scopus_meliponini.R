#!/usr/bin/env Rscript

# Retrieves all Scopus records mentioning at least one genus
# listed in Meliponini_genus_list.xlsx using the rscopus package.

required_packages <- c("readxl", "rscopus")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Instale os pacotes ausentes antes de rodar o script: ",
    paste(missing_packages, collapse = ", "),
    "\nExemplo: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
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
      "Usage:\n",
      "  Rscript scripts/download_scopus_meliponini.R [options]\n\n",
      "Options:\n",
      "  --input=FILE        Workbook containing the genus list. Default: Meliponini_genus_list.xlsx\n",
      "  --genus-col=COLUMN     Column containing genus names. Default: Genero/Gênero\n",
      "  --output-dir=DIR     Output directory. Default: data/scopus\n",
      "  --field=CAMPO          Campo Scopus pesquisado. Default: TITLE-ABS-KEY\n",
      "  --count=N              Records per page. Default: 25\n",
      "  --max-count=N          Maximum number of downloaded records. Default: 20000\n",
      "  --view=STANDARD        View da API: STANDARD ou COMPLETE. Default: STANDARD\n",
      "  --verbose              Mostra o log detalhado do rscopus.\n",
      "  --api-key=KEY        Elsevier/Scopus key; Elsevier_API or ELSEVIER_API_KEY may also be used.\n"
    )
  )
  quit(status = 0)
}

input_file <- args$input %||% "Meliponini_genus_list.xlsx"
output_dir <- args[["output-dir"]] %||% "data/scopus"
requested_genus_col <- args[["genus-col"]]
search_field <- toupper(args$field %||% "TITLE-ABS-KEY")
count <- as.integer(args$count %||% 25L)
max_count <- as.integer(args[["max-count"]] %||% 20000L)
view <- toupper(args$view %||% "STANDARD")
api_key <- args[["api-key"]] %||% Sys.getenv("Elsevier_API") %||% Sys.getenv("ELSEVIER_API_KEY")
verbose <- isTRUE(args$verbose)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file, call. = FALSE)
}

if (is.na(count) || count < 1) {
  stop("--count must be a positive integer.", call. = FALSE)
}

if (is.na(max_count) || max_count < 1) {
  stop("--max-count must be a positive integer.", call. = FALSE)
}

if (!view %in% c("STANDARD", "COMPLETE")) {
  stop("--view must be STANDARD or COMPLETE.", call. = FALSE)
}

if (identical(api_key, "")) {
  stop(
    "API key not found. Set Elsevier_API or run with --api-key=YOUR_KEY.",
    call. = FALSE
  )
}

normalize_name <- function(x) {
  tolower(iconv(x, from = "", to = "ASCII//TRANSLIT"))
}

escape_scopus_string <- function(x) {
  gsub('"', '\\"', x, fixed = TRUE)
}

genus_table <- readxl::read_excel(input_file)

candidate_cols <- c(requested_genus_col, "Gênero", "Genero", "genus", "Genus")
candidate_cols <- candidate_cols[!is.na(candidate_cols) & nzchar(candidate_cols)]
genus_col <- candidate_cols[candidate_cols %in% names(genus_table)][1]

if (is.na(genus_col)) {
  normalized_cols <- normalize_name(names(genus_table))
  genus_col <- names(genus_table)[normalized_cols %in% normalize_name(candidate_cols)][1]
}

if (is.na(genus_col)) {
  stop(
    "No genus column was found. Available columns: ",
    paste(names(genus_table), collapse = ", "),
    call. = FALSE
  )
}

genera <- genus_table[[genus_col]]
genera <- trimws(as.character(genera))
genera <- unique(genera[!is.na(genera) & nzchar(genera)])
genera <- sort(genera)

if (length(genera) == 0) {
  stop("The genus column is empty: ", genus_col, call. = FALSE)
}

query_terms <- sprintf('%s("%s")', search_field, escape_scopus_string(genera))
query <- paste(query_terms, collapse = " OR ")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

rscopus::set_api_key(api_key)

message("Input file: ", input_file)
message("Genus column: ", genus_col)
message("Genera in query: ", length(genera))
message("Campo pesquisado: ", search_field)
message("Tamanho da query: ", nchar(query), " caracteres")
message("Iniciando busca na Scopus...")

result <- rscopus::scopus_search(
  query = query,
  api_key = api_key,
  count = count,
  view = view,
  max_count = max_count,
  verbose = verbose,
  wait_time = 0.25
)

raw_df <- if (length(result$entries) > 0) {
  rscopus::gen_entries_to_df(result$entries)$df
} else {
  data.frame()
}

deduplicate_scopus_records <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }

  key <- rep(NA_character_, nrow(df))

  if ("prism:doi" %in% names(df)) {
    doi <- tolower(trimws(as.character(df[["prism:doi"]])))
    key[!is.na(doi) & nzchar(doi)] <- paste0("doi:", doi[!is.na(doi) & nzchar(doi)])
  }

  if ("eid" %in% names(df)) {
    eid <- trimws(as.character(df[["eid"]]))
    use_eid <- is.na(key) & !is.na(eid) & nzchar(eid)
    key[use_eid] <- paste0("eid:", eid[use_eid])
  }

  if ("dc:identifier" %in% names(df)) {
    identifier <- trimws(as.character(df[["dc:identifier"]]))
    use_identifier <- is.na(key) & !is.na(identifier) & nzchar(identifier)
    key[use_identifier] <- paste0("id:", identifier[use_identifier])
  }

  key[is.na(key)] <- paste0("row:", seq_len(nrow(df))[is.na(key)])
  df[!duplicated(key), , drop = FALSE]
}

articles_df <- deduplicate_scopus_records(raw_df)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
csv_file <- file.path(output_dir, paste0("scopus_meliponini_articles_", timestamp, ".csv"))
raw_csv_file <- file.path(output_dir, paste0("scopus_meliponini_articles_raw_", timestamp, ".csv"))
rds_file <- file.path(output_dir, paste0("scopus_meliponini_search_", timestamp, ".rds"))
query_file <- file.path(output_dir, paste0("scopus_meliponini_query_", timestamp, ".txt"))

utils::write.csv(articles_df, csv_file, row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(raw_df, raw_csv_file, row.names = FALSE, fileEncoding = "UTF-8")
writeLines(query, query_file, useBytes = TRUE)

saveRDS(
  list(
    query = query,
    genera = genera,
    input_file = input_file,
    genus_col = genus_col,
    search_field = search_field,
    count = count,
    max_count = max_count,
    view = view,
    downloaded_at = Sys.time(),
    result = result,
    raw_records = raw_df,
    deduplicated_records = articles_df
  ),
  rds_file
)

message("Busca concluida.")
message("Raw records: ", nrow(raw_df))
message("Records after deduplication: ", nrow(articles_df))
message("Deduplicated CSV: ", csv_file)
message("CSV bruto: ", raw_csv_file)
message("RDS completo: ", rds_file)
message("Query salva em: ", query_file)
