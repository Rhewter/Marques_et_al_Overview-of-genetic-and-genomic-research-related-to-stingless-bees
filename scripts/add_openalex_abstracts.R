#!/usr/bin/env Rscript

# Adiciona titulos e resumos do OpenAlex ao CSV baixado da Scopus.
# O OpenAlex representa resumos como "abstract_inverted_index"; este script
# reconstrói o texto e preserva os metadados originais da Scopus.

required_packages <- c("httr2", "jsonlite")
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
      "Uso:\n",
      "  Rscript scripts/add_openalex_abstracts.R --input=ARQUIVO_SCOPUS.csv [opcoes]\n\n",
      "Opcoes:\n",
      "  --input=ARQUIVO       CSV deduplicado gerado pelo script da Scopus.\n",
      "  --output=ARQUIVO      CSV de saida. Padrao: *_with_openalex_abstracts.csv\n",
      "  --batch-size=N        DOIs por chamada ao OpenAlex. Padrao: 25\n",
      "  --max-dois=N          Consulta apenas os N primeiros DOIs, util para teste.\n",
      "  --mailto=EMAIL        Email para uso educado da API OpenAlex.\n"
    )
  )
  quit(status = 0)
}

input_file <- args$input

if (is.null(input_file)) {
  candidates <- Sys.glob("data/scopus/scopus_meliponini_articles_*.csv")
  candidates <- candidates[!grepl("_raw_|_with_", candidates)]
  if (length(candidates) > 0) {
    input_file <- candidates[which.max(file.info(candidates)$mtime)]
  }
}

if (is.null(input_file) || !file.exists(input_file)) {
  stop(
    "Informe um CSV valido com --input=ARQUIVO. ",
    "Exemplo: --input=data/scopus/scopus_meliponini_articles_20260501_165336.csv",
    call. = FALSE
  )
}

batch_size <- as.integer(args[["batch-size"]] %||% 25L)
if (is.na(batch_size) || batch_size < 1) {
  stop("--batch-size precisa ser um inteiro positivo.", call. = FALSE)
}

max_dois <- args[["max-dois"]]
if (!is.null(max_dois)) {
  max_dois <- as.integer(max_dois)
  if (is.na(max_dois) || max_dois < 1) {
    stop("--max-dois precisa ser um inteiro positivo.", call. = FALSE)
  }
}

output_file <- args$output
if (is.null(output_file)) {
  output_file <- sub("\\.csv$", "_with_openalex_abstracts.csv", input_file)
}

mailto <- args$mailto

normalize_doi <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:", "", x)
  x[!nzchar(x)] <- NA_character_
  x
}

reconstruct_abstract <- function(inverted_index) {
  if (is.null(inverted_index) || length(inverted_index) == 0) {
    return(NA_character_)
  }

  positions <- unlist(inverted_index, use.names = FALSE)
  words <- rep(names(inverted_index), lengths(inverted_index))

  if (length(positions) == 0 || length(words) == 0) {
    return(NA_character_)
  }

  words[order(as.integer(positions))]
  paste(words[order(as.integer(positions))], collapse = " ")
}

fetch_openalex_batch <- function(dois, mailto = NULL) {
  filter_value <- paste0("doi:", paste(dois, collapse = "|"))

  request <- httr2::request("https://api.openalex.org/works") |>
    httr2::req_url_query(
      filter = filter_value,
      select = "doi,title,abstract_inverted_index",
      `per-page` = length(dois)
    ) |>
    httr2::req_user_agent("Stingless_bees_review/0.1 (https://openalex.org)")

  if (!is.null(mailto) && nzchar(mailto)) {
    request <- httr2::req_url_query(request, mailto = mailto)
  }

  response <- httr2::req_perform(request)
  parsed <- httr2::resp_body_json(response, simplifyVector = FALSE)

  if (is.null(parsed$results) || length(parsed$results) == 0) {
    return(data.frame())
  }

  rows <- lapply(parsed$results, function(item) {
    data.frame(
      doi_norm = normalize_doi(item$doi %||% NA_character_),
      openalex_title = item$title %||% NA_character_,
      abstract = reconstruct_abstract(item$abstract_inverted_index),
      abstract_source = if (is.null(item$abstract_inverted_index)) NA_character_ else "OpenAlex",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

scopus_df <- utils::read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

if (!"prism:doi" %in% names(scopus_df)) {
  stop("O CSV de entrada nao contem a coluna prism:doi.", call. = FALSE)
}

if (!"dc:title" %in% names(scopus_df)) {
  stop("O CSV de entrada nao contem a coluna dc:title.", call. = FALSE)
}

scopus_df$doi_norm <- normalize_doi(scopus_df[["prism:doi"]])
unique_dois <- unique(scopus_df$doi_norm[!is.na(scopus_df$doi_norm)])
if (!is.null(max_dois)) {
  unique_dois <- head(unique_dois, max_dois)
}

message("Arquivo de entrada: ", input_file)
message("Registros Scopus: ", nrow(scopus_df))
message("DOIs unicos para consulta no OpenAlex: ", length(unique_dois))

batches <- split(unique_dois, ceiling(seq_along(unique_dois) / batch_size))
openalex_rows <- vector("list", length(batches))

for (i in seq_along(batches)) {
  message("OpenAlex lote ", i, "/", length(batches), " (", length(batches[[i]]), " DOIs)")
  openalex_rows[[i]] <- fetch_openalex_batch(batches[[i]], mailto = mailto)
  Sys.sleep(0.11)
}

openalex_df <- do.call(rbind, openalex_rows)

if (is.null(openalex_df) || nrow(openalex_df) == 0) {
  openalex_df <- data.frame(
    doi_norm = character(),
    openalex_title = character(),
    abstract = character(),
    abstract_source = character(),
    stringsAsFactors = FALSE
  )
}

openalex_df <- openalex_df[!duplicated(openalex_df$doi_norm), , drop = FALSE]

merged <- merge(scopus_df, openalex_df, by = "doi_norm", all.x = TRUE, sort = FALSE)
merged$title <- merged[["dc:title"]]

preferred_cols <- c(
  "eid",
  "dc:identifier",
  "prism:doi",
  "title",
  "abstract",
  "abstract_source",
  "openalex_title",
  "dc:creator",
  "prism:publicationName",
  "prism:coverDate",
  "subtypeDescription",
  "citedby-count"
)
preferred_cols <- preferred_cols[preferred_cols %in% names(merged)]
remaining_cols <- setdiff(names(merged), c(preferred_cols, "doi_norm"))
merged <- merged[, c(preferred_cols, remaining_cols), drop = FALSE]

utils::write.csv(merged, output_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Busca concluida.")
message("Registros com DOI encontrados no OpenAlex: ", sum(!is.na(merged$openalex_title)))
message("Registros com resumo OpenAlex: ", sum(!is.na(merged$abstract) & nzchar(merged$abstract)))
message("CSV com titulos e resumos: ", output_file)
