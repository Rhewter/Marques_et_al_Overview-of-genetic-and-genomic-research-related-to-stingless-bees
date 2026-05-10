#!/usr/bin/env Rscript

# Busca na Web of Science Core Collection todos os registros que mencionam ao
# menos um dos generos listados em Meliponini_genus_list.xlsx.
#
# O script usa a Web of Science Expanded API por padrao:
#   https://api.clarivate.com/api/wos
#
# A chave deve ser informada via --api-key ou pelas variaveis de ambiente
# WOS_API_KEY, WEB_OF_SCIENCE_API_KEY ou CLARIVATE_API_KEY.

required_packages <- c("readxl", "httr2", "jsonlite")
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
      "  Rscript scripts/download_wos_meliponini.R [opcoes]\n\n",
      "Opcoes:\n",
      "  --input=ARQUIVO             Planilha com a lista de generos. Padrao: Meliponini_genus_list.xlsx\n",
      "  --genus-col=COLUNA          Coluna que contem os generos. Padrao: Genero/Genero\n",
      "  --output-dir=PASTA          Pasta de saida. Padrao: data/wos\n",
      "  --api-base=URL              Endpoint da WoS API. Padrao: https://api.clarivate.com/api/wos\n",
      "  --api-key=CHAVE             Chave Clarivate/WoS. Tambem pode usar WOS_API_KEY, WEB_OF_SCIENCE_API_KEY ou CLARIVATE_API_KEY.\n",
      "  --database-id=ID            Base WoS. Padrao: WOS\n",
      "  --field=CAMPO               Campo WoS pesquisado. Padrao: TS\n",
      "  --count=N                   Registros por pagina. Padrao: 100\n",
      "  --max-count=N               Maximo global de registros baixados. Padrao: 20000\n",
      "  --terms-per-query=N         Generos por lote de query. Padrao: 30\n",
      "  --option-view=FS            Visao da Expanded API, por exemplo FS ou SR. Padrao: FS\n",
      "  --lang=IDIOMA               Idioma da API. Padrao: en\n",
      "  --wait-time=SEGUNDOS        Pausa entre requisicoes. Padrao: 0.25\n",
      "  --dry-run                   Apenas salva/mostra as queries, sem acessar a API.\n"
    )
  )
  quit(status = 0)
}

input_file <- args$input %||% "Meliponini_genus_list.xlsx"
output_dir <- args[["output-dir"]] %||% "data/wos"
requested_genus_col <- args[["genus-col"]]
api_base <- args[["api-base"]] %||% "https://api.clarivate.com/api/wos"
api_key <- args[["api-key"]] %||%
  Sys.getenv("WOS_API_KEY") %||%
  Sys.getenv("WEB_OF_SCIENCE_API_KEY") %||%
  Sys.getenv("CLARIVATE_API_KEY")
database_id <- args[["database-id"]] %||% "WOS"
search_field <- toupper(args$field %||% "TS")
count <- as.integer(args$count %||% 100L)
max_count <- as.integer(args[["max-count"]] %||% 20000L)
terms_per_query <- as.integer(args[["terms-per-query"]] %||% 30L)
option_view <- toupper(args[["option-view"]] %||% "FS")
lang <- args$lang %||% "en"
wait_time <- as.numeric(args[["wait-time"]] %||% 0.25)
dry_run <- isTRUE(args[["dry-run"]])

if (!file.exists(input_file)) {
  stop("Arquivo de entrada nao encontrado: ", input_file, call. = FALSE)
}

if (is.na(count) || count < 1 || count > 100) {
  stop("--count precisa ser um inteiro entre 1 e 100 para a WoS API.", call. = FALSE)
}

if (is.na(max_count) || max_count < 1) {
  stop("--max-count precisa ser um inteiro positivo.", call. = FALSE)
}

if (is.na(terms_per_query) || terms_per_query < 1) {
  stop("--terms-per-query precisa ser um inteiro positivo.", call. = FALSE)
}

if (is.na(wait_time) || wait_time < 0) {
  stop("--wait-time precisa ser numerico e nao negativo.", call. = FALSE)
}

if (!dry_run && identical(api_key, "")) {
  stop(
    "API key nao encontrada. Defina WOS_API_KEY, WEB_OF_SCIENCE_API_KEY ou CLARIVATE_API_KEY; ou rode com --api-key=SUA_CHAVE.",
    call. = FALSE
  )
}

normalize_name <- function(x) {
  tolower(iconv(x, from = "", to = "ASCII//TRANSLIT"))
}

escape_wos_phrase <- function(x) {
  gsub('"', '\\"', x, fixed = TRUE)
}

split_batches <- function(x, batch_size) {
  split(x, ceiling(seq_along(x) / batch_size))
}

build_wos_query <- function(terms, field) {
  quoted_terms <- sprintf('"%s"', escape_wos_phrase(terms))
  sprintf("%s=(%s)", field, paste(quoted_terms, collapse = " OR "))
}

compact_scalar <- function(x, max_chars = 2000) {
  if (is.null(x)) {
    return(NA_character_)
  }

  if (is.atomic(x)) {
    out <- paste(stats::na.omit(as.character(x)), collapse = "; ")
  } else if (is.list(x)) {
    names_x <- names(x)
    value_names <- c("content", "value", "text", "_", "#text", "label")
    value_name <- value_names[value_names %in% names_x][1]
    if (!is.na(value_name)) {
      out <- compact_scalar(x[[value_name]], max_chars = max_chars)
    } else {
      out <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
    }
  } else {
    out <- as.character(x)
  }

  out <- trimws(out)
  if (!nzchar(out)) {
    return(NA_character_)
  }
  substr(out, 1, max_chars)
}

find_first_named <- function(x, candidate_names) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm)) {
      hit <- which(tolower(nm) %in% tolower(candidate_names))[1]
      if (!is.na(hit)) {
        return(x[[hit]])
      }
    }

    for (item in x) {
      found <- find_first_named(item, candidate_names)
      if (!is.null(found)) {
        return(found)
      }
    }
  }

  NULL
}

find_all_named <- function(x, candidate_names) {
  found <- list()

  walk <- function(y) {
    if (!is.list(y)) {
      return(invisible(NULL))
    }

    nm <- names(y)
    if (!is.null(nm)) {
      hits <- which(tolower(nm) %in% tolower(candidate_names))
      if (length(hits) > 0) {
        for (hit in hits) {
          found[[length(found) + 1]] <<- y[[hit]]
        }
      }
    }

    for (item in y) {
      walk(item)
    }
    invisible(NULL)
  }

  walk(x)
  found
}

record_has_uid <- function(x) {
  !is.null(find_first_named(x, c("UID", "uid", "UT", "ut", "uniqueId", "unique_id")))
}

find_records <- function(response_json) {
  candidates <- list(
    response_json$Data$Records$records$REC,
    response_json$Data$Records$records,
    response_json$Records$records$REC,
    response_json$Records$records,
    response_json$hits,
    response_json$documents
  )

  for (candidate in candidates) {
    if (is.null(candidate)) {
      next
    }

    if (is.list(candidate) && length(candidate) > 0) {
      if (record_has_uid(candidate)) {
        return(list(candidate))
      }
      if (all(vapply(candidate, is.list, logical(1)))) {
        return(candidate)
      }
    }
  }

  list()
}

get_records_found <- function(response_json) {
  value <- find_first_named(
    response_json,
    c("RecordsFound", "recordsFound", "total", "totalRecords", "totalResults")
  )
  value <- gsub(",", "", compact_scalar(value), fixed = TRUE)
  value <- suppressWarnings(as.integer(value))
  if (length(value) == 0 || is.na(value)) {
    NA_integer_
  } else {
    value
  }
}

extract_title_by_type <- function(record, desired_type) {
  titles <- find_all_named(record, c("title", "titles"))
  title_items <- list()

  for (title_obj in titles) {
    if (is.list(title_obj) && all(vapply(title_obj, is.list, logical(1)))) {
      title_items <- c(title_items, title_obj)
    } else {
      title_items[[length(title_items) + 1]] <- title_obj
    }
  }

  for (title_obj in title_items) {
    if (!is.list(title_obj)) {
      next
    }
    title_type <- compact_scalar(title_obj$type %||% title_obj$`@type` %||% title_obj$role)
    if (!is.na(title_type) && tolower(title_type) %in% desired_type) {
      return(compact_scalar(title_obj))
    }
  }

  NA_character_
}

extract_doi <- function(record) {
  doi <- compact_scalar(find_first_named(record, c("doi", "DOI")))
  if (!is.na(doi)) {
    return(doi)
  }

  scalar_record <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null")
  match <- regmatches(
    scalar_record,
    regexpr("10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+", scalar_record, perl = TRUE)
  )
  if (length(match) == 0 || identical(match, character(0))) {
    NA_character_
  } else {
    match[[1]]
  }
}

extract_year <- function(record) {
  year <- compact_scalar(find_first_named(record, c("pubyear", "pubYear", "year", "PY")))
  year <- regmatches(year, regexpr("[12][0-9]{3}", year))
  if (length(year) == 0) {
    NA_character_
  } else {
    year[[1]]
  }
}

extract_record <- function(record, query_id) {
  item_title <- extract_title_by_type(record, c("item"))
  source_title <- extract_title_by_type(record, c("source"))

  if (is.na(item_title)) {
    item_title <- compact_scalar(find_first_named(record, c("title", "Title", "TI")))
  }

  data.frame(
    query_id = query_id,
    uid = compact_scalar(find_first_named(record, c("UID", "uid", "UT", "ut", "uniqueId", "unique_id"))),
    doi = extract_doi(record),
    title = item_title,
    source_title = source_title,
    year = extract_year(record),
    document_type = compact_scalar(find_first_named(record, c("doctype", "docType", "documentType", "DT"))),
    times_cited = compact_scalar(find_first_named(record, c("timesCited", "times_cited", "TC"))),
    authors = compact_scalar(find_first_named(record, c("names", "authors", "AU"))),
    abstract = compact_scalar(find_first_named(record, c("abstract", "abstract_text", "AB")), max_chars = 10000),
    keywords = compact_scalar(find_first_named(record, c("keywords", "DE", "ID")), max_chars = 5000),
    record_json = jsonlite::toJSON(record, auto_unbox = TRUE, null = "null"),
    stringsAsFactors = FALSE
  )
}

deduplicate_wos_records <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }

  key <- rep(NA_character_, nrow(df))

  if ("uid" %in% names(df)) {
    uid <- trimws(as.character(df$uid))
    use_uid <- !is.na(uid) & nzchar(uid)
    key[use_uid] <- paste0("uid:", uid[use_uid])
  }

  if ("doi" %in% names(df)) {
    doi <- tolower(trimws(as.character(df$doi)))
    use_doi <- is.na(key) & !is.na(doi) & nzchar(doi)
    key[use_doi] <- paste0("doi:", doi[use_doi])
  }

  if ("title" %in% names(df)) {
    title <- normalize_name(trimws(as.character(df$title)))
    use_title <- is.na(key) & !is.na(title) & nzchar(title)
    key[use_title] <- paste0("title:", title[use_title])
  }

  key[is.na(key)] <- paste0("row:", seq_len(nrow(df))[is.na(key)])
  df[!duplicated(key), , drop = FALSE]
}

fetch_wos_page <- function(query, first_record, count, query_id) {
  req <- httr2::request(api_base) |>
    httr2::req_headers(
      `X-ApiKey` = api_key,
      Accept = "application/json"
    ) |>
    httr2::req_url_query(
      databaseId = database_id,
      usrQuery = query,
      count = count,
      firstRecord = first_record,
      lang = lang
    ) |>
    httr2::req_retry(max_tries = 4)

  if (!is.null(option_view) && nzchar(option_view)) {
    req <- httr2::req_url_query(req, optionView = option_view)
  }

  response <- httr2::req_perform(req)
  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  records <- find_records(body)

  list(
    query_id = query_id,
    first_record = first_record,
    records_found = get_records_found(body),
    records = records,
    body = body
  )
}

genus_table <- readxl::read_excel(input_file)

candidate_cols <- c(requested_genus_col, "Genero", "Gênero", "genus", "Genus")
candidate_cols <- candidate_cols[!is.na(candidate_cols) & nzchar(candidate_cols)]
genus_col <- candidate_cols[candidate_cols %in% names(genus_table)][1]

if (is.na(genus_col)) {
  normalized_cols <- normalize_name(names(genus_table))
  genus_col <- names(genus_table)[normalized_cols %in% normalize_name(candidate_cols)][1]
}

if (is.na(genus_col)) {
  stop(
    "Nao encontrei a coluna de generos. Colunas disponiveis: ",
    paste(names(genus_table), collapse = ", "),
    call. = FALSE
  )
}

genera <- genus_table[[genus_col]]
genera <- trimws(as.character(genera))
genera <- unique(genera[!is.na(genera) & nzchar(genera)])
genera <- sort(genera)

if (length(genera) == 0) {
  stop("A coluna de generos esta vazia: ", genus_col, call. = FALSE)
}

query_batches <- split_batches(genera, terms_per_query)
queries <- vapply(query_batches, build_wos_query, character(1), field = search_field)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
csv_file <- file.path(output_dir, paste0("wos_meliponini_articles_", timestamp, ".csv"))
raw_jsonl_file <- file.path(output_dir, paste0("wos_meliponini_records_raw_", timestamp, ".jsonl"))
rds_file <- file.path(output_dir, paste0("wos_meliponini_search_", timestamp, ".rds"))
query_file <- file.path(output_dir, paste0("wos_meliponini_queries_", timestamp, ".txt"))

writeLines(
  paste0("Query ", seq_along(queries), ":\n", queries, "\n"),
  query_file,
  useBytes = TRUE
)

message("Arquivo de entrada: ", input_file)
message("Coluna de generos: ", genus_col)
message("Generos na consulta: ", length(genera))
message("Campo pesquisado: ", search_field)
message("Lotes de query: ", length(queries), " (", terms_per_query, " generos por lote)")
message("Endpoint WoS: ", api_base)
message("Database ID: ", database_id)
message("Option view: ", option_view)
message("Queries salvas em: ", query_file)

if (dry_run) {
  message("Dry-run concluido. Nenhuma requisicao foi enviada para a API.")
  quit(status = 0)
}

all_pages <- list()
all_records <- list()
downloaded <- 0L

for (query_id in seq_along(queries)) {
  query <- queries[[query_id]]
  first_record <- 1L
  records_found <- NA_integer_

  message("Iniciando lote ", query_id, "/", length(queries), "...")

  repeat {
    remaining <- max_count - downloaded
    if (remaining <= 0) {
      message("Limite global --max-count alcancado: ", max_count)
      break
    }

    page_count <- min(count, remaining)
    page <- fetch_wos_page(query, first_record, page_count, query_id)
    records_found <- page$records_found
    records <- page$records

    all_pages[[length(all_pages) + 1]] <- page

    if (length(records) == 0) {
      message("Nenhum registro retornado no lote ", query_id, " a partir de firstRecord=", first_record, ".")
      break
    }

    extracted <- lapply(records, extract_record, query_id = query_id)
    all_records <- c(all_records, extracted)

    downloaded <- downloaded + length(records)
    message(
      "Lote ", query_id,
      ": baixados ", length(records),
      " nesta pagina; total global ", downloaded,
      if (!is.na(records_found)) paste0("; encontrados no lote ", records_found) else ""
    )

    first_record <- first_record + length(records)

    if (!is.na(records_found) && first_record > records_found) {
      break
    }

    if (downloaded >= max_count) {
      break
    }

    if (wait_time > 0) {
      Sys.sleep(wait_time)
    }
  }

  if (downloaded >= max_count) {
    break
  }
}

raw_df <- if (length(all_records) > 0) {
  do.call(rbind, all_records)
} else {
  data.frame()
}

articles_df <- deduplicate_wos_records(raw_df)

utils::write.csv(articles_df, csv_file, row.names = FALSE, fileEncoding = "UTF-8")

if (nrow(raw_df) > 0) {
  writeLines(raw_df$record_json, raw_jsonl_file, useBytes = TRUE)
} else {
  file.create(raw_jsonl_file)
}

saveRDS(
  list(
    queries = queries,
    genera = genera,
    input_file = input_file,
    genus_col = genus_col,
    api_base = api_base,
    database_id = database_id,
    search_field = search_field,
    count = count,
    max_count = max_count,
    terms_per_query = terms_per_query,
    option_view = option_view,
    downloaded_at = Sys.time(),
    pages = all_pages,
    raw_records = raw_df,
    deduplicated_records = articles_df
  ),
  rds_file
)

message("Busca concluida.")
message("Registros brutos: ", nrow(raw_df))
message("Registros apos deduplicacao: ", nrow(articles_df))
message("CSV deduplicado: ", csv_file)
message("JSONL bruto: ", raw_jsonl_file)
message("RDS completo: ", rds_file)
message("Queries salvas em: ", query_file)
