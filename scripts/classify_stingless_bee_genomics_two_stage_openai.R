#!/usr/bin/env Rscript

# Classificacao em duas etapas:
# 1) classifica todos os artigos usando apenas o titulo;
# 2) usa titulo + resumo somente para casos de menor confianca/checagem.

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

args <- parse_args(commandArgs(trailingOnly = TRUE))

if (isTRUE(args$help) || isTRUE(args$h)) {
  cat(
    paste0(
      "Uso:\n",
      "  Rscript scripts/classify_stingless_bee_genomics_two_stage_openai.R [opcoes]\n\n",
      "Opcoes:\n",
      "  --input=ARQUIVO             CSV com title e abstract. Padrao: CSV mais recente *_titles_abstracts_*.csv\n",
      "  --output=ARQUIVO            CSV final classificado.\n",
      "  --checkpoint-prefix=PREFIX  Prefixo dos JSONL de checkpoint.\n",
      "  --model=MODELO              Modelo OpenAI para ambas as etapas. Padrao: gpt-4.1-mini\n",
      "  --title-model=MODELO        Modelo para triagem por titulo. Padrao: --model\n",
      "  --abstract-model=MODELO     Modelo para revisao com resumo. Padrao: --model\n",
      "  --title-batch-size=N        Artigos por chamada na etapa de titulo. Padrao: 100\n",
      "  --abstract-batch-size=N     Artigos por chamada na etapa com resumo. Padrao: 25\n",
      "  --skip-abstract-review      Executa somente a etapa de classificacao por titulo.\n",
      "  --max-records=N             Classifica apenas os N primeiros registros, util para teste.\n",
      "  --api-key=CHAVE             Chave OpenAI. Tambem pode usar OPENAI_API_KEY.\n"
    )
  )
  quit(status = 0)
}

input_file <- args$input
if (is.null(input_file)) {
  candidates <- Sys.glob("data/scopus/*_titles_abstracts_*.csv")
  if (length(candidates) > 0) input_file <- candidates[which.max(file.info(candidates)$mtime)]
}
if (is.null(input_file) || !file.exists(input_file)) {
  stop("Informe um CSV valido com --input=ARQUIVO.", call. = FALSE)
}

model <- args$model %||% "gpt-4.1-mini"
title_model <- args[["title-model"]] %||% model
abstract_model <- args[["abstract-model"]] %||% model
api_key <- args[["api-key"]] %||% Sys.getenv("OPENAI_API_KEY")
if (identical(api_key, "")) {
  stop("API key nao encontrada. Defina OPENAI_API_KEY ou use --api-key=SUA_CHAVE.", call. = FALSE)
}

title_batch_size <- as.integer(args[["title-batch-size"]] %||% 100L)
abstract_batch_size <- as.integer(args[["abstract-batch-size"]] %||% 25L)
skip_abstract_review <- isTRUE(args[["skip-abstract-review"]])
max_records <- args[["max-records"]]
if (!is.null(max_records)) max_records <- as.integer(max_records)

if (is.na(title_batch_size) || title_batch_size < 1 || title_batch_size > 150) {
  stop("--title-batch-size precisa ser entre 1 e 150.", call. = FALSE)
}
if (is.na(abstract_batch_size) || abstract_batch_size < 1 || abstract_batch_size > 75) {
  stop("--abstract-batch-size precisa ser entre 1 e 75.", call. = FALSE)
}
if (!is.null(max_records) && (is.na(max_records) || max_records < 1)) {
  stop("--max-records precisa ser um inteiro positivo.", call. = FALSE)
}

output_file <- args$output %||% sub("\\.csv$", "_genomics_ai_two_stage.csv", input_file)
checkpoint_prefix <- args[["checkpoint-prefix"]] %||% sub("\\.csv$", "_genomics_ai_two_stage", output_file)
title_jsonl <- paste0(checkpoint_prefix, "_title.jsonl")
abstract_jsonl <- paste0(checkpoint_prefix, "_abstract_review.jsonl")

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(title_jsonl), recursive = TRUE, showWarnings = FALSE)

truncate_text <- function(x, max_chars) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  x <- trimws(gsub("\\s+", " ", x))
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars), " [...]"), x)
}

json_scalar <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x) || !nzchar(as.character(x))) NA_character_ else as.character(x)
}

schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = list("classifications"),
  properties = list(
    classifications = list(
      type = "array",
      items = list(
        type = "object",
        additionalProperties = FALSE,
        required = list(
          "record_id", "is_genetics_genomics_stingless_bees", "relevance",
          "sample_relationship", "genetics_genomics_aspects", "confidence",
          "evidence", "rationale", "needs_manual_check", "decision_basis"
        ),
        properties = list(
          record_id = list(type = "integer"),
          is_genetics_genomics_stingless_bees = list(type = "boolean"),
          relevance = list(type = "string", enum = list("yes", "no", "unclear")),
          sample_relationship = list(
            type = "string",
            enum = list(
              "bee_itself",
              "bee_associated_sample",
              "other_organism_not_clearly_bee_associated",
              "not_applicable",
              "unclear"
            )
          ),
          genetics_genomics_aspects = list(
            type = "array",
            items = list(
              type = "string",
              enum = list(
                "dna_barcoding", "phylogenetics_or_taxonomy", "population_genetics",
                "genome_or_mitogenome", "transcriptomics_or_gene_expression",
                "metabarcoding_or_metagenomics", "microbiome", "molecular_marker",
                "other_genetics_genomics", "none"
              )
            )
          ),
          confidence = list(type = "string", enum = list("high", "medium", "low")),
          evidence = list(
            type = "array",
            items = list(
              type = "object",
              additionalProperties = FALSE,
              required = list("field", "quote", "why_it_matters"),
              properties = list(
                field = list(type = "string", enum = list("title", "abstract", "previous_title_classification")),
                quote = list(type = "string"),
                why_it_matters = list(type = "string")
              )
            )
          ),
          rationale = list(type = "string"),
          needs_manual_check = list(type = "boolean"),
          decision_basis = list(type = "string", enum = list("title_only", "title_and_abstract"))
        )
      )
    )
  )
)

base_rules <- paste(
  "Voce e um revisor sistematico especialista em abelhas sem ferrao (Meliponini), genetica e genomica.",
  "A pergunta e: o artigo trata de genetica/genomica relacionada a abelhas sem ferrao?",
  "Considere SIM quando houver evidencia de DNA barcoding, COI, 16S, mtDNA, mitogenoma, genoma, transcriptoma, expressao genica, filogenia molecular, SNP, microssatelite, marcador molecular, metabarcoding, metagenomica ou microbioma.",
  "A relacao com abelhas sem ferrao precisa ser clara.",
  "Para outros organismos, como polen, microbiota, plantas ou patogenos, conte como SIM apenas se a amostra for claramente da abelha, do corpo dela, do ninho, produto do ninho ou material carregado pela abelha sem ferrao.",
  "Marque NAO para quimica, comportamento, ecologia, morfologia, produto apicola, toxicologia ou manejo sem evidencia molecular/genetica/genomica.",
  "Marque UNCLEAR quando a informacao disponivel for insuficiente.",
  "Forneca evidencias curtas copiadas do texto disponivel para auditoria visual.",
  sep = "\n"
)

title_prompt <- paste(
  base_rules,
  "ETAPA 1: use APENAS o titulo. Nao invente informacoes que dependeriam do resumo.",
  "Use confidence='high' somente quando o titulo deixar a decisao muito clara.",
  "Use confidence='medium' ou 'low' e needs_manual_check=true quando o resumo puder mudar a decisao.",
  "decision_basis deve ser 'title_only'.",
  sep = "\n"
)

abstract_prompt <- paste(
  base_rules,
  "ETAPA 2: revise uma classificacao preliminar usando titulo + resumo.",
  "Voce pode confirmar ou alterar a decisao preliminar.",
  "Se o resumo estiver ausente ou ainda ambiguo, mantenha needs_manual_check=true.",
  "decision_basis deve ser 'title_and_abstract'.",
  sep = "\n"
)

extract_output_text <- function(response_body) {
  if (!is.null(response_body$output_text)) return(response_body$output_text)
  chunks <- character()
  for (item in response_body$output %||% list()) {
    for (content in item$content %||% list()) {
      if (!is.null(content$text)) chunks <- c(chunks, content$text)
    }
  }
  paste(chunks, collapse = "")
}

call_openai <- function(records, system_prompt, request_model) {
  body <- list(
    model = request_model,
    input = list(
      list(role = "system", content = system_prompt),
      list(
        role = "user",
        content = paste(
          "Classifique os registros abaixo e responda somente no JSON schema solicitado.",
          "Retorne uma classificacao para cada record_id.",
          jsonlite::toJSON(list(records = records), auto_unbox = TRUE, null = "null", pretty = TRUE),
          sep = "\n\n"
        )
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "stingless_bee_genomics_two_stage",
        strict = TRUE,
        schema = schema
      )
    )
  )

  last_error <- NULL

  for (attempt in seq_len(4)) {
    parsed <- tryCatch({
      response <- httr2::request("https://api.openai.com/v1/responses") |>
        httr2::req_method("POST") |>
        httr2::req_headers(Authorization = paste("Bearer", api_key), `Content-Type` = "application/json") |>
        httr2::req_body_json(body, auto_unbox = TRUE) |>
        httr2::req_retry(
          max_tries = 8,
          retry_on_failure = TRUE,
          is_transient = function(resp) httr2::resp_status(resp) %in% c(408, 409, 425, 429, 500:599),
          backoff = ~ min(90, 2^.x)
        ) |>
        httr2::req_timeout(180) |>
        httr2::req_perform()

      response_body <- httr2::resp_body_json(response, simplifyVector = FALSE)
      output_text <- extract_output_text(response_body)
      if (!nzchar(output_text)) stop("Resposta sem output_text da API OpenAI.", call. = FALSE)
      jsonlite::fromJSON(output_text, simplifyVector = FALSE)
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })

    if (!is.null(parsed) && !is.null(parsed$classifications)) {
      return(parsed$classifications)
    }

    message("Resposta invalida da API; tentando novamente (", attempt, "/4): ", last_error)
    Sys.sleep(min(30, 2^attempt))
  }

  stop("Falha ao obter JSON valido da API OpenAI apos retries: ", last_error, call. = FALSE)
}

read_jsonl <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
}

completed_ids <- function(path) {
  items <- read_jsonl(path)
  if (length(items) == 0) return(integer())
  unique(vapply(items, function(x) as.integer(x$record_id), integer(1)))
}

append_jsonl <- function(items, path) {
  lines <- vapply(items, function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), character(1))
  cat(paste0(lines, "\n"), file = path, append = TRUE)
}

items_to_df <- function(items) {
  if (length(items) == 0) return(data.frame())
  rows <- lapply(items, function(item) {
    evidence <- item$evidence %||% list()
    evidence_text <- vapply(evidence, function(e) {
      paste0(e$field %||% "", ": ", e$quote %||% "", " [", e$why_it_matters %||% "", "]")
    }, character(1))
    data.frame(
      record_id = as.integer(item$record_id),
      is_genetics_genomics_stingless_bees = isTRUE(item$is_genetics_genomics_stingless_bees),
      relevance = item$relevance %||% NA_character_,
      sample_relationship = item$sample_relationship %||% NA_character_,
      genetics_genomics_aspects = paste(item$genetics_genomics_aspects %||% character(), collapse = "; "),
      confidence = item$confidence %||% NA_character_,
      evidence = paste(evidence_text, collapse = " || "),
      rationale = item$rationale %||% NA_character_,
      needs_manual_check = isTRUE(item$needs_manual_check),
      decision_basis = item$decision_basis %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[!duplicated(out$record_id, fromLast = TRUE), , drop = FALSE]
}

run_batches <- function(df, batch_size, checkpoint, prompt, make_record, request_model) {
  done <- completed_ids(checkpoint)
  pending <- df[!df$record_id %in% done, , drop = FALSE]
  message("Checkpoint: ", checkpoint)
  message("Ja concluidos: ", length(intersect(df$record_id, done)), "; pendentes: ", nrow(pending))
  if (nrow(pending) == 0) return(invisible(NULL))

  classify_records_safely <- function(records, depth = 0) {
    results <- call_openai(records, prompt, request_model = request_model)
    returned <- vapply(results, function(x) as.integer(x$record_id), integer(1))
    expected <- vapply(records, function(x) as.integer(x$record_id), integer(1))
    valid <- results[returned %in% expected]
    missing <- setdiff(expected, returned)

    if (length(valid) > 0) {
      append_jsonl(valid, checkpoint)
    }

    if (length(missing) == 0) {
      return(invisible(NULL))
    }

    if (depth >= 3 || length(missing) == length(expected)) {
      missing_records <- records[expected %in% missing]
      for (record in missing_records) {
        retry_result <- call_openai(list(record), prompt, request_model = request_model)
        retry_returned <- vapply(retry_result, function(x) as.integer(x$record_id), integer(1))
        if (!as.integer(record$record_id) %in% retry_returned) {
          stop("A API nao retornou record_id: ", record$record_id, call. = FALSE)
        }
        append_jsonl(retry_result[retry_returned == as.integer(record$record_id)], checkpoint)
      }
      return(invisible(NULL))
    }

    message("Resposta parcial; reprocessando ", length(missing), " record_id faltantes")
    missing_records <- records[expected %in% missing]
    sub_batches <- split(seq_along(missing_records), ceiling(seq_along(missing_records) / max(1, floor(length(missing_records) / 2))))
    for (idx in sub_batches) {
      classify_records_safely(missing_records[idx], depth = depth + 1)
    }
  }

  batches <- split(seq_len(nrow(pending)), ceiling(seq_len(nrow(pending)) / batch_size))
  for (i in seq_along(batches)) {
    batch_df <- pending[batches[[i]], , drop = FALSE]
    records <- lapply(seq_len(nrow(batch_df)), function(j) make_record(batch_df[j, , drop = FALSE]))
    message("Lote ", i, "/", length(batches), " (", length(records), " artigos)")
    classify_records_safely(records)
    Sys.sleep(0.15)
  }
}

articles <- utils::read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)
for (col in c("title", "abstract")) {
  if (!col %in% names(articles)) stop("Coluna ausente no CSV: ", col, call. = FALSE)
}
articles$record_id <- seq_len(nrow(articles))
if (!is.null(max_records)) articles <- head(articles, max_records)

message("Arquivo de entrada: ", input_file)
message("Registros no escopo: ", nrow(articles))
message("Modelo padrao: ", model)
message("Modelo etapa titulo: ", title_model)
message("Modelo etapa resumo: ", abstract_model)

message("Etapa 1: titulo")
run_batches(
  articles,
  title_batch_size,
  title_jsonl,
  title_prompt,
  function(row) {
    list(
      record_id = as.integer(row$record_id),
      eid = json_scalar(row$eid %||% NA_character_),
      doi = json_scalar(row[["prism:doi"]] %||% NA_character_),
      title = truncate_text(row$title, 900)
    )
  },
  request_model = title_model
)

title_df <- items_to_df(read_jsonl(title_jsonl))
title_df <- title_df[title_df$record_id %in% articles$record_id, , drop = FALSE]

if (skip_abstract_review) {
  review_articles <- articles[FALSE, , drop = FALSE]
  message("Etapa 2: revisao com resumo ignorada por --skip-abstract-review")
} else {
  review_ids <- title_df$record_id[
    title_df$confidence != "high" |
      title_df$relevance == "unclear" |
      title_df$needs_manual_check
  ]
  review_articles <- articles[articles$record_id %in% review_ids, , drop = FALSE]
}

message("Etapa 2: revisao com resumo")
message("Registros selecionados para revisar com resumo: ", nrow(review_articles))
if (!skip_abstract_review && nrow(review_articles) > 0) {
  title_lookup <- title_df
  run_batches(
    review_articles,
    abstract_batch_size,
    abstract_jsonl,
    abstract_prompt,
    function(row) {
      previous <- title_lookup[title_lookup$record_id == row$record_id, , drop = FALSE]
      list(
        record_id = as.integer(row$record_id),
        eid = json_scalar(row$eid %||% NA_character_),
        doi = json_scalar(row[["prism:doi"]] %||% NA_character_),
        title = truncate_text(row$title, 900),
        abstract = truncate_text(row$abstract, 2500),
        previous_title_classification = list(
          relevance = json_scalar(previous$relevance),
          confidence = json_scalar(previous$confidence),
          rationale = json_scalar(previous$rationale)
        )
      )
    },
    request_model = abstract_model
  )
}

abstract_df <- items_to_df(read_jsonl(abstract_jsonl))
if (nrow(abstract_df) > 0) {
  abstract_df <- abstract_df[abstract_df$record_id %in% articles$record_id, , drop = FALSE]
  abstract_df$decision_basis <- "title_and_abstract"
}

classification_df <- title_df
if (nrow(abstract_df) > 0) {
  classification_df <- classification_df[!classification_df$record_id %in% abstract_df$record_id, , drop = FALSE]
  classification_df <- rbind(classification_df, abstract_df)
}

final_df <- merge(articles, classification_df, by = "record_id", all.x = TRUE, sort = FALSE)
final_df$is_genetics_genomics_stingless_bees <- final_df$relevance == "yes"

preferred_cols <- c(
  "record_id", "eid", "prism:doi", "title", "abstract",
  "is_genetics_genomics_stingless_bees", "relevance", "sample_relationship",
  "genetics_genomics_aspects", "confidence", "decision_basis", "evidence",
  "rationale", "needs_manual_check", "dc:creator", "prism:publicationName",
  "prism:coverDate"
)
preferred_cols <- preferred_cols[preferred_cols %in% names(final_df)]
final_df <- final_df[, c(preferred_cols, setdiff(names(final_df), preferred_cols)), drop = FALSE]

utils::write.csv(final_df, output_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Classificacao concluida.")
message("CSV final: ", output_file)
message("Checkpoint titulos: ", title_jsonl)
message("Checkpoint resumos: ", abstract_jsonl)
message("Registros classificados como sim: ", sum(final_df$relevance == "yes", na.rm = TRUE))
message("Registros incertos: ", sum(final_df$relevance == "unclear", na.rm = TRUE))
message("Registros revisados com resumo: ", sum(final_df$decision_basis == "title_and_abstract", na.rm = TRUE))
message("Registros para checagem manual: ", sum(final_df$needs_manual_check, na.rm = TRUE))
