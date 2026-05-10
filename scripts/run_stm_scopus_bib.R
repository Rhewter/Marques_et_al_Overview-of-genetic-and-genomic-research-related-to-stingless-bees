#!/usr/bin/env Rscript

# Analise de Structural Topic Modelling (STM) para exportacao BibTeX da Scopus.
# O script importa o .bib, prepara texto de titulo/resumo/palavras-chave,
# testa valores de K, ajusta o melhor modelo e plota a prevalencia dos topicos
# ao longo dos anos para destacar temas em ascensao recente.

required_packages <- c(
  "bibliometrix",
  "dplyr",
  "ggplot2",
  "purrr",
  "readxl",
  "readr",
  "stm",
  "stringr",
  "tibble",
  "tidyr"
)

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

suppressPackageStartupMessages({
  library(bibliometrix)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readxl)
  library(readr)
  library(stm)
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

as_integer_arg <- function(x, default, name, min_value = 1L) {
  value <- as.integer(x %||% default)
  if (is.na(value) || value < min_value) {
    stop("--", name, " precisa ser um inteiro >= ", min_value, ".", call. = FALSE)
  }
  value
}

as_logical_arg <- function(x, default = FALSE) {
  if (is.null(x)) {
    return(default)
  }
  if (is.logical(x)) {
    return(isTRUE(x))
  }
  tolower(as.character(x)) %in% c("1", "true", "t", "yes", "y", "sim", "s")
}

parse_k_values <- function(x) {
  raw <- strsplit(x, ",", fixed = TRUE)[[1]]
  values <- as.integer(trimws(raw))
  values <- values[!is.na(values)]
  unique(values[values >= 2])
}

normalize_metric <- function(x, higher_is_better = TRUE) {
  x <- as.numeric(x)
  if (!higher_is_better) {
    x <- -x
  }
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  if (isTRUE(all.equal(rng[[1]], rng[[2]]))) {
    return(rep(0.5, length(x)))
  }
  (x - rng[[1]]) / diff(rng)
}

safe_text <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  x
}

normalize_stopword_tokens <- function(x) {
  x <- safe_text(x)
  x_ascii <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x_all <- tolower(c(x, x_ascii))
  tokens <- stringr::str_extract_all(x_all, "[[:alpha:]][[:alpha:]-]*")
  tokens <- unlist(tokens, use.names = FALSE)
  tokens <- gsub("-", "", tokens)
  unique(tokens[nchar(tokens) >= 3])
}

drop_vocab_terms <- function(documents, vocab, drop_terms) {
  drop_terms <- unique(drop_terms)
  drop_idx <- which(vocab %in% drop_terms)

  if (length(drop_idx) == 0) {
    return(list(documents = documents, vocab = vocab, dropped = character()))
  }

  keep_vocab <- !(seq_along(vocab) %in% drop_idx)
  old_to_new <- integer(length(vocab))
  old_to_new[keep_vocab] <- seq_len(sum(keep_vocab))

  filtered_documents <- lapply(documents, function(doc) {
    if (is.null(doc) || length(doc) == 0) {
      return(doc)
    }

    keep_tokens <- keep_vocab[doc[1, ]]
    doc <- doc[, keep_tokens, drop = FALSE]
    if (ncol(doc) > 0) {
      doc[1, ] <- old_to_new[doc[1, ]]
    }
    doc
  })

  list(
    documents = filtered_documents,
    vocab = vocab[keep_vocab],
    dropped = vocab[drop_idx]
  )
}

extract_taxon_stopwords <- function(species_file) {
  if (is.null(species_file) || !nzchar(species_file)) {
    return(character())
  }

  if (!file.exists(species_file)) {
    stop("Planilha de especies nao encontrada: ", species_file, call. = FALSE)
  }

  species_tbl <- readxl::read_excel(species_file, guess_max = 10000)
  normalized_names <- iconv(names(species_tbl), from = "", to = "ASCII//TRANSLIT", sub = "")
  normalized_names <- tolower(normalized_names)
  normalized_names <- gsub("[^a-z]+", "", normalized_names)

  taxon_cols <- names(species_tbl)[
    grepl("nomecientifico|genero|subgenero|epiteto", normalized_names)
  ]

  if (length(taxon_cols) == 0) {
    stop(
      "Nao encontrei colunas taxonomicas na planilha. ",
      "Esperava algo como Nome cientifico, Genero, Subgenero ou Epiteto especifico.",
      call. = FALSE
    )
  }

  taxon_values <- unlist(species_tbl[taxon_cols], use.names = FALSE)
  normalize_stopword_tokens(taxon_values)
}

read_extra_stopwords <- function(extra_stopwords_file) {
  if (is.null(extra_stopwords_file) || !nzchar(extra_stopwords_file)) {
    return(character())
  }

  if (!file.exists(extra_stopwords_file)) {
    stop("Arquivo de stopwords adicionais nao encontrado: ", extra_stopwords_file, call. = FALSE)
  }

  lines <- readLines(extra_stopwords_file, warn = FALSE, encoding = "UTF-8")
  lines <- sub("#.*$", "", lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  normalize_stopword_tokens(lines)
}

extract_genus_names <- function(species_file) {
  if (is.null(species_file) || !nzchar(species_file) || !file.exists(species_file)) {
    return(character())
  }

  species_tbl <- readxl::read_excel(species_file, guess_max = 10000)
  normalized_names <- iconv(names(species_tbl), from = "", to = "ASCII//TRANSLIT", sub = "")
  normalized_names <- tolower(normalized_names)
  normalized_names <- gsub("[^a-z]+", "", normalized_names)
  genus_col <- names(species_tbl)[normalized_names == "genero"]

  if (length(genus_col) == 0) {
    return(character())
  }

  genera <- unique(trimws(as.character(species_tbl[[genus_col[[1]]]])))
  genera <- genera[!is.na(genera) & nzchar(genera)]
  sort(genera)
}

detect_document_genera <- function(text, genera) {
  if (length(genera) == 0) {
    return(tibble(
      mentioned_genera = character(length(text)),
      n_genera_mentioned = integer(length(text)),
      genus_raw = rep("Unclassified", length(text))
    ))
  }

  text_lower <- tolower(safe_text(text))
  genus_lower <- tolower(genera)
  names(genus_lower) <- genera

  detected <- lapply(text_lower, function(one_text) {
    hits <- genera[vapply(genus_lower, function(g) {
      grepl(paste0("\\b", g, "\\b"), one_text, perl = TRUE)
    }, logical(1))]
    sort(unique(hits))
  })

  tibble(
    mentioned_genera = vapply(detected, paste, character(1), collapse = "; "),
    n_genera_mentioned = lengths(detected),
    genus_raw = vapply(detected, function(x) {
      if (length(x) == 0) {
        "Unclassified"
      } else if (length(x) == 1) {
        x[[1]]
      } else {
        "Multiple"
      }
    }, character(1))
  )
}

collapse_sparse_genera <- function(genus_raw, min_docs = 5L) {
  counts <- table(genus_raw)
  keep <- names(counts)[counts >= min_docs | names(counts) %in% c("Multiple", "Unclassified")]
  collapsed <- ifelse(genus_raw %in% keep, genus_raw, "Other_rare_genera")
  factor(collapsed)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

if (isTRUE(args$help) || isTRUE(args$h)) {
  cat(
    paste0(
      "Uso:\n",
      "  Rscript scripts/run_stm_scopus_bib.R [opcoes]\n\n",
      "Opcoes:\n",
      "  --input=ARQUIVO          BibTeX da Scopus.\n",
      "                           Padrao: data/scopus_export_May_2-2026_a80702bd-6141-4355-8ab3-3ee911a5ead3_Meliponini_genetics_and_genomics.bib\n",
      "  --input-format=auto|scopus_bib|csv\n",
      "                           Formato de entrada. CSV precisa ter title, abstract, year, journal, doi.\n",
      "  --output-dir=DIR         Diretorio de saida. Padrao: results/stm\n",
      "  --species-file=ARQUIVO   XLSX com nomes de especies/generos a remover.\n",
      "                           Padrao: data/Meliponini_species.xlsx\n",
      "  --extra-stopwords-file=ARQUIVO\n",
      "                           TXT/CSV simples com stopwords adicionais, uma por linha.\n",
      "  --document-covariates-file=ARQUIVO\n",
      "                           CSV com covariaveis por artigo; une por article_id ou doc_id.\n",
      "  --prevalence-covariate=year|genus|region|subtribe\n",
      "                           Covariavel de prevalencia do STM. Padrao: year\n",
      "  --genus-min-docs=N       Generos com menos de N documentos viram Other_rare_genera. Padrao: 5\n",
      "  --k-values=LISTA         Valores de K separados por virgula. Padrao: 5,8,10,12,15,20,25,30\n",
      "  --min-docfreq=N          Mantem termos presentes em pelo menos N documentos. Padrao: 3\n",
      "  --max-docfreq-prop=P     Remove termos em mais de P dos documentos. Padrao: 0.85\n",
      "  --seed=N                 Semente reprodutivel. Padrao: 1234\n",
      "  --force-k=N              Ajusta o modelo final com este K, mantendo searchK completo.\n",
      "  --cores=N                Nucleos para searchK. Padrao: 1\n",
      "  --top-words=N            Palavras por topico nas tabelas. Padrao: 12\n",
      "  --frontier-window=N      Janela final, em anos, para detectar topicos ascendentes. Padrao: 5\n",
      "  --stem=true|false        Aplica stemming. Padrao: false\n",
      "  --overwrite              Sobrescreve saidas existentes.\n"
    )
  )
  quit(status = 0)
}

input_file <- args$input %||%
  "data/scopus_export_May_2-2026_a80702bd-6141-4355-8ab3-3ee911a5ead3_Meliponini_genetics_and_genomics.bib"
input_format <- args[["input-format"]] %||% "auto"
output_dir <- args[["output-dir"]] %||% "results/stm"
species_file <- args[["species-file"]] %||% "data/Meliponini_species.xlsx"
extra_stopwords_file <- args[["extra-stopwords-file"]]
document_covariates_file <- args[["document-covariates-file"]]
prevalence_covariate <- args[["prevalence-covariate"]] %||% "year"
genus_min_docs <- as_integer_arg(args[["genus-min-docs"]], 5L, "genus-min-docs", 1L)
k_values <- parse_k_values(args[["k-values"]] %||% "5,8,10,12,15,20,25,30")
min_docfreq <- as_integer_arg(args[["min-docfreq"]], 3L, "min-docfreq", 1L)
top_words <- as_integer_arg(args[["top-words"]], 12L, "top-words", 3L)
frontier_window <- as_integer_arg(args[["frontier-window"]], 5L, "frontier-window", 2L)
seed <- as_integer_arg(args$seed, 1234L, "seed", 1L)
force_k <- args[["force-k"]]
if (!is.null(force_k)) force_k <- as_integer_arg(force_k, NA_integer_, "force-k", 1L)
cores <- as_integer_arg(args$cores, 1L, "cores", 1L)
stem <- as_logical_arg(args$stem, FALSE)
overwrite <- as_logical_arg(args$overwrite, FALSE)
max_docfreq_prop <- as.numeric(args[["max-docfreq-prop"]] %||% 0.85)

if (is.na(max_docfreq_prop) || max_docfreq_prop <= 0 || max_docfreq_prop > 1) {
  stop("--max-docfreq-prop precisa estar no intervalo (0, 1].", call. = FALSE)
}

if (!prevalence_covariate %in% c("year", "genus", "region", "subtribe")) {
  stop("--prevalence-covariate precisa ser 'year', 'genus', 'region' ou 'subtribe'.", call. = FALSE)
}

if (!input_format %in% c("auto", "scopus_bib", "csv")) {
  stop("--input-format precisa ser auto, scopus_bib ou csv.", call. = FALSE)
}

if (!file.exists(input_file)) {
  stop("Arquivo de entrada nao encontrado: ", input_file, call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Algumas funcoes do stm usam graficos base internamente; manter um dispositivo
# temporario aberto evita a criacao acidental de Rplots.pdf no diretorio raiz.
base_plot_sink <- tempfile(pattern = "stm_base_plots_", fileext = ".pdf")
grDevices::pdf(base_plot_sink)
on.exit({
  while (grDevices::dev.cur() > 1) {
    grDevices::dev.off()
  }
  unlink(base_plot_sink)
}, add = TRUE)

output_paths <- file.path(
  output_dir,
  c(
    "stm_corpus_metadata.csv",
    "stm_searchk_results.csv",
    "stm_taxon_stopwords.csv",
    "stm_extra_stopwords.csv",
    "stm_genus_assignments.csv",
    "stm_searchk_diagnostics.png",
    "stm_final_model.rds",
    "stm_searchk_object.rds",
    "stm_topic_labels.csv",
    "stm_topic_quality.csv",
    "stm_document_topics.csv",
    "stm_topic_genus_prevalence.csv",
    "stm_topics_by_genus.png",
    "stm_topic_year_prevalence.csv",
    "stm_topics_over_time.png",
    "stm_frontier_topics.csv",
    "stm_frontier_topics.png"
  )
)

if (!overwrite && any(file.exists(output_paths))) {
  stop(
    "Algumas saidas ja existem em ", output_dir, ". ",
    "Use --overwrite para sobrescrever.",
    call. = FALSE
  )
}

set.seed(seed)

if (input_format == "auto") {
  input_format <- if (grepl("\\.csv$", input_file, ignore.case = TRUE)) "csv" else "scopus_bib"
}

if (input_format == "csv") {
  message("Importando corpus CSV: ", input_file)
  csv <- readr::read_csv(input_file, show_col_types = FALSE)
  required_fields <- c("title", "year")
  missing_fields <- setdiff(required_fields, names(csv))
  if (length(missing_fields) > 0) {
    stop("Campos obrigatorios ausentes no CSV: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  metadata <- csv %>%
    mutate(
      doc_id = row_number(),
      title = safe_text(title),
      abstract = safe_text(if ("abstract" %in% names(.)) abstract else ""),
      author_keywords = safe_text(if ("author_keywords" %in% names(.)) author_keywords else ""),
      index_keywords = safe_text(if ("index_keywords" %in% names(.)) index_keywords else ""),
      year = suppressWarnings(as.integer(year)),
      journal = safe_text(if ("journal" %in% names(.)) journal else ""),
      doi = safe_text(if ("doi" %in% names(.)) doi else ""),
      source_database = safe_text(if ("source_database" %in% names(.)) source_database else ""),
      text = str_squish(paste(title, abstract, author_keywords, index_keywords, sep = ". "))
    ) %>%
    filter(!is.na(year), nzchar(text)) %>%
    mutate(year_scaled = as.numeric(scale(year)))
} else {
  message("Importando BibTeX Scopus: ", input_file)
  bib <- bibliometrix::convert2df(input_file, dbsource = "scopus", format = "bibtex")

  required_fields <- c("TI", "AB", "PY")
  missing_fields <- setdiff(required_fields, names(bib))
  if (length(missing_fields) > 0) {
    stop("Campos obrigatorios ausentes no BibTeX convertido: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }

  metadata <- bib %>%
    mutate(
      doc_id = row_number(),
      title = safe_text(if ("TI_raw" %in% names(.)) TI_raw else TI),
      abstract = safe_text(if ("AB_raw" %in% names(.)) AB_raw else AB),
      author_keywords = safe_text(if ("DE_raw" %in% names(.)) DE_raw else if ("DE" %in% names(.)) DE else ""),
      index_keywords = safe_text(if ("ID_raw" %in% names(.)) ID_raw else if ("ID" %in% names(.)) ID else ""),
      year = suppressWarnings(as.integer(PY)),
      journal = safe_text(if ("SO" %in% names(.)) SO else ""),
      doi = safe_text(if ("DI" %in% names(.)) DI else ""),
      source_database = "Scopus",
      text = str_squish(paste(title, abstract, author_keywords, index_keywords, sep = ". "))
    ) %>%
    filter(!is.na(year), nzchar(text)) %>%
    mutate(year_scaled = as.numeric(scale(year)))
}

genus_names <- extract_genus_names(species_file)
genus_assignments <- detect_document_genera(metadata$text, genus_names)
metadata <- bind_cols(metadata, genus_assignments) %>%
  mutate(genus_for_stm = collapse_sparse_genera(genus_raw, min_docs = genus_min_docs))

if (!is.null(document_covariates_file) && nzchar(document_covariates_file)) {
  if (!file.exists(document_covariates_file)) {
    stop("Arquivo de covariaveis por documento nao encontrado: ", document_covariates_file, call. = FALSE)
  }

  message("Importando covariaveis por documento: ", document_covariates_file)
  document_covariates <- readr::read_csv(document_covariates_file, show_col_types = FALSE)

  if ("article_id" %in% names(metadata) && "article_id" %in% names(document_covariates)) {
    document_covariates <- document_covariates %>%
      mutate(article_id = suppressWarnings(as.integer(article_id))) %>%
      select(-any_of(c("title", "year", "doi", "journal", "abstract", "author_keywords", "index_keywords")))
    metadata <- metadata %>%
      mutate(article_id = suppressWarnings(as.integer(article_id))) %>%
      left_join(document_covariates, by = "article_id")
  } else if ("doc_id" %in% names(document_covariates)) {
    document_covariates <- document_covariates %>%
      mutate(doc_id = suppressWarnings(as.integer(doc_id))) %>%
      select(-any_of(c("title", "year", "doi", "journal", "abstract", "author_keywords", "index_keywords")))
    metadata <- metadata %>%
      left_join(document_covariates, by = "doc_id")
  } else {
    stop(
      "O arquivo de covariaveis precisa conter article_id ou doc_id para unir aos documentos.",
      call. = FALSE
    )
  }
}

if ("region_for_stm" %in% names(metadata)) {
  metadata <- metadata %>%
    mutate(
      region_for_stm = safe_text(region_for_stm),
      region_for_stm = if_else(nzchar(region_for_stm), region_for_stm, "Unassigned"),
      region_for_stm = factor(
        region_for_stm,
        levels = unique(c("Neotropical", "Indo-Australasian", "Afrotropical", "Multi-region", "Unassigned", region_for_stm))
      )
    )
}

if ("subtribe_for_stm" %in% names(metadata)) {
  metadata <- metadata %>%
    mutate(
      subtribe_for_stm = safe_text(subtribe_for_stm),
      subtribe_for_stm = if_else(nzchar(subtribe_for_stm), subtribe_for_stm, "Unassigned"),
      subtribe_for_stm = factor(
        subtribe_for_stm,
        levels = unique(c("Meliponina", "Hypotrigonina", "Multi-subtribe", "Unassigned", subtribe_for_stm))
      )
    )
}

if (nrow(metadata) < 20) {
  stop("Corpus muito pequeno depois da filtragem: ", nrow(metadata), " documentos.", call. = FALSE)
}

readr::write_csv(metadata, file.path(output_dir, "stm_corpus_metadata.csv"))
genus_assignment_columns <- intersect(
  c(
    "doc_id", "article_id", "title", "year", "doi", "source_database",
    "genus_raw", "genus_for_stm", "n_genera_mentioned", "mentioned_genera",
    "region_for_stm", "subtribe_for_stm", "article_region_assignment",
    "article_subtribe_assignment", "studied_genera", "n_studied_genera"
  ),
  names(metadata)
)
readr::write_csv(
  metadata %>%
    select(all_of(genus_assignment_columns)),
  file.path(output_dir, "stm_genus_assignments.csv")
)

message("Generos detectados na planilha: ", length(genus_names))
message("Distribuicao da covariavel de genero:")
print(sort(table(metadata$genus_for_stm), decreasing = TRUE))

custom_stopwords <- c(
  "article", "articles", "author", "authors", "copyright", "elsevier",
  "springer", "wiley", "frontiers", "rights", "reserved",
  "study", "studies", "result", "results", "using", "used", "also",
  "show", "showed", "found", "first", "new", "different", "among",
  "bee", "bees", "stingless", "meliponini", "meliponinae", "apidae",
  "hymenoptera", "melipona", "species", "genus", "genera"
)

taxon_stopwords <- extract_taxon_stopwords(species_file)
extra_stopwords <- read_extra_stopwords(extra_stopwords_file)
custom_stopwords <- unique(c(custom_stopwords, taxon_stopwords, extra_stopwords))

readr::write_csv(
  tibble(stopword = sort(taxon_stopwords)),
  file.path(output_dir, "stm_taxon_stopwords.csv")
)
readr::write_csv(
  tibble(stopword = sort(extra_stopwords)),
  file.path(output_dir, "stm_extra_stopwords.csv")
)

message(
  "Removendo ", length(taxon_stopwords),
  " termos taxonomicos de generos/especies a partir de: ", species_file
)
if (length(extra_stopwords) > 0) {
  message(
    "Removendo ", length(extra_stopwords),
    " stopwords adicionais a partir de: ", extra_stopwords_file
  )
}

message("Processando texto para STM: ", nrow(metadata), " documentos")
processed <- stm::textProcessor(
  documents = metadata$text,
  metadata = metadata,
  lowercase = TRUE,
  removestopwords = TRUE,
  removenumbers = TRUE,
  removepunctuation = TRUE,
  stem = stem,
  wordLengths = c(3, Inf),
  sparselevel = 1,
  language = "en",
  customstopwords = custom_stopwords,
  verbose = FALSE
)

manual_drop <- drop_vocab_terms(processed$documents, processed$vocab, custom_stopwords)
processed$documents <- manual_drop$documents
processed$vocab <- manual_drop$vocab
if (length(manual_drop$dropped) > 0) {
  message(
    "Remocao adicional direta no vocabulario: ",
    length(manual_drop$dropped), " termos encontrados."
  )
}

max_docfreq <- max(min_docfreq + 1L, floor(nrow(metadata) * max_docfreq_prop))
prepared <- stm::prepDocuments(
  processed$documents,
  processed$vocab,
  processed$meta,
  lower.thresh = min_docfreq,
  upper.thresh = max_docfreq,
  verbose = FALSE
)

documents <- prepared$documents
vocab <- prepared$vocab
meta <- prepared$meta %>%
  mutate(doc_index = row_number())

if ("region_for_stm" %in% names(meta)) {
  meta <- meta %>%
    mutate(region_for_stm = droplevels(factor(region_for_stm)))
}

if ("subtribe_for_stm" %in% names(meta)) {
  meta <- meta %>%
    mutate(subtribe_for_stm = droplevels(factor(subtribe_for_stm)))
}

if (length(documents) < 20 || length(vocab) < 30) {
  stop(
    "Corpus insuficiente depois da preparacao: ",
    length(documents), " documentos e ", length(vocab), " termos. ",
    "Tente reduzir --min-docfreq.",
    call. = FALSE
  )
}

k_values <- k_values[k_values < length(documents)]
if (length(k_values) == 0) {
  stop("Nenhum K valido. Use valores menores que o numero de documentos preparados.", call. = FALSE)
}

if (prevalence_covariate == "genus") {
  if (nlevels(meta$genus_for_stm) < 2) {
    stop(
      "A covariavel de genero tem menos de dois niveis depois da preparacao. ",
      "Tente reduzir --genus-min-docs ou revise a deteccao de generos.",
      call. = FALSE
    )
  }
  prevalence_formula <- ~ genus_for_stm
  message("Covariavel de prevalencia do STM: genus_for_stm")
  message("Niveis usados: ", paste(levels(meta$genus_for_stm), collapse = ", "))
} else if (prevalence_covariate == "region") {
  if (!"region_for_stm" %in% names(meta)) {
    stop(
      "A covariavel region_for_stm nao esta disponivel. ",
      "Forneca --document-covariates-file com essa coluna.",
      call. = FALSE
    )
  }
  if (nlevels(meta$region_for_stm) < 2) {
    stop("A covariavel region_for_stm tem menos de dois niveis depois da preparacao.", call. = FALSE)
  }
  prevalence_formula <- ~ region_for_stm
  message("Covariavel de prevalencia do STM: region_for_stm")
  message("Niveis usados: ", paste(levels(meta$region_for_stm), collapse = ", "))
} else if (prevalence_covariate == "subtribe") {
  if (!"subtribe_for_stm" %in% names(meta)) {
    stop(
      "A covariavel subtribe_for_stm nao esta disponivel. ",
      "Forneca --document-covariates-file com essa coluna.",
      call. = FALSE
    )
  }
  if (nlevels(meta$subtribe_for_stm) < 2) {
    stop("A covariavel subtribe_for_stm tem menos de dois niveis depois da preparacao.", call. = FALSE)
  }
  prevalence_formula <- ~ subtribe_for_stm
  message("Covariavel de prevalencia do STM: subtribe_for_stm")
  message("Niveis usados: ", paste(levels(meta$subtribe_for_stm), collapse = ", "))
} else {
  prevalence_formula <- ~ s(year)
  message("Covariavel de prevalencia do STM: s(year)")
}

message(
  "Corpus preparado: ", length(documents), " documentos, ",
  length(vocab), " termos. Testando K = ", paste(k_values, collapse = ", ")
)
searchk <- stm::searchK(
  documents = documents,
  vocab = vocab,
  K = k_values,
  prevalence = prevalence_formula,
  data = meta,
  init.type = "Spectral",
  cores = cores,
  heldout.seed = seed,
  verbose = FALSE
)

searchk_results <- as_tibble(lapply(searchk$results, function(x) {
  if (is.list(x) && all(lengths(x) == 1L)) {
    return(unlist(x, use.names = FALSE))
  }
  x
})) %>%
  mutate(
    score_semcoh = if ("semcoh" %in% names(.)) normalize_metric(semcoh, TRUE) else NA_real_,
    score_exclusivity = if ("exclus" %in% names(.)) normalize_metric(exclus, TRUE) else NA_real_,
    score_heldout = if ("heldout" %in% names(.)) normalize_metric(heldout, TRUE) else NA_real_,
    score_residual = if ("residual" %in% names(.)) normalize_metric(residual, FALSE) else NA_real_,
    score_bound = if ("bound" %in% names(.)) normalize_metric(bound, TRUE) else NA_real_,
    composite_score = rowMeans(
      pick(starts_with("score_")),
      na.rm = TRUE
    )
  )

if (all(is.na(searchk_results$composite_score))) {
  stop("Nao foi possivel calcular score composto para escolha de K.", call. = FALSE)
}

composite_best_k <- searchk_results$K[which.max(searchk_results$composite_score)]
best_k <- composite_best_k
if (!is.null(force_k) && !is.na(force_k)) {
  if (!force_k %in% searchk_results$K) {
    stop("--force-k precisa estar presente em --k-values para manter diagnosticos comparaveis.", call. = FALSE)
  }
  best_k <- force_k
}
searchk_results <- searchk_results %>%
  mutate(
    composite_rank = rank(-composite_score, ties.method = "first"),
    selected_for_final_model = K == best_k,
    selected_by_composite_score = K == composite_best_k
  )
readr::write_csv(searchk_results, file.path(output_dir, "stm_searchk_results.csv"))
saveRDS(searchk, file.path(output_dir, "stm_searchk_object.rds"))

numeric_searchk_cols <- names(searchk_results)[vapply(searchk_results, is.numeric, logical(1))]
searchk_plot_cols <- intersect(
  c("heldout", "residual", "semcoh", "exclus", "bound", "lbound", "em.its", "composite_score"),
  numeric_searchk_cols
)

searchk_long <- searchk_results %>%
  select(K, all_of(searchk_plot_cols)) %>%
  pivot_longer(-K, names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    metric = recode(
      metric,
      heldout = "Held-out likelihood",
      residual = "Residual dispersion",
      semcoh = "Semantic coherence",
      exclus = "Exclusivity",
      bound = "Lower bound",
      lbound = "Lower bound",
      em.its = "EM iterations",
      composite_score = "Composite score"
    )
  )

ggplot(searchk_long, aes(x = K, y = value)) +
  geom_line(linewidth = 0.4, color = "#2f4858") +
  geom_point(size = 1.8, color = "#2f4858") +
  geom_vline(xintercept = best_k, linetype = "dashed", color = "#b23a48") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "Diagnostics for selecting the number of topics",
    subtitle = if (best_k == composite_best_k) {
      paste("K selected by the composite score:", best_k)
    } else {
      paste("Final K:", best_k, "| best composite-score K:", composite_best_k)
    },
    x = "Number of topics (K)",
    y = NULL
  ) +
  theme_minimal(base_size = 11)
ggsave(file.path(output_dir, "stm_searchk_diagnostics.png"), width = 10, height = 7, dpi = 300)

message("Ajustando modelo final com K = ", best_k)
if (best_k != composite_best_k) {
  message("Observacao: K = ", composite_best_k, " teve maior score composto nesta execucao, mas --force-k selecionou K = ", best_k)
}
final_model <- stm::stm(
  documents = documents,
  vocab = vocab,
  K = best_k,
  prevalence = prevalence_formula,
  data = meta,
  init.type = "Spectral",
  seed = seed,
  verbose = FALSE
)

saveRDS(
  list(
    model = final_model,
    documents = documents,
    vocab = vocab,
    meta = meta,
    best_k = best_k,
    composite_best_k = composite_best_k,
    args = args
  ),
  file.path(output_dir, "stm_final_model.rds")
)

labels <- stm::labelTopics(final_model, n = top_words)
topic_labels <- tibble(topic = seq_len(best_k)) %>%
  mutate(
    prob = apply(labels$prob, 1, paste, collapse = ", "),
    frex = apply(labels$frex, 1, paste, collapse = ", "),
    lift = apply(labels$lift, 1, paste, collapse = ", "),
    score = apply(labels$score, 1, paste, collapse = ", "),
    short_label = paste0("T", topic, ": ", vapply(strsplit(frex, ", "), function(x) paste(head(x, 3), collapse = " / "), character(1)))
  )
readr::write_csv(topic_labels, file.path(output_dir, "stm_topic_labels.csv"))

quality <- tibble(
  topic = seq_len(best_k),
  semantic_coherence = stm::semanticCoherence(final_model, documents = documents, M = top_words),
  exclusivity = stm::exclusivity(final_model, M = top_words)
) %>%
  left_join(select(topic_labels, topic, short_label), by = "topic")
readr::write_csv(quality, file.path(output_dir, "stm_topic_quality.csv"))

doc_topic_metadata_columns <- intersect(
  c(
    "doc_id", "article_id", "title", "year", "source_database",
    "genus_raw", "genus_for_stm", "mentioned_genera",
    "region_for_stm", "subtribe_for_stm", "article_region_assignment",
    "article_subtribe_assignment", "studied_genera", "n_studied_genera",
    "journal", "doi"
  ),
  names(meta)
)

doc_topics <- as_tibble(final_model$theta, .name_repair = "minimal") %>%
  setNames(paste0("topic_", seq_len(best_k))) %>%
  mutate(doc_index = row_number(), .before = 1) %>%
  bind_cols(select(meta, all_of(doc_topic_metadata_columns)))
readr::write_csv(doc_topics, file.path(output_dir, "stm_document_topics.csv"))

topic_genus <- as_tibble(final_model$theta, .name_repair = "minimal") %>%
  setNames(paste0("topic_", seq_len(best_k))) %>%
  mutate(genus_for_stm = meta$genus_for_stm) %>%
  pivot_longer(starts_with("topic_"), names_to = "topic_id", values_to = "prevalence") %>%
  mutate(topic = as.integer(sub("^topic_", "", topic_id))) %>%
  group_by(genus_for_stm, topic) %>%
  summarise(
    mean_prevalence = mean(prevalence),
    n_documents = n(),
    .groups = "drop"
  ) %>%
  group_by(topic) %>%
  mutate(
    topic_mean = mean(mean_prevalence),
    prevalence_lift = mean_prevalence - topic_mean
  ) %>%
  ungroup() %>%
  left_join(select(topic_labels, topic, short_label), by = "topic") %>%
  arrange(genus_for_stm, topic)
readr::write_csv(topic_genus, file.path(output_dir, "stm_topic_genus_prevalence.csv"))

genus_order <- topic_genus %>%
  distinct(genus_for_stm, n_documents) %>%
  arrange(desc(n_documents), genus_for_stm) %>%
  pull(genus_for_stm)

ggplot(
  topic_genus %>%
    mutate(
      genus_for_stm = factor(genus_for_stm, levels = genus_order),
      topic_label = factor(short_label, levels = rev(topic_labels$short_label))
    ),
  aes(x = genus_for_stm, y = topic_label, fill = mean_prevalence)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "C", labels = function(x) paste0(round(x * 100), "%")) +
  labs(
    title = "Prevalencia media dos topicos por genero detectado",
    subtitle = "Categorias raras foram agregadas conforme --genus-min-docs; Multiple indica documentos com mais de um genero mencionado",
    x = "Genero/categoria",
    y = "Topico",
    fill = "Prevalencia"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
ggsave(file.path(output_dir, "stm_topics_by_genus.png"), width = 13, height = 8, dpi = 300)

topic_year <- as_tibble(final_model$theta, .name_repair = "minimal") %>%
  setNames(paste0("topic_", seq_len(best_k))) %>%
  mutate(year = meta$year) %>%
  pivot_longer(starts_with("topic_"), names_to = "topic_id", values_to = "prevalence") %>%
  mutate(topic = as.integer(sub("^topic_", "", topic_id))) %>%
  group_by(year, topic) %>%
  summarise(
    mean_prevalence = mean(prevalence),
    n_documents = n(),
    .groups = "drop"
  ) %>%
  complete(
    year = seq(min(meta$year), max(meta$year)),
    topic = seq_len(best_k),
    fill = list(mean_prevalence = 0, n_documents = 0)
  ) %>%
  left_join(select(topic_labels, topic, short_label), by = "topic") %>%
  arrange(topic, year) %>%
  group_by(topic) %>%
  mutate(
    prevalence_smooth = as.numeric(stats::filter(mean_prevalence, rep(1 / 3, 3), sides = 2)),
    prevalence_smooth = if_else(is.na(prevalence_smooth), mean_prevalence, prevalence_smooth)
  ) %>%
  ungroup()
readr::write_csv(topic_year, file.path(output_dir, "stm_topic_year_prevalence.csv"))

recent_start <- max(meta$year) - frontier_window + 1L
frontier_topics <- topic_year %>%
  filter(year >= recent_start) %>%
  group_by(topic, short_label) %>%
  summarise(
    recent_mean_prevalence = mean(mean_prevalence),
    recent_slope = if (n_distinct(year) >= 2) coef(lm(mean_prevalence ~ year))[[2]] else NA_real_,
    current_prevalence = mean_prevalence[which.max(year)],
    current_year = max(year),
    .groups = "drop"
  ) %>%
  arrange(desc(recent_slope), desc(current_prevalence)) %>%
  mutate(frontier_rank = row_number(), .before = 1)
readr::write_csv(frontier_topics, file.path(output_dir, "stm_frontier_topics.csv"))

top_frontier_candidates <- frontier_topics %>%
  filter(!is.na(recent_slope), recent_slope > 0)
top_frontier <- top_frontier_candidates %>%
  slice_head(n = min(6, nrow(top_frontier_candidates)))

topic_year_plot <- topic_year %>%
  mutate(
    topic_label = factor(short_label, levels = topic_labels$short_label),
    frontier = topic %in% top_frontier$topic
  )

ggplot(topic_year_plot, aes(x = year, y = prevalence_smooth, group = topic)) +
  geom_line(aes(color = frontier, linewidth = frontier), alpha = 0.85) +
  scale_color_manual(values = c(`FALSE` = "#9aa0a6", `TRUE` = "#b23a48"), guide = "none") +
  scale_linewidth_manual(values = c(`FALSE` = 0.35, `TRUE` = 0.9), guide = "none") +
  facet_wrap(~ topic_label, scales = "free_y") +
  labs(
    title = "Prevalencia media dos topicos ao longo do tempo",
    subtitle = paste0("Linhas destacadas: topicos com inclinacao positiva mais forte nos ultimos ", frontier_window, " anos"),
    x = "Ano",
    y = "Prevalencia media estimada"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(hjust = 0))
ggsave(file.path(output_dir, "stm_topics_over_time.png"), width = 13, height = 9, dpi = 300)

ggplot(
  topic_year %>% semi_join(top_frontier, by = "topic") %>% mutate(topic_label = factor(short_label, levels = top_frontier$short_label)),
  aes(x = year, y = prevalence_smooth, color = topic_label)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  labs(
    title = "Temas de fronteira em genetica/genomica de Meliponini",
    subtitle = paste0("Topicos com maior tendencia positiva desde ", recent_start),
    x = "Ano",
    y = "Prevalencia media estimada",
    color = "Topico"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
ggsave(file.path(output_dir, "stm_frontier_topics.png"), width = 10, height = 6, dpi = 300)

message("Concluido.")
message("K selecionado: ", best_k)
message("Principais saidas:")
message("  - ", file.path(output_dir, "stm_searchk_results.csv"))
message("  - ", file.path(output_dir, "stm_topic_labels.csv"))
message("  - ", file.path(output_dir, "stm_topics_over_time.png"))
message("  - ", file.path(output_dir, "stm_frontier_topics.csv"))
message("  - ", file.path(output_dir, "stm_frontier_topics.png"))
