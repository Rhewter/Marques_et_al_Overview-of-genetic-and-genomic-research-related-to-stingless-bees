#!/usr/bin/env Rscript

# Uses AI to determine whether an article actually studies a taxon
# (genus or species) rather than merely mentioning it. The script starts from
# candidates detected through taxonomic matching and produces
# auditable species- and genus-level counts.

required_packages <- c(
  "bibliometrix",
  "dplyr",
  "httr2",
  "jsonlite",
  "openxlsx",
  "purrr",
  "readr",
  "readxl",
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
    paste(sprintf('\"%s\"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(bibliometrix)
  library(dplyr)
  library(httr2)
  library(jsonlite)
  library(openxlsx)
  library(purrr)
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

safe_text <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  x
}

normalize_spaces <- function(x) {
  stringr::str_squish(safe_text(x))
}

truncate_text <- function(x, max_chars = 2200) {
  x <- normalize_spaces(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars), " [...]"), x)
}

compact_unique <- function(x, max_items = Inf) {
  x <- unique(x[!is.na(x) & nzchar(as.character(x))])
  if (length(x) == 0) return("")
  if (is.finite(max_items) && length(x) > max_items) {
    return(paste0(paste(head(x, max_items), collapse = "; "), "; ..."))
  }
  paste(x, collapse = "; ")
}

read_jsonl <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) return(tibble())
  lines <- readLines(path, warn = FALSE)
  rows <- lapply(lines[nzchar(lines)], function(line) {
    jsonlite::fromJSON(line, simplifyVector = FALSE)
  })
  bind_rows(lapply(rows, as_tibble))
}

append_jsonl <- function(rows, path) {
  con <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  for (i in seq_len(nrow(rows))) {
    writeLines(jsonlite::toJSON(as.list(rows[i, ]), auto_unbox = TRUE, null = "null"), con)
  }
}

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

args <- parse_args(commandArgs(trailingOnly = TRUE))

if (isTRUE(args$help) || isTRUE(args$h)) {
  cat(
    paste0(
      "Usage:\n",
      "  Rscript scripts/classify_taxon_studied_openai.R [options]\n\n",
      "Options:\n",
      "  --bib=FILE              Scopus BibTeX file.\n",
      "  --input-csv=FILE        Alternative CSV containing title, abstract, year, journal, and doi.\n",
      "  --candidate-workbook=XLSX  Workbook produced by count_articles_by_meliponini_genus.R.\n",
      "  --species-xlsx=FILE     Species workbook. Default: data/Meliponini_species.xlsx\n",
      "  --genus-xlsx=FILE       Genus workbook. Default: Meliponini_genus_list.xlsx\n",
      "  --output-dir=DIR           Output directory. Default: data/article_counts_ai\n",
      "  --model=MODEL             OpenAI model. Default: gpt-4.1-mini\n",
      "  --batch-size=N             Article-taxon pairs per request. Default: 12\n",
      "  --max-candidates=N         Limit candidates; useful for testing.\n",
      "  --api-key=KEY            OpenAI key; OPENAI_API_KEY may also be used.\n"
    )
  )
  quit(status = 0)
}

bib_file <- args$bib %||%
  "data/scopus_export_May_2-2026_a80702bd-6141-4355-8ab3-3ee911a5ead3_Meliponini_genetics_and_genomics.bib"
input_csv <- args[["input-csv"]]
candidate_workbook <- args[["candidate-workbook"]] %||%
  "data/article_counts/meliponini_article_counts_by_genus.xlsx"
species_file <- args[["species-xlsx"]] %||% "data/Meliponini_species.xlsx"
genus_file <- args[["genus-xlsx"]] %||% "Meliponini_genus_list.xlsx"
output_dir <- args[["output-dir"]] %||% "data/article_counts_ai"
model <- args$model %||% "gpt-4.1-mini"
batch_size <- as.integer(args[["batch-size"]] %||% 12L)
max_candidates <- args[["max-candidates"]]
if (!is.null(max_candidates)) max_candidates <- as.integer(max_candidates)
api_key <- args[["api-key"]] %||% Sys.getenv("OPENAI_API_KEY")

if (identical(api_key, "")) {
  stop("API key not found. Set OPENAI_API_KEY or use --api-key.", call. = FALSE)
}
if (is.na(batch_size) || batch_size < 1 || batch_size > 30) {
  stop("--batch-size must be between 1 and 30.", call. = FALSE)
}
if (!is.null(max_candidates) && (is.na(max_candidates) || max_candidates < 1)) {
  stop("--max-candidates must be a positive integer.", call. = FALSE)
}
if (!file.exists(candidate_workbook)) stop("Candidate workbook not found: ", candidate_workbook, call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
checkpoint_file <- file.path(output_dir, "taxon_studied_ai_classifications.jsonl")

if (!is.null(input_csv)) {
  if (!file.exists(input_csv)) stop("CSV not found: ", input_csv, call. = FALSE)
  message("Importando corpus CSV: ", input_csv)
  csv <- readr::read_csv(input_csv, show_col_types = FALSE)
  docs <- csv %>%
    mutate(
      article_id = row_number(),
      title = normalize_spaces(title),
      abstract = normalize_spaces(if ("abstract" %in% names(.)) abstract else ""),
      author_keywords = normalize_spaces(if ("author_keywords" %in% names(.)) author_keywords else ""),
      index_keywords = normalize_spaces(if ("index_keywords" %in% names(.)) index_keywords else ""),
      year = suppressWarnings(as.integer(if ("year" %in% names(.)) year else NA_integer_)),
      doi = normalize_spaces(if ("doi" %in% names(.)) doi else ""),
      journal = normalize_spaces(if ("journal" %in% names(.)) journal else "")
    ) %>%
    select(article_id, title, abstract, author_keywords, index_keywords, year, journal, doi)
} else {
  message("Importando corpus BibTeX: ", bib_file)
  bib <- bibliometrix::convert2df(bib_file, dbsource = "scopus", format = "bibtex")
  docs <- bib %>%
    mutate(
      article_id = row_number(),
      title = normalize_spaces(if ("TI_raw" %in% names(.)) TI_raw else TI),
      abstract = normalize_spaces(if ("AB_raw" %in% names(.)) AB_raw else if ("AB" %in% names(.)) AB else ""),
      author_keywords = normalize_spaces(if ("DE_raw" %in% names(.)) DE_raw else if ("DE" %in% names(.)) DE else ""),
      index_keywords = normalize_spaces(if ("ID_raw" %in% names(.)) ID_raw else if ("ID" %in% names(.)) ID else ""),
      year = suppressWarnings(as.integer(if ("PY" %in% names(.)) PY else NA_integer_)),
      doi = normalize_spaces(if ("DI" %in% names(.)) DI else ""),
      journal = normalize_spaces(if ("SO" %in% names(.)) SO else "")
    ) %>%
    select(article_id, title, abstract, author_keywords, index_keywords, year, journal, doi)
}

species_matches <- readxl::read_excel(candidate_workbook, sheet = "species_article_matches") %>%
  transmute(
    article_id = as.integer(article_id),
    taxon_rank = "species",
    taxon_name = scientific_name,
    genus = genus,
    candidate_source = compact_unique(match_type)
  ) %>%
  distinct(article_id, taxon_rank, taxon_name, genus, .keep_all = TRUE)

genus_matches <- readxl::read_excel(candidate_workbook, sheet = "genus_name_article_matches") %>%
  transmute(
    article_id = as.integer(article_id),
    taxon_rank = "genus",
    taxon_name = genus,
    genus = genus,
    candidate_source = "genus_name"
  ) %>%
  distinct(article_id, taxon_rank, taxon_name, genus, .keep_all = TRUE)

candidates <- bind_rows(species_matches, genus_matches) %>%
  distinct(article_id, taxon_rank, taxon_name, genus, .keep_all = TRUE) %>%
  arrange(article_id, taxon_rank, taxon_name) %>%
  mutate(candidate_id = row_number(), .before = 1) %>%
  left_join(docs, by = "article_id") %>%
  mutate(
    title = truncate_text(title, 450),
    abstract = truncate_text(abstract, 2000),
    author_keywords = truncate_text(author_keywords, 450),
    index_keywords = truncate_text(index_keywords, 450)
  )

if (!is.null(max_candidates)) {
  candidates <- candidates %>% slice_head(n = max_candidates)
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
          "candidate_id", "article_id", "taxon_rank", "taxon_name",
          "study_relationship", "count_as_studied", "confidence",
          "evidence", "rationale"
        ),
        properties = list(
          candidate_id = list(type = "integer"),
          article_id = list(type = "integer"),
          taxon_rank = list(type = "string", enum = list("species", "genus")),
          taxon_name = list(type = "string"),
          study_relationship = list(
            type = "string",
            enum = list("focal_studied", "included_studied", "contextual_mention", "not_about_taxon", "unclear")
          ),
          count_as_studied = list(type = "boolean"),
          confidence = list(type = "string", enum = list("high", "medium", "low")),
          evidence = list(type = "string"),
          rationale = list(type = "string")
        )
      )
    )
  )
)

system_prompt <- paste(
  "You are assisting a systematic review of genetics/genomics studies on stingless bees (Meliponini).",
  "Task: for each candidate article-taxon pair, decide whether the article actually STUDIES that taxon, not merely mentions it.",
  "Use only title, abstract, author keywords, and indexed keywords supplied.",
  "count_as_studied=true when the taxon is a focal organism or one of the organisms/samples/data analyzed in the article.",
  "For species: count if that species is sampled, sequenced, genetically analyzed, cytogenetically analyzed, included in phylogeny/barcoding/population genetics/microbiome/pathogen/honey/nest molecular analysis, or used as an explicit study organism.",
  "For genus: count if the article studies the genus as a group or studies one or more species belonging to that genus.",
  "count_as_studied=false when the taxon is only background, a comparison example, a taxonomic list, a database/reference mention, or appears only in a broad keyword list without evidence it was analyzed.",
  "Use study_relationship='focal_studied' for the main taxon/taxa, 'included_studied' for taxa included in multi-taxon analyses, 'contextual_mention' for background/comparison mentions, 'not_about_taxon' for false positives, and 'unclear' when evidence is insufficient.",
  "Return concise evidence copied or closely paraphrased from the supplied text.",
  sep = "\n"
)

call_openai <- function(batch) {
  records <- batch %>%
    transmute(
      candidate_id,
      article_id,
      taxon_rank,
      taxon_name,
      genus,
      candidate_source,
      title,
      abstract,
      author_keywords,
      index_keywords
    )

  body <- list(
    model = model,
    input = list(
      list(role = "system", content = system_prompt),
      list(
        role = "user",
        content = paste(
          "Classify these candidate article-taxon pairs. Return exactly one classification per candidate_id.",
          jsonlite::toJSON(list(candidates = records), auto_unbox = TRUE, null = "null", pretty = TRUE),
          sep = "\n\n"
        )
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "taxon_studied_classifier",
        strict = TRUE,
        schema = schema
      )
    )
  )

  last_error <- NULL
  for (attempt in seq_len(4)) {
    parsed <- tryCatch({
      response <- httr2::request("https://api.openai.com/v1/responses") |>
        httr2::req_headers(Authorization = paste("Bearer", api_key)) |>
        httr2::req_body_json(body, auto_unbox = TRUE) |>
        httr2::req_timeout(120) |>
        httr2::req_perform()
      httr2::resp_body_json(response, simplifyVector = FALSE)
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })

    if (!is.null(parsed)) {
      output_text <- extract_output_text(parsed)
      decoded <- tryCatch(jsonlite::fromJSON(output_text, simplifyVector = TRUE), error = function(e) {
        last_error <<- paste("JSON parse error:", conditionMessage(e), "Output:", substr(output_text, 1, 500))
        NULL
      })
      if (!is.null(decoded) && !is.null(decoded$classifications)) {
        return(as_tibble(decoded$classifications))
      }
    }
    Sys.sleep(2 ^ attempt)
  }

  stop("Failed to call OpenAI after retries. Last error: ", last_error, call. = FALSE)
}

existing <- read_jsonl(checkpoint_file)
done_ids <- if (nrow(existing) > 0) unique(existing$candidate_id) else integer()
remaining <- candidates %>% filter(!candidate_id %in% done_ids)

message("Candidatos totais: ", nrow(candidates))
message("Ja classificados no checkpoint: ", length(done_ids))
message("Restantes: ", nrow(remaining))

if (nrow(remaining) > 0) {
  batches <- split(remaining, ceiling(seq_len(nrow(remaining)) / batch_size))
  for (i in seq_along(batches)) {
    message("Batch ", i, "/", length(batches), " (", nrow(batches[[i]]), " candidates)")
    result <- call_openai(batches[[i]]) %>%
      mutate(
        candidate_id = as.integer(candidate_id),
        article_id = as.integer(article_id),
        count_as_studied = as.logical(count_as_studied)
      )
    append_jsonl(result, checkpoint_file)
    Sys.sleep(0.2)
  }
}

classifications <- read_jsonl(checkpoint_file) %>%
  mutate(
    candidate_id = as.integer(candidate_id),
    article_id = as.integer(article_id),
    count_as_studied = as.logical(count_as_studied)
  ) %>%
  filter(candidate_id %in% candidates$candidate_id) %>%
  distinct(candidate_id, .keep_all = TRUE)

classified_candidates <- candidates %>%
  select(candidate_id, article_id, taxon_rank, taxon_name, genus, candidate_source, title, year, journal, doi) %>%
  left_join(classifications, by = c("candidate_id", "article_id", "taxon_rank", "taxon_name")) %>%
  arrange(taxon_rank, genus, taxon_name, article_id)

species_ai_counts <- classified_candidates %>%
  filter(taxon_rank == "species") %>%
  group_by(genus, scientific_name = taxon_name) %>%
  summarise(
    n_articles_ai_studied_species = n_distinct(article_id[count_as_studied %in% TRUE]),
    n_candidate_articles_species = n_distinct(article_id),
    studied_article_ids = compact_unique(as.character(article_id[count_as_studied %in% TRUE]), max_items = 100),
    studied_titles = compact_unique(title[count_as_studied %in% TRUE], max_items = 20),
    .groups = "drop"
  )

species_raw <- readxl::read_excel(species_file)
species_ai_summary <- species_raw %>%
  left_join(species_ai_counts, by = c("Nome científico" = "scientific_name", "Gênero" = "genus")) %>%
  mutate(
    n_articles_ai_studied_species = replace_na(n_articles_ai_studied_species, 0),
    n_candidate_articles_species = replace_na(n_candidate_articles_species, 0),
    studied_article_ids = replace_na(studied_article_ids, ""),
    studied_titles = replace_na(studied_titles, "")
  )

genus_species_counts <- classified_candidates %>%
  filter(taxon_rank == "species", count_as_studied %in% TRUE) %>%
  group_by(genus) %>%
  summarise(
    n_articles_ai_studied_species_in_genus = n_distinct(article_id),
    n_species_ai_studied_in_genus = n_distinct(taxon_name),
    ai_studied_species = compact_unique(taxon_name, max_items = 80),
    ai_studied_article_ids_from_species = compact_unique(as.character(article_id), max_items = 120),
    .groups = "drop"
  )

genus_direct_counts <- classified_candidates %>%
  filter(taxon_rank == "genus", count_as_studied %in% TRUE) %>%
  group_by(genus) %>%
  summarise(
    n_articles_ai_studied_genus_direct = n_distinct(article_id),
    ai_studied_article_ids_direct_genus = compact_unique(as.character(article_id), max_items = 120),
    .groups = "drop"
  )

genus_combined_counts <- bind_rows(
  classified_candidates %>%
    filter(taxon_rank == "species", count_as_studied %in% TRUE) %>%
    select(genus, article_id),
  classified_candidates %>%
    filter(taxon_rank == "genus", count_as_studied %in% TRUE) %>%
    select(genus, article_id)
) %>%
  distinct() %>%
  group_by(genus) %>%
  summarise(
    n_articles_ai_studied_genus_combined = n_distinct(article_id),
    ai_studied_article_ids_combined = compact_unique(as.character(article_id), max_items = 150),
    .groups = "drop"
  )

genus_raw <- readxl::read_excel(genus_file)
genus_ai_summary <- genus_raw %>%
  rename(genus = `Gênero`) %>%
  left_join(genus_species_counts, by = "genus") %>%
  left_join(genus_direct_counts, by = "genus") %>%
  left_join(genus_combined_counts, by = "genus") %>%
  mutate(
    n_articles_ai_studied_species_in_genus = replace_na(n_articles_ai_studied_species_in_genus, 0),
    n_species_ai_studied_in_genus = replace_na(n_species_ai_studied_in_genus, 0),
    n_articles_ai_studied_genus_direct = replace_na(n_articles_ai_studied_genus_direct, 0),
    n_articles_ai_studied_genus_combined = replace_na(n_articles_ai_studied_genus_combined, 0),
    ai_studied_species = replace_na(ai_studied_species, ""),
    ai_studied_article_ids_from_species = replace_na(ai_studied_article_ids_from_species, ""),
    ai_studied_article_ids_direct_genus = replace_na(ai_studied_article_ids_direct_genus, ""),
    ai_studied_article_ids_combined = replace_na(ai_studied_article_ids_combined, "")
  ) %>%
  arrange(desc(n_articles_ai_studied_genus_combined), genus)

method_notes <- tibble(
  item = c(
    "primary_species_count",
    "primary_genus_count",
    "candidate_generation",
    "count_as_studied_true",
    "limitations"
  ),
  description = c(
    "n_articles_ai_studied_species: unique articles where AI judged that the species was studied or included in analyzed data.",
    "n_articles_ai_studied_genus_combined: conservative genus count combining species-level studied evidence and direct genus-level studied evidence.",
    "Candidates were generated by species-binomial/contextual-abbreviation and genus-name regex matches before AI review.",
    "The AI counted focal and included taxa, but rejected background-only/contextual mentions.",
    "Classification uses title, abstract, and keywords only; full texts may change some decisions."
  )
)

output_workbook <- file.path(output_dir, "meliponini_ai_studied_taxa_counts.xlsx")
openxlsx::write.xlsx(
  list(
    genus_ai_counts = genus_ai_summary,
    species_ai_counts = species_ai_summary,
    ai_classified_candidates = classified_candidates,
    method_notes = method_notes
  ),
  output_workbook,
  overwrite = TRUE
)

utils::write.csv(genus_ai_summary, file.path(output_dir, "meliponini_ai_studied_genus_counts.csv"), row.names = FALSE)
utils::write.csv(species_ai_summary, file.path(output_dir, "meliponini_ai_studied_species_counts.csv"), row.names = FALSE)

message("Complete.")
message("Main output: ", output_workbook)
message("Use n_articles_ai_studied_species for species.")
message("Use n_articles_ai_studied_genus_combined for genera.")
