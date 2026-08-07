# Genetic and genomic research on stingless bees: data and reproducible analysis

This repository contains the curated data, analysis scripts, and derived outputs supporting the manuscript **“Overview of genetic and genomic research related to stingless bees (Meliponini): an AI-assisted science mapping and structural topic modeling analysis.”**

The study is a systematic evidence map and bibliometric synthesis of genetic and genomic research on stingless bees. The final corpus combines Scopus and Web of Science records and contains 410 peer-reviewed articles published from 1950 through April 2026. Structural topic modeling (STM) is used to quantify thematic, temporal, taxonomic, biogeographic, and subtribal patterns in the publication record.

## Repository contents

```text
.
├── data/
│   ├── merged/                         # Final corpus and article-level covariates
│   ├── article_counts_ai_merged_scopus_wos_articles_only/
│   │                                      # Human-audited AI-assisted taxon classifications
│   ├── article_counts_merged_scopus_wos_articles_only/
│   │                                      # Taxon-level article-count summaries
│   ├── phylo/                          # Source phylogeny
│   ├── Meliponini_species.xlsx         # Species reference list
│   ├── stm_extra_stopwords_localities_residuals.txt
│   └── stm_topic_labels_merged_scopus_wos_articles_only_k10.csv
├── results/
│   ├── article_final_merged_scopus_wos_articles_only/
│   │                                      # STM models, diagnostics, tables, and figures
│   └── phylo_genus_merged_scopus_wos_articles_only/
│                                          # Genus-level tree, audit data, and heatmaps
├── scripts/                            # Retrieval, curation, modeling, and plotting code
├── Meliponini_genus_list.xlsx          # Genus reference list
├── LICENSE
└── README.md
```

## Analysis-ready inputs

- `data/merged/merged_scopus_wos_articles_only_genetics_genomics_corpus_20260503.csv`: final curated corpus used by the analysis.
- `data/merged/article_taxon_covariates_scopus_wos_articles_only_20260503.csv`: article-level taxonomic, biogeographic, and subtribal covariates.
- `data/article_counts_ai_merged_scopus_wos_articles_only/taxon_studied_ai_classifications.jsonl`: archived, human-audited article–taxon classifications.
- `Meliponini_genus_list.xlsx` and `data/Meliponini_species.xlsx`: reference lists used for taxonomic matching and stopword construction.
- `data/phylo/meliponini_lepeco2024_opentree.tre`: source phylogeny used for genus-level mapping.

The primary key used to connect the final corpus, audited classifications, and covariate files is `article_id`. Files containing aggregated counts are derived products and are not required to rerun the main workflow.

## Reproduce the final analysis

### Requirements

- R 4.1 or later
- A POSIX-compatible shell
- Internet access only when installing packages; the final analysis itself uses local archived inputs

Install the required R packages:

```r
install.packages(c(
  "ape",
  "bibliometrix",
  "dplyr",
  "ggplot2",
  "jsonlite",
  "purrr",
  "readr",
  "readxl",
  "stm",
  "stringr",
  "tibble",
  "tidyr"
))
```

From the repository root, run:

```sh
bash scripts/run_article_final_analysis.sh
```

The workflow uses a fixed random seed (`2026`) and rebuilds:

1. article-level taxonomic, biogeographic, and subtribal covariates;
2. the final `K = 10` STM with `s(year)` as the prevalence specification;
3. sensitivity diagnostics for `K = 5, 8, 10, 12, 15, 20, 25, 30`;
4. topic labels, topic-prevalence tables, and manuscript figures;
5. region- and subtribe-covariate STM summaries; and
6. genus-level phylogenetic mapping of research effort.

Existing result directories are replaced only when the workflow calls the relevant script with its explicit overwrite option. Run the workflow in a clean clone if the original archived outputs must be retained unchanged for comparison.

## Main outputs

- `results/article_final_merged_scopus_wos_articles_only/stm_time_k10/`: final publication-year STM, document-topic proportions, diagnostics, and temporal summaries.
- `results/article_final_merged_scopus_wos_articles_only/stm_region_k10/`: STM fitted with biogeographic region as the prevalence covariate.
- `results/article_final_merged_scopus_wos_articles_only/stm_subtribe_k10/`: STM fitted with subtribe as the prevalence covariate.
- `results/article_final_merged_scopus_wos_articles_only/`: analysis-ready tables and publication figures.
- `results/phylo_genus_merged_scopus_wos_articles_only/`: genus-level phylogenetic audit, tree, edge data, and heatmaps.

## AI-assisted curation and provenance

AI was used for two curation tasks: eligibility screening and distinguishing taxa actually studied from taxa mentioned only as context. Both procedures used structured outputs and explicit decision criteria. Their implementation is preserved in:

- `scripts/classify_stingless_bee_genomics_two_stage_openai.R`
- `scripts/classify_taxon_studied_openai.R`

Model outputs were checked and corrected by the authors. The audited outputs used in the analyses are archived under `data/article_counts_ai_merged_scopus_wos_articles_only/`.

The main reproduction workflow intentionally does not call the OpenAI API. External models and provider behavior can change, and an API rerun would require credentials and might not reproduce the audited decisions exactly. Reproducibility of the reported analyses therefore begins from the archived human-audited classifications. The API scripts are retained to document provenance and decision rules, not as a required step for reproducing the published results.

## Optional provenance workflows

The remaining scripts document upstream record retrieval, deduplication, metadata enrichment, and corpus construction. They are not required to reproduce the final analyses from the archived inputs. Some require additional packages (`httr2`, `openxlsx`, or `rscopus`), database subscriptions, and API credentials supplied through environment variables. No credentials are stored in this repository.

Because Scopus, Web of Science, OpenAlex, and model-provider records can change, rerunning an upstream retrieval workflow may produce a corpus that differs from the archived April 2026 corpus.

## Data considerations

Bibliographic records were obtained from Scopus and Web of Science. Users are responsible for complying with the applicable database licenses and terms of use when redistributing or reusing record-level metadata. The repository contains no API keys or authentication tokens.

## License

Code and repository materials are distributed under the terms in `LICENSE`. Third-party bibliographic metadata and source phylogenies may remain subject to their original licenses or terms of use.
