# Overview of genetic and genomic research related to stingless bees

This repository contains the curated data, scripts, and final outputs used in the manuscript:

**Overview of genetic and genomic research related to stingless bees (Meliponini): an AI-assisted science mapping and structural topic modeling analysis**

The study is a systematic evidence map and bibliometric synthesis of genetic and genomic research on stingless bees. The final corpus combines Scopus and Web of Science records, retains 410 peer-reviewed articles published between 1950 and April 2026, and uses structural topic modelling (STM) to describe thematic, temporal, taxonomic, biogeographic, and subtribal patterns in the literature.

## Authors

Larissa de Oliveira Rosa Marques; Jamira Dias Rocha; Leonardo Carlos Jeronimo Corvalan; Jullia Costa dos Reis; Cintia Pelegrineti Targueta; Pedro Vale de Azevedo Brito; Carlos de Melo e Silva Neto; Thiago Mafra Batista; Mariana Pires de Campos Telles; Renata de Oliveira Dias; Rhewter Nunes.

## Repository Organization

```text
.
|-- data/
|   |-- merged/
|   |-- article_counts_ai_merged_scopus_wos_articles_only/
|   |-- article_counts_merged_scopus_wos_articles_only/
|   |-- phylo/
|   |-- Meliponini_species.xlsx
|   |-- stm_extra_stopwords_localities_residuals.txt
|   `-- stm_topic_labels_merged_scopus_wos_articles_only_k10.csv
|-- results/
|   |-- article_final_merged_scopus_wos_articles_only/
|   `-- phylo_genus_merged_scopus_wos_articles_only/
|-- scripts/
|-- Meliponini_genus_list.xlsx
|-- LICENSE
`-- README.md
```

## Main Inputs

- `data/merged/merged_scopus_wos_articles_only_genetics_genomics_corpus_20260503.csv`: final curated corpus used in the manuscript.
- `data/merged/article_taxon_covariates_scopus_wos_articles_only_20260503.csv`: article-level taxonomic, biogeographic, and subtribal covariates.
- `data/article_counts_ai_merged_scopus_wos_articles_only/taxon_studied_ai_classifications.jsonl`: audited AI-assisted classification of taxa actually studied in each article.
- `Meliponini_genus_list.xlsx` and `data/Meliponini_species.xlsx`: genus and species reference lists used for taxonomic matching and stopword construction.
- `data/phylo/meliponini_lepeco2024_opentree.tre`: Meliponini phylogeny used for genus-level phylogenetic mapping.

## Main Outputs

- `results/article_final_merged_scopus_wos_articles_only/`: article-ready STM outputs, tables, and figures.
- `results/article_final_merged_scopus_wos_articles_only/stm_time_k10/`: final STM model with publication year as the prevalence covariate.
- `results/article_final_merged_scopus_wos_articles_only/stm_region_k10/`: STM run with biogeographic region as the prevalence covariate.
- `results/article_final_merged_scopus_wos_articles_only/stm_subtribe_k10/`: STM run with subtribe as the prevalence covariate.
- `results/phylo_genus_merged_scopus_wos_articles_only/`: genus-level phylogenetic tree, audit tables, and heatmap files.

## Reproducing the Final Analysis

The final reproducible workflow starts from the curated Scopus + Web of Science articles-only corpus and the audited taxon classifications. From the repository root, run:

```sh
scripts/run_article_final_analysis.sh
```

This script rebuilds:

- article-level taxonomic, biogeographic, and subtribal covariates;
- the final STM with `K = 10` and `s(year)` as the prevalence covariate;
- K-selection diagnostics for `K = 5, 8, 10, 12, 15, 20, 25, 30`;
- article-ready topic labels, topic prevalence tables, and figures;
- STM summaries by biogeographic region and subtribe;
- genus-level phylogenetic mapping of research effort.

## Software Requirements

The final workflow uses R scripts. Install the required R packages before running `scripts/run_article_final_analysis.sh`:

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

The final workflow does not require API keys. Optional provenance scripts that retrieve or classify bibliographic records additionally use `httr2`, `openxlsx`, and `rscopus`, and may require Scopus, Web of Science, or OpenAI credentials.

## AI-Assisted Steps

The manuscript used AI-assisted screening and taxon-classification steps, followed by human checking, correction, and validation. The audited outputs of those steps are included in `data/article_counts_ai_merged_scopus_wos_articles_only/`.

The final analysis script does not rerun the OpenAI API classification by default because it requires an API key and may change with model/provider updates. Scripts for those provenance steps are retained in `scripts/` for transparency, but the manuscript analyses should be reproduced from the audited classification files included here.

## Files Intentionally Not Included

This repository was curated from a larger working repository. Exploratory analyses, superseded STM runs, tests, temporary files, working drafts, and intermediate outputs not used in the final manuscript were intentionally excluded to keep the public repository focused on reviewer access and scientific reproducibility.

## License

The repository is distributed under the license specified in `LICENSE`.

## Disclaimer

The scripts, data-processing workflow, and derived outputs in this repository are provided for transparency and scientific reproducibility. They are distributed as is, without warranties of correctness, fitness for a particular purpose, or continued maintenance. The authors are not responsible for errors, omissions, or consequences arising from the use, modification, or redistribution of these materials. Users are responsible for independently validating the outputs for their own purposes.
