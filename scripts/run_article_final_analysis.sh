#!/usr/bin/env bash
set -euo pipefail

# Reproducible final analysis workflow for the Meliponini genetics/genomics
# review. This script starts from the curated Scopus + WoS articles-only corpus
# and the already-audited AI taxon classifications. Re-running the OpenAI
# classification itself is kept as a separate optional step because it requires
# an API key and can change with model/provider updates.

CORPUS="data/merged/merged_scopus_wos_articles_only_genetics_genomics_corpus_20260503.csv"
AI_DIR="data/article_counts_ai_merged_scopus_wos_articles_only"
COVARIATES="data/merged/article_taxon_covariates_scopus_wos_articles_only_20260503.csv"
TOPIC_LABELS="data/stm_topic_labels_merged_scopus_wos_articles_only_k10.csv"
STM_DIR="results/article_final_merged_scopus_wos_articles_only/stm_time_k10"
REGION_STM_DIR="results/article_final_merged_scopus_wos_articles_only/stm_region_k10"
SUBTRIBE_STM_DIR="results/article_final_merged_scopus_wos_articles_only/stm_subtribe_k10"
ARTICLE_DIR="results/article_final_merged_scopus_wos_articles_only"
PHYLO_DIR="results/phylo_genus_merged_scopus_wos_articles_only"

if [[ ! -f "$CORPUS" ]]; then
  echo "Missing curated corpus: $CORPUS" >&2
  echo "Run scripts/build_merged_scopus_wos_corpus.R first, or restore the final corpus file." >&2
  exit 1
fi

if [[ ! -f "$AI_DIR/taxon_studied_ai_classifications.jsonl" ]]; then
  echo "Missing audited AI taxon classifications in: $AI_DIR" >&2
  exit 1
fi

Rscript scripts/build_article_taxon_covariates.R \
  --corpus="$CORPUS" \
  --ai-classifications="$AI_DIR/taxon_studied_ai_classifications.jsonl" \
  --genus-workbook=Meliponini_genus_list.xlsx \
  --output="$COVARIATES"

Rscript scripts/run_stm_scopus_bib.R \
  --input="$CORPUS" \
  --input-format=csv \
  --output-dir="$STM_DIR" \
  --species-file=data/Meliponini_species.xlsx \
  --extra-stopwords-file=data/stm_extra_stopwords_localities_residuals.txt \
  --prevalence-covariate=year \
  --k-values=5,8,10,12,15,20,25,30 \
  --force-k=10 \
  --min-docfreq=3 \
  --max-docfreq-prop=0.85 \
  --seed=2026 \
  --cores=1 \
  --overwrite

Rscript scripts/prepare_article_final_outputs.R \
  --stm-dir="$STM_DIR" \
  --output-dir="$ARTICLE_DIR" \
  --topic-labels-file="$TOPIC_LABELS" \
  --genus-workbook=Meliponini_genus_list.xlsx \
  --ai-classifications="$AI_DIR/taxon_studied_ai_classifications.jsonl" \
  --ai-genus-counts="$AI_DIR/meliponini_ai_studied_genus_counts.csv" \
  --frontier-window=5 \
  --top-n-genera=10

Rscript scripts/run_stm_scopus_bib.R \
  --input="$CORPUS" \
  --input-format=csv \
  --document-covariates-file="$COVARIATES" \
  --output-dir="$REGION_STM_DIR" \
  --species-file=data/Meliponini_species.xlsx \
  --extra-stopwords-file=data/stm_extra_stopwords_localities_residuals.txt \
  --prevalence-covariate=region \
  --k-values=10 \
  --force-k=10 \
  --min-docfreq=3 \
  --max-docfreq-prop=0.85 \
  --seed=2026 \
  --cores=1 \
  --overwrite

Rscript scripts/run_stm_scopus_bib.R \
  --input="$CORPUS" \
  --input-format=csv \
  --document-covariates-file="$COVARIATES" \
  --output-dir="$SUBTRIBE_STM_DIR" \
  --species-file=data/Meliponini_species.xlsx \
  --extra-stopwords-file=data/stm_extra_stopwords_localities_residuals.txt \
  --prevalence-covariate=subtribe \
  --k-values=10 \
  --force-k=10 \
  --min-docfreq=3 \
  --max-docfreq-prop=0.85 \
  --seed=2026 \
  --cores=1 \
  --overwrite

Rscript scripts/summarize_stm_covariate_prevalence.R \
  --stm-dir="$REGION_STM_DIR" \
  --output-dir="$ARTICLE_DIR" \
  --topic-labels-file="$TOPIC_LABELS" \
  --covariate=region_for_stm \
  --prefix=stm_region_covariate_k10 \
  --covariate-title="biogeographic region"

Rscript scripts/summarize_stm_covariate_prevalence.R \
  --stm-dir="$REGION_STM_DIR" \
  --output-dir="$ARTICLE_DIR" \
  --topic-labels-file="$TOPIC_LABELS" \
  --covariate=region_for_stm \
  --prefix=stm_region_covariate_k10_single_region \
  --covariate-title="biogeographic region" \
  --keep-levels="Neotropical,Indo-Australasian,Afrotropical"

Rscript scripts/summarize_stm_covariate_prevalence.R \
  --stm-dir="$SUBTRIBE_STM_DIR" \
  --output-dir="$ARTICLE_DIR" \
  --topic-labels-file="$TOPIC_LABELS" \
  --covariate=subtribe_for_stm \
  --prefix=stm_subtribe_covariate_k10 \
  --covariate-title=subtribe

Rscript scripts/summarize_stm_covariate_prevalence.R \
  --stm-dir="$SUBTRIBE_STM_DIR" \
  --output-dir="$ARTICLE_DIR" \
  --topic-labels-file="$TOPIC_LABELS" \
  --covariate=subtribe_for_stm \
  --prefix=stm_subtribe_covariate_k10_main_subtribes \
  --covariate-title=subtribe \
  --keep-levels="Meliponina,Hypotrigonina"

Rscript scripts/build_genus_level_phylogeny.R \
  --input-tree=data/phylo/meliponini_lepeco2024_opentree.tre \
  --counts-file="$AI_DIR/meliponini_ai_studied_taxa_counts.xlsx" \
  --counts-sheet=genus_ai_counts \
  --output-dir="$PHYLO_DIR"

Rscript scripts/plot_genus_phylo_branch_heatmap.R \
  --tree="$PHYLO_DIR/meliponini_genus_level_conservative.tre" \
  --counts-file="$AI_DIR/meliponini_ai_studied_taxa_counts.xlsx" \
  --counts-sheet=genus_ai_counts \
  --count-column=n_articles_ai_studied_genus_combined \
  --output-dir="$PHYLO_DIR" \
  --plot-prefix=meliponini_genus_phylo_branch_heatmap \
  --scale=log1p \
  --basal-compression=1.8 \
  --mark-regions=true
