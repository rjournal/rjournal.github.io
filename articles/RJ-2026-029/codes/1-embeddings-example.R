## ---------------------------------------------------------------
## Blocking for record linkage using the {blocking} R package
## ---------------------------------------------------------------
## This script demonstrates how to use the {blocking} package to
## perform blocking for record linkage on a dataset of foreigners.
## Three approaches are compared:
##   1. Text-based blocking with default 2-character shingles
##   2. Text-based blocking with 3-character shingles
##   3. Embedding-based blocking using a local Ollama model
##
## Required packages: blocking, data.table, ragnar
## Required setup: local Ollama instance with "embeddinggemma" model
## ---------------------------------------------------------------

library(blocking)
library(data.table)
library(ragnar)

## -- Load and prepare the data ----------------------------------

## Built-in dataset with duplicate records of foreigners
data("foreigners")
setDT(foreigners)

## Concatenate identity fields into a single text string for blocking
foreigners[, txt := paste(fname, sname, surname, date, country, region)]

## -- Generate embeddings via local Ollama -----------------------

foreigners_emb <- embed_ollama(
  data.frame(text = foreigners$txt),
  base_url = "http://localhost:11434",
  model = "embeddinggemma",
  batch_size = 10L
)

## -- Split data into reference (x) and query (y) sets -----------

## Count how many records share the same true identity
foreigners[, count := .N, true_id]
foreigners[, row_id := 1:.N]

## Reference set: one record per true identity (first occurrence)
foreigners_sub <- foreigners[, head(.SD, 1), true_id]

## Query set: all remaining (duplicate) records
foreigners_query <- foreigners[!row_id %in% foreigners_sub$row_id]

## -- Build the ground-truth block structure ----------------------
## Maps each query record (y) to its matching reference record (x)
## based on the known true_id; used to evaluate blocking quality

foreigners_matches <- merge(
  x = foreigners_sub[, .(x = 1:.N, true_id)],
  y = foreigners_query[, .(y = 1:.N, true_id)],
  by = "true_id"
)
foreigners_matches[, block := as.numeric(as.factor(true_id))]

## -- Approach 1: text blocking with default shingles (k=2) ------

set.seed(2026)
foreigners_shingles_result <- blocking(
  x = foreigners_sub$txt,
  y = foreigners_query$txt,
  true_blocks = foreigners_matches[, .(x, y, block)],
  n_threads = 8,
  seed = 2026
)

## -- Approach 2: text blocking with 3-character shingles ---------

foreigners_shingles_result3 <- blocking(
  x = foreigners_sub$txt,
  y = foreigners_query$txt,
  true_blocks = foreigners_matches[, .(x, y, block)],
  n_threads = 8,
  seed = 2026,
  control_txt = controls_txt(n_shingles = 3L)
)

## -- Approach 3: embedding-based blocking ------------------------
## Uses pre-computed Ollama embeddings instead of raw text

foreigners_sub_emb   <- foreigners_emb$embedding[foreigners_sub$row_id, ]
foreigners_query_emb <- foreigners_emb$embedding[foreigners_query$row_id, ]

colnames(foreigners_sub_emb) <- colnames(foreigners_query_emb) <-
  paste0("v", 1:ncol(foreigners_query_emb))

foreigners_emb_result <- blocking(
  x = foreigners_sub_emb,
  y = foreigners_query_emb,
  true_blocks = foreigners_matches[, .(x, y, block)],
  n_threads = 8,
  seed = 2026
)

## -- Compare results ---------------------------------------------

foreigners_shingles_result
foreigners_shingles_result3
foreigners_emb_result
