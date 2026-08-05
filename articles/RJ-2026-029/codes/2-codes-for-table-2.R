## ---------------------------------------------------------------
## Table 2: Comparison of ANN algorithms for blocking in deduplication
## ---------------------------------------------------------------
## This script reproduces Table 2 from the paper, which compares
## various approximate nearest neighbour (ANN) algorithms for
## creating blocks in a deduplication setting. Methods compared:
##   - Five ANN algorithms from the {blocking} package:
##     nnd, hnsw, annoy, lsh, kd
##   - Two configurations of k-means LSH from the {klsh} package:
##     klsh with 10 blocks and klsh with 100 blocks
##
## Dataset: RLdata500 (500 records with 50 true duplicates)
## Metrics: precision, FPR, FNR, accuracy, F1 score
## Required packages: blocking, data.table, knitr, klsh
## ---------------------------------------------------------------

library(blocking)
library(data.table)
library(knitr)

## -- Load and prepare the data ----------------------------------

data("RLdata500")
setDT(RLdata500)

## Count records per true entity (to identify duplicates)
RLdata500[, id_count := .N, ent_id]

## Create a single blocking key string per record by concatenating
## all name and date fields (lowercased, zero-padded month/day)
RLdata500[, txt := tolower(paste0(
  fname_c1, fname_c2, lname_c1, lname_c2, by,
  sprintf("%02d", bm), sprintf("%02d", bd)
))]

## -- Compute total pair counts for evaluation -------------------

## Number of true matching pairs (combinations within each entity)
n_pairs_true <- sum(table(RLdata500$ent_id) * (table(RLdata500$ent_id) - 1) / 2)

## Total number of record pairs
n_pairs_all <- NROW(RLdata500) * (NROW(RLdata500) - 1) / 2

## -- Prepare ground-truth block structure -----------------------

set.seed(2025)
true_blocks <- RLdata500[, c("rec_id", "ent_id"), with = FALSE]
setnames(true_blocks, old = c("rec_id", "ent_id"), c("x", "block"))

## -- Evaluate ANN algorithms from {blocking} --------------------
## Loop over five ANN backends: nnd, hnsw, annoy, lsh, kd
## Each produces blocking results evaluated against true_blocks

eval_metrics <- list()
ann <- c("nnd", "hnsw", "annoy", "lsh", "kd")

for (algorithm in ann) {
  res <- blocking(
    x = RLdata500$txt,
    ann = algorithm,
    true_blocks = true_blocks
  )
  eval_metrics[[algorithm]] <- res$metrics
  cat("\n=== Confusion Matrix:", algorithm, "===\n")
  print(res$confusion)
}

## -- Evaluate klsh with 10 blocks --------------------------------
## Uses k-means locality-sensitive hashing (klsh) on raw fields
## p=20 random projections, k=2 hash functions, 10 target blocks

blocks_klsh_10 <- klsh::klsh(
  r.set = RLdata500[, c("fname_c1", "fname_c2", "lname_c1",
                        "lname_c2", "by", "bm", "bd")],
  p = 20, num.blocks = 10, k = 2
)

## Extract precision/recall from klsh's own evaluation function
klsh_10_metrics <- klsh::confusion.from.blocking(
  blocking = blocks_klsh_10,
  true_ids = RLdata500$ent_id
)[-1]

## Reconstruct the confusion matrix from precision and recall
c_rec  <- klsh_10_metrics$recall
c_prec <- klsh_10_metrics$precision
val_tp <- round(c_rec * n_pairs_true)
val_fn <- n_pairs_true - val_tp
val_fp <- if (c_prec == 0) 0 else round(val_tp * (1 / c_prec - 1))
val_tn <- n_pairs_all - (val_tp + val_fp + val_fn)

cat("\n=== Confusion Matrix: klsh_10 ===\n")
print(matrix(c(val_tp, val_fn, val_fp, val_tn), nrow = 2, byrow = TRUE,
             dimnames = list(c("Actual Positive", "Actual Negative"),
                             c("Predicted Positive", "Predicted Negative"))))

## Compute F1 and store metrics
klsh_10_metrics$f1_score <- with(klsh_10_metrics,
                                 2 * precision * recall / (precision + recall))
eval_metrics$klsh_10 <- unlist(klsh_10_metrics)

## -- Evaluate klsh with 100 blocks -------------------------------
## Same setup as above but with finer blocking (100 target blocks)

blocks_klsh_100 <- klsh::klsh(
  r.set = RLdata500[, c("fname_c1", "fname_c2", "lname_c1",
                        "lname_c2", "by", "bm", "bd")],
  p = 20, num.blocks = 100, k = 2
)

klsh_100_metrics <- klsh::confusion.from.blocking(
  blocking = blocks_klsh_100,
  true_ids = RLdata500$ent_id
)[-1]

c_rec  <- klsh_100_metrics$recall
c_prec <- klsh_100_metrics$precision
val_tp <- round(c_rec * n_pairs_true)
val_fn <- n_pairs_true - val_tp
val_fp <- if (c_prec == 0) 0 else round(val_tp * (1 / c_prec - 1))
val_tn <- n_pairs_all - (val_tp + val_fp + val_fn)

cat("\n=== Confusion Matrix: klsh_100 ===\n")
print(matrix(c(val_tp, val_fn, val_fp, val_tn), nrow = 2, byrow = TRUE,
             dimnames = list(c("Actual Positive", "Actual Negative"),
                             c("Predicted Positive", "Predicted Negative"))))

klsh_100_metrics$f1_score <- with(klsh_100_metrics,
                                  2 * precision * recall / (precision + recall))
eval_metrics$klsh_100 <- unlist(klsh_100_metrics)

## -- Table 2: Final comparison -----------------------------------
## Combine all metrics and display selected columns as percentages

round(do.call(rbind, eval_metrics) * 100, 2)[
  , c("precision", "fpr", "fnr", "accuracy", "f1_score")] |>
  kable(digits = 2)
