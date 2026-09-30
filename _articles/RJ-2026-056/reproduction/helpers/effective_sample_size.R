effective_sample_size <- function(samples) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    message(
      "Package 'posterior' is required to compute effective sample size (ESS).\n",
      "Install it with install.packages('posterior') to enable ESS diagnostics."
    )
    return(NA_real_)
  }

  as.numeric(posterior::ess_basic(as.numeric(samples)))
}
