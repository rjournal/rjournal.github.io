# ============================================================
# Additional simulations (censoring and computing)
# (1) does survdnn performance change with censoring? (cindex, ibs)
# (2) how much speedup do we get from torch cpu threads (1 vs 12)?
# ============================================================

# config -----------------------------------------------------------------------

pacman::p_load(survdnn, survival, torch, tidyverse, functionals)


dgp_a <- 1.5
dgp_b <- 0.05

tau_admin <- 10
censor_rates <- c(0.10, 0.30, 0.50, 0.70, 0.90)
pi_test <- 0.30

sim_r <- 20
sim_ntrain <- 2000
sim_ntest <- 2000
losses <- c("cox", "cox_l2", "aft", "coxtime")

torch_threads_sim <- 4

bench_r <- 3
bench_ns <- c(100, 500, 1000, 10000)
bench_threads <- c(1, 12)
pi_fixed_bench <- 0.30

hp <- list(
  hidden = c(32, 16),
  activation = "relu",
  lr = 1e-4,
  epochs = 300,
  optimizer = "adam",
  optim_args = list(weight_decay = 0),
  dropout = 0.3,
  batch_norm = TRUE,
  callbacks = NULL,
  verbose = FALSE,
  .device = "cpu"
  )

metric_lab <- c(cindex = "c-index", ibs = "integrated brier score")


# progress helper (single-line updater)  ---------------------------------------

progress <- local({
  t0 <- proc.time()[["elapsed"]]
  function(i, n, label = "progress") {
    elapsed <- proc.time()[["elapsed"]] - t0
    rate <- if (i > 0) elapsed / i else NA_real_
    left <- if (!is.na(rate)) (n - i) * rate else NA_real_
    cat(sprintf(
      "\r[%s] %d/%d (%.1f%%) | elapsed %s | eta %s",
      label, i, n, 100 * i / n,
      format(as.difftime(elapsed, units = "secs"), digits = 2),
      ifelse(is.na(left), "na", format(as.difftime(left, units = "secs"), digits = 2))
    ))
    if (i == n) cat("\n")
    invisible(NULL)
  }
  })



# helpers  ---------------------------------------------------------------------

set_threads <- function(torch_threads) {
  torch::torch_set_num_threads(torch_threads)
  if (requireNamespace("rhpcblasctl", quietly = TRUE)) {
    rhpcblasctl::blas_set_num_threads(1)
    rhpcblasctl::omp_set_num_threads(1)
    }
  }

form_surv <- function() {
  survival::Surv(time, status) ~ x1 + x2 + x3 + x4 + x5 + x6
  }

sim_event_times_years <- function(n, seed, a = dgp_a, b = dgp_b) {
  set.seed(seed)
  x1 <- runif(n, -2, 2)
  x2 <- runif(n, -2, 2)
  x3 <- runif(n, -2, 2)
  x4 <- rnorm(n)
  x5 <- rnorm(n)
  x6 <- rbinom(n, 1, 0.5)

  lp <- sin(pi * x1 * x2) + 2 * (x3 - 0.5)^2 + x4 + x5 + x6
  u  <- runif(n)
  t  <- (1 / b) * ((-log(u) / exp(lp))^(1 / a))

  list(
    x  = data.frame(x1, x2, x3, x4, x5, x6),
    t  = as.numeric(t),
    lp = as.numeric(lp)
  )
  }

make_total_censoring <- function(t, tau = 10, pi_target = 0.30, seed = NULL) {
  stopifnot(all(t > 0), tau > 0, pi_target >= 0, pi_target < 1)
  if (!is.null(seed)) set.seed(seed)

  u <- runif(length(t))

  pi_admin <- mean(t > tau)
  if (pi_admin > pi_target + 1e-10) {
    c <- rep(tau, length(t))
    time <- pmin(t, c)
    status <- as.integer(t <= c)
    return(list(time = time, status = status,
                achieved_pi = mean(status == 0),
                lambda_exp = 0))
    }

  pi_hat <- function(lambda) {
    cexp <- if (lambda <= 0) rep(Inf, length(t)) else -log(u) / lambda
    c <- pmin(tau, cexp)
    mean(t > c)
    }

  lambda <- if (abs(pi_target - pi_admin) < 1e-12) {
    0
    } else {
    f <- function(lambda) pi_hat(lambda) - pi_target
    upper <- 0.1
    while (f(upper) < 0) {
      upper <- upper * 2
      if (upper > 1e6) stop("could not bracket root for exp censoring calibration.")
    }
    stats::uniroot(f, lower = 0, upper = upper, tol = 1e-10)$root
    }

  cexp <- if (lambda <= 0) rep(Inf, length(t)) else -log(u) / lambda
  c <- pmin(tau, cexp)

  time <- pmin(t, c)
  status <- as.integer(t <= c)

  list(time = time, status = status,
       achieved_pi = mean(status == 0),
       lambda_exp = lambda)
  }

make_df <- function(x, time, status) {
  d <- x
  d$time <- as.numeric(time)
  d$status <- as.integer(status)
  d
  }

make_times_grid_events <- function(time, status, probs = c(0.2, 0.4, 0.6, 0.8)) {
  ev <- time[status == 1]
  base <- if (length(ev) >= 5) ev else time
  as.numeric(stats::quantile(base, probs = probs, names = FALSE, type = 8))
  }


# (1) censoring sensitivity ----------------------------------------------------

message("\n[1/2] censoring sensitivity simulation...")

set_threads(torch_threads_sim)

run_one_rep <- function(r, seed0 = 1, rep_total = sim_r) {
  seed_r <- seed0 + r

  tr <- sim_event_times_years(sim_ntrain, seed = seed_r)
  te <- sim_event_times_years(sim_ntest,  seed = seed_r + 10000)

  cens_te <- make_total_censoring(te$t, tau = tau_admin, pi_target = pi_test, seed = seed_r + 77777)
  te_df <- make_df(te$x, cens_te$time, cens_te$status)
  times_grid <- make_times_grid_events(te_df$time, te_df$status)

  oracle_fit <- survival::coxph(Surv(time, status) ~ lp_true, data = cbind(te_df, lp_true = te$lp))
  oracle_c <- as.numeric(survival::concordance(oracle_fit)$concordance)
  oracle_row <- tibble(
    rep = r,
    test_cens_achieved = cens_te$achieved_pi,
    oracle_cindex = oracle_c,
    sd_lp_emp_test = sd(te$lp)
    )

  form <- form_surv()

  n_fit <- length(censor_rates) * length(losses)
  fit_i <- 0L
  out <- vector("list", n_fit)

  for (pi in censor_rates) {
    cens_tr <- make_total_censoring(tr$t, tau = tau_admin, pi_target = pi, seed = seed_r + 20000)
    tr_df <- make_df(tr$x, cens_tr$time, cens_tr$status)

    for (loss in losses) {
      fit_i <- fit_i + 1L
      progress(fit_i, n_fit, sprintf("censoring sim rep %d/%d", r, rep_total))

      seed_fit <- seed_r + as.integer(pi * 1000) + match(loss, losses)

      mod <- survdnn(
        formula    = form,
        data       = tr_df,
        hidden     = hp$hidden,
        activation = hp$activation,
        lr         = hp$lr,
        epochs     = hp$epochs,
        loss       = loss,
        optimizer  = hp$optimizer,
        optim_args = hp$optim_args,
        verbose    = hp$verbose,
        dropout    = hp$dropout,
        batch_norm = hp$batch_norm,
        callbacks  = hp$callbacks,
        .seed      = seed_fit,
        .device    = hp$.device,
        na_action  = "fail"
        )

      ev <- evaluate_survdnn(
        model   = mod,
        metrics = c("cindex", "ibs"),
        times   = times_grid,
        newdata = te_df,
        na_action = "fail",
        verbose = FALSE
        )

      out[[fit_i]] <- bind_cols(
        tibble(
          rep = r,
          cens_target = pi,
          cens_achieved = cens_tr$achieved_pi,
          lambda_exp_train = cens_tr$lambda_exp,
          loss = loss,
          test_cens_achieved = cens_te$achieved_pi
        )[rep(1, nrow(ev)), ],
        ev
      )
    }
    }

  list(results = bind_rows(out), oracle = oracle_row)
  }

ans <- map(seq_len(sim_r), function(r) {
  progress(r, sim_r, "censoring sim (rep)")
  run_one_rep(r, rep_total = sim_r)
  })

res_cens <- bind_rows(map(ans, "results"))
oracle_cens <- bind_rows(map(ans, "oracle"))

summary_cens <- res_cens %>%
  group_by(cens_target, loss, metric) %>%
  summarise(
    cens_achieved_mean = mean(cens_achieved),
    cens_achieved_sd   = sd(cens_achieved),
    value_mean = mean(value, na.rm = TRUE),
    value_sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(metric, cens_target, loss)

write_csv(res_cens,     "censoring_sensitivity_raw.csv")
write_csv(summary_cens, "censoring_sensitivity_summary.csv")
write_csv(oracle_cens,  "censoring_sensitivity_oracle.csv")

plot_summary <- summary_cens %>%
  mutate(
    metric = factor(metric, levels = c("cindex", "ibs")),
    loss   = factor(loss, levels = losses)
    )

p_cens <- ggplot(plot_summary, aes(x = cens_target, y = value_mean, color = loss, group = loss)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_errorbar(aes(ymin = value_mean - value_sd, ymax = value_mean + value_sd),
                width = 0.02, linewidth = 0.6) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1, labeller = labeller(metric = metric_lab)) +
  scale_x_continuous(breaks = censor_rates, labels = percent_format(accuracy = 1)) +
  labs(
    x = "target censoring (train)",
    y = "mean ± sd across replications",
    title = "censoring sensitivity (survdnn)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("censoring_sensitivity_mean_sd.png",
       p_cens, width = 12, height = 4.5, dpi = 300)

ach <- res_cens %>%
  group_by(rep, cens_target) %>%
  summarise(achieved = unique(cens_achieved), .groups = "drop") %>%
  group_by(cens_target) %>%
  summarise(mean = mean(achieved), sd = sd(achieved), .groups = "drop")

p_cal <- ggplot(ach, aes(x = cens_target, y = mean)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.8) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.02, linewidth = 0.6) +
  scale_x_continuous(breaks = censor_rates, labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "target censoring",
    y = "achieved censoring (mean ± sd)",
    title = "censoring calibration"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("censoring_sensitivity_calibration.png",
       p_cal, width = 6, height = 4.5, dpi = 300)


# (2) cpu threads benchmark ----------------------------------------------------

message("\n[2/2] cpu threads benchmark (sequential)...")

bench_one <- function(n, loss, threads, rep_id) {

  set_threads(threads)

  seed <- 1000 + 100 * rep_id + n + match(loss, losses) + 10 * threads

  tr <- sim_event_times_years(n, seed = seed)
  cens <- make_total_censoring(tr$t, tau = tau_admin, pi_target = pi_fixed_bench, seed = seed + 999)
  tr_df <- make_df(tr$x, cens$time, cens$status)

  form <- form_surv()
  times_grid <- make_times_grid_events(tr_df$time, tr_df$status)

  t0 <- proc.time()[["elapsed"]]
  mod <- survdnn(
    formula    = form,
    data       = tr_df,
    hidden     = hp$hidden,
    activation = hp$activation,
    lr         = hp$lr,
    epochs     = hp$epochs,
    loss       = loss,
    optimizer  = hp$optimizer,
    optim_args = hp$optim_args,
    verbose    = hp$verbose,
    dropout    = hp$dropout,
    batch_norm = hp$batch_norm,
    callbacks  = hp$callbacks,
    .seed      = seed,
    .device    = hp$.device,
    na_action  = "fail"
    )

  train_s <- proc.time()[["elapsed"]] - t0

  t1 <- proc.time()[["elapsed"]]
  evaluate_survdnn(mod, metrics = c("cindex"), times = times_grid, newdata = tr_df, na_action = "fail")
  eval_s <- proc.time()[["elapsed"]] - t1

  tibble(
    n = n, loss = loss, threads = threads, rep = rep_id,
    cens_achieved = cens$achieved_pi,
    train_s = train_s,
    eval_s = eval_s
  )
  }


grid_bench <- tidyr::expand_grid(
  n = bench_ns,
  loss = losses,
  threads = bench_threads,
  rep = seq_len(bench_r)
  )

n_jobs <- nrow(grid_bench)
res_list <- vector("list", n_jobs)


for (i in seq_len(n_jobs)) {
  progress(i, n_jobs, "cpu bench")
  row <- grid_bench[i, ]
  res_list[[i]] <- bench_one(row$n, row$loss, row$threads, row$rep)
}

res_bench <- bind_rows(res_list)

summary_bench <- res_bench %>%
  group_by(threads, n, loss) %>%
  summarise(
    train_median_s = median(train_s),
    train_iqr_s    = IQR(train_s),
    eval_median_s  = median(eval_s),
    eval_iqr_s     = IQR(eval_s),
    .groups = "drop"
  ) %>%
  arrange(threads, n, loss)

speedup_bench <- summary_bench %>%
  select(threads, n, loss, train_median_s) %>%
  tidyr::pivot_wider(names_from = threads, values_from = train_median_s, names_prefix = "t") %>%
  mutate(speedup_train = t1 / t12) %>%
  arrange(n, loss)

write_csv(res_bench,     "bench_threads_raw.csv")
write_csv(summary_bench, "bench_threads_summary.csv")
write_csv(speedup_bench, "bench_threads_speedup.csv")
writeLines(capture.output(sessionInfo()), "sessioninfo_bench_threads.txt")

sumtbl_plot <- summary_bench %>%
  mutate(
    loss = factor(loss, levels = losses),
    threads = factor(threads, levels = c(1, 12), labels = c("1 thread", "12 threads"))
  )

p_time <- ggplot(sumtbl_plot, aes(x = n, y = train_median_s, color = loss, group = loss)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_wrap(~ threads, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = bench_ns) +
  labs(
    x = "sample size (n)",
    y = "training time (median seconds)",
    title = "single-fit cpu scaling via torch_set_num_threads()"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("bench_threads_train_time_vs_n.png",
       p_time, width = 12, height = 4.5, dpi = 300)

p_speed <- ggplot(speedup_bench %>% mutate(loss = factor(loss, levels = losses)),
                  aes(x = n, y = speedup_train, color = loss, group = loss)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.8) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = bench_ns) +
  labs(
    x = "sample size (n)",
    y = "speedup = median time(1) / median time(12)",
    title = "intra-fit speedup from torch cpu multithreading"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("bench_threads_speedup.png",
       p_speed, width = 8, height = 4.5, dpi = 300)

pu_tbl <- summary_bench %>%
  mutate(
    train_fmt = sprintf("%.2f [%.2f]", train_median_s, train_iqr_s),
    eval_fmt  = sprintf("%.2f [%.2f]", eval_median_s, eval_iqr_s)
  ) %>%
  select(threads, n, loss, train_fmt, eval_fmt) %>%
  tidyr::pivot_wider(
    names_from = threads,
    values_from = c(train_fmt, eval_fmt),
    names_sep = "__"
  ) %>%
  left_join(speedup_bench %>% select(n, loss, speedup_train), by = c("n", "loss")) %>%
  arrange(n, loss)

write_csv(pu_tbl, "bench_threads_pu_table.csv")

message("\ndone completed! (all outputs saved to project root)\n")
