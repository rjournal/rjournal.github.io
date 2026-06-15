
pacman::p_load(readr, dplyr, tidyr, ggplot2, scales, ggh4x)

# load outputs

res_cens <- read_csv("censoring_sensitivity_raw.csv", show_col_types = FALSE)
summary_cens <- read_csv("censoring_sensitivity_summary.csv", show_col_types = FALSE)
oracle_cens <- read_csv("censoring_sensitivity_oracle.csv", show_col_types = FALSE)

res_bench <- read_csv("bench_threads_raw.csv", show_col_types = FALSE)
summary_bench <- read_csv("bench_threads_summary.csv", show_col_types = FALSE)
speedup_bench <- read_csv("bench_threads_speedup.csv", show_col_types = FALSE)

metric_lab <- c(cindex = "C-index", ibs = "Integrated Brier Score")


# plot 1: censoring sensitivity (use achieved censoring on x)

loss_levels <- unique(res_cens$loss)

sum_plot <- sum_plot %>%
  group_by(cens_target) %>%
  mutate(
    x_disc = factor(
      cens_target,
      levels = sort(unique(cens_target)),
      labels = percent_format(accuracy = 1)(sort(unique(cens_target)))
    )
  ) %>%
  ungroup()

p_cens <- ggplot(
  sum_plot2,
  aes(x = x_disc, y = value_mean, color = loss, group = loss)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    nrow = 1,
    labeller = labeller(metric = metric_lab)
  ) +
  ggh4x::facetted_pos_scales(
    y = list(
      scale_y_continuous(limits = c(0.60, 0.85)),  # c-index panel
      scale_y_continuous(limits = c(0.16, 0.30))   # ibs panel (note: 0.16 not 1.6)
    )
  ) +
  labs(
    x = "censoring level (train)",
    y = "mean across replications"
    #title = "Censoring Sensitivity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("plot_censoring_sensitivity.png", p_cens, width = 12, height = 4.5, dpi = 300)

# plot 2: CPU threads benchmark (speedup curve: 1 thread / 12 threads)

speed_plot <- speedup_bench %>%
  mutate(
    loss = factor(loss, levels = loss_levels)
  )

p_speed <- ggplot(
  speed_plot,
  aes(x = n, y = speedup_train, color = loss, group = loss)
) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.8) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_x_continuous(
    breaks = seq(0, max(speed_plot$n), by = 1000)
  ) +
  labs(
    x = "sample size",
    y = "multithreading (1 vs 12 threads)" ## median
    #title = "CPU multithreading speedup (torch)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("plot_bench_threads_speedup.png", p_speed, width = 8, height = 4.5, dpi = 300)

# optional: quick check prints
print(p_cens)
print(p_speed)
