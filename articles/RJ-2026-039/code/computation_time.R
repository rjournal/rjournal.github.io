
library(admix)

# Argument 'i' is the number of samples under consideration, 'size' refers to the sample size
prepare_data <- function(i, size) {
  known_d <- eval(parse(text=paste("list(", paste("g", 1:i, "='norm'", sep = "", collapse = ", "), ")", sep = "")))
  unknown_d <- eval(parse(text=paste("list(", paste("f", 1:i, "='norm'", sep = "", collapse = ", "), ")", sep = "")))
  admixMod <- expr_admixMod <- mix <- sim <- expr_sim <- vector(mode = "list", length = length(known_d))
  list_known_p <- list_unknown_p <- numeric(length = i) 
  for (j in 1:i) {
    list_known_p[j] <- paste("list('mean'=", 2*j, ", 'sd'=1)", sep="")
    list_unknown_p[j] <- paste("list('mean'=0, 'sd'=1)", sep="") 
  }
  known_p <- eval(parse(text=paste("list(", paste("g",1:length(known_d),"=", list_known_p,  sep="",collapse=", "), ")", sep="")))
  unknown_p <- eval(parse(text=paste("list(",paste("f",1:length(unknown_d),"=",list_unknown_p,sep="", collapse=", "),")",sep="")))
  for (j in 1:i) { 
    mix[[j]] <- twoComp_mixt(n = size, weight = 0.4, comp.dist = list(unknown_d[[j]], known_d[[j]]), 
                             comp.param = list(unknown_p[[j]], known_p[[j]]))
    sim[[j]] <- get_mixture_data(mix[[j]])
    expr_admixMod[[j]] <- paste("admix_model(knownComp_dist=known_d[[",j,"]],knownComp_param=known_p[[",j,"]])",sep="")
    admixMod[[j]] <- eval(parse(text = expr_admixMod[[j]]))
  }
  return(list(simulated_data = sim, admix_models = admixMod))
}
nsamples_estim <- function(k) {
  dat <- prepare_data(i = k, size = 2000)
  admix_estim(samples = dat$simulated_data, admixMod = dat$admix_models, est_method = 'PS')
}
nsamples_test <- function(k) {
  dat <- prepare_data(i = k, size = 2000)
  admix_test(samples = dat$simulated_data, admixMod = dat$admix_models, test_method = 'icv',
             tune_penalty = F, n_sim_tab = 100, parallel = T, n_cpu = 8)
}
nsamples_clustering <- function(k) {
  dat <- prepare_data(i = k, size = 2000)
  admix_cluster(samples = dat$simulated_data, admixMod = dat$admix_models, 
                tune_penalty = F, echo = F, n_sim_tab = 100, parallel = T, n_cpu = 8)
}
size_estim <- function(n) {
  dat <- prepare_data(i = 3, size = n)
  admix_estim(samples = dat$simulated_data, admixMod = dat$admix_models, est_method = 'PS')
}
size_test <- function(n) {
  dat <- prepare_data(i = 3, size = n)
  admix_test(samples = dat$simulated_data, admixMod = dat$admix_models, test_method = 'icv',
             tune_penalty = F, n_sim_tab = 100, parallel = T, n_cpu = 8)
}
size_clustering <- function(n) {
  dat <- prepare_data(i = 3, size = n)
  admix_cluster(samples = dat$simulated_data, admixMod = dat$admix_models, 
                tune_penalty = F, echo = F, n_sim_tab = 100, parallel = T, n_cpu = 8)
}

# Study the computation cost depending on the number of samples considered:
n_samples <- 2:9
nsamples_times_estim <- nsamples_times_test <- nsamples_times_cluster <- vector(mode = "numeric", length = length(n_samples))
for (k in 1:length(n_samples)) {
  print(k)
  nsamples_times_estim[k] <- system.time(nsamples_estim(n_samples[k]))[3]
  nsamples_times_test[k] <- system.time(nsamples_test(n_samples[k]))[3]
  nsamples_times_cluster[k] <- system.time(nsamples_clustering(n_samples[k]))[3]
}
# Now, we set K=3 and see how the sample size affects the computation time:
samp_size <- c(500, 2000, 10000, 50000)
size_times_estim <- size_times_test <- size_times_cluster <- vector(mode = "numeric", length = length(samp_size))
for (k in 1:length(samp_size)) {
  print(k)
  size_times_estim[k] <- system.time(size_estim(samp_size[k]))[3]
  size_times_test[k] <- system.time(size_test(samp_size[k]))[3]
  size_times_cluster[k] <- system.time(size_clustering(samp_size[k]))[3]
}

#par(mar = c(2.5,3.8,1.5,1.5), mfrow = c(1,2))

df_nsamples <- data.frame(x = n_samples, y1 = nsamples_times_estim, y2 = nsamples_times_test, y3 = nsamples_times_cluster)
nsamples_scale_factor <- max(df_nsamples$y3) / max(df_nsamples$y1)
p1 <- ggplot(df_nsamples, aes(x = x)) +
  geom_line(aes(y = y1, linetype = "admix_estim"), size = 1.2, linewidth = 0.6) +
  geom_line(aes(y = y2 / nsamples_scale_factor, linetype = "admix_test"), size = 1.2, linewidth = 0.6) +
  geom_line(aes(y = y3 / nsamples_scale_factor, linetype = "admix_cluster"), size = 1.2, linewidth = 0.6) +
  scale_y_continuous(name = "Estimation with PS (seconds)", 
                     sec.axis = sec_axis(~ . * nsamples_scale_factor, name = "Test / clustering with IBM-ICV (seconds)")) +
  scale_linetype_manual(values = c("admix_estim" = "solid", "admix_test" = "dashed", "admix_cluster" = "dotted")) +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(linetype = "", x = "# samples")

df_size <- data.frame(x = samp_size, y1 = size_times_estim, y2 = size_times_test, y3 = size_times_cluster)
size_scale_factor <- max(df_size$y3) / max(df_size$y1)
p2 <- ggplot(df_size, aes(x = x)) +
  geom_line(aes(y = y1, linetype = "admix_estim"), size = 1.2, linewidth = 0.6) +
  geom_line(aes(y = y2 / size_scale_factor, linetype = "admix_test"), size = 1.2, linewidth = 0.6) +
  geom_line(aes(y = y3 / size_scale_factor, linetype = "admix_cluster"), size = 1.2, linewidth = 0.6) +
  scale_y_continuous(name = "Estimation with PS (seconds)", 
                     sec.axis = sec_axis(~ . * size_scale_factor, name = "Test / Clustering with IBM-ICV (seconds)")) +
  scale_linetype_manual(values = c("admix_estim" = "solid", "admix_test" = "dashed", "admix_cluster" = "dotted")) +
  theme_minimal() +
  labs(linetype = "", x = "sample size")

library(patchwork)
p1 <- p1 + theme(legend.position = "none")  # on supprime à gauche
(p1 | plot_spacer() | p2) + 
  plot_layout(
    widths = c(1, 0.1, 1),   # 👈 contrôle précis de l’espace
    guides = "collect"
  ) & 
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),   # taille du texte
    legend.title = element_text(size = 15),  # taille du titre (si présent)
    legend.key.size = unit(1.3, "cm")        # taille des symboles/lignes
  )