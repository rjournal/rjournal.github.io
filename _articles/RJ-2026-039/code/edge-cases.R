
library(admix)
library(tidyverse)

set.seed(18122023)

# We only study the quality of estimations since the quality of testing and clustering strongly
# depends on the efficiency of the estimation process => More details can be found in the cited publications.

# =========================================================
# Monte Carlo study
# =========================================================

# Parameters
n_rep <- 50
samp_size <- 1000
#samp_size <- c(100, 500)
p <- 0.5


#--------------------- 1. SMALL WEIGHT --------------------#

# Parameters
prop <- c(0.5, 0.1, 0.02)

results_smallW <- expand.grid(rep = 1:n_rep, samp_size = samp_size, prop = prop, Method = c("BVdk", "PS", "IBM"))
results_smallW$AbsError <- NA
results_smallW$RelAbsError <- NA

p_BVdk <- p_PS <- p_IBM <- matrix(NA, nrow = n_rep, ncol = length(prop))

counter <- 1
for (rep in 1:n_rep)
{
  cat("\nReplication:", rep, "\n")
  for (i in 1:length(samp_size))
  {
    for (j in 1:length(prop)) 
    {
      mix1 <- twoComp_mixt(n = samp_size[i], weight = prop[j],
                           comp.dist = list(f = "norm", g = "norm"),
                           comp.param = list(f = list("mean" = 0, "sd" = 1), 
                                             g = list("mean" = 2, "sd" = 0.5)))
      data1 <- get_mixture_data(mix1)
      admix_mod1 <- admix_model(knownComp_dist = mix1$comp.dist$g,
                                knownComp_param = mix1$comp.param$g)
      # ---------------------------------------------
      # BVdk estimation
      BVdk <- admix_estim(samples = list(data1), admixMod = list(admix_mod1), est_method = "BVdk")
      p_BVdk[rep,j] <- get_mixing_weights(BVdk)
      err_BVdk <- abs(p_BVdk[rep,j] - prop[j])
      rel_err_BVdk <- abs(p_BVdk[rep,j] - prop[j]) / prop[j]
      # ---------------------------------------------
      # PS estimation
      PS <- admix_estim(samples = list(data1), admixMod = list(admix_mod1), est_method = "PS")
      p_PS[rep,j] <- get_mixing_weights(PS)
      err_PS <- abs(p_PS[rep,j] - prop[j])
      rel_err_PS <- abs(p_PS[rep,j] - prop[j]) / prop[j]
      # ---------------------------------------------
      # Second mixture for IBM
      mix2 <- twoComp_mixt(n = samp_size[i], weight = 0.7, 
                           comp.dist = list(f = "norm", g = "norm"),
                           comp.param = list(f = list("mean" = 0, "sd" = 1), 
                                             g = list("mean" = 3, "sd" = 1)))
      data2 <- get_mixture_data(mix2)
      admix_mod2 <- admix_model(knownComp_dist = mix2$comp.dist$g, knownComp_param = mix2$comp.param$g)
      IBM <- admix_estim(samples = list(data1, data2), admixMod = list(admix_mod1, admix_mod2), est_method = "IBM")
      p_IBM[rep,j] <- get_mixing_weights(IBM)[1]
      err_IBM <- abs(p_IBM[rep,j] - prop[j])
      rel_err_IBM <- abs(p_IBM[rep,j] - prop[j]) / prop[j]
      # ---------------------------------------------
      # Save results
      results_smallW$AbsError[results_smallW$rep == rep & results_smallW$samp_size == samp_size[i] & results_smallW$prop == prop[j] &
                                results_smallW$Method == "BVdk"] <- err_BVdk
      results_smallW$AbsError[results_smallW$rep == rep & results_smallW$samp_size == samp_size[i] & results_smallW$prop == prop[j] &
                                results_smallW$Method == "PS"] <- err_PS
      results_smallW$AbsError[results_smallW$rep == rep & results_smallW$samp_size == samp_size[i] & results_smallW$prop == prop[j] &
                                results_smallW$Method == "IBM"] <- err_IBM
      results_smallW$RelAbsError[results_smallW$rep == rep & results_smallW$samp_size == samp_size[i] & results_smallW$prop == prop[j] &
                                results_smallW$Method == "BVdk"] <- rel_err_BVdk
      results_smallW$RelAbsError[results_smallW$rep == rep & results_smallW$samp_size == samp_size[i] & results_smallW$prop == prop[j] &
                                results_smallW$Method == "PS"] <- rel_err_PS
      results_smallW$RelAbsError[results_smallW$rep == rep & results_smallW$samp_size == samp_size[i] & results_smallW$prop == prop[j] &
                                results_smallW$Method == "IBM"] <- rel_err_IBM
      mix1 <- data1 <- admix_mod1 <- mix2 <- data2 <- admix_mod2 <- NULL
      BVdk <- PS <- IBM <- err_BVdk <- err_PS <- err_IBM <- rel_err_BVdk <- rel_err_PS <- rel_err_IBM <- NULL
    }
  }  
}

# =========================================================
# Summary statistics
summary_results_smallW <- results_smallW %>%
  group_by(samp_size, prop, Method) %>%
  summarise(
    MinError = min(RelAbsError),
    MaxError = max(RelAbsError),
    MedianError = median(RelAbsError),
    MeanError = mean(RelAbsError),
    SdError   = sd(RelAbsError),
    SE        = SdError / sqrt(n()),
    LowerCI   = MeanError - 1.96 * SE,
    UpperCI   = MeanError + 1.96 * SE,
    .groups = "drop"
  )

mean(summary_results_smallW$MinError)
mean(summary_results_smallW$MaxError)
mean(summary_results_smallW$MeanError)

summary_p_smallW_BVdk <- rbind(apply(p_BVdk, 2, summary), apply(p_BVdk, 2, sd))
rownames(summary_p_smallW_BVdk) <- c("Min.","Q1","Median","Mean","Q3","Max","Sd")
summary_p_smallW_PS <- rbind(apply(p_PS, 2, summary), apply(p_PS, 2, sd))
rownames(summary_p_smallW_PS) <- c("Min.","Q1","Median","Mean","Q3","Max","Sd")
summary_p_smallW_IBM <- rbind(apply(p_IBM, 2, summary), apply(p_IBM, 2, sd))
rownames(summary_p_smallW_IBM) <- c("Min.","Q1","Median","Mean","Q3","Max","Sd")
summary_p_smallW <- cbind(summary_p_smallW_BVdk, summary_p_smallW_PS, summary_p_smallW_IBM)
colnames(summary_p_smallW) <- c("BVdk:p=0.5","BVdk:p=0.1","BVdk:p=0.02","PS:p=0.5","PS:p=0.1","PS:p=0.02",
                                "IBM:p=0.5","IBM:p=0.1","IBM:p=0.02")
summary_p_smallW <- summary_p_smallW[ ,c("BVdk:p=0.02","PS:p=0.02","IBM:p=0.02","BVdk:p=0.1","PS:p=0.1","IBM:p=0.1",
                                         "BVdk:p=0.5","PS:p=0.5","IBM:p=0.5")]
library(kableExtra)
kable(summary_p_smallW, format = "latex", booktabs = TRUE, caption = "Mon tableau")


# =========================================================
# GRAPH : Distribution of errors
p_labs <- c("0.02" = "p = 0.02", "0.1" = "p = 0.1", "0.3" = "p = 0.3", "0.5" = "p = 0.5")
g2 <- ggplot(results_smallW, aes(x = Method, y = RelAbsError, fill = Method)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8) +
  scale_y_log10() +
  facet_grid(samp_size ~ prop, 
             labeller = labeller(prop = p_labs,
                                 samp_size = c("1000" = "Sample size = 1000"))) +
  theme_bw(base_size = 14) + 
  labs(title = "Effect of the weight of the unknown component",
       x = "Method", y = "Relative absolute error (log10 scale)") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
print(g2)



#-------------------- 2. OVERLAP --------------------#

moy <- c(0, 0.9, 5)

results_overlap <- expand.grid(rep = 1:n_rep, samp_size = samp_size, mu = moy, Method = c("BVdk", "PS", "IBM"))
results_overlap$AbsError <- NA

p_BVdk_overlap <- p_PS_overlap <- p_IBM_overlap <- matrix(NA, nrow = n_rep, ncol = length(moy))

for(rep in 1:n_rep)
{
  cat("\nReplication:", rep, "\n")
  for(i in 1:length(samp_size))
  {
    for(j in 1:length(moy))
    {
      # ---------------------------------------------
      # First mixture
      mix1 <- twoComp_mixt(n = samp_size[i], weight = p, comp.dist = list(f = "norm", g = "norm"),
                           comp.param = list(f = list(mean = moy[j], sd = 1), g = list(mean = 1, sd = 1)))
      #plot(mix1)
      data1 <- get_mixture_data(mix1)
      admix_mod1 <- admix_model(knownComp_dist = mix1$comp.dist$g, knownComp_param = mix1$comp.param$g)
      # ---------------------------------------------
      # BVdk estimation
      BVdk <- admix_estim(samples = list(data1), admixMod = list(admix_mod1), est_method = "BVdk")
      p_BVdk_overlap[rep,j] <- get_mixing_weights(BVdk)
      err_BVdk <- abs(p_BVdk_overlap[rep,j] - p)
      # ---------------------------------------------
      # PS estimation
      PS <- admix_estim(samples = list(data1), admixMod = list(admix_mod1), est_method = "PS")
      p_PS_overlap[rep,j] <- get_mixing_weights(PS)
      err_PS <- abs(p_PS_overlap[rep,j] - p)
      # ---------------------------------------------
      # Second mixture for IBM
      mix2 <- twoComp_mixt(n = samp_size[i], weight = p, comp.dist = list(f = "norm", g = "norm"),
                           comp.param = list(f = list(mean = moy[j], sd = 1), g = list(mean = 3, sd = 1)))
      data2 <- get_mixture_data(mix2)
      admix_mod2 <- admix_model(knownComp_dist = mix2$comp.dist$g, knownComp_param = mix2$comp.param$g)
      IBM <- admix_estim(samples = list(data1, data2), admixMod = list(admix_mod1, admix_mod2), est_method = "IBM")
      p_IBM_overlap[rep,j] <- get_mixing_weights(IBM)[1]
      err_IBM <- abs(p_IBM_overlap[rep,j] - p)
      # ---------------------------------------------
      # Save results
      results_overlap$AbsError[results_overlap$rep == rep & results_overlap$samp_size == samp_size[i] & results_overlap$mu == moy[j] &
                                 results_overlap$Method == "BVdk"] <- err_BVdk
      results_overlap$AbsError[results_overlap$rep == rep & results_overlap$samp_size == samp_size[i] & results_overlap$mu == moy[j] &
                                 results_overlap$Method == "PS"] <- err_PS
      results_overlap$AbsError[results_overlap$rep == rep & results_overlap$samp_size == samp_size[i] & results_overlap$mu == moy[j] &
                                 results_overlap$Method == "IBM"] <- err_IBM
      mix1 <- data1 <- admix_mod1 <- mix2 <- data2 <- admix_mod2 <- NULL
      BVdk <- PS <- IBM <- err_BVdk <- err_PS <- err_IBM <- NULL
    }
  }
}

# =========================================================
# Add overlap measure
results_overlap <- results_overlap %>% 
  mutate(distance = abs(mu - 1))
results_overlap <- results_overlap %>%
  mutate(
    distance = round(distance, 2),
    distance = factor(distance)
  )

# =========================================================
# Summary statistics
summary_results_overlap <- results_overlap %>%
  group_by(samp_size, distance, Method) %>%
  summarise(
    MinError = min(AbsError),
    MaxError = max(AbsError),
    MedianError = median(AbsError),
    MeanError = mean(AbsError),
    SdError   = sd(AbsError),
    SE        = SdError / sqrt(n()),
    LowerCI   = MeanError - 1.96 * SE,
    UpperCI   = MeanError + 1.96 * SE,
    .groups = "drop"
  )

mean(summary_results_overlap$MinError)
mean(summary_results_overlap$MaxError)

median(results_overlap[(results_overlap$samp_size==1000) & (results_overlap$Method=="IBM"), "AbsError"])
median(results_overlap[(results_overlap$samp_size==1000) & (results_overlap$Method=="PS"), "AbsError"])
mean(results_overlap[(results_overlap$samp_size==1000) & (results_overlap$Method=="PS"), "AbsError"])
mean(summary_results_overlap$MeanError)

summary_p_BVdk_overlap <- rbind(apply(p_BVdk_overlap, 2, summary), apply(p_BVdk_overlap, 2, sd))
rownames(summary_p_BVdk_overlap) <- c("Min.","Q1","Median","Mean","Q3","Max","Sd")
summary_p_PS_overlap <- rbind(apply(p_PS_overlap, 2, summary), apply(p_PS_overlap, 2, sd))
rownames(summary_p_PS_overlap) <- c("Min.","Q1","Median","Mean","Q3","Max","Sd")
summary_p_IBM_overlap <- rbind(apply(p_IBM_overlap, 2, summary), apply(p_IBM_overlap, 2, sd))
rownames(summary_p_IBM_overlap) <- c("Min.","Q1","Median","Mean","Q3","Max","Sd")
summary_p_overlap <- cbind(summary_p_BVdk_overlap, summary_p_PS_overlap, summary_p_IBM_overlap)
colnames(summary_p_overlap) <- c("BVdk:strong","BVdk:huge","BVdk:no","PS:strong","PS:huge","PS:no",
                                "IBM:strong","IBM:huge","IBM:no")
summary_p_overlap <- summary_p_overlap[ ,c("BVdk:no","PS:no","IBM:no","BVdk:strong","PS:strong","IBM:strong",
                                         "BVdk:huge","PS:huge","IBM:huge")]
kable(summary_p_overlap, format = "latex", booktabs = TRUE, caption = "Mon tableau")

# =========================================================
# GRAPH : Distribution of errors
distance_labs <- c("0.1" = "Huge overlap", "1" = "Strong overlap", "4" = "Weak overlap")
g1 <- ggplot(results_overlap, aes(x = Method, y = AbsError, fill = Method)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8) +
  scale_y_log10() +
  facet_grid(samp_size ~ distance, labeller = labeller(distance = distance_labs, 
                                                       samp_size = c("1000" = "Sample size = 1000"))) +
  theme_bw(base_size = 14) + 
  labs(title = "Effect of the overlap of mixture components",
       x = "Method",y = "Absolute error (log10 scale)") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
print(g1)


#--------------------- 3. MISPECIFICATION OF THE KNOWN COMPONENT --------------------#

# => everything fails, which is expected!

# Parameters : strong to weak mispecification, finally well-specified
known_mu <- c(-2, 1, 1.9, 2)

results_mispec <- expand.grid(rep = 1:n_rep, samp_size = samp_size, mu = known_mu, Method = c("BVdk", "PS", "IBM"))
results_mispec$AbsError <- NA

p_BVdk_mis <- p_PS_mis <- p_IBM_mis <- matrix(NA, nrow = n_rep, ncol = length(known_mu))

for (rep in 1:n_rep)
{
  cat("\nReplication:", rep, "\n")
  for (i in 1:length(samp_size))
  {
    for (j in 1:length(known_mu)) 
    {
      mix1 <- twoComp_mixt(n = samp_size[i], weight = p,
                           comp.dist = list(f = "norm", g = "norm"),
                           comp.param = list(f = list("mean" = 0, "sd" = 1), 
                                             g = list("mean" = 2, "sd" = 0.5)))
      data1 <- get_mixture_data(mix1)
      admix_mod1 <- admix_model(knownComp_dist = "norm", 
                                knownComp_param = list("mean" = known_mu[j], "sd" = 0.5))
      # ---------------------------------------------
      # BVdk estimation
      BVdk <- admix_estim(samples = list(data1), admixMod = list(admix_mod1), est_method = "BVdk")
      p_BVdk_mis[rep,j] <- get_mixing_weights(BVdk)
      err_BVdk <- abs(p_BVdk_mis[rep,j] - p)
      # ---------------------------------------------
      # PS estimation
      PS <- admix_estim(samples = list(data1), admixMod = list(admix_mod1), est_method = "PS")
      p_PS_mis[rep,j] <- get_mixing_weights(PS)
      err_PS <- abs(p_PS_mis[rep,j] - p)
      # ---------------------------------------------
      # Second mixture for IBM
      mix2 <- twoComp_mixt(n = samp_size[i], weight = 0.3,
                           comp.dist = list(f = "norm", g = "norm"),
                           comp.param = list(f = list("mean" = 0, "sd" = 1), 
                                             g = list("mean" = -2, "sd" = 1)))
      data2 <- get_mixture_data(mix2)
      admix_mod2 <- admix_model(knownComp_dist = mix2$comp.dist$g, knownComp_param = mix2$comp.param$g)
      IBM <- admix_estim(samples = list(data1, data2), admixMod = list(admix_mod1, admix_mod2), est_method = "IBM")
      p_IBM_mis[rep,j] <- get_mixing_weights(IBM)[1]
      err_IBM <- abs(p_IBM_mis[rep,j] - p)
      # ---------------------------------------------
      # Save results
      results_mispec$AbsError[results_mispec$rep == rep & results_mispec$samp_size == samp_size[i] & results_mispec$mu == known_mu[j] &
                                results_mispec$Method == "BVdk"] <- err_BVdk
      results_mispec$AbsError[results_mispec$rep == rep & results_mispec$samp_size == samp_size[i] & results_mispec$mu == known_mu[j] &
                                results_mispec$Method == "PS"] <- err_PS
      results_mispec$AbsError[results_mispec$rep == rep & results_mispec$samp_size == samp_size[i] & results_mispec$mu == known_mu[j] &
                                results_mispec$Method == "IBM"] <- err_IBM
      mix1 <- data1 <- admix_mod1 <- mix2 <- data2 <- admix_mod2 <- NULL
      BVdk <- PS <- IBM <- err_BVdk <- err_PS <- err_IBM <- NULL
    }
  }  
}

# Add overlap measure
results_mispec <- results_mispec %>% 
  mutate(distance = abs(2 - mu))
results_mispec <- results_mispec %>%
  mutate(
    distance = round(distance, 2),
    distance = factor(distance)
  )

# =========================================================
# Summary statistics
summary_results_mispec <- results_mispec %>%
  group_by(samp_size, distance, Method) %>%
  summarise(
    MinError = min(AbsError),
    MaxError = max(AbsError),
    MedianError = median(AbsError),
    MeanError = mean(AbsError),
    SdError   = sd(AbsError),
    SE        = SdError / sqrt(n()),
    LowerCI   = MeanError - 1.96 * SE,
    UpperCI   = MeanError + 1.96 * SE,
    .groups = "drop"
  )

minError_results_mispec <- mean(summary_results_mispec$MinError)
maxError_results_mispec <- mean(summary_results_mispec$MaxError)
meanError_results_mispec <- mean(summary_results_mispec$MeanError)

summary_p_BVdk_mis <- rbind(apply(p_BVdk_mis, 2, summary), apply(p_BVdk_mis, 2, sd))
rownames(summary_p_BVdk_mis) <- c("Min.","Q1","Median","Mean","Q3","Max","Sd")
summary_p_PS_mis <- rbind(apply(p_PS_mis, 2, summary), apply(p_PS_mis, 2, sd))
rownames(summary_p_PS_mis) <- c("Min.","Q1","Median","Mean","Q3","Max","Sd")
summary_p_IBM_mis <- rbind(apply(p_IBM_mis, 2, summary), apply(p_IBM_mis, 2, sd))
rownames(summary_p_IBM_mis) <- c("Min.","Q1","Median","Mean","Q3","Max","Sd")
summary_p_mis <- cbind(summary_p_BVdk_mis, summary_p_PS_mis, summary_p_IBM_mis)
colnames(summary_p_mis) <- c("BVdk:strong","BVdk:moderate","BVdk:almost well","BVdk:well",
                             "PS:strong","PS:moderate","PS:almost well","PS:well",
                             "IBM:strong","IBM:moderate","IBM:almost well","IBM:well")
summary_p_mis <- summary_p_mis[ ,c("BVdk:well","PS:well","IBM:well","BVdk:almost well","PS:almost well","IBM:almost well",
                                   "BVdk:moderate","PS:moderate","IBM:moderate","BVdk:strong","PS:strong","IBM:strong")]
kable(summary_p_mis, format = "latex", booktabs = TRUE, caption = "Mon tableau")

# =========================================================
# GRAPH : Distribution of errors
mispec_labs <- c("0" = "Well specified", "0.1" = "Almost well specified", "1" = "Moderate mispecification", "4" = "Strong mispecification")
g3 <- ggplot(results_mispec, aes(x = Method, y = AbsError, fill = Method)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8) +
  scale_y_log10() +
  facet_grid(samp_size ~ distance, labeller = labeller(distance = mispec_labs,
                                                       samp_size = c("1000" = "Sample size = 1000"))) +
  theme_bw(base_size = 14) + 
  labs(title = "Effect of mispecification of the known component",
       x = "Method",y = "Absolute error (log10 scale)") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
print(g3)


bis_summary_results_mispec <- summary_results_mispec

bis_summary_results_mispec$samp_size <- bis_summary_results_mispec$MaxError <- bis_summary_results_mispec$SdError <- 
  bis_summary_results_mispec$SE <- bis_summary_results_mispec$LowerCI <- bis_summary_results_mispec$UpperCI <- NULL
kable(bis_summary_results_mispec, format = "latex", booktabs = TRUE, caption = "Mon tableau")
