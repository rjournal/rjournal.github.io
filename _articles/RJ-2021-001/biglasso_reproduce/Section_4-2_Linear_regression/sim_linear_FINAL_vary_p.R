
# ============================================================
# Case 2: vary p
# ============================================================
rm(list = ls())
gc()

# Assume current working directory set to be folder "biglasso_reproduce/"
# Change to this directory if not.
# setwd("~/GitHub/biglasso_experiment/biglasso_reproduce/")
setwd("./Section_4-2_Linear_regression/")
source("./sim_utilities_linear_parallel.R")

set.seed(1234)
date <- Sys.Date()

n <- 1000
p <- c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000)
q <- 20
eff.nonzero <- 1
corr <- 0
rep <- 20
methods <- c('picasso', 'ncvreg', 'glmnet', 'biglasso (1 core)',
             'biglasso (2 cores)', 'biglasso (4 cores)', 'biglasso (8 cores)')
eps <- 1e-6
lam.min <- 0.1
lam.log <- FALSE
sigma <- 0.1
backingfile <- 'back.bin'
descrpfile <- 'descrb.desc'
backingpath <- getwd()

# ===================================
# testing parameters
n <- 100
p <- c(50, 100, 200)
rep <- 2
# ===================================

res <- sim(n = n, p = p, q = q, eff.nonzero = eff.nonzero, corr = corr, 
           rep = rep, methods = methods, eps = eps, lam.min = lam.min, 
           lam.log = lam.log, sigma = sigma, backingfile = backingfile, 
           descrpfile = descrpfile, backingpath = backingpath)

res.post <- post_analysis(res, date)

## Post analysis
# ===================================
load(paste0(date, "_vary_p_results.RData"))

methods <- res$parms$methods
which.vary <- res$parms$case$which.vary
sim.case <- res$parms$case$sim.case

if (sim.case == "vary_n") {
  xlab <- 'Number of observations'
} else if (sim.case == "vary_p") {
  xlab <- 'Number of features'
} else if (sim.case == 'vary_q') {
  xlab <- 'Number of active features'
} else if (sim.case == 'vary_beta') {
  xlab <- 'Magnitude of beta'
} else if (sim.case == 'vary_corr') {
  xlab <- 'Magnitude of correlation'
} else if (sim.case == 'vary_sigma') {
  xlab <- 'Sigma'
} else {
  xlab <- 'NA'
}

cat("\n============================================================\n")
cat("\nMean: \n\n")
time.mean <- apply(res$time.all, c(2, 3), mean, na.rm = TRUE)
rownames(time.mean) <- methods
colnames(time.mean) <- which.vary
print(time.mean)

cat("\nSE: \n\n")
time.se <- apply(res$time.all, c(2, 3), function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) {
    return(NA)
  } else {
    return(sd(x) / sqrt(length(x)))
  }
})
rownames(time.se) <- methods
colnames(time.se) <- which.vary
print(time.se)

## plot
# -----------------------------------------------------------------------------
rule.name <- methods
time.df <- data.frame(time = matrix(t(time.mean), ncol = 1, byrow = T),
                      Method = rep(rule.name, each = length(which.vary)),
                      Which.vary = rep(which.vary, length(methods)))

## package comparison
time.df.pkgs <- subset(time.df, Method %in% c('picasso', "ncvreg", "glmnet", "SSR-BEDPP", 
                                              'biglasso (2 cores)', 'biglasso (4 cores)', 'biglasso (8 cores)'))
time.df.pkgs$Method <- revalue(time.df.pkgs$Method, c("SSR-BEDPP"="biglasso (1 core)"))
time.df.pkgs$Package <- time.df.pkgs$Method

time.df.pkgs$Package <- factor(time.df.pkgs$Method, c('picasso', "ncvreg", "glmnet",
                                                      "biglasso (1 core)", 'biglasso (2 cores)',
                                                      'biglasso (4 cores)', 'biglasso (8 cores)'))

gp.pkgs <- ggplot(time.df.pkgs, aes(x = Which.vary, y = time, color = Package)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = pretty(range(time.df$Which.vary)),
                     limits = range(time.df$Which.vary)) +
  scale_y_continuous(breaks = pretty(range(time.df$time))) +
  xlab("Number of features") +
  ylab("Computing time (s)") +
  # theme(legend.position = 'top') +
  theme_bw() +
  theme(legend.position = c(.2, .7),
        # axis.text = element_text(size = 18),
        # axis.title = element_text(size = 18),
        # legend.title = element_text(size = 16)
        # ,legend.text = element_text(size = 16)
        # ,legend.key.size = unit(1.4, 'lines')
  )

date <- Sys.Date()
pdf(file = paste0(date, '_', sim.case, '_pkgs.pdf'), width = 5, height = 4)
print(gp.pkgs)
dev.off()
