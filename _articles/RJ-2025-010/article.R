## Replication code for paper accompanying 'SimEngine' R package
##
## Avi Kenny and Charles J. Wolock
##
## The code follows the organization of the paper, divided by section.

## Required packages for replication
library("ggplot2")
library("MASS")
library("dplyr")
library("tidyr")
library("SimEngine")


############################################################################.
##### Section 3: Overview of simulation workflow and primary functions #####
############################################################################.

set.seed(1)

# run only if not installed
if (F) {
  install.packages("SimEngine")
}

# Chunk 3.1.1
library(SimEngine)
sim <- new_sim()

# Chunk 3.2.1
create_data <- function(n) {
  return(rpois(n=n, lambda=20))
}
create_data(n=10)

# Chunk 3.3.1
est_lambda <- function(dat, type) {
  if (type=="M") { return(mean(dat)) }
  if (type=="V") { return(var(dat)) }
}
dat <- create_data(n=1000)
est_lambda(dat=dat, type="M")
est_lambda(dat=dat, type="V")

# Chunk 3.4.1
sim %<>% set_levels(
  estimator = c("M", "V"),
  n = c(10, 100, 1000)
)

# Chunk 3.5.1
sim %<>% set_script(function() {
  dat <- create_data(n=L$n)
  lambda_hat <- est_lambda(dat=dat, type=L$estimator)
  return (list("lambda_hat"=lambda_hat))
})

# Chunk 3.6.1
sim %<>% set_config(
  num_sim = 100,
  packages = c("ggplot2", "stringr")
)

# Chunk 3.7.1
sim %<>% run()

# Chunk 3.8.1
sim %>% summarize(
  list(stat="bias", name="bias_lambda", estimate="lambda_hat", truth=20),
  list(stat="mse", name="mse_lambda", estimate="lambda_hat", truth=20)
)

# Chunk 3.8.2
head(sim$results)

# Chunk 3.9.1
sim %<>% set_config(num_sim = 200)
sim %<>% set_levels(
  estimator = c("M", "V"),
  n = c(10, 100, 1000, 10000)
)

# Chunk 3.9.2
sim %<>% update_sim()

# Chunk 3.9.3
sim %>% summarize(
  list(stat="bias", name="bias_lambda", estimate="lambda_hat", truth=20),
  list(stat="mse", name="mse_lambda", estimate="lambda_hat", truth=20)
)



######################################.
##### Section 4: Parallelization #####
######################################.

# Results are not displayed for this code
if (F) {

  # Chunk 4.1.1
  sim <- new_sim()
  sim %<>% set_config(parallel = TRUE)
  
  # Chunk 4.1.1b
  for (p in c(FALSE, TRUE)) {
    sim <- new_sim()
    sim %<>% set_config(num_sim=20, parallel=p)
    sim %<>% set_script(function() {
      Sys.sleep(1)
      return (list("x"=1))
    })
    sim %<>% run()
    print(paste("Parallelizing:", p))
    print(sim %>% vars("total_runtime"))
  }
  
  # Chunk 4.1.2
  sim %<>% set_config(n_cores = 2)

  # Chunk 4.2.1
  sim <- new_sim()
  create_data <- function(n) { return(rpois(n=n, lambda=20)) }
  est_lambda <- function(dat, type) {
    if (type=="M") { return(mean(dat)) }
    if (type=="V") { return(var(dat)) }
  }
  sim %<>% set_levels(estimator = c("M","V"), n = c(10,100,1000))
  sim %<>% set_script(function() {
    dat <- create_data(L$n)
    lambda_hat <- est_lambda(dat=dat, type=L$estimator)
    return(list("lambda_hat"=lambda_hat))
  })
  sim %<>% set_config(num_sim=100)
  sim %<>% run()
  sim %>% summarize()

  # Chunk 4.2.2
  run_on_cluster(
    first = {
      sim <- new_sim()
      create_data <- function(n) { return(rpois(n=n, lambda=20)) }
      est_lambda <- function(dat, type) {
        if (type=="M") { return(mean(dat)) }
        if (type=="V") { return(var(dat)) }
      }
      sim %<>% set_levels(estimator = c("M","V"), n = c(10,100,1000))
      sim %<>% set_script(function() {
        dat <- create_data(L$n)
        lambda_hat <- est_lambda(dat=dat, type=L$estimator)
        return(list("lambda_hat"=lambda_hat))
      })
      sim %<>% set_config(num_sim=100, n_cores=20)
    },
    main = {
      sim %<>% run()
    },
    last = {
      sim %>% summarize()
    },
    cluster_config = list(js="slurm")
  )

  # Chunk 4.2.3
  #> #!/bin/bash
  #> Rscript my_simulation.R

  # Chunk 4.2.4
  #> sbatch --export=sim_run='first' run_sim.sh
  #> sbatch --export=sim_run='main' --array=1-20 --depend=afterok:101 run_sim.sh
  #> sbatch --export=sim_run='last' --depend=afterok:102 run_sim.sh

  # Chunk 4.2.4b
  #> jid1=$(sbatch --export=sim_run='first' run_sim.sh | sed 's/Submitted batch job //')
  #> jid2=$(sbatch --export=sim_run='main' --array=1-20 --depend=afterok:$jid1 run_sim.sh | sed 's/Submitted batch job //')
  #> sbatch --export=sim_run='last' --depend=afterok:$jid2 run_sim.sh
  
  # Chunk 4.2.5
  #> sbatch --export=sim_run='main' --array=1-5 --depend=afterok:101 run_sim.sh

  # Chunk 4.3.1
  run_on_cluster(
    first = {...},
    main = {...},
    last = {...},
    cluster_config = list(tid_var="SLURM_ARRAY_TASK_ID")
  )

  # Chunk 4.3.2
  update_sim_on_cluster(
    first = {
      sim <- readRDS("sim.rds")
      sim %<>% set_levels(n=c(100,500,1000))
    },
    main = {
      sim %<>% update_sim()
    },
    last = {
      sim %>% summarize()
    },
    cluster_config = list(js="slurm")
  )

}



#############################################.
##### Section 5: Advanced functionality #####
#############################################.

set.seed(1)

# Chunk 5.1.1
sim <- new_sim()
create_data <- function(n) { rnorm(n=n, mean=3) }
est_mean <- function(dat, type) {
  if (type=="est_mean") { return(mean(dat)) }
  if (type=="est_median") { return(median(dat)) }
}
sim %<>% set_levels(est=c("est_mean","est_median"))
sim %<>% set_config(num_sim=3)
sim %<>% set_script(function() {
  dat <- create_data(n=100)
  mu_hat <- est_mean(dat=dat, type=L$est)
  return(list(
    "mu_hat" = round(mu_hat,2),
    "dat_1" = round(dat[1],2)
  ))
})
sim %<>% run()

# Chunk 5.1.2
sim$results[order(sim$results$rep_id),c(1:7)!=5]

# Chunk 5.1.3
sim <- new_sim()
create_data <- function(n) { rnorm(n=n, mean=3) }
est_mean <- function(dat, type) {
  if (type=="est_mean") { return(mean(dat)) }
  if (type=="est_median") { return(median(dat)) }
}
sim %<>% set_levels(est=c("est_mean","est_median"))
sim %<>% set_config(num_sim=3, batch_levels=NULL)
sim %<>% set_script(function() {
  batch({
    dat <- create_data(n=100)
  })
  mu_hat <- est_mean(dat=dat, type=L$est)
  return(list(
    "mu_hat" = round(mu_hat,2),
    "dat_1" = round(dat[1],2)
  ))
})
sim %<>% run()

# Chunk 5.1.4
sim$results[order(sim$results$rep_id),c(1:7)!=5]

# Chunk 5.1.5
sim <- new_sim()
create_data <- function(n, mu) { rnorm(n=n, mean=mu) }
est_mean <- function(dat, type) {
  if (type=="est_mean") { return(mean(dat)) }
  if (type=="est_median") { return(median(dat)) }
}
sim %<>% set_levels(n=c(10,100), mu=c(3,5), est=c("est_mean","est_median"))
sim %<>% set_config(num_sim=2, batch_levels=c("n", "mu"), return_batch_id=T)
sim %<>% set_script(function() {
  batch({
    dat <- create_data(n=L$n, mu=L$mu)
  })
  mu_hat <- est_mean(dat=dat, type=L$est)
  return(list(
    "mu_hat" = round(mu_hat,2),
    "dat_1" = round(dat[1],2)
  ))
})
sim %<>% run()
sim$results[order(sim$results$batch_id),c(1:10)!=8]

set.seed(1)

# Chunk 5.2.1
sim <- new_sim()
sim %<>% set_levels(n = c(200,400,800))

# Chunk 5.2.2
sim <- new_sim()
sim %<>% set_levels(
  n = c(10,100),
  distribution = list(
    "Beta 1" = list(type="Beta", params=c(0.3, 0.7)),
    "Beta 2" = list(type="Beta", params=c(1.5, 0.4)),
    "Normal" = list(type="Normal", params=c(3.0, 0.2))
  )
)
create_data <- function(n, type, params) {
  if (type=="Beta") {
    return(rbeta(n, shape1=params[1], shape2=params[2]))
  } else if (type=="Normal") {
    return(rnorm(n, mean=params[1], sd=params[2]))
  }
}
sim %<>% set_script(function() {
  x <- create_data(L$n, L$distribution$type, L$distribution$params)
  return(list("y"=mean(x)))
})
sim %<>% run()

# Chunk 5.2.3
sim %>% summarize(list(stat="mean", x="y"))

set.seed(1)

# Chunk 5.3.1
sim <- new_sim()
sim %<>% set_levels(n=c(10, 100, 1000))
create_data <- function(n) {
  x <- runif(n)
  y <- 3 + 2*x + rnorm(n)
  return(data.frame("x"=x, "y"=y))
}
sim %<>% set_config(num_sim=2)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  model <- lm(y~x, data=dat)
  return(list(
    "beta0_hat" = model$coefficients[[1]],
    "beta1_hat" = model$coefficients[[2]],
    ".complex" = list(
      "model" = model,
      "cov_mtx" = vcov(model)
    )
  ))
})
sim %<>% run()

# Chunk 5.3.2
head(sim$results)

# Chunk 5.3.3
c5 <- get_complex(sim, sim_uid=5)
print(summary(c5$model))
print(c5$cov_mtx)

set.seed(1)

# Chunk 5.4.1
sim %<>% set_config(seed=123)

# Chunk 5.4.2
sim <- new_sim()
print(vars(sim, "seed"))

set.seed(1)

# Chunk 5.5.1
sim <- new_sim()
sim %<>% set_config(num_sim=2)
sim %<>% set_levels(
  Sigma = list(
    s1 = list(mtx=matrix(c(3,1,1,2), nrow=2)),
    s3 = list(mtx=matrix(c(4,3,3,9), nrow=2)),
    s2 = list(mtx=matrix(c(1,2,2,1), nrow=2)),
    s4 = list(mtx=matrix(c(8,2,2,6), nrow=2))
  )
)
sim %<>% set_script(function() {
  x <- MASS::mvrnorm(n=1, mu=c(0,0), Sigma=L$Sigma$mtx)
  return(list(x1=x[1], x2=x[2]))
})
sim %<>% run()
print(sim$errors)

# Chunk 5.5.1b
sim %<>% update_sim(keep_errors=FALSE)

# Chunk 5.5.2
sim %<>% set_config(stop_at_error=TRUE)

set.seed(1)

# Chunk 5.6.1
sim <- new_sim()
create_data <- function(n) { rpois(n, lambda=5) }
est_mean <- function(dat) {
  return(mean(dat))
}
sim %<>% set_levels(n=c(10,100,1000))
sim %<>% set_config(num_sim=5)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- est_mean(dat=dat)
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim %>% summarize(
  list(stat="mse", name="lambda_mse", estimate="lambda_hat", truth=5),
  mc_se = TRUE
)



##########################################################.
##### Appendix A: Simulation-based power calculation #####
##########################################################.

set.seed(1)

# Chunk A.1
sim <- new_sim()
create_rct_data <- function(n, mu_0, mu_1, sigma_0, sigma_1) {
  group <- sample(rep(c(0,1),n))
  outcome <- (1-group) * rnorm(n=n, mean=mu_0, sd=sigma_0) +
    group * rnorm(n=n, mean=mu_1, sd=sigma_1)
  return(data.frame("group"=group, "outcome"=outcome))
}
create_rct_data(n=3, mu_0=3, mu_1=4, sigma_0=0.1, sigma_1=0.1)

# Chunk A.2 (no output)
run_test <- function(data) {
  test_result <- t.test(outcome~group, data=data)
  return(as.integer(test_result$p.value<0.05))
}

# Chunk A.3 (no output)
sim %<>% set_script(function() {
  data <- create_rct_data(n=L$n, mu_0=17, mu_1=18, sigma_0=2, sigma_1=2)
  reject <- run_test(data)
  return (list("reject"=reject))
})
sim %<>% set_levels(n=c(20,40,60,80))
sim %<>% set_config(num_sim=1000)

# Chunk A.4
sim %<>% run()
power_sim <- sim %>% summarize(
  list(stat="mean", name="power", x="reject")
)
print(power_sim)

# Chunk A.5
power_formula <- sapply(c(20,40,60,80), function(n) {
  pnorm(sqrt((n*(17-18)^2)/(2^2+2^2)) - qnorm(0.025, lower.tail=F))
})
library(ggplot2)
ggplot(data.frame(
  n = rep(c(20,40,60,80), 2),
  power = c(power_sim$power, power_formula),
  which = rep(c("Simulation","Formula"), each=4)
), aes(x=n, y=power, color=factor(which))) +
  geom_line() +
  theme_bw() +
  labs(color="Method", y="Power", x="Sample size (per group)")


###############################################################.
##### Appendix B: Comparing two standard error estimators #####
###############################################################.

set.seed(1)

# Chunk B.1
sim <- new_sim()
create_regression_data <- function(n) {
  beta <- c(-1, 10)
  x <- rnorm(n)
  sigma2 <- exp(x)
  y <- rnorm(n=n, mean=(beta[1]+beta[2]*x), sd = sqrt(sigma2))
  return(data.frame(x=x, y=y))
}

# Chunk B.2
dat <- create_regression_data(n=500)
linear_model <- lm(y~x, data=dat)
dat$residuals <- linear_model$residuals
library(ggplot2)
ggplot(dat, aes(x=x, y=residuals)) +
  geom_point() +
  theme_bw() +
  labs(x="x", y="residual")

# Chunk B.3
model_vcov <- function(data) {
  mod <- lm(y~x, data=data)
  return(list("coef"=mod$coefficients, "vcov"=diag(vcov(mod))))
}
sandwich_vcov <- function(data) {
  mod <- lm(y~x, data=data)
  return(list("coef"=mod$coefficients, "vcov"=diag(vcovHC(mod))))
}

# Chunk B.4
sim %<>% set_script(function() {
  data <- create_regression_data(n=L$n)
  estimates <- use_method(L$estimator, list(data))
  return(list(
    "beta0_est" = estimates$coef[1],
    "beta1_est" = estimates$coef[2],
    "beta0_se_est" = sqrt(estimates$vcov[1]),
    "beta1_se_est" = sqrt(estimates$vcov[2])
  ))
})
sim %<>% set_levels(
  estimator = c("model_vcov", "sandwich_vcov"),
  n = c(50, 100, 500, 1000)
)
sim %<>% set_config(
  num_sim = 500,
  seed = 24,
  packages = c("sandwich")
)
sim %<>% run()

# Chunk B.5
summarized_results <- sim %>% summarize(
  list(stat="mean", name="mean_se_beta0", x="beta0_se_est"),
  list(stat="mean", name="mean_se_beta1", x="beta1_se_est"),
  list(stat="coverage", name="cov_beta0", estimate="beta0_est",
       se="beta0_se_est", truth=-1),
  list(stat="coverage", name="cov_beta1", estimate="beta1_est",
       se="beta1_se_est", truth=10)
)
print(summarized_results)

# Chunk B.6
library(dplyr)
library(tidyr)
plot_results <- function(summarized_results, which_graph, n_est) {
  if (n_est == 3) {
    values <- c("#999999", "#E69F00", "#56B4E9")
    breaks <- c("model_vcov", "sandwich_vcov", "bootstrap_vcov")
    labels <- c("Model-based", "Sandwich", "Bootstrap")
  } else {
    values <- c("#999999", "#E69F00")
    breaks <- c("model_vcov", "sandwich_vcov")
    labels <- c("Model-based", "Sandwich")
  }
  if (which_graph == "width") {
    summarized_results %>%
      pivot_longer(
        cols = c("mean_se_beta0", "mean_se_beta1"),
        names_to = "parameter",
        names_prefix = "mean_se_"
      ) %>%
      mutate(value_j = jitter(value, amount = 0.01)) %>%
      ggplot(aes(x=n, y=1.96*value_j, color=estimator)) +
      geom_line(aes(linetype=parameter)) +
      geom_point() +
      theme_bw() +
      ylab("Average CI width") +
      xlab("Sample size") +
      scale_color_manual(
        values = values,
        breaks = breaks,
        name = "SE estimator",
        labels = labels
      ) +
      scale_linetype_discrete(
        breaks = c("beta0", "beta1"),
        name = "Parameter",
        labels = c(expression(beta[0]), expression(beta[1]))
      )
  } else {
    summarized_results %>%
      pivot_longer(
        cols = c("cov_beta0","cov_beta1"),
        names_to = "parameter",
        names_prefix = "cov_"
      ) %>%
      mutate(value_j = jitter(value, amount = 0.01)) %>%
      ggplot(aes(x=n, y=value, color=estimator)) +
      geom_line(aes(linetype = parameter)) +
      geom_point() +
      theme_bw() +
      ylab("Coverage") +
      xlab("Sample size") +
      scale_color_manual(
        values = values,
        breaks = breaks,
        name = "SE estimator",
        labels = labels
      ) +
      scale_linetype_discrete(
        breaks = c("beta0", "beta1"),
        name = "Parameter",
        labels = c(expression(beta[0]), expression(beta[1]))
      ) +
      geom_hline(yintercept=0.95)
  }
}

# Chunk B.7.1
plot_results(summarized_results, "width", 2)

# Chunk B.7.2
plot_results(summarized_results, "coverage", 2)

# Chunk B.8
bootstrap_vcov <- function(data) {
  mod <- lm(y~x, data=data)
  boot_ests <- matrix(NA, nrow=100, ncol=2)
  for (j in 1:100) {
    indices <- sample(1:nrow(data), size=nrow(data), replace=TRUE)
    boot_dat <- data[indices,]
    boot_mod <- lm(y~x, data=boot_dat)
    boot_ests[j,] <- boot_mod$coefficients
  }
  boot_v1 <- var(boot_ests[,1])
  boot_v2 <- var(boot_ests[,2])
  return(list("coef"=mod$coefficients, "vcov"=c(boot_v1, boot_v2)))
}
sim %<>% set_levels(
  estimator = c("model_vcov", "sandwich_vcov", "bootstrap_vcov"),
  n = c(50, 100, 500, 1000)
)
sim %<>% set_config(
  num_sim = 500,
  seed = 24,
  parallel = TRUE,
  n_cores = 2,
  packages = c("sandwich")
)
sim %<>% update_sim()

# Chunk B.9.1
summarized_results <- sim %>% summarize(
  list(stat="mean", name="mean_se_beta0", x="beta0_se_est"),
  list(stat="mean", name="mean_se_beta1", x="beta1_se_est"),
  list(stat="coverage", name="cov_beta0", estimate="beta0_est",
       se="beta0_se_est", truth=-1),
  list(stat="coverage", name="cov_beta1", estimate="beta1_est",
       se="beta1_se_est", truth=10)
)
plot_results(summarized_results, "width", 3)

# Chunk B.9.2
plot_results(summarized_results, "coverage", 3)



##############################.
##### sessionInfo() call #####
##############################.

sessionInfo()
