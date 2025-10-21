# 1PL LSIRM For Dichotomous Data ----------------
# remotes::install_github("jiniuslab/lsirm12pl")

library(lsirm12pl)
data <- lsirm12pl::TDRI
data <- data[complete.cases(data),]
head(data)

lsirm_result <- lsirm(data ~ lsirm1pl(chains = 4, multicore = 2, seed = 2025))
summary(lsirm_result, chain.idx = 1, estimate = 'mean', CI = 0.95)

diagnostic(lsirm_result,
           draw.item = list("beta" = c(1)),
           gelman.diag = TRUE)

gof(lsirm_result, chain.idx = 1)

plot(lsirm_result, option = "beta", chain.idx = 1)
plot(lsirm_result, option = "theta", chain.idx = 1)

plot(lsirm_result, option = "interaction", chain.idx = 1)
plot(lsirm_result, option = "interaction", rotation = TRUE, chain.idx = 1)


set.seed(2025)
plot(lsirm_result, cluster = "spectral", chain.idx = 1)

set.seed(2025)
plot(lsirm_result, cluster = "neyman", chain.idx = 1)


# 1PL LSIRM For Dichotomous Data - Flexible Modeling Options ----------------
set.seed(2025)
lsirm_result <- lsirm(data ~ lsirm1pl(fixed_gamma = TRUE))
lsirm_result <- lsirm(data ~ lsirm1pl(spikenslab = TRUE))
lsirm_result$pi_estimate

data <- lsirm12pl::TDRI
data[is.na(data)] <- 99

lsirm_result <- lsirm(data ~ lsirm1pl(missing_data = "mcar"))
plot(lsirm_result, option = "interaction")

lsirm_result <- lsirm(data ~ lsirm1pl(missing_data = "mar"))
plot(lsirm_result, option = "interaction")

lsirm_result$imp_estimate


# 2PL LSIRM For Dichotomous Data ----------------
library(lsirm12pl)
data <- lsirm12pl::TDRI
data <- data[complete.cases(data),]
lsirm_result <- lsirm(data ~ lsirm2pl(chains = 4, multicore = 2, seed = 2025))

diagnostic(lsirm_result,
           draw.item = list(beta = c(1)),
           gelman.diag = T)

gof(lsirm_result, chain.idx = 1)

plot(lsirm_result, option = "beta", chain.idx = 1)
plot(lsirm_result, option = "theta", chain.idx = 1)
plot(lsirm_result, option = "alpha", chain.idx = 1)

plot(lsirm_result, option = "interaction", chain.idx = 1)
plot(lsirm_result, option = "interaction", rotation = TRUE, chain.idx = 1)

set.seed(2025)
plot(lsirm_result, cluster = "spectral", chain.idx = 1)

set.seed(2025)
plot(lsirm_result, cluster = "neyman", chain.idx = 1)


lsirm_result <- lsirm(data ~ lsirm2pl(fixed_gamma = TRUE))
lsirm_result <- lsirm(data ~ lsirm2pl(spikenslab = TRUE))
lsirm_result <- lsirm(data ~ lsirm2pl(missing_data = "mcar"))
lsirm_result <- lsirm(data ~ lsirm2pl(missing_data = "mar"))

# LSIRM For Continuous Item Responses Data ----------------
data <- lsirm12pl::BFPT
data[(data==0)|(data==6)] = NA
reverse <- c(2, 4, 6, 8, 10, 11, 13, 15, 16, 17,
             18, 19, 20, 21, 23, 25, 27, 32, 34,
             36, 42, 44, 46)
data[, reverse] <- 6 - data[, reverse]
data <- data[complete.cases(data),]
lsirm_result <- lsirm(data ~ lsirm1pl(niter = 25000, nburn = 5000, nthin = 10,
                                      jump_beta = 0.08, jump_theta = 0.3,
                                      jump_gamma  = 1.0,
                                      chains = 4, multicore = 2, seed = 2025))

diagnostic(lsirm_result, draw.item = list(beta = c("AGR1")))

gof(lsirm_result, chain.idx = 1)

plot(lsirm_result, option = "beta", chain.idx = 1)
plot(lsirm_result, option = "theta", chain.idx = 1)

plot(lsirm_result, chain.idx = 1)
plot(lsirm_result, rotation = TRUE, chain.idx = 1)

set.seed(2025)
plot(lsirm_result, cluster = "neyman", chain.idx = 1)
plot(lsirm_result, cluster = "spectral", chain.idx = 1)

lsirm_result <- lsirm(data ~ lsirm1pl(fixed_gamma = TRUE))
lsirm_result <- lsirm(data ~ lsirm1pl(spikenslab = TRUE))
lsirm_result <- lsirm(data ~ lsirm1pl(missing_data = "mcar"))
lsirm_result <- lsirm(data ~ lsirm1pl(missing_data = "mar"))


