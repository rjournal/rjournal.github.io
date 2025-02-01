library(LUCIDus)
# load data
data("simulated_HELIX_data")
simulated_data <- simulated_HELIX_data
exposome <- as.matrix(simulated_data[["phenotype"]]$hs_hg_m_scaled)
colnames(exposome) <- "hs_hg_m_scaled"
methylomics <- simulated_data$methylome
ck18 <- as.matrix(simulated_data[["phenotype"]]$ck18_scaled)
colnames(ck18) <- "ck18_scaled"
ck18_cat <- ifelse(ck18 > mean(ck18), 1, 0)
covars <- c("hs_child_age_yrs_None","e3_sex_None")
covs <- simulated_data[["phenotype"]][covars]
covs$e3_sex_None <- ifelse(covs$e3_sex_None == "male", 1, 0)


# Fit a LUCID model with a continuous outcome
fit1 <- lucid(G = exposome, Z = methylomics, Y = ck18, init_omic.data.model = NULL,
              lucid_model = "early", family = "normal", K = 2)
# MLE of the LUCID model
# fit1$res_Beta
# fit1$res_Mu
# fit1$res_Sigma
# fit1$res_Gamma
# IPs of the sample
# fit1$inclusion.p
 # Fit LUCID model with a binary outcome
fit2 <- lucid(G = exposome, Z = methylomics, Y = ck18_cat, init_omic.data.model = NULL,
               lucid_model = "early", family = "binary", K = 2)


# Fit LUCID model with spherical shape, equal volume
fit3.1 <- lucid(G = exposome, Z = methylomics, Y = ck18,
                lucid_model = "early", family = "normal", K = 2,
                init_par = "mclust",init_omic.data.model = "EII")
# Fit LUCID model with random guess
fit3.2 <- lucid(G = exposome, Z = methylomics, Y = ck18,
                lucid_model = "early", family = "normal", K = 2,
                init_par = "random")
# Fit LUCID model with ellipsoidal shape, varying volume, shape, and orientation
fit4 <- lucid(G = exposome, Z = methylomics, Y = ck18,
              lucid_model = "early", family = "normal", K = 2,
              init_omic.data.model = "VVV")


# Fit an unsupervised LUCID model
fit5 <- lucid(G = exposome, Z = methylomics, Y = ck18, lucid_model = "early",
              family = "normal", K = 2, useY = FALSE)


# Include covariates as G to X covariates
fit6 <- lucid(G = exposome, Z = methylomics, Y = ck18, lucid_model = "early", K = 2,
              family = "normal", CoG = covs)
# Include covariates as X to Y covariates
fit7 <- lucid(G = exposome, Z = methylomics, Y = ck18, lucid_model = "early", K = 2,
              family = "normal", CoY = covs)


# summarize a simple lucid model with a continuous outcome
summary(fit1)


# Visualize LUCID model via a Sankey diagram
plot(fit1)
# Change the node color
plot(fit1, G_color = "yellow")
# Change the link color
plot(fit1, pos_link_color = "red", neg_link_color = "green")


fit8 <- lucid(G = exposome, Z = methylomics, Y = ck18, lucid_model = "early", 
              family = "normal", K = 2:6)
# Check the optimal K
fit8$K
# Look into the tuning process in more details
tune_K <- tune_lucid(G = exposome, Z = methylomics, Y = ck18, lucid_model = "early", 
                     family = "normal", K = 2:6)
fit9 <- tune_K$best_model
ggplot(data = tune_K$tune_list, aes(x = K, y = BIC, label = round(BIC, 0))) + 
       geom_point() +
       geom_line() + 
       geom_text(hjust = -0.2)


# Variable selection for G
set.seed(1008)
# Add 10 more noise variables to the exposome
noise <- matrix(rnorm(420 * 10), nrow = 420)
exposome_noise <- cbind(exposome, noise)
fit10 <- lucid(G = exposome_noise, Z = methylomics, Y = ck18, 
               lucid_model = "early", family = "normal", K = 2,
               Rho_G = seq(0, 0.4, by = 0.01), seed = 1008)
# 1/11 exposures are selected
  
# Summary of optimal lucid model
# summary(fit10)
# Variable selection for Z
# add 10 more noise variables to the methylomics
methylomics_noise <- cbind(methylomics, noise)
fit11 <- lucid(G = exposome, Z = methylomics_noise, Y = ck18, 
               lucid_model = "early", family = "normal", K = 2,
               Rho_Z_Mu = 5:10, Rho_Z_Cov = seq(0.1, 0.4, by = 0.1), seed = 1008)

# 10/20 omics variables are selected  
  
# Summary of optimal lucid model
# summary(fit11)

# Variable selection for G and Z jointly
fit12 <- lucid(G = exposome_noise, Z = methylomics_noise, Y = ck18, 
               lucid_model = "early", family = "normal", K = 2,
               Rho_G = 0.01, Rho_Z_Mu = 10, Rho_Z_Cov = 0.5, seed = 123)

# 1/11 exposures are selected 
# 11/20 omics variables are selected 


# Variable selection for Z (high number of features M)
set.seed(1008)
# Add 200 more noise features to the methylomics
noise <- matrix(rnorm(420 * 200), nrow = 420)
methylomics_noise <- cbind(methylomics, noise)
fit13 <- lucid(G = exposome, Z = methylomics_noise, Y = ck18, 
               family = "normal", lucid_model = "early", K = 2,
               Rho_Z_Mu = 20, Rho_Z_Cov = 0.6)
# Summary of optimal lucid model
# summary(fit13))


# Bootstrap to obtain 95% CI (by default) for LUCID
set.seed(123)
boot1 <- boot_lucid(G = exposome, Z = methylomics, Y = ck18, lucid_model = "early", 
                    model = fit1, R = 200)
# 90% CIs
boot2 <- boot_lucid(G = exposome, Z = methylomics, Y = ck18, lucid_model = "early", 
                    model = fit1, R = 200, conf = 0.9)

# Summary table with 95% bootstrap CIs
summary(fit1, boot.se = boot1)


# Predict cluster with information of Y
pred1 <- predict_lucid(model = fit1, lucid_model = "early", G = exposome, Z = methylomics, Y = ck18)
# Predict cluster without information of Y
pred2 <- predict_lucid(model = fit1, lucid_model = "early", G = exposome, Z = methylomics)
# Predicted cluster label
table(pred1$pred.x)
# Predicted outcome
pred1$pred.y[1:5]


library(visdat)
set.seed(1)
methylomics_miss_sporadic <- methylomics_miss_listwise <- as.matrix(methylomics)
index <- arrayInd(sample(1000, 0.1 * 1000), dim(methylomics))
methylomics_miss_sporadic[index] <- NA # sporadic missing pattern
methylomics_miss_listwise[sample(1:100, 30), ] <- NA # listwise missing pattern
vis_miss(as.data.frame(methylomics_miss_sporadic))
vis_miss(as.data.frame(methylomics_miss_listwise))



fit14 <- lucid(G = exposome, Z = methylomics_miss_listwise, Y = ck18, 
               lucid_model = "early", K = 2, family = "normal")
fit15 <- lucid(G = exposome, Z = methylomics_miss_sporadic, Y = ck18, 
               lucid_model = "early", K = 2, family = "normal")
# summary(fit15)



#Create a list of multi-omics data
omics_lst <- simulated_data[-which(names(simulated_data) == "phenotype")]
Z = omics_lst[c(1:3)]  
#LUCID in parallel, adjusting for the covariates for the E-X and X-Y associations
fit16 <- lucid(G = exposome, Z = Z, Y = ck18, K = list(2, 2, 2), family = "normal", 
               CoY = covs, CoG = covs,
               lucid_model = "parallel", useY = TRUE)
#print the summary of the LUCID in parallel model      
summary(fit16)

#LUCID in parallel, tune for the number of clusters for methylomics
fit17 <- lucid(G = exposome, Z = Z, Y = ck18, K = list(2, 2:3, 2),family = "normal",
               lucid_model = "parallel", useY = TRUE)
# summary(fit17)


#LUCID in serial, adjusting for the covariates of the E-X and X-Y associations
#Tune the number of clusters for methylome and transcriptome
#All 3 sub-models are LUCID early integration models and each with two latent clusters for each layer
fit18 <- lucid(G = exposome, Z = Z, Y = ck18, 
               lucid_model = "serial", CoY = covs, CoG = covs, 
               K = list(2:3, 2:3,2), useY = TRUE, family = "normal")
#Print the summary of the LUCID in serial model
summary(fit18)

# LUCID in serial with the first sub-model being LUCID in parallel and the second sub-model being LUCID early integration
# Rearrange the omics list to match the new structure of the LUCID in serial model
Z_new = list(list(omics_lst$methylome, omics_lst$transcriptome), omics_lst$miRNA)
#Fit the LUCID in serial model with the new omics list and the new K list 
fit19 <- lucid(G = exposome, Z = Z_new, Y = ck18, 
               lucid_model = "serial", CoY = covs, CoG = covs, 
               K = list(list(2, 2), 2), useY = TRUE, family = "normal")
summary(fit19)
