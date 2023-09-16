library(ClusROC)

## ---- Loading data ----
data("MouseNeurons")
head(MouseNeurons)

## ---- The cluster-effect model without Box-Cox transformation ----
out_md_0 <- clus_lme(fixed_formula = Lamp5_cpm ~ age_days,
                     name_class = "subclass_label", name_clust =  "genotype_id",
                     data = MouseNeurons)
print(out_md_0)
plot(out_md_0)

pdf(file = "diagnosis_model_0.pdf", width = 7, height = 7)
plot(out_md_0)
dev.off()

## ---- The cluster-effect model with Box-Cox transformation ----
out_md <- clus_lme(fixed_formula = Lamp5_cpm ~ age_days,
                   name_class = "subclass_label", name_clust =  "genotype_id",
                   data = MouseNeurons, boxcox = TRUE)
print(out_md)
plot(out_md)

pdf(file = "diagnosis_model.pdf", width = 7, height = 7)
plot(out_md)
dev.off()

## ---- covariate-specific VUS ----
out_vus <- clus_vus(out_clus_lme = out_md,
                    newdata = data.frame(age_days = c(54, 60, 66)))
print(out_vus)

ci_clus_vus(out_vus)

## ---- covariate-specific ROC surface ----
clus_roc_surface(out_clus_lme = out_md, newdata = data.frame(age_days = 58),
                 main = "Age-Specific ROC surface, at 58 days")

rgl::rgl.snapshot("ROCS_Age_58.png")


clus_roc_surface(out_clus_lme = out_md, newdata = data.frame(age_days = 58),
                 main = "Age-Specific ROC surface, at 58 days",
                 ellips = TRUE, thresholds = c(350, 1350))

rgl::rgl.snapshot("ROCS_Age_58_ellip.png")

clus_tcfs(out_clus_lme = out_md, newdata = data.frame(age_days = c(54, 58, 62)),
          thresholds = c(350, 1350), ap_var = TRUE)

## ---- covariate-specific optimal pairs of thresholds ----
out_thresh <- clus_opt_thres3(method = c("GYI", "CtP", "MV"),
                              out_clus_lme = out_md,
                              newdata = data.frame(age_days = c(55, 65, 75)),
                              ap_var = TRUE,
                              control = list(n_boot = 1000, parallel = TRUE,
                                             ncpus = 8))
print(out_thresh)

plot(out_thresh, colors = c("forestgreen", "blue", "red"),
     xlims = c(250, 1300), ylims = c(500, 2250), names.labels = "Age in days:",
     size.point = 0.9)

plot(out_thresh, colors = c("forestgreen", "blue", "red"),
     xlims = c(250, 1300), ylims = c(500, 2250), names.labels = "Age in days:",
     size.point = 0.9, file.name = "optThres_ellip.pdf", width = 6, height = 4)

## ---- Example 2: EnergyEthiopia data ----

## ---- Loading data ----
data("EnergyEthiopia")
head(EnergyEthiopia)

## uqid ---> cluster: household
## energy2 ---> class:
## clean fuel only (j = 1),
## a mix of clean and biomass fuels (j = 2),
## biomass fuel only (j = 3).

## ---- The cluster-effect model ----
out_md_enery <- clus_lme(
  fixed_formula = lrconsaeu ~ hhs_ft + lfirewood_pr + lcharcol_pr + 
    lkerosene_pr,
  name_class = "energy2", name_clust = "uqid",
  data = EnergyEthiopia, boxcox = FALSE
)

print(out_md_enery)

pdf(file = "diagnosis_model_energy.pdf", width = 7, height = 7)
plot(out_md_enery)
dev.off()

## ---- covariate-specific VUS ----
out_vus_enery <- clus_vus(
  out_clus_lme = out_md_enery,
  newdata = data.frame(hhs_ft = c("small", "medium", "large", "very large"), 
                       lfirewood_pr = c(1, 1, 1, 1), 
                       lcharcol_pr = c(-1, -1, -1, -1), 
                       lkerosene_pr = c(2, 2, 2, 2))
)

out_vus_enery
ci_clus_vus(out_vus_enery)

## ---- covariate-specific ROC surface ----

clus_roc_surface(
  out_clus_lme = out_md_enery,
  newdata = data.frame(hhs_ft = "large", lfirewood_pr = 1,
                       lcharcol_pr = -1, lkerosene_pr = 2),
  file_name = "ROCS_Energy_ex1.png"
)


out_tcfs_energy <- clus_tcfs(
  out_clus_lme = out_md_enery,
  newdata = data.frame(hhs_ft = c("small", "medium", "large", "very large"), 
                       lfirewood_pr = c(1, 1, 1, 1), 
                       lcharcol_pr = c(-1, -1, -1, -1), 
                       lkerosene_pr = c(2, 2, 2, 2)),
  thresholds = c(3.75, 4.75), ap_var = TRUE
)
out_tcfs_energy

## ---- covariate-specific optimal pairs of thresholds ----
out_thresh_enery <- clus_opt_thres3(
  method = c("GYI", "CtP", "MV"), out_clus_lme = out_md_enery,
  newdata = data.frame(hhs_ft = c("small", "medium", "large", "very large"), 
                       lfirewood_pr = c(1, 1, 1, 1), 
                       lcharcol_pr = c(-1, -1, -1, -1), 
                       lkerosene_pr = c(2, 2, 2, 2)),
  ap_var = TRUE
)

out_thresh_enery

plot(out_thresh_enery, colors = c("orange", "red", "blue", "forestgreen"), 
     file_name = "optThres_energy_ellip.pdf", nrow_legend = 2, 
     width = 6, height = 4)
