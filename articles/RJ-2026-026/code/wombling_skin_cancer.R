require(nimblewomble)
require(nimble)
require(metR)
require(cowplot)
require(ggplot2)
require(patchwork)
require(MBA)
require(raster)
load("code/genes.RData")

############
# HVG Gene #
############
id = seq(1, nrow(genes_analysis), by = 6)
coords = genes_analysis[id, c("x", "y")]
colnames(coords) = c("array_row", "array_col")

gene = "COL1A1"

y = genes_analysis[id, gene]; 
N = length(y)
p1 = sp_ggplot(data_frame = data.frame(coords, z = y), 
               extend = FALSE, title = gene)

raster_surf = raster(mba.surf(data.frame(cbind(coords, z = y)),
                              no.X = 300,
                              no.Y = 300,
                              h = 5,
                              m = 2,
                              extend = TRUE, sp = FALSE)$xyz.est)
x = rasterToContour(raster_surf, nlevel = 10)
x.levels <- as.numeric(as.character(x$level))

curves.pm.subset = subset(x, level == 60)
curves.pm.1 = curves.pm.subset@lines[[1]]@Lines[[1]]@coords; colnames(curves.pm.1) = c("x", "y")
curves.pm.2 = curves.pm.subset@lines[[1]]@Lines[[2]]@coords; colnames(curves.pm.2) = c("x", "y")

mc_sp = gp_fit(coords = coords,
               y = y, kernel = "matern2")
mc_sp$estimates
model = zbeta_samples(coords = coords,
                      y = y,
                      model = mc_sp$mcmc,
                      kernel = "matern2") 

estimates = t(round(apply(model, 2, quantile, probs = c(0.5, 0.025, 0.975)), 3))

yfit = estimates[paste0("z[", 1:N, "]"), "50%"] + estimates["beta[0]", "50%"]
ylow = estimates[paste0("z[", 1:N, "]"), "2.5%"] + estimates["beta[0]", "2.5%"]
yhigh = estimates[paste0("z[", 1:N, "]"), "97.5%"] + estimates["beta[0]", "97.5%"]
fit_frame = data.frame(cbind(est = yfit, `2.5%` = ylow, `97.5%` = yhigh))
fit_frame$sig = significance(data_frame = fit_frame)
colnames(fit_frame) = c("est", "2.5%", "97.5%", "sig")

p2 = sp_ggplot(data_frame = data.frame(coords, z = yfit, sig = fit_frame$sig), 
               extend = FALSE, title = gene)
p1 + p2

###############
# Create Grid #
###############
gene_bdry = chull(coords[,"array_row"], coords[,"array_col"])
gene.shp = spPolygons(as.matrix(coords[gene_bdry,], ncol = 2))

range_x = range(coords[,"array_row"])
range_y = range(coords[,"array_col"])

xseq = seq(range_x[1], range_x[2], length.out = 21)[-c(1, 21)]
yseq = seq(range_y[1], range_y[2], length.out = 21)[-c(1, 21)]

grid = expand.grid(xseq, yseq)
tmp = sp::over(sp::SpatialPoints(grid), gene.shp)
grid = grid[!is.na(tmp),]

colnames(grid) = c("array_row", "array_col")

grid = as.matrix(grid)
coords = as.matrix(coords)

# takes about ~15 mins
# gradients = sprates(grid = grid,
#                     coords = coords,
#                     model = model,
#                     kernel = "matern2")
# pre-saved output
load("code/data/COL1A1_gradients.RData")

p3 = sp_ggplot(data_frame = data.frame(grid,
                                       z = gradients$estimate.sx[,"50%"],
                                       sig = gradients$estimate.sx$sig),
               extend = FALSE, title = paste0(gene, "-SX"))
p4 = sp_ggplot(data_frame = data.frame(grid,
                                       z = gradients$estimate.sy[,"50%"],
                                       sig = gradients$estimate.sy$sig),
               extend = FALSE, title = paste0(gene, "-SY"))
p5 = sp_ggplot(data_frame = data.frame(grid,
                                        z = gradients$estimate.sxx[,"50%"],
                                        sig = gradients$estimate.sxx$sig),
               extend = FALSE, title = paste0(gene, "-SXX"))
p6 = sp_ggplot(data_frame = data.frame(grid,
                                        z = gradients$estimate.sxy[,"50%"],
                                        sig = gradients$estimate.sxy$sig),
               extend = FALSE, title = paste0(gene, "-SXY"))
p7 = sp_ggplot(data_frame = data.frame(grid,
                                        z = gradients$estimate.syy[,"50%"],
                                        sig = gradients$estimate.syy$sig),
               extend = FALSE, title = paste0(gene, "-SYY"))
((p2 + p3 + p4)/(p5 + p6 + p7))


curve = curves.pm.2
tvec = sapply(1:(nrow(curve) - 1), function(x) sqrt(sum((curve[(x + 1),] - curve[x,])^2)))
umat = as.matrix(t(sapply(1:(nrow(curve) - 1), function(x) (curve[(x + 1),] - curve[x,])))/tvec)

# takes about 30-35mins
# wm_c1 = spwombling(coords = coords,
#                    curve = curves.pm.2,
#                    model = model,
#                    kernel = "matern2")
# pre-saved output
load("code/data/COL1A1_wm.RData")

# Total wombling measure for gradient
colSums(wm_c1$estimate.wm.1[,-4]); round(colSums(wm_c1$estimate.wm.1[,-4])/sum(tvec), 3)
# Total wombling measure for curvature
colSums(wm_c1$estimate.wm.2[,-4]); round(colSums(wm_c1$estimate.wm.2[,-4])/sum(tvec), 3)

# Color code points based on significance
col.pts.1 = sapply(wm_c1$estimate.wm.1$sig, function(x){
  if(x == 1) return("green")
  else if(x == -1) return("cyan")
  else return(NA)
})

col.pts.2 = sapply(wm_c1$estimate.wm.2$sig, function(x){
  if(x == 1) return("green")
  else if(x == -1) return("cyan")
  else return(NA)
})

p8 = sp_ggplot(data_frame = data.frame(coords, y), 
               extend = FALSE, title = gene)
p9 = p8 + geom_path(curve, mapping = aes(x, y), linewidth = 2) +
  geom_path(curve, mapping = aes(x, y),
            colour = c(col.pts.1, NA), linewidth = 1, na.rm = TRUE)
p10 = p8 + geom_path(curve, mapping = aes(x, y), linewidth = 2) +
  geom_path(curve, mapping = aes(x, y),
            colour = c(col.pts.2, NA), linewidth = 1, na.rm = TRUE)
p9 + p10
