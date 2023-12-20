## ROC plots
library(fnets)
library(ggplot2)
library(gridExtra)

grid <- seq(0, 1, length.out = 200)
## delta

# e1

pl <- list()
np <- cbind(c(200, 200, 500, 500),
            c(50, 100, 100, 200))
adap_th_list <-  list(adap_th_20050,
                   adap_th_200100,
                   adap_th_500100,
                   adap_th_500200)
adap_th_random_list <- list(adap_th_20050_random,
                         adap_th_200100_random,
                         adap_th_500100_random,
                         adap_th_500200_random)
adap_th_band_list <- list(adap_th_20050_band,
                       adap_th_200100_band,
                       adap_th_500100_band,
                       adap_th_500200_band)
adap_th_list_list <- list(adap_th_list, adap_th_random_list, adap_th_band_list)
 
 
k <- 3 #omega.lasso #fig 7
for(rr in 1:nrow(np)){
  n <- np[rr, 1]; p <- np[rr, 2]
  
  df <- data.frame()
  for(ii in c(1,3) ){
    #load(file = paste('~/downloads/sim/sim_station_var/res/C', ii - 1, '_n', n, 'p', p, '.RData', sep = ''))
    ls <- adap_th_list_list[[ii]][[rr]]
    
    mm <- c('E1', 'E2', 'E2')[ii]
    # ls$roc
    tmp <- data.frame(FDR = grid[1:max(which(ls$roc[, k, 2] > 0))], TPR = ls$roc[1:max(which(ls$roc[, k, 2] > 0)), k, 1]/ls$roc[1:max(which(ls$roc[, k, 2] > 0)), k, 2], 
                      n = n, p = p, method = 'CLIME', model = mm)
    df <- rbind.data.frame(df, tmp)
    
    tmp <- data.frame(FDR = grid[1:max(which(ls$roc_adap[, k, 2] > 0))], TPR = ls$roc_adap[1:max(which(ls$roc_adap[, k, 2] > 0)), k, 1]/ls$roc_adap[1:max(which(ls$roc_adap[, k, 2] > 0)), k, 2], 
                      n = n, p = p, method = 'ACLIME', model = mm)
    df <- rbind.data.frame(df, tmp)
  }
  
  pl[[rr]] <- 
    ggplot(df, aes(x = FDR, y = TPR, color = model, linetype = method)) +
    geom_line() +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(title = paste("n = ", n, ", p = ", p, sep = '')) +
    theme_classic() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    geom_vline(xintercept = .05, linetype = 4, color = 'darkgrey') 
  if(rr == 1) pl[[rr]] <- pl[[rr]] + theme(legend.position = c(.8, .5), legend.key.height = unit(.1, 'cm')) + guides(fill = guide_legend(ncol = 2))
  if(rr != 1) pl[[rr]] <- pl[[rr]] + theme(legend.position = "none")
}

grid.arrange(grobs = pl, layout_matrix = matrix(1:4, nrow = 2, byrow = FALSE))

