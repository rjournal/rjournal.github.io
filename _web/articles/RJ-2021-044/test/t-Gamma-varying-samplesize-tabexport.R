rm(list=ls(all=TRUE))



if(FALSE)
{
  load("res-gamma-n1000-n1e+07-M10.RData")
  resgamma_n1e7 <- resgamma
  load("res-gamma-n1e+08-n1e+08-M4.RData")
  load("res-gamma-n1e+09-n1e+09-M4.RData")
  
  resgamma <- cbind(resgamma_n1e7,
                    "1e+08"=apply(resgamma_n1e8["time", , , "n1e+08"], 1, mean),
                    "1e+09"=apply(resgamma_n1e9["time", , , "n1e+09"], 1, mean))
  
}



tabtime<-resgamma[-1, ]
row.names(tabtime)<- c("MLE", "LCE")
colnames(tabtime) <- paste0("10^", 3:9)

capture.output(xtable( tabtime, label="tab:gamma:time:varying:size",
                      caption="Average computation time (s)", digits=4),
               file="tab-gamma-varying-size.tex")
