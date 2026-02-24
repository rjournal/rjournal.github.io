#01CreateManuscriptInR.R
install.packages("tinytex") # one-time
tinytex::install_tinytex()      # one-time
tinytex::tlmgr_update()         # if already installed
tinytex::is_tinytex()     

tinytex::tlmgr_install("pgf")
tinytex::tlmgr_install("fancyhdr")
tinytex::tlmgr_install("microtype")
tinytex::tlmgr_install("setspace")
tinytex::tlmgr_install("titlesec")
tinytex::tlmgr_install("placeins")
tinytex::tlmgr_install("caption")
tinytex::tlmgr_install("environ")
tinytex::tlmgr_install("upquote")

tinytex::tlmgr_install(c(
  "psnfss",              # Palatino + friends (pslatex, mathpazo live here)
  "pslatex",
  "mathpazo",
  "palatino",
  "microtype",
  "cm-super"             # full T1 Computer Modern; often helps with T1/font issues
))

tinytex::tlmgr_install(c("collection-fontsrecommended", "collection-latexrecommended"))

install.packages("tikzDevice")
install.packages("rjtools") # one-time

library(rjtools)
setwd(file.path(getwd(),"05OurPublication2"))
name <- "memshare"
create_article(name = name,file = xfun::with_ext(name,"Rmd"))

##prepare via chat gpt the table for MacOs ----
load(file,path(getwd(),"05OurPublication/data/DF_Results_mac.rda"))
is_num_DF_Results<- sapply(DF_Results, is.numeric)
DF_Results[is_num_DF_Results] <- lapply(DF_Results[is_num_DF_Results], function(x) {
  return(round(x,4))
})
dput(DF_Results) #->prompting

ind <- which(stat_amad$Type=="memshare"|stat_amad$Type=="SharedObject")
stat_amad_sel <- stat_amad[ind,]

#is_num_stat_amad<- sapply(stat_amad, is.numeric)
# stat_amad[is_num_stat_amad] <- lapply(stat_amad[is_num_stat_amad], function(x) {
#   return(round(x,6))
# })
Diff_Sec <- round(as.numeric(stat_amad_sel$Diff_Sec[,1]),5)
MemoryDiff_MB <- round(as.numeric(stat_amad_sel$MemoryDiff_MB[,1]),5)
mem_after_call <- round(as.numeric(stat_amad_sel$mem_after_call[,1]),5)
DF_out <- data.frame(stat_amad_sel$Exponent,stat_amad_sel$Type,Diff_Sec,MemoryDiff_MB,mem_after_call)
dput(DF_out) #->prompting


##prepare via chat gpt the table for Windows for appendix ----
load(file.path(getwd(),"05OurPublication/data/DF_Results_Windows.rda"))
is_num_DF_Results<- sapply(DF_Results, is.numeric)
DF_Results[is_num_DF_Results] <- lapply(DF_Results[is_num_DF_Results], function(x) {
  return(round(x,4))
})
dput(DF_Results) #->prompting

ind <- which(stat_amad$Type=="memshare"|stat_amad$Type=="SharedObject")
stat_amad_sel <- stat_amad[ind,]

#is_num_stat_amad<- sapply(stat_amad, is.numeric)
# stat_amad[is_num_stat_amad] <- lapply(stat_amad[is_num_stat_amad], function(x) {
#   return(round(x,6))
# })
Diff_Sec <- round(as.numeric(stat_amad_sel$Diff_Sec[,1]),5)
MemoryDiff_MB <- round(as.numeric(stat_amad_sel$MemoryDiff_MB[,1]),5)
mem_after_call <- round(as.numeric(stat_amad_sel$mem_after_call[,1]),5)
DF_out <- data.frame(stat_amad_sel$Exponent,stat_amad_sel$Type,Diff_Sec,MemoryDiff_MB,mem_after_call)
dput(DF_out) #->prompting
