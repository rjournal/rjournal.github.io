## ----message=F,warning=F---------------------------------------------
library(data.table)
library(lubridate)
library(ggplot2)
library(plyr)
library(parallel)

rm(list = ls())


## --------------------------------------------------------------------
file.i <- "CRSP_1960_2019.csv"
select.var <- c("PERMCO","date","COMNAM","SHROUT","SHRCD","DLRET",
                "DLSTCD","DLPDT","EXCHCD","RET","PRC","CUSIP")
DT  <- fread(file.i,select = select.var)


## --------------------------------------------------------------------
DT <- DT[DT$SHRCD %in% 10:11,]
DT <- DT[!is.na(DT$PRC),]
DT <- DT[!DT$PRC %in% (-(4:9)*11),]

DT$RET <- as.numeric(DT$RET)
DT <- DT[!is.na(DT$RET),]

DT$PRC <- abs(DT$PRC)
DT$MKTCAP <- DT$PRC*DT$SHROUT
DT <- DT[DT$EXCHCD %in% 1:3,]


## --------------------------------------------------------------------
DT <- unique(DT)
DT <- DT[order(DT$PERMCO,DT$date),]
DT[ , `:=`( duplicate_N = .N   ) , by= list(PERMCO,date)]
table(DT$duplicate_N)/nrow(DT)*100


## --------------------------------------------------------------------
head(DT[DT$duplicate_N == 7,],7)


## --------------------------------------------------------------------
DT[ , `:=`( duplicate_N = .N   ) , by= list(CUSIP,date)]
table(DT$duplicate_N)/nrow(DT)*100
DT$duplicate_N <- NULL


## --------------------------------------------------------------------
DT$date <- ymd(DT$date)
DT$date <- ceiling_date(DT$date,"m") - 1


## --------------------------------------------------------------------
crsp_keep <- 2
DT[ , `:=`( N_obs = .N   ) , by= list(CUSIP)]
DT <- DT[DT$N_obs >= crsp_keep,]
DT$N_obs <- NULL


## --------------------------------------------------------------------
summary(DT$RET)


## --------------------------------------------------------------------
DT$DLRET <- as.numeric(DT$DLRET)
DT <- DT[order(DT$CUSIP,DT$date),]
DT[,`:=` (last_date = date[.N]),by = list(CUSIP) ]

DT[DT$DLSTCD %in% 100,"DLSTCD"] <- NA
cusip_delist <- unique(DT[!is.na(DT$DLSTCD),])$CUSIP

DT_delist <- DT[DT$CUSIP %in% cusip_delist,]
DT_delist_last <- DT_delist[, .SD[.N],  by= list(CUSIP)]
rm(DT_delist)
DT_delist_last$date <- ceiling_date(DT_delist_last$date + 1,"m")- 1
DT_delist_last$RET <- DT_delist_last$DLRET

na_dlret_index <- is.na(DT_delist_last$RET)
DT_delist_last_na <- DT_delist_last[na_dlret_index,]
DT_delist_last <-DT_delist_last[!na_dlret_index,]

select_code <- c(500,520:551,573,574,580,584)
DT_delist_last_na[DT_delist_last_na$DLSTCD %in% select_code,"RET"] <- -0.3
DT_delist_last_na[!DT_delist_last_na$DLSTCD %in% select_code,"RET"] <- -1

DT_delist_last <- rbind(DT_delist_last,DT_delist_last_na)
DT_delist_last$MKTCAP <- DT_delist_last$MKTCAP*(1 + DT_delist_last$RET)
DT_delist_last$PRC <- DT_delist_last$PRC*(1 + DT_delist_last$RET)

DT <- rbind(DT,DT_delist_last)
DT <- DT[order(DT$CUSIP,DT$date),]
summary(DT$RET)


## --------------------------------------------------------------------
PORT_RET <- DT[,list(EW_RET = lapply(.SD,mean,na.rm = T)), 
               by = list(date), .SDcols = "RET"]
PORT_RET2 <- DT[,list(VW_RET = lapply(.SD,
                                      function(x) sum(x*MKTCAP/sum(MKTCAP,na.rm = T),na.rm = T))),
                by = list(date), .SDcols = "RET"]
PORT_RET <- merge(PORT_RET,PORT_RET2)
rm(PORT_RET2)
PORT_RET <- PORT_RET[order(PORT_RET$date),]


## ----fig.align="center"----------------------------------------------
bar.col <- "gray"
ggplot_recession0 <- geom_rect(fill = bar.col,col = bar.col,
                               aes(xmin=date("1973-11-30"), 
                                   xmax=date("1975-03-31"), 
                                   ymin=-Inf, ymax=Inf))
ggplot_recession1 <- geom_rect(fill = bar.col,col = bar.col,
                               aes(xmin=date("1980-01-31"),
                                   xmax=date("1980-07-31"),
                                   ymin=-Inf, ymax=Inf))
ggplot_recession2 <- geom_rect(fill = bar.col,col = bar.col,
                               aes(xmin=date("1981-07-31"), 
                                   xmax=date("1982-11-30"), 
                                   ymin=-Inf, ymax=Inf))
ggplot_recession3 <- geom_rect(fill = bar.col,col = bar.col,
                               aes(xmin=date("1990-07-31"), 
                                   xmax=date("1991-03-31"), 
                                   ymin=-Inf, ymax=Inf))
ggplot_recession4 <- geom_rect(fill = bar.col,col = bar.col,
                               aes(xmin=date("2001-03-31"), 
                                   xmax=date("2001-11-30"), 
                                   ymin=-Inf, ymax=Inf))
ggplot_recession5 <- geom_rect(fill = bar.col,col = bar.col,
                               aes(xmin=date("2007-12-31"), 
                                   xmax=date("2009-06-30"), 
                                   ymin=-Inf, ymax=Inf))

ds.plot1 <- data.frame(date = PORT_RET$date, CumRet =  cumsum(PORT_RET$EW_RET),
                       Type = "Equally-Weighted" )
ds.plot2 <- data.frame(date = PORT_RET$date, CumRet =  cumsum(PORT_RET$VW_RET),
                       Type = "Value-Weighted" )
ds.plot <- rbind(ds.plot1,ds.plot2)
ds.plot$Type <- as.factor(ds.plot$Type)

p <- ggplot(ds.plot, aes(date, CumRet,colour = Type))
p <- p +  ggplot_recession0 + ggplot_recession1 + 
  ggplot_recession2 + ggplot_recession3 +
  ggplot_recession4 + ggplot_recession5
p <- p + geom_line(alpha = 0.4)
p <- p + xlab("Date") + ylab("Cumulative Return")
p <- p + geom_abline(intercept = 0, slope = 0, color="black",  linetype="dashed", size=0.2)
p


## ----message=F,warning=F---------------------------------------------
library(dplyr)
cut.n <- 5
DT <- DT[,`:=` (Group_Size = ntile(MKTCAP,cut.n)), by = list(date)]
N_G <- DT[,.N, by = list(date,Group_Size) ]
N_G[,mean(N), by = list(Group_Size)]



## --------------------------------------------------------------------
DT <- DT[order(DT$CUSIP,DT$date),]
DT <- DT[,`:=` (RET_1 = shift(RET,-1)), by = list(CUSIP)]


## --------------------------------------------------------------------
DT_size <- DT[,lapply(.SD,mean,na.rm = T),by = list(Group_Size),
              .SDcols = c("RET","RET_1","PRC","SHROUT","MKTCAP","EXCHCD")]
DT_size <- DT_size[order(DT_size$Group_Size),]
DT_size$RET_1 <- DT_size$RET_1*12
DT_size$RET <- DT_size$RET*12
DT_size


## --------------------------------------------------------------------
file.j <- "COMPUSTAT_1960_2020.csv"
select.var2 <- tolower(c("FYEARQ","FQTR","cusip","cik","sic","niq","atq","ceqq"))
DT2  <- unique(fread(file.j,select = select.var2))


## --------------------------------------------------------------------
table(nchar(DT$CUSIP))

## --------------------------------------------------------------------
table(nchar(DT2$cusip))


## --------------------------------------------------------------------
DT2 <- DT2[!nchar(DT2$cusip) == 0,]
DT2$CUSIP <- substr(DT2$cusip,0,8)
DT2$cusip <- NULL
DT2 <- unique(DT2[DT2$CUSIP %in% DT$CUSIP,])


## --------------------------------------------------------------------
DT2$date <- ymd(DT2$fyearq*10000 + DT2$fqtr*3*100 + 1)
DT2$date <- DT2$date + months(6)
DT2$date <- ceiling_date(DT2$date,"q") - 1
DT2 <- DT2[month(DT2$date) == 6,]
DT2$fyearq <- DT2$fqtr <- NULL


## --------------------------------------------------------------------
DT2 <- unique(DT2)
DT2[ , `:=`( duplicate_N = .N   ) , by= list(CUSIP,date)]
table(DT2$duplicate_N)
DT2$duplicate_N <- NULL


## --------------------------------------------------------------------
DT2 <- na.omit(DT2)
DT2$ROA <- DT2$niq/DT2$atq
DT2$ROE <- DT2$niq/DT2$ceqq
DT2$BL <- DT2$atq/DT2$ceqq
DT2$Industry <- floor(DT2$sic/1000)

DT2_sum <- DT2[,lapply(.SD,median,na.rm = T), by = list(Industry),
               .SDcols = c("atq","ROA","ROE","BL")]
DT2_sum <- DT2_sum[order(DT2_sum$Industry),]
DT2_sum


## ----fig.align="center"----------------------------------------------
DT2_Fin <- DT2[DT2$Industry == 6,]
DT2_Fin <- DT2_Fin[,`:=` (Group_Size = ntile(atq,10)), by = list(date)]
DT2_Fin <- DT2_Fin[,`:=` (Total_Assets = sum(atq)), by = list(date)]
DT2_Fin <- DT2_Fin[,lapply(.SD, function(x) sum(x/Total_Assets)), 
                   by = list(date,Group_Size), .SDcols = "atq"]
DT2_Fin_Top <- DT2_Fin[DT2_Fin$Group_Size == 10,]
DT2_Fin_Top <- DT2_Fin_Top[order(DT2_Fin_Top$date),]
plot(atq~date,data = DT2_Fin_Top,type = "l",
     main = "Proportion of Assets held by Top 10% in Financial Industry",
     ylab = "", xlab = "Date")
grid(10)


## --------------------------------------------------------------------
BM <- DT[,c("date","CUSIP","MKTCAP","PRC")]
DT2 <- merge(DT2,BM, by = c("CUSIP","date"), all = F)
DT2$BM_ratio <- DT2$ceqq/(DT2$MKTCAP/1000)
DT2$ME <- DT2$MKTCAP
DT2$PRC_LAST <- DT2$PRC
DT2$MKTCAP <-  DT2$PRC <- NULL
DT2 <- DT2[order(DT2$CUSIP,DT2$date),]
DT2[,`:=` (N_years = 1:.N), by = list(CUSIP)]
rm(BM); gc()


## --------------------------------------------------------------------
prev_june_f <- function(x) floor_date(floor_date(x,"m") + months(6),"y") - months(6) - 1
DT$date_june <- prev_june_f(DT$date)
DT2$date_june <- DT2$date

DT2$date <- NULL


## --------------------------------------------------------------------
x <- DT$date[1:13]
x


## --------------------------------------------------------------------
floor_date(x,"m") + months(6)


## --------------------------------------------------------------------
floor_date(floor_date(x,"m") + months(6),"y")


## --------------------------------------------------------------------
floor_date(floor_date(x,"m") + months(6),"y") - months(6) - 1


## --------------------------------------------------------------------
DT12 <- merge(DT,DT2,by = c("CUSIP","date_june")) 


## --------------------------------------------------------------------
DT12 <- DT12[,`:=` (Group_ME = ntile(ME,5)), by = list(date_june)]
DT12 <- DT12[,`:=` (Group_BM = ntile(BM_ratio,5)), by = list(date_june,Group_ME)]


## --------------------------------------------------------------------
PORT_RET <- DT12[,list(VW_RET = sum(RET*ME/sum(ME))  ),
                 by = list(date,Group_ME,Group_BM)]
PORT_RET <- PORT_RET[order(PORT_RET$date,PORT_RET$Group_BM,PORT_RET$Group_ME),]
PORT_RET$VW_RET <- as.numeric(PORT_RET$VW_RET)*100

ret_matrix <- PORT_RET[,lapply(.SD,mean,na.rm = T),
                       by = list(Group_ME,Group_BM), .SDcols = "VW_RET"]
ret_matrix <- ret_matrix[order(ret_matrix$Group_BM,ret_matrix$Group_ME),]
ret_matrix <- matrix(as.numeric(ret_matrix$VW_RET),5)
rownames(ret_matrix) <- 1:5
colnames(ret_matrix) <- paste("BM",1:5,sep = "_")
rownames(ret_matrix)[1] <- "Small"
rownames(ret_matrix)[5] <- "Big"
colnames(ret_matrix)[1] <- "Low"
colnames(ret_matrix)[5] <- "High"
round(data.frame(ret_matrix*12),2)


## --------------------------------------------------------------------
FF_file <- 
  "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
temp <- tempfile()
download.file(FF_file,temp)
unz_files <- unzip(temp)
ds <- read.csv(unz_files,skip = 3)
flag_obs <- grep("Annual",ds[,1],ignore.case = T)
ds <- ds[1:(flag_obs-1),]
names(ds)[1] <- "date"
ds <- data.frame(apply(ds, 2, as.numeric))
ds$date <- ceiling_date(ymd(ds$date*100+ 01),"m")-1
tail(ds)


## --------------------------------------------------------------------
PORT_RET_RA <- merge(PORT_RET,ds)
PORT_RET_RA$RAR <- PORT_RET_RA$VW_RET - PORT_RET_RA$RF

lm_list <- dlply(PORT_RET_RA,c("Group_ME","Group_BM"),
                 function(x)  lm( RAR ~ Mkt.RF +  SMB +   HML, data = x   )    )  
rar_matrix <- lapply(lm_list, coef)
rar_matrix <- sapply(rar_matrix,function(x) x[[1]])
rar_matrix <- matrix(rar_matrix,5,5,byrow = T)

rownames(rar_matrix) <- 1:5
colnames(rar_matrix) <- paste("BM",1:5,sep = "_")
rownames(rar_matrix)[1] <- "Small"
rownames(rar_matrix)[5] <- "Big"
colnames(rar_matrix)[1] <- "Low"
colnames(rar_matrix)[5] <- "High"

rar_matrix <- round(data.frame(rar_matrix*12),2)
rar_matrix


## ----echo = F--------------------------------------------------------
fwrite(DT12,"DT12.csv")


## ----warning=F,message=F---------------------------------------------
FF3_anomaly <- function(min_price,year_keep = 1,year1 = 1965,year2 =  2019) {
  DT12_sub <- DT12
  keep_cusip_date <- unique(DT12_sub[,list(CUSIP,date_june,N_years,PRC_LAST)])
  keep_cusip_date <- keep_cusip_date[keep_cusip_date$N_years >= year_keep,]
  keep_cusip_date <- keep_cusip_date[keep_cusip_date$PRC_LAST >= min_price ,]
  
  keep_cusip_date <- unique(keep_cusip_date[,list(CUSIP,date_june)])
  DT12_sub <- merge(DT12_sub,keep_cusip_date)
  
  DT12_sub <- DT12_sub[,`:=` (Group_ME = ntile(ME,5)), by = list(date_june)]
  DT12_sub <- DT12_sub[,`:=` (Group_BM = ntile(BM_ratio,5)), by = list(date_june,Group_ME)]
  N_G <- DT12_sub[,.N, by = list(date,Group_ME,Group_BM) ]
  N_G <- (N_G[,mean(N), by = list(Group_ME,Group_BM) ])
  N_G <- N_G[order(N_G$Group_ME,N_G$Group_BM),] 
  N_G <- matrix(N_G$V1,5,5,byrow = T)
  rownames(N_G) <- 1:5
  colnames(N_G) <- paste("BM",1:5,sep = "_")
  rownames(N_G)[1] <- "Small"
  rownames(N_G)[5] <- "Big"
  colnames(N_G)[1] <- "Low"
  colnames(N_G)[5] <- "High"
  N_G <- round((N_G))
  
  PORT_RET <- DT12_sub[,list(VW_RET = sum(RET*ME/sum(ME))  ),
                       by = list(date,Group_ME,Group_BM)]
  PORT_RET <- PORT_RET[order(PORT_RET$date,PORT_RET$Group_BM,PORT_RET$Group_ME),]
  PORT_RET$VW_RET <- as.numeric(PORT_RET$VW_RET)*100
  
  PORT_RET_RA <- merge(PORT_RET,ds)
  PORT_RET_RA$RAR <- PORT_RET_RA$VW_RET - PORT_RET_RA$RF
  PORT_RET_RA_sub <- PORT_RET_RA
  PORT_RET_RA_sub <- PORT_RET_RA_sub[year(PORT_RET_RA_sub$date) >= year1,] 
  PORT_RET_RA_sub <- PORT_RET_RA_sub[year(PORT_RET_RA_sub$date) <= year2 ,] 
  
  ret_matrix <- PORT_RET_RA_sub[,lapply(.SD,mean,na.rm = T),
                                by = list(Group_ME,Group_BM), .SDcols = "VW_RET"]
  ret_matrix <- ret_matrix[order(ret_matrix$Group_BM,ret_matrix$Group_ME),]
  ret_matrix <- matrix(as.numeric(ret_matrix$VW_RET),5)
  rownames(ret_matrix) <- 1:5
  colnames(ret_matrix) <- paste("BM",1:5,sep = "_")
  rownames(ret_matrix)[1] <- "Small"
  rownames(ret_matrix)[5] <- "Big"
  colnames(ret_matrix)[1] <- "Low"
  colnames(ret_matrix)[5] <- "High"
  ret_matrix <- round(data.frame(ret_matrix*12),2)
  
  
  lm_list <- dlply(PORT_RET_RA_sub,c("Group_ME","Group_BM"), 
                   function(x)  lm( RAR ~ Mkt.RF +  SMB +   HML, data = x   )    )  
  rar_matrix <- lapply(lm_list, coef)
  rar_matrix <- sapply(rar_matrix,function(x) x[[1]])
  rar_matrix <- matrix(rar_matrix,5,5,byrow = T)
  rownames(rar_matrix) <- 1:5
  colnames(rar_matrix) <- paste("BM",1:5,sep = "_")
  rownames(rar_matrix)[1] <- "Small"
  rownames(rar_matrix)[5] <- "Big"
  colnames(rar_matrix)[1] <- "Low"
  colnames(rar_matrix)[5] <- "High"
  rar_matrix <- round(data.frame(rar_matrix*12),2)
  
  
  list(N_G,ret_matrix,rar_matrix)
  
}





## --------------------------------------------------------------------
p_seq <- 0:50
list_port_price <- mclapply(p_seq, function(p) FF3_anomaly(p),mc.cores = 4)
gc()


## --------------------------------------------------------------------
list_port_price[[1]][[2]]


## --------------------------------------------------------------------
list_port_price[[6]][[2]]


## ----fig.align="center"----------------------------------------------
RAR_list <- lapply(list_port_price,function(x) x[[2]])
RAR_list_Value <- t(sapply(RAR_list, function(x) x[,5] - x[,1]  ))
colnames(RAR_list_Value) <- rownames(RAR_list[[1]])
RAR_list_Size <- t(sapply(RAR_list, function(x) x[1,] - x[5,]  ))

ds.plot <- lapply(1:5, function(i) data.frame(HML =  RAR_list_Value[,i],
                                              Min_Price = p_seq, Size = i))
ds.plot <- Reduce(rbind,ds.plot)
ds.plot <- ds.plot[order(ds.plot$Size,ds.plot$Min_Price),]
ds.plot <- ds.plot[ds.plot$Size %in% c(1,3,5),]
ds.plot$Size <- as.factor(ds.plot$Size)
p <- ggplot(ds.plot,aes(x = Min_Price, y = HML, colour = Size, shape = Size))
p <- p + geom_point() + geom_line()
p <- p + geom_abline(intercept = 0, slope = 0, color="black",  linetype="dashed")
p


## ----fig.align="center"----------------------------------------------
ds.plot <- lapply(1:5, function(i) data.frame(SMB =  unlist(RAR_list_Size[,i]),
                                              Min_Price = p_seq, BM = i))
ds.plot <- Reduce(rbind,ds.plot)
ds.plot <- ds.plot[order(ds.plot$BM,ds.plot$Min_Price),]
ds.plot <- ds.plot[ds.plot$BM %in% c(1,3,5),]
ds.plot$BM <- as.factor(ds.plot$BM)
p <- ggplot(ds.plot,aes(x = Min_Price, y = SMB, colour = BM, shape = BM))
p <- p + geom_point() + geom_line()
p <- p + geom_abline(intercept = 0, slope = 0, color="black",  linetype="dashed")
p


############
# let's compute beta on a rolling window
library(rollRegres)
for(est_window in c(120)){
  BETA <- DT[,c("date","CUSIP","RET")]
  BETA <- merge(BETA,ds[,c("date","Mkt.RF","RF")], by = "date")
  BETA$E_RET <- (BETA$RET - BETA$RF/100)*100
  BETA$RF <- NULL
  # keep stocks with at least 12 months
  BETA[, count := (.N), by = CUSIP]
  BETA <- BETA[order(BETA$CUSIP,BETA$date),]
  BETA <- BETA[BETA$count   > est_window,]
  
  BETA <- BETA %>%
    group_by(CUSIP) %>%
    do(.,mutate(.,Beta = roll_regres(E_RET ~ Mkt.RF,data = .,est_window)$coef[,2]))
  
  BETA <- data.table(BETA)
  BETA <- na.omit(BETA[,list(date,CUSIP,Beta)])
  
  DT_capm <- merge(DT,BETA, by = c("CUSIP","date"), all = F)
  DT_capm <- DT_capm[order(DT_capm$CUSIP,DT_capm$date),]
  DT_capm <- DT_capm[,`:=` (Group_Beta = ntile(Beta,10)), by = list(date)]
  
  
  
  ## --------------------------------------------------------------------
  PORT_RET <- DT_capm[,list(EW_RET = 12*100*mean(RET_1,na.rm = T),
                            VW_RET = 12*100*sum(RET_1*MKTCAP/sum(MKTCAP),na.rm = T),
                            EW_BETA =  mean(Beta,na.rm = T),
                            VW_BETA = sum(Beta*MKTCAP/sum(MKTCAP),na.rm = T)),
                      by = list(date,Group_Beta)]
  PORT_RET <- PORT_RET[order(PORT_RET$Group_Beta,PORT_RET$date),]
  
  CAPM_result <- PORT_RET[,lapply(.SD,mean,na.rm = T),
                          by = list(Group_Beta), 
                          .SDcols = c("EW_RET","VW_RET","EW_BETA","VW_BETA")]
  CAPM_result <- CAPM_result[order(CAPM_result$Group_Beta),]
  CAPM_result
  
  ds_sub <- ds[ds$date %in% PORT_RET$date,]
  
  
  p2 <- ggplot(CAPM_result, aes(VW_BETA, VW_RET)) +
    geom_point()
  p2 <- p2 + geom_smooth(method = "lm")
  p2 <- p2 +ylim(c(5,15)) + xlim(c(0,2.5))
  p2 <- p2 + geom_abline(intercept = mean(ds_sub$RF)*12, slope = mean(ds_sub$Mkt.RF)*12, color="red", 
                         linetype="dotted", size=1)
  
}



