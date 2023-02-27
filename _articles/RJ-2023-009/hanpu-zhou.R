#to get figure 1 
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if(!require("ggpubr")){install.packages("ggpubr")}

CI_num <- c(6, 4, 16, 11, 8, 4, 10, 8, 8, 12, 10, 11)
BS_num <- c(2, 1, 4, 4, 1, 2, 4, 1, 3, 0, 2, 1)
IBS_num <- c(3, 0, 3, 3, 2, 4, 4, 5, 1, 1, 1, 1)
IAE_num <- c(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0,0)
ISE_num <- c(0, 0, 1, 0, 0, 2, 2, 2, 2, 1, 2, 0)
mydata <- data.frame(CI_num, BS_num, IBS_num, IAE_num, ISE_num)
metrics <-  c('C-index', 'Brier score', 'IBS', 'IAE', 'ISE')
names(mydata) <-  metrics
tot_num <- apply(mydata, 2, sum)
data_bar <-  data.frame(metrics = factor(metrics,levels = c("C-index", "Brier score", "IBS", "IAE", "ISE")),
                      tot_num)

#bar
p1 <-  ggplot(data_bar, aes(x=metrics, y=tot_num, fill = metrics)) + 
  geom_bar(stat = "identity", width=0.5)+
  theme(legend.position = 'none',
        title = element_text(size = 15),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  labs(x = '', y = 'Frequency',title = 'Frequency of metrics from 2010 to 2021')+
  scale_fill_manual(values = c("#4169E1", "#6495ED","#C71585", "#88CCFF","#008080"))

#line
data_line0  <-  cbind('year' =  c(2010:2021), mydata)
data_line  <-  melt(data_line0, id = 'year')
names(data_line)  <-  c("year", 'metrics_id', 'values')
p2 <-  ggplot(aes(x = year, y = values,shape = metrics_id ,color = metrics_id,fill = metrics_id), data = data_line)+
  geom_line(size = 1)+
  geom_point(color="black", size=2)+
  labs(x = '', y = 'Frequency')+
  scale_shape_manual(values = c(0,1,2,3,5))+
  theme(legend.text=element_text(size=12), 
        axis.text=element_text(size=12),  
        axis.title=element_text(size=14)) 
p2 <-  p2 + scale_x_continuous(breaks=c(2010:2021))
p2 <-  p2 + scale_color_manual(values = c("#4169E1", "#6495ED","#C71585", "#88CCFF","#008080"))

re_barline <- ggarrange(p1,p2, nrow = 2)
re_barline
ggsave("bothbarline.pdf",width = 12,height = 7)



#to get figure 2
# library
library(ggplot2)
tem <-  cbind(rep(c(2010:2021),5),melt(data_line0[,-1]))
data <-  data.frame(tem)
names(data) <-  c("year","metrics_id","Frequency")

# Stacked
re_pies <- ggplot(data, aes(fill=metrics_id, y=Frequency, x=year)) + 
      geom_bar(position="fill", stat="identity",width = 0.8)+  #position="stack"
      scale_x_continuous(breaks=c(2010:2021))+
      scale_fill_manual(values = c("#4169E1", "#6495ED","#C71585", "#88CCFF","#008080"))+
      labs(x = '', y = 'Frequency',title = 'Proportion of metrics from 2010 to 2021')

re_pies
ggsave("pie.pdf",width = 10,height = 4)



#to get figure 3
if(!require("caret")){install.packages("caret")}
if(!require("randomForestSRC")){install.packages("randomForestSRC")}
if(!require("survival")){install.packages("survival")}
if(!require("pec")){install.packages("pec")}
if(!require("SurvMetrics")){install.packages("SurvMetrics")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if(!require("ggpubr")){install.packages("ggpubr")}

set.seed(1)
GetAllMetrics <- function(mydata){
  C <- 5    
  ans_cox <- rep(0, 5)
  
  for (i in 1:C) {
    index_data <- createFolds(1:nrow(mydata),2)
    metrics_cox <- rep(0, 5)
    names(metrics_cox) <- c('Cindex','Brier Score', 'IBS', 'IAE', 'ISE')
    
    for (cv in 1:2){
      train_data <- mydata[index_data[[cv]],]
      test_data <- mydata[index_data[[-cv]],]
      
      dis_time <- sort(train_data$time[train_data$status==1])
      fitcox <- coxph(Surv(time,status)~., data = train_data, x = TRUE)
      mat_cox <- predictSurvProb(fitcox, test_data, dis_time)
      
      med_index <- median(1:length(dis_time))
      surv_obj <- Surv(test_data$time,test_data$status)
      
      metrics_cox[1] <- metrics_cox[1] + Cindex(surv_obj, mat_cox[,med_index])
      metrics_cox[2] <- metrics_cox[2] + Brier(surv_obj, mat_cox[,med_index], dis_time[floor(med_index)])
      metrics_cox[3] <- metrics_cox[3] + IBS(surv_obj, mat_cox, dis_time)
      metrics_cox[4] <- metrics_cox[4] + IAEISE(surv_obj, mat_cox, dis_time)[1]
      metrics_cox[5] <- metrics_cox[5] + IAEISE(surv_obj, mat_cox, dis_time)[2]
    }
    ans_cox <- ans_cox + metrics_cox/2
  }
  ans_cox <- ans_cox/C
  ansmetrics <- ans_cox
  return(ansmetrics)
}

Scenario1 <- as.data.frame(matrix(0, ncol = 5))
colnames(Scenario1) <- c('Cindex','Brier Score', 'IBS', 'IAE', 'ISE')
Scenario2 <- Scenario1
Scenario3 <- Scenario1

N <- 20     #repeat 20 times 5*2 CV

#In the current version of the SurvMetrics package, 
#the computation speed for IBS depends on the data scale and 
#may be a bit slow at the moment, 
#we will improve it further in the future.

for (k in 1:N) {
  mydata <- SDGM1(N = 300, p = 25, c_mean = 0.4)
  ans <- GetAllMetrics(mydata)
  Scenario1[k,] <- ans
}


for (k in 1:N) {
  mydata <- SDGM3(N = 300, p = 25, u_max = 9)
  ans <- GetAllMetrics(mydata)
  Scenario2[k,] <- ans
}


for (k in 1:N) {
  mydata <- SDGM4(N = 300, p =  25, c_step = 0.6)
  ans <- GetAllMetrics(mydata)
  Scenario3[k,] <- ans
}


#get the boxplot
Assumption1 <- Scenario1$Cindex
Assumption2 <- Scenario2$Cindex
Assumption3 <- Scenario3$Cindex
mydata <- data.frame(Assumption1, Assumption2, Assumption3)
x_name <- c("Scenario1", "Scenario2", "Scenario3")
names(mydata) <- x_name

p_mydata <- melt(mydata)
names(p_mydata) <- c('Dataset', 'Cindex')
p1 <- ggplot(p_mydata, aes(x = Dataset, y = Cindex, fill = Dataset))+
  geom_boxplot()+
  scale_fill_manual(values = c("#FFBBCC", "#88CCFF","#48D1CC"))+
  labs(x = '')+
  theme(legend.position = 'none', 
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"))

Assumption1 <- Scenario1$`Brier Score`
Assumption2 <- Scenario2$`Brier Score`
Assumption3 <- Scenario3$`Brier Score`
mydata <- data.frame(Assumption1, Assumption2, Assumption3)
x_name <- c("Scenario1", "Scenario2", "Scenario3")
names(mydata) <- x_name

p_mydata <- melt(mydata)
names(p_mydata) <- c('Dataset', 'BrierScore')
p2 <- ggplot(p_mydata, aes(x = Dataset, y = BrierScore, fill = Dataset))+
  geom_boxplot()+
  scale_fill_manual(values = c("#FFBBCC", "#88CCFF","#48D1CC"))+
  labs(x = '')+
  theme(legend.position = 'none', 
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"))


Assumption1 <- Scenario1$IBS
Assumption2 <- Scenario2$IBS
Assumption3 <- Scenario3$IBS
mydata <- data.frame(Assumption1, Assumption2, Assumption3)
x_name <- c("Scenario1", "Scenario2", "Scenario3")
names(mydata) <- x_name

p_mydata <- melt(mydata)
names(p_mydata) <- c('Dataset', 'IBS')
p3 <- ggplot(p_mydata, aes(x = Dataset, y = IBS, fill = Dataset))+
  geom_boxplot()+
  scale_fill_manual(values = c("#FFBBCC", "#88CCFF","#48D1CC"))+
  labs(x = '')+
  theme(legend.position = 'none', 
        axis.text=element_text(size=18),  
        axis.title=element_text(size=20,face="bold")) 

Assumption1 <- Scenario1$IAE
Assumption2 <- Scenario2$IAE
Assumption3 <- Scenario3$IAE
mydata <- data.frame(Assumption1, Assumption2, Assumption3)
x_name <- c("Scenario1", "Scenario2", "Scenario3")
names(mydata) <- x_name

p_mydata <- melt(mydata)
names(p_mydata) <- c('Dataset', 'IAE')
p4 <- ggplot(p_mydata, aes(x = Dataset, y = IAE, fill = Dataset))+
  geom_boxplot()+
  scale_fill_manual(values = c("#FFBBCC", "#88CCFF","#48D1CC"))+
  labs(x = '')+
  theme(legend.position = 'none', 
        axis.text=element_text(size=18),  
        axis.title=element_text(size=20,face="bold")) 


Assumption1 <- Scenario1$ISE
Assumption2 <- Scenario2$ISE
Assumption3 <- Scenario3$ISE
mydata <- data.frame(Assumption1, Assumption2, Assumption3)
x_name <- c("Scenario1", "Scenario2", "Scenario3")
names(mydata) <- x_name

p_mydata <- melt(mydata)
names(p_mydata) <- c('Dataset', 'ISE')
p5 <- ggplot(p_mydata, aes(x = Dataset, y = ISE, fill = Dataset))+
  geom_boxplot()+
  scale_fill_manual(values = c("#FFBBCC", "#88CCFF","#48D1CC"))+
  labs(x = '')+
  theme(legend.position = 'none', 
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"))


re_boxplot <- ggarrange(p1,p2,p3,p4,p5, ncol = 3, nrow = 2,
                        labels = c(1:5))

re_boxplot
ggsave("simuladataexp.pdf",width = 15,height = 12)



#table 4 AND figure 4
#1. data preparation
if(!require("randomForestSRC")){install.packages("randomForestSRC")}
if(!require("survival")){install.packages("survival")}
if(!require("pec")){install.packages("pec")}
if(!require("SurvMetrics")){install.packages("SurvMetrics")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if(!require("ggpubr")){install.packages("ggpubr")}

set.seed(1)
mydata <- kidney[, -1]
train_index <- sample(1:nrow(mydata), 0.7 * nrow(mydata))
train_data <- mydata[train_index, ]
test_data <- mydata[-train_index, ]

#2. fit the RSF model and Cox model to predict the testing set
#2.1 RSF model
fit_rsf <- rfsrc(Surv(time,status)~., data = train_data)  #fit the RSF model
distime <- fit_rsf$time.interest  #get the survival time of events
med_index <- median(1:length(distime))  #the index of median survival time of events
mat_rsf <- predict(fit_rsf, test_data)$survival  #get the survival probability matrix
vec_rsf <- mat_rsf[ ,med_index]  #median survival probability of all samples

#2.2 Cox model
fit_cox <- coxph(Surv(time,status)~., data = train_data, x = TRUE)  #fit the Cox model
mat_cox <- predictSurvProb(fit_cox, test_data, distime)  #get the survival probability matrix
vec_cox <- mat_cox[ ,med_index]

#3. get all the metrics by SurvMetrics
#3.1 CI BS IBS IAE ISE based on RSF model
times <- test_data$time
status <- test_data$status
Cindex_rsf <- Cindex(Surv(times, status), vec_rsf)
BS_rsf <- Brier(Surv(times, status), vec_rsf, distime[med_index])
IBS_rsf <- IBS(Surv(times, status), mat_rsf, distime)  # distime can be replaced by range(distime)
IAE_rsf <- IAEISE(Surv(times, status), mat_rsf, distime)[1]
ISE_rsf <- IAEISE(Surv(times, status), mat_rsf, distime)[2]

#CI BS IBS IAE ISE based on Cox model
Cindex_cox <- Cindex(Surv(times, status), vec_cox)
BS_cox <- Brier(Surv(times, status), vec_cox, distime[med_index])
IBS_cox <- IBS(Surv(times, status), mat_cox, distime)
IAE_cox <- IAEISE(Surv(times, status), mat_cox, distime)[1]
ISE_cox <- IAEISE(Surv(times, status), mat_cox, distime)[2]

x1 <- c(Cindex_cox, Cindex_rsf)
x2 <- c(BS_cox, BS_rsf)
x3 <- c(IBS_cox, IBS_rsf)
x4 <- c(IAE_cox, IAE_rsf)
x5 <- c(ISE_cox, ISE_rsf)
res <- data.frame("Cindex" = x1, "BS" = x2, "IBS" = x3, "IAE" = x4, "ISE" = x5)
rownames(res) <- c("Cox", "RSF")

res_plot <- melt(res)
modelsname <- rep(c("Cox","RSF"),5)
res3 <- data.frame(res_plot$variable, res_plot$value, modelsname)
names(res3) <- c("metrics","values","models")

res_cindex <- res3[1:2,-1]
p1 <- ggplot(res_cindex, aes(x=models, y=values),fill = models) + 
  geom_bar(stat = "identity", width=0.5, aes(fill = models))+
  scale_fill_manual(values = c("#6495ED", "#F08080"))+ 
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  labs(title = "C-index")

res_BS <- res3[3:4,-1]
p2 <- ggplot(res_BS, aes(x=models, y=values),fill = models) + 
  geom_bar(stat = "identity", width=0.5, aes(fill = models))+
  scale_fill_manual(values = c("#6495ED", "#F08080"))+ 
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  labs(title = "BS")

res_IBS <- res3[5:6,-1]
p3 <- ggplot(res_IBS, aes(x=models, y=values),fill = models) + 
  geom_bar(stat = "identity", width=0.5, aes(fill = models))+
  scale_fill_manual(values = c("#6495ED", "#F08080"))+ 
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  labs(title = "IBS")

res_IAE <- res3[7:8,-1]
p4 <- ggplot(res_IAE, aes(x=models, y=values),fill = models) + 
  geom_bar(stat = "identity", width=0.5, aes(fill = models))+
  scale_fill_manual(values = c("#6495ED", "#F08080"))+ 
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  labs(title = "IAE")

res_ISE <- res3[9:10,-1]
p5 <- ggplot(res_ISE, aes(x=models, y=values),fill = models) + 
  geom_bar(stat = "identity", width=0.5, aes(fill = models))+
  scale_fill_manual(values = c("#6495ED", "#F08080"))+ 
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  labs(title = "ISE")

re_realdata <- ggarrange(p1,p2,p3,p4,p5,ncol = 5)
re_realdata
ggsave("kidneyres.pdf",width = 15,height = 6)
res
write.csv(res, "table4.csv")



