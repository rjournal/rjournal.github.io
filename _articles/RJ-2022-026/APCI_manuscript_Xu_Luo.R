
# R script for the example code used in the submission to The R journal, MS 2021-78
# APCI: An R and Stata Package for Age-Period-Cohort Analysis
# Authors: Jiahui Xu and Liying Luo

# load APCI packages and other necessary packages to run this script
library(APCI)
library(magrittr)
library(tidyverse)

# load empirical data -----
data(cpsmen)
data(cpswomen)

# check the first five rows of the data
head(cpsmen, n = 5)
head(cpswomen, n = 5)

# APC-I model without covariates
no_cov <- APCI::apci(outcome = "labforce", 
                     age = "age", 
                     period = "year", 
                     weight = "asecwt",
                     data = cpswomen, 
                     dev.test=FALSE, 
                     family = "binomial")

# APC-I model with the covariate "educc"
with_cov <- APCI::apci(outcome = "labforce",
                       age = "age",
                       period = "year",
                       covariate = c("educc"),
                       weight = "asecwt",
                       data = cpswomen, print=F,
                       dev.test=FALSE, family = "binomial")

# summary of the model results
summary(with_cov)

# Figure 1
agelabel <- c("20-24","25-29","30-34","35-39",
              "40-44","45-49","50-54","55-59",
              "60-64")
yearlabel <- c("1990-1994","1995-1999","2000-2004",
               "2005-2009","2010-2014","2015-2019")

options(warnings = -1L)
g1 <- cpsmen%>%
  select(age,year,labforce,asecwt)%>%
  na.omit%>%
  group_by(age,year)%>%
  summarise(value = mean(labforce,na.rm = T),.groups="drop")%>%
  mutate(group="Men")

g2 <- cpswomen%>%
  select(age,year,labforce,asecwt)%>%
  na.omit%>%
  group_by(age,year)%>%
  summarise(value = mean(labforce,na.rm = T),.groups="drop")%>%
  mutate(group="Women")

raw1 <- rbind(g1,g2)%>%
  ggplot(.,aes(x=year,group=age,y = value,col=age))+
  geom_point()+
  geom_path()+
  theme_bw()+
  scale_x_discrete(label=yearlabel)+
  scale_color_discrete(name = "Age", labels = agelabel)+
  theme(axis.text.x = element_text(angle = 90, vjust=.5, hjust = 1))+
  facet_grid(.~group)+
  labs(x = "Period Group",names = "Age",y = as.character("Labor Force Participation Rate"))

raw2 <- rbind(g1,g2)%>%
  ggplot(.,aes(x=age,group=year,y = value,col=year))+              
  geom_point()+
  geom_path()+
  theme_bw()+
  scale_x_discrete(label=agelabel)+
  scale_color_discrete(name = "Year", labels = yearlabel)+
  theme(axis.text.x = element_text(angle = 90, vjust=.5, hjust = 1))+
  facet_grid(.~group)+
  labs(x = "Age Group",names = "Period",y = as.character("Labor Force Participation Rate"))

figure1 <- ggpubr::ggarrange(raw2,raw1,ncol=1,nrow=2)
figure1
ggsave("figure1.pdf",width=12,height=9)

# inter-cohort average deviations
with_cov$cohort_average
options(width = 70)
print(data.frame(c_avg_group = with_cov$cohort_average[,1],
                 c_avg_est = sprintf("%.3f",as.numeric(with_cov$cohort_average[,2])),
                 c_avg_se = sprintf("%.3f",as.numeric(with_cov$cohort_average[,3])),
                 c_avg_t = sprintf("%.3f",as.numeric(with_cov$cohort_average[,4])),
                 c_avg_p = sprintf("%.3f",as.numeric(with_cov$cohort_average[,5])),
                 c_avg_sig = with_cov$cohort_average[,6]
))

# inter-cohort cohort slope
with_cov$cohort_slope
options(width = 70)
print(data.frame(c_slp_group = with_cov$cohort_slope[,1],
                 c_slp_est = sprintf("%.3f",as.numeric(with_cov$cohort_slope[,2])),
                 c_slp_se = sprintf("%.3f",as.numeric(with_cov$cohort_slope[,3])),
                 c_slp_t = sprintf("%.3f",as.numeric(with_cov$cohort_slope[,4])),
                 c_slp_p = sprintf("%.3f",as.numeric(with_cov$cohort_slope[,5])),
                 c_slp_sig = with_cov$cohort_slope[,6]
))

# intra-cohort life course dynamics
with_cov$int_matrix
# intra-cohort life course dynamics (first six rows)
head(data.frame(iaesti = sprintf("%.3f",as.numeric(with_cov$int_matrix[,1])),
                 iase = sprintf("%.3f",as.numeric(with_cov$int_matrix[,2])),
                 iap = sprintf("%.3f",as.numeric(with_cov$int_matrix[,3])),
                 iasig = with_cov$int_matrix[,4],
                 cohortindex = with_cov$int_matrix[,5]
))

# organize the results to keep three decimal digits
A <- with_cov$model$data$age %>% nlevels()
P <- with_cov$model$data$year %>% nlevels()
matrix(with_cov$int_matrix$iaesti, A, P)
options(width = 120)
matrix(str_pad(paste0(sprintf("%.3f",as.numeric(with_cov$int_matrix$iaesti)),
                      with_cov$int_matrix$iasig),width = 9,"left"," "),9,6) %>%
  as.data.frame() %>%
  `rownames<-`(paste0("age #",1:A)) %>%
  `colnames<-`(paste0("period #",1:P)) %>% 
  .[A:1,]

# Figure 2
apci.plot.heatmap(model = with_cov, age = "age",period = 'year',
                  color_map = c('blue','yellow'))
apci.plot.heatmap(model = with_cov, age = "age",period = 'year',
                  color_map = c('blue','yellow'))+
  labs(caption = "")+ # customize the figures for the manuscript
  scale_x_discrete(label = yearlabel)+
  scale_y_discrete(label = agelabel)+
  theme(axis.text.x = element_text(angle = 90, vjust=.5, hjust = 1))
ggsave("figure2.pdf",width=12,height=9)

# Figure 3
apci.bar(model = with_cov, age = "age",period = "year",
         cohort_label = seq(1930,1995,5))
ggsave("figure3.pdf",width=12,height=9)


# unequal age and period group: age is group by 5 years 
# and period is grouped by 10 years
# The unequal grouping is not recommended
data(women9017)
unequal_APCI <- APCI::apci(outcome = "inlfc",
                           age = "age",
                           period = "year",
                           weight = "wt",
                           data = women9017,dev.test=FALSE,
                           print = T,
                           family = "binomial",
                           unequal_interval = TRUE,
                           age_range = 20:64,
                           period_range = 1990:2019,
                           age_interval = 5,
                           period_interval = 10)


# load simulated cross-sectional data (GEE Extension) -----
data("simulation")
# first rows of the simulation data
head(simulation)
# fit an APC-I model
APC_I <- APCI::apci(outcome = "y",
                    age = "age",
                    period = "period",
                    data = simulation,
                    dev.test=FALSE,
                    family = "gaussian",
                    print = F)

# visualize the results: explore phase
apci.plot(model=APC_I, age = "age",
          period = 'period',outcome_var = "y",type='explore')
# visualize the results: model phase
apci.plot(model=APC_I, age = "age",
          period = 'period',outcome_var = "y",type='model')


# simulated panel data
simulation_gee <- simulation
simulation_gee$id <- 1:nrow(simulation_gee)
simulation_gee = simulation_gee[sample(nrow(simulation_gee),30000,replace=T),]
model_gee <- apci(outcome = "y",
                  age = "age",
                  period = "period",
                  cohort = NULL,
                  weight = NULL,
                  covariate = NULL,
                  data=simulation_gee,
                  family ="gaussian",
                  dev.test = FALSE,
                  print = TRUE,
                  gee = TRUE,
                  id = "id",
                  corstr = "exchangeable")
summary(model_gee)

# cite APCI package
toBibtex(citation("APCI",auto=TRUE))

