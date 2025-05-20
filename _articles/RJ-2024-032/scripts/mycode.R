# loading libraries
library(boiwsa)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(kableExtra)
#Figure 2
ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=boiwsa::gasoline.data$date,
                                  y=boiwsa::gasoline.data$y),color="royalblue")+
  ggplot2::theme_bw()+
  ggplot2::ylab("Barrels per day (millions)")+
  ggplot2::xlab("Year")


# Sec. 3.1 Empirical example

# performing seasonal adjustment using automatic model selection
res <- boiwsa::boiwsa(x=boiwsa::gasoline.data$y,
                      dates=boiwsa::gasoline.data$date)

# Figure 3
plt1 <- ggplot2::ggplot() +
  ggplot2::ggtitle("Original (blue) and Seasonally adjusted (green)") +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::gasoline.data$date, y = boiwsa::gasoline.data$y, color = "orig")) +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::gasoline.data$date, y = res$sa, color = "sa")) +
  ggplot2::theme_bw() +
  ggplot2::xlab(" ") +
  ggplot2::ylab("") +  # Removed empty space in y-axis label
  ggplot2::scale_color_manual(name = "",
                              values = c("orig" = 'royalblue', "sa" = "green"),
                              labels = c("Original", "Seasonally adjusted")) +
  ggplot2::theme(legend.position = "None",
                 legend.text = ggplot2::element_text(size = 10))

sf <- ggplot2::ggplot() +
  ggplot2::ggtitle("Seasonal") +
  ggplot2::geom_line(ggplot2::aes(x = gasoline.data$date, y = res$seasonal.factors, color = "sf")) +
  ggplot2::xlab(" ") +
  ggplot2::ylab("") +  # Removed empty space in y-axis label
  ggplot2::scale_color_manual(name = "",
                              values = c("sf" = 'royalblue'),
                              labels = c("Seasonal Factors")) +
  ggplot2::theme_bw() +
  theme(legend.text = ggplot2::element_text(size = 10),
        legend.position = "None")

tr1 <- ggplot2::ggplot() +
  ggplot2::ggtitle("Trend") +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::gasoline.data$date, y = res$trend, color = "tr")) +
  ggplot2::xlab(" ") +
  ggplot2::ylab("") +  # Removed empty space in y-axis label
  ggplot2::scale_color_manual(name = "",
                              values = c("tr" = 'blue'),
                              labels = c("Trend")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 legend.position = "None")


gridExtra::grid.arrange(plt1, tr1, sf, nrow = 3)

# Figure 4
boiwsa::plot_spec(res)

# Figure 5
ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=boiwsa::lbm$date,
                                  y=boiwsa::lbm$IES_IN_W_ADJ),color="royalblue")+
  ggplot2::theme_bw()+
  ggplot2::ylab("Number of claims per week")+
  ggplot2::xlab("Year")



# Sec. 3.2 Empirical example

# creating an input for simple_td 
boiwsa::dates_il%>%
  dplyr::select(DATE_VALUE,ISR_WORKING_DAY_PART)%>%
  `colnames<-`(c("date","WORKING_DAY_PART"))%>%
  dplyr::mutate(date=as.Date(date))->df.td
# creating a matrix with a working day variable
td <- boiwsa::simple_td(dates = boiwsa::lbm$date,df.td = df.td)

# generating the Rosh Hashanah and Pesach moving holiday variables
rosh <- boiwsa::my_rosh(dates=boiwsa::lbm$date,
                        holiday.dates = boiwsa::holiday_dates_il$rosh)
# renaming (make sure that all the variables in H have distinct names)
colnames(rosh) <- paste0("rosh",colnames(rosh))

pesach <- boiwsa::my_rosh(dates=boiwsa::lbm$date,
                          holiday.dates = boiwsa::holiday_dates_il$pesah,
                          start=3,end=-1)
colnames(pesach)=paste0("pesach",colnames(pesach))


# combining the prior adjustment variables in a single matrix
H <- as.matrix(cbind(rosh[,-1],pesach[,-1],td[,-1]))
# running seasonal adjustment routine
res <- boiwsa::boiwsa(x=boiwsa::lbm$IES_IN_W_ADJ,
                      dates = boiwsa::lbm$date,
                      H=H,
                      out.threshold = 3.8)

# Figure 6

plt1 <- ggplot2::ggplot() +
  ggplot2::ggtitle("Original (blue) and Seasonally adjusted (green)") +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::lbm$date, y = boiwsa::lbm$IES_IN_W_ADJ, color = "orig")) +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::lbm$date, y = res$sa, color = "sa")) +
  ggplot2::theme_bw() +
  ggplot2::xlab(" ") +
  ggplot2::ylab("") +  # Removed empty space in y-axis label
  ggplot2::scale_color_manual(name = "",
                              values = c("orig" = 'royalblue', "sa" = "green"),
                              labels = c("Original", "Seasonally adjusted")) +
  ggplot2::theme(legend.position = "None",
                 legend.text = ggplot2::element_text(size = 10))

sf <- ggplot2::ggplot() +
  ggplot2::ggtitle("Seasonal") +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::lbm$date, y = res$seasonal.factors, color = "sf")) +
  ggplot2::xlab(" ") +
  ggplot2::ylab("") +  # Removed empty space in y-axis label
  ggplot2::scale_color_manual(name = "",
                              values = c("sf" = 'royalblue'),
                              labels = c("Seasonal Factors")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 legend.position = "None")

tr1 <- ggplot2::ggplot() +
  ggplot2::ggtitle("Trend") +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::lbm$date, y = res$trend, color = "tr")) +
  ggplot2::xlab(" ") +
  ggplot2::ylab("") +  # Removed empty space in y-axis label
  ggplot2::scale_color_manual(name = "",
                              values = c("tr" = 'blue'),
                              labels = c("Trend")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 legend.position = "None")

gridExtra::grid.arrange(plt1, tr1, sf, nrow = 3)


# incresing out.threshold
res <- boiwsa::boiwsa(x=boiwsa::lbm$IES_IN_W_ADJ,
                      dates = boiwsa::lbm$date,
                      H=H,
                      out.threshold = 5)

# Figure 7

plt1 <- ggplot2::ggplot() +
  ggplot2::ggtitle("Original (blue) and Seasonally adjusted (green)") +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::lbm$date, y = boiwsa::lbm$IES_IN_W_ADJ, color = "orig")) +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::lbm$date, y = res$sa, color = "sa")) +
  ggplot2::theme_bw() +
  ggplot2::xlab(" ") +
  ggplot2::ylab("") +  # Removed empty space in y-axis label
  ggplot2::scale_color_manual(name = "",
                              values = c("orig" = 'royalblue', "sa" = "green"),
                              labels = c("Original", "Seasonally adjusted")) +
  ggplot2::theme(legend.position = "None",
                 legend.text = ggplot2::element_text(size = 10))

sf <- ggplot2::ggplot() +
  ggplot2::ggtitle("Seasonal") +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::lbm$date, y = res$seasonal.factors, color = "sf")) +
  ggplot2::xlab(" ") +
  ggplot2::ylab("") +  # Removed empty space in y-axis label
  ggplot2::scale_color_manual(name = "",
                              values = c("sf" = 'royalblue'),
                              labels = c("Seasonal Factors")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 legend.position = "None")

tr1 <- ggplot2::ggplot() +
  ggplot2::ggtitle("Trend") +
  ggplot2::geom_line(ggplot2::aes(x = boiwsa::lbm$date, y = res$trend, color = "tr")) +
  ggplot2::xlab(" ") +
  ggplot2::ylab("") +  # Removed empty space in y-axis label
  ggplot2::scale_color_manual(name = "",
                              values = c("tr" = 'blue'),
                              labels = c("Trend")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10),
                 legend.position = "None")

gridExtra::grid.arrange(plt1, tr1, sf, nrow = 3)

# Figure 8
boiwsa::plot_spec(res)