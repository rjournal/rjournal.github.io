## Revisiting Historical Bar Graphics on Epidemics in the Era of R ggplot2 ##
## by Sami Aldag, Dogukan Topcuoglu and Gul Inan ##

## The Figures in the main manuscript and supplementary file can be reproduced with the R codes below.
## Note that for this version of the R codes, we omitted the local letters
## (which are not available in English alphabet) for convenience.
## Figures are ordered as in the files. 



## Required package
library(ggplot2)

#####  The figures in the main file ####

## Figure 3: Smallpox vaccine administered in various regions of the country, between the period 1925-1937 ##

# We are dividing the original "number of vaccines administered" by "1000" thousand to scale the graphic.

number_vaccine <- c(854750, 1356896, 1418362, 1280931, 1394799, 1637811, 1452892, 
                    1418992, 1686364, 1908213, 1761660, 2888906, 1672048)/1000
                    
year <- rep(seq(1925,1937), 1)

data_fig <- data.frame("x" = year, "y" = number_vaccine)

y_label <- c("854.750", "1.356.896", "1.418.362", "1.280.931","1.394.799", 
             "1.637.811", "1.452.892", "1.418.992", "1.686.364", "1.908.213", 
             "1.761.660", "2.888.906",  "1.672.048")
           
xcor <- rep(paste(c(seq(1925,1937)), sep = ""), 1)
ycor <- number_vaccine + 150


ggplot(data=data_fig,aes(x = factor(year), y = number_vaccine)) +
  geom_col(fill = "black") +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(colour = "black", size = 1.0), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 8, face = "bold", 
                                   hjust = 0.45, vjust = 0.5, angle = 89.99),
        axis.title.x = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  annotate("text", x = xcor, y = ycor, label = y_label, color = "white", 
           fontface = "bold.italic", size = 3, angle = 90, hjust = 1.9, vjust = 0.2) +
  annotate("text", x = "1927", y = 2818.362, label = "Yurdun muhtelif bolgelerinde", 
           fontface = "bold.italic", size = 3, hjust = 0.2, vjust = -1.0) +
  annotate("text", x = "1928", y = 2700.362, label = "1925-1937", fontface = "bold.italic", 
           size = 3, hjust = -0.1, vjust = -1.0) +
  annotate("text", x = "1927", y = 2600.362, label ="Seneleri zarfinda yapilan", 
           fontface = "bold.italic", size=3, hjust = 0.1, vjust = -1.0) +
  annotate("text", x = "1927", y = 2450.362, label ="Cicek asisi", fontface = "bold", 
           size = 4, hjust = -0.3, vjust = -1.0)


## Figure 3: Smallpox vaccine administered in various regions of the country, between the period 1925-1937 ##
## In this example, ggtext package is used to integrate multi-line figure title.


# We are dividing the original "number of vaccines administered" by "1000" thousand to scale the graphic.
library(ggtext) 

number_vaccine <- c(854750, 1356896, 1418362, 1280931, 1394799, 1637811, 1452892, 
                    1418992, 1686364, 1908213, 1761660, 2888906, 1672048)/1000

year <- rep(seq(1925,1937),1)
  
data_fig <- data.frame("x" = year, "y" = number_vaccine)
  
y_label <- c("854.750", "1.356.896", "1.418.362", "1.280.931","1.394.799", 
             "1.637.811", "1.452.892",    "1.418.992", "1.686.364", "1.908.213", 
             "1.761.660", "2.888.906", "1.672.048")
            
xcor <- rep(paste(c(seq(1925, 1937)), sep = ""), 1)
ycor <- number_vaccine + 100
  
  
ggplot(data=data_fig,aes(x = factor(year), y = number_vaccine)) +
  geom_col(fill = "black")+
  theme_classic()+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(colour = "black", size = 1.0), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 8, face = "bold", 
                                   hjust = 0.45, vjust = 0.5, angle = 89.99),
        axis.title.x = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  annotate("text", x = xcor, y = ycor, label = y_label, color = "white", 
           fontface = "bold.italic", size = 2, angle = 90, hjust = 1.9, vjust = 0.2) +
  labs(title="<span style = 'font-size:10pt'><b><i>Yurdun muhtelif bolgelerinde <br> 1925-1937 <br> 
       Seneleri zarfinda yapilan </i></b></span><br><b><span style = 'font-size:16pt'>Cicek asisi</span></b>") +
  theme(plot.title.position = "panel",
        plot.title = element_textbox_simple(width = grid::unit(2.2, "in"), #box width
                                            halign = 0.5,  hjust  = 0.5) )  
    #horizontal alignment of text within box
    #horizontal alignment of box
                                 
  
## Figure 5: The service of hospitals and dispensaries within the Department of Control of Trachoma, 1925-1937 ##

Inpatient <- c(64,298,581,312,312,425,1257,1644,2073,2215,2215,2332,2579)
Outpatient <- c(99,642,909,646,702,2694,2418,4287,4212,4318,3719,4034,4916)
Time <- rep(seq(1925,1937), 2)
Value <- c(Inpatient,Outpatient)
Group <- factor(c(rep(c("Yatirilarak tedavi sayisi", 
                        "Yapilan ameliyat       \" "), each = length(Inpatient))),
                levels=c("Yatirilarak tedavi sayisi", 
                         "Yapilan ameliyat       \" "))
data <- data.frame("x" = Time, "y" = Value, "grp" = Group)

xcor <- rep(c(seq(1925,1937)), 2)
ycor <- Value + 90

y.label1 <- c("64","298","581","312","312","425"," 1.257"," 1.644"," 
              2.073","  2.215","  2.215","  2.332","  2.579",
              "       \u201399","642","909","646","702","  2.694",
              "  2.418","","","","","","")
y.label2 <- c("","","","","","","","","","","","","","","","","","","","",
              "4.287","4.212","4.318","3.719","4.034","4.916")

ggplot(data = data,aes(x = Time, y = Value, fill = Group, alpha = Group)) +  
  geom_col(position = "identity", color = "black", size = 1.25, width = 0.65) +
  scale_fill_manual(values = c("black","white")) +
  scale_alpha_manual(values = c(1, 0)) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.2), 
        axis.ticks.x=element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  scale_y_continuous(expand = c(0,0)) +
  annotate("text", x = xcor, y = ycor, label = y.label1, color = "black", size = 4,
            fontface = "bold.italic", angle = 90, hjust = 0.4) +
  annotate("text", x = xcor, y = ycor, label = y.label2,
           color = "black", size=4, fontface = "bold.italic", 
           angle = 90, hjust = 1.8)+
  theme(legend.title = element_blank(), legend.text = element_text(size=12, face = "bold.italic"),
        legend.key = element_rect(size = 5, fill = "white", colour = "white"),
        legend.position = c(0,0.82), legend.justification = c(0,1), legend.direction = 'vertical',
        legend.spacing.x = unit(0.15,"cm")) +
  guides(fill = guide_legend(keywidth = 0.78, keyheight = 0.30, default.unit = "inch")) +
  labs(title = "TRAHOM MUCADELE TESKILATI") +
  theme(plot.title=element_text(color = "black", size = 12, face = "bold", 
                                family = "sans", vjust = -10, hjust = 0.1))+
  annotate("text", x = 1927.5,  y = 4550, label = "Hastane ve Dispanserler Faaliyeti",
           size = 4, fontface = "bold.italic") +
  annotate("text", x = 1927.5,  y = 4250, label = "1925 \u2013 1937", fontface = "bold", 
           size=3.5) +
  annotate("rect", xmin = 1924.8, xmax = 1925.2, ymin = 90, ymax = 105, color = "white",
           fill = "white")

## Figure 7: The service of the Zonguldak Government hospital, 1924-1937 ##

Inpatient <- c(598,701,523,376,568,687,774,777,815,1031,1223,1374,1528,1733)
Outpatient <- c(NA,67,69,173,272,291,183,161,690,1531,3203,2443,3661,4432)
##To fit "725" into box I assigned value of 690 to it. Its label is still 725.
Difference <- ifelse(Inpatient<Outpatient,Inpatient,0)
Time <- seq(1924,1937)
data <- data.frame("x" = Time, "y1" = Inpatient, "y2" = Outpatient, "y3" = Difference)

xcor <- rep(paste(c(seq(1924,1937)), sep = ""), 1)
ycor1 <- Inpatient + 100
ycor2 <- Outpatient + 120

y.label1 <- c("598","701","523","376","568","687","774","777","815","1.031",
              "1.223","1.374","1.528","1.733")
y.label2 <- c("","67","69","173","272","291","183","161","725","","","","","")
y.label3 <- c("","","","","","","","","","1.531","3.203","2.443","3.661","4.432")


ggplot(data=data,aes(x = factor(Time))) +
  geom_col(aes(y = y1,fill = "black"), col = "black", size = 1, width = 0.85) +
  geom_col(aes(y = y2,fill = "white"), col = "black", size = 1, width = 0.85) +
  geom_col(aes(y = y3,fill = "black"), col = "black", size = 1, width = 0.85) +
  scale_fill_manual(name = "", values=c("black", "white"),
                    labels=c("Yatirilarak tedavi sayisi","Ayakta         \"           \" ")) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.1), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 12), 
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  theme(plot.margin = margin(0.9, 0.9, 0.9, 0.9, "cm")) +
  annotate("text", x = xcor, y = ycor1, label = y.label1, color = "black", size = 3.0, 
           fontface = "bold.italic", angle = 90, hjust = 0.2) +
  annotate("text", x = xcor, y = ycor2, label = y.label2, color = "white", size=3, 
           fontface = "bold.italic", angle = 0, vjust = 0.758) +
  annotate("text", x = xcor, y = ycor2, label = y.label3, color = "black", size = 3.0, 
           fontface = "bold.italic",  angle = 90, hjust = 0.2) +
  coord_cartesian(clip = 'off') +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12, face = "bold.italic"),
        legend.key = element_rect(size = 5, fill = "white", colour = "white"),
        legend.position = c(0,0.90), legend.justification = c(0,1), legend.direction = 'vertical',
        legend.spacing.x = unit(0.15,"cm")) +
  guides(fill=guide_legend(override.aes = list(size = 1.2), keywidth = 0.78, 
                           keyheight = 0.30, default.unit = "inch")) +
  ggtitle("ZONGULDAK HASTANESININ FAALIYETI \n                    1924 \u2013 1937") +
  theme(plot.title = element_text(face = "bold", color = "black", size = 12, 
                                family = "sans", vjust = -10, hjust = 0.2))

## Warning is just for missing values.


## Figure 9: The workload of private hospitals, 1926-1937 ##

Inpatient <- c(17700, 22661, 24851, 37117, 54065, 50562, 54318, 57570,
               62301, 63468, 70025, 74033)
Outpatient <- c(NA, 45333, 50286, 116765, 164381, 218366, 264382, 299398,
                351547, 374318, 308366, 357450)
Time <- rep(seq(1926,1937), 2)
Value <- c(Inpatient, Outpatient)
Group <- factor(c(rep(c("Yatakda tedavi sayisi", "Ayakta          \"       \" "), 
                      each = length(Inpatient))),
                      levels = c("Yatakda tedavi sayisi", "Ayakta          \"       \" "))
data <- data.frame("x" = Time, "y" = Value, "grp" = Group)
xcor <- rep(seq(1926,1937), 2)
ycor <- Value + 100
y.label1 <- c("17.700","","","37.117","54.065","50.562","54.318","57.570","62.301",
              "63.468","70.025","74.033", "16.009","45.333","50.286","116.765","164.381",
              "218.366","264.382","299.398", "351.547","374.318","308.366","357.450")
              
y.label2 <- c("","22.661","24.851","","","","","","","","","","","","","","","","","","","","","")

ggplot(data = data, aes(x = Time, y = Value, fill = Group, alpha = Group)) +
  geom_col(position = position_dodge(width = 0.3), color = "black", 
           size = 1.35, width = 1.5) +
  scale_fill_manual(values = c("black","white")) +
  scale_alpha_manual(values = c(1, 0)) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.2), 
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(color = "black", size = 14, face = "bold"), 
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks=c(1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937)) +
  theme(plot.margin = margin(0.9, 0.9, 0.9, 0.9, "cm"))+
  annotate("text", x = xcor, y = ycor, label = y.label1, color = "black", fontface = "bold.italic", 
           angle = 90,  hjust = -0.1, size=4) +
  annotate("text", x = xcor, y = ycor, label = y.label2, color = "black", fontface = "bold.italic",
            angle = 0,  hjust = 0.4, vjust= -0.5 , size=3.5) +
  annotate("text", x = 1926, y = 16009, label = "16.009", size = 4,  fontface = "bold.italic",
           angle = 90, vjust = -2.5, hjust = 0) +
  annotate("rect", xmin = 1925.5, xmax = 1926.2, ymin = 0, ymax = 14000, color = "black",
            fill = "white", size = 1.35) +
  coord_cartesian(clip = 'off') +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12, face = "bold.italic"),
        legend.key = element_rect(size = 5, fill = "white", colour = "white"),
        legend.position = c(0,1), legend.justification = c(0,1), legend.direction = 'vertical',
        legend.spacing.x = unit(0.15,"cm")) +
  guides(fill = guide_legend(keywidth = 0.78, keyheight = 0.30, default.unit = "inch")) +
  ggtitle("HUSUSI IDARELERE AIT HASTANELER MESAISI \n 1926 \u2013 1937")+
  theme(plot.title = element_text(color = "black", size = 14, family = "sans", hjust = 0.4),
        plot.subtitle = element_text(size = 14, family = "sans", hjust = 0.45))

## Warning is just for missing values.


## Figure 11: The workload of sample hospitals, 1924-1937 ##

Time <- rep(seq(1924,1937),2)
Inpatient <- c(3278,4791,4235,4537,4997,5466,5994,6004,5756,7151,10415,12018,
               16382,18014)
Outpatient <- c(NA,10651,15496,16050,22727,30712,36358,48448,56652,66822,100985,
                116123,193488,232407)
Value <- c(Inpatient, Outpatient)
Group <- factor(c(rep(c("Yatirilarak tedavi","Ayakta tedavi"), 
                each = length(Inpatient))),
                levels = c("Yatirilarak tedavi","Ayakta tedavi"))

numune <- data.frame(Time, Value, Group)

xcor1 <- seq(1924,1937) - 0.2
xcor2 <- seq(1924,1937) + 0.15
ycor1 <- Inpatient
ycor2 <- Outpatient
annolabel1 <- c("3.278","4.791","4.235","4.537","4.997","5.466","5.994","6.004",
                "5.756","7.151","10 415","12.018","16.382","19.014")
annolabel2 <- c("","10.651","15.496","16.050","22.727","30.712","26.358","48 448",
                "56.652","66.822","100.985","116.123","193.488","232.407")

ggplot(numune, aes(x = Time, y = Value, fill = Group)) +
  geom_col(position = position_dodge2(preserve = "single"), color = "black", 
           size = 1.25, width = 0.70) +
  scale_fill_manual(values=c("black","white")) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.2), 
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(color = "black", size = 12, vjust = 0.5), 
        axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1924,1937), expand = c(0,0.1)) +
  theme(plot.margin = margin(1.5, 1.5, 1.5, 1.5, "cm")) +
  annotate("text",x = xcor1, y = ycor1, label = annolabel1, size = 3.7, fontface = "bold.italic",  
           angle = 90, hjust = -0.1) +
  annotate("text", x = xcor2, y = ycor2, label = annolabel2,  size = 3.7, fontface = "bold.italic",
           angle = 90, hjust = -0.1)+
  coord_cartesian(clip = 'off')+
  theme(legend.title = element_blank(), legend.text = element_text(size = 14, face = "bold.italic"),
        legend.key = element_rect(size = 5, fill = "white", colour = "white"),
        legend.position = c(0.05,1), legend.justification = c(0,1), legend.direction = 'vertical',
        legend.spacing.x = unit(0.15,"cm"), legend.box.spacing = unit(1,"cm")) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.2, default.unit = "inch")) +
  ggtitle("NUMUNE HASTANELERI MESAISI \n      1924 \u2013 1937\n\n")+
  theme(plot.title = element_text(hjust = 0.4),
        plot.subtitle = element_text(hjust = 0.4, size = 14.5))


## Figure 13: The laboratory workload for Malaria struggle, 1925-1937 ##

Time <- rep(seq(1925,1937),2)
Treatment <- c(16579,148264,212568,243296,354455,370073,530595,557081,
               547332,570575,594530,654668,734041)
Diagnosis <- c(1434,14791,10190,9928,36186,45653,61241,72500,50609,
               48744,40842,62466,69850)
Value <- c(Treatment, Diagnosis)
Group <- factor(c(rep(c("Kan muayenesi sayisi","Musbet kan           \" "), 
                each = length(Treatment))),
                levels = c("Kan muayenesi sayisi","Musbet kan           \" "))
Malaria <- data.frame(Time, Value, Group)

xcor1 <- seq(1925,1937) - 0.2
xcor2 <- seq(1925,1937) + 0.2
xcor3 <- seq(1929,1937) - 0.2
ycor1 <- Treatment
ycor2 <- Diagnosis
ycor3 <- c(354455,370073,530595,557081,547332,570575,594530,654668,734041)
annolabel1 <- c("16.579","148.264","212 568","243.296","","","","","","","","","")
annolabel2 <- c("1.434","14.791","10.190","9.928","36.186","45.653","61.241","72.500",
                "50.609","48.744","40.842","62.466","60.850")
annolabel3 <- c("354.455","370.073","530.595","557.081","547.332","570.575","594.530",
                "654.668","734.041")

ggplot(Malaria, aes(x = Time, y = Value, fill = Group)) +
  geom_col(position = position_dodge2(), color = "black", 
           size = 1.25, width = 0.70) +
  scale_fill_manual(values=c("white","black")) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.2), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 22, face = "bold", 
                                   hjust = 0.7, vjust = 0.5, angle = 89.99),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1925,1937), expand = c(0,0.1)) +
  theme(plot.margin = unit(c(2,1,2,1), "lines")) +
  annotate("text",x = xcor1, y = ycor1, label = annolabel1, fontface = "bold.italic", 
           angle = 90,  hjust = -0.1) +
  annotate("text",x = xcor2, y = ycor2, label = annolabel2, fontface = "bold.italic", 
           angle = 90, hjust = -0.1) +
  annotate("text",x = xcor3, y = ycor3, label = annolabel3, fontface = "bold.italic", 
           angle = 90,  hjust = 1.3) +
  coord_cartesian(clip = 'off')+
  theme(legend.title = element_blank(), legend.text = element_text(size = 14, face = "bold.italic"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = c(0,0.85), legend.justification = c(0,1), 
        legend.direction ='vertical', legend.spacing.x = unit(0.25,"cm"),
        legend.box.spacing = unit(1.0,"cm")) +
  guides(fill = guide_legend(keywidth = 0.6, keyheight=0.5, default.unit = "inch")) +
  annotate("text", x = 1928, y = 734040, label = "SITMA MUCADELESI", size = 6) +
  annotate("text", x = 1928, y = 700000, label = "Laboratuvar mesaisi", size = 7, 
           fontface = "italic") +
  annotate("text", x = 1928, y = 665000, label = "1925 \u2013 1937", size = 7.5, 
           fontface = "bold")

## Figure 15: The drugs sent by the Department of Control of Syphilis to cities for treatment, 1926-1937 ##

Time <- rep(seq(1926,1937),4)
arsenobenzol <- c(NA,17,17,14,32,32,56,10,24,36,25,31)
bizmut <- c(108,25,33,69,84,62,3,5,13,29,38,70)
civa <- c(93,115,72,61,80,41,154,172,173,201,177,202)
iyodur <- c(NA,NA,NA,NA,NA,NA,NA,NA,26,34,35,28)
Value <- c(arsenobenzol,bizmut,civa,iyodur)
Group <- factor(c(rep(c("Arsenobenzol","Bizmut mustahlebi","Civa",
                "Iyodur ve civa merhemi"), each = length(civa))),
                levels = c("Arsenobenzol","Bizmut mustahlebi","Civa",
                           "Iyodur ve civa merhemi"))
varib <- c(26,34,35,28)
xnokta <- runif(144,1934.2,1934.35)
ynokta <- runif(144,0,varib)
ek1 <- c(0,1,2,3)
xcor1 <- seq(1926,1937) -0.3
xcor2 <- seq(1926,1937) - 0.1
xcor3 <- seq(1926,1937) + 0.09
xcor4 <- seq(1926,1937) + 0.27
ycor1 <- arsenobenzol
ycor2 <- bizmut
ycor3 <- civa
ycor4 <- iyodur
xlab <- runif(24,1928,1928.2)
ylab <- runif(24,169,174)

syphilis <- data.frame(Time,Value,Group,xnokta,ynokta)

annolabel1 <- c("","17","17","14","32","32","56","10","24","36","25","31")
annolabel2 <- c("108","25","33","69","84","62","3","5","13","29","38","70")
annolabel3 <- c("93","115","72","61","80","41","154","172","173","201","177","202")
annolabel4 <- c("","","","","","","","","26","34","35","28")
Line <- c(0,0.06,0.12)
add <- seq(0,11)
xline <- c(1925.65) + add
xlinend <- c(1926.35) + add

ggplot(syphilis, aes(x = Time, y = Value, fill = Group)) +
  geom_col(position = position_dodge(), color = "black", size = 1.25, 
           width = 0.70) +
  scale_fill_manual(values = c("black","white","white","white"))  +
  geom_point(x = xnokta + ek1, y=ynokta + ek1, size = 0.5, colour = "black")+
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 12, face = "bold", 
                                   hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank()) +
  annotate("segment", x = xline, xend = xlinend, y = 0, yend = 0, size = 1.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1926,1937), expand = c(0,0.1)) +
  theme(plot.margin = unit(c(2,1,1,1), "lines")) +
  annotate("text",x = xcor1, y = ycor1, label = annolabel1, angle = 90, 
           fontface = "bold.italic", hjust = -0.2, size = 3) +
  annotate("text",x = xcor2, y = ycor2, label = annolabel2, angle = 90, 
           fontface = "bold.italic",   hjust = -0.5, size = 3) +
  annotate("text",x = xcor3, y = ycor3, label = annolabel3, angle = 90, 
           fontface = "bold.italic", hjust = -0.2, size = 3) +
  annotate("text",x = xcor4, y = ycor4, label = annolabel4, angle = 90, 
           fontface = "bold.italic", hjust = -0.2, size = 3) +
  coord_cartesian(clip = 'off') +
  theme(legend.position= "none") +
  ##Vertical lines are integrated
  annotate("segment", x = 1926 + Line, xend = 1926 + Line, y = 0, yend = 93, size = 1) +
  annotate("segment", x = 1927 + Line, xend = 1927 + Line, y = 0, yend = 115, size = 1) +
  annotate("segment", x = 1928 + Line, xend = 1928 + Line, y = 0, yend = 72, size = 1) +
  annotate("segment", x = 1929 + Line, xend = 1929 + Line, y = 0, yend = 61, size = 1) +
  annotate("segment", x = 1930 + Line, xend = 1930 + Line, y = 0, yend = 80, size = 1) +
  annotate("segment", x = 1931 + Line, xend = 1931 + Line, y = 0, yend = 41, size = 1) +
  annotate("segment", x = 1932 + Line, xend = 1932 + Line, y = 0, yend = 154, size = 1) +
  annotate("segment", x = 1933 + Line, xend = 1933 + Line, y = 0, yend = 172, size = 1) +
  annotate("segment", x = 1934 + Line, xend = 1934 + Line, y = 0, yend = 173, size = 1) +
  annotate("segment", x = 1935 + Line, xend = 1935 + Line, y = 0, yend = 201, size = 1) +
  annotate("segment", x = 1936 + Line, xend = 1936 + Line, y = 0, yend = 177, size = 1) +
  annotate("segment", x = 1937 + Line, xend = 1937 + Line, y = 0, yend = 202, size = 1) +
  ##Legend boxes are integrated
  annotate("rect", xmin = 1928, xmax = 1928.2, ymin = 190, ymax = 195, colour = "black", 
           fill = "black", size = 1) +
  annotate("rect", xmin = 1928, xmax = 1928.2, ymin = 183, ymax = 188, colour = "black", 
           fill = "white", size = 1) +
  annotate("rect", xmin = 1928, xmax = 1928.2, ymin = 176, ymax = 181, colour = "black", 
           fill = "white", size = 1) +
  annotate("rect", xmin = 1928, xmax = 1928.2, ymin = 169, ymax = 174, colour = "black", 
           fill = "white", size = 1) +
  ##Legend titles are integrated
  annotate("text", x = 1928.8, y = 192.5, label = "Arsenobenzol          ", size = 3.2, 
           fontface = "italic") +
  annotate("text", x = 1928.8, y = 185.5, label = "Bizmut mustahlebi     ", size = 3.2, 
           fontface="italic") +
  annotate("text", x = 1928.67, y = 178.5, label = "Civa                  ", size = 3.2, 
           fontface = "italic") +
  annotate("text", x = 1928.82, y = 171.5, label = "Iyodur ve civa merhemi", size = 3.2, 
           fontface = "italic") +
  annotate("segment", x = seq(1928.008,1928.20,0.03), xend = seq(1928.008,1928.20,0.03), 
           y = 176, yend = 181, size = 1) +
  annotate("point", x = xlab, y = ylab, size = 0.5, colour = "black") +
  #3-line title integrated
  annotate("text", x = 1931, y = 230, label = "Devlet Frengi Mucadele Teskilati tarafindan tedavi icin", 
           size = 5) +
  annotate("text", x = 1931, y = 218, label = "Vilayetlere gonderilen ilaclar", 
           size = 6, fontface = "bold") +
  annotate("text", x = 1931, y = 205, label = "1926 \u2013 1937", size = 4.5) +
  #Caption integrated
  labs(caption = "Sayilar kiloyu gostermektedir") +
  theme(plot.caption = element_text(face = "bold.italic", vjust = -5, hjust = 0.1))

## Figure 15: The drugs sent by the Department of Control of Syphilis to cities for treatment, 1926-1937 ##
## Now we used ggpattern package along with ggtext package

library(ggpattern)
library(ggtext) 

Time <- rep(seq(1926,1937),4)
arsenobenzol <- c(NA,17,17,14,32,32,56,10,24,36,25,31)
bizmut <- c(108,25,33,69,84,62,3,5,13,29,38,70)
civa <- c(93,115,72,61,80,41,154,172,173,201,177,202)
iyodur <- c(NA,NA,NA,NA,NA,NA,NA,NA,26,34,35,28)
Value <- c(arsenobenzol,bizmut,civa,iyodur)
Group <- factor(c(rep(c("Arsenobenzol","Bizmut mustahlebi",
                        "Civa","Iyodur ve civa merhemi"), each = length(civa))),
                levels = c("Arsenobenzol","Bizmut mustahlebi","Civa","Iyodur ve civa merhemi"))

syphilis <- data.frame(Time,Value,Group)

ggplot(syphilis, aes(x = factor(Time), y = Value, fill = Group, pattern = Group)) +
  geom_col_pattern(position = position_dodge2(preserve = "single"),  color = "black", 
                   pattern_fill = "black", pattern_angle = 90,  pattern_density = 0.15,
                   pattern_spacing = 0.01, pattern_key_scale_factor = 0.6) + 
  scale_fill_manual(values = c("black", "white","white","white")) +
  scale_pattern_manual(values = c("Arsenobenzol" = "none", "Bizmut mustahlebi" = "none",
                                  "Civa" = "stripe", "Iyodur ve civa merhemi" = "circle")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=8, face = "bold.italic"),
        legend.position = c(0.08,0.92), legend.justification=c(0,1), 
        legend.key = element_rect(size = 5, fill = "white", colour = "white"),
        legend.spacing.x = unit(0.3,"cm"),
        legend.direction = 'vertical') +
  guides(fill = guide_legend(keyheight = 0.30, default.unit = "inch")) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(size=0.5), axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 12, face = "bold", 
                                   hjust = 0.5, vjust = 0.5),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  #Title and caption integrated
  labs(title="<span style = 'font-size:10pt'>Devlet Frengi Mucadele Teskilati tarafindan tedavi icin </span><br>
              <b><span style = 'font-size:14pt'>Vilayetlere gonderilen ilaclar</span></b><br> 
              <span style = 'font-size:10pt'>1925-1937 </span>") + 
  theme(plot.title.position = "panel",
        plot.title = element_textbox_simple(width = grid::unit(2.2, "in"), #box width
                                            halign = 0.5,    #horizontal alignment of text within box
                                            hjust  = 0.5) ) +
  labs(caption = "Sayilar kiloyu gostermektedir") +
  theme(plot.caption = element_text(face = "bold.italic", vjust = -5, hjust = 0.1)) +
  theme(plot.margin = margin(0.5, 1.5, 1.5, 1.5, "cm")) 

# Note that ggpattern renders the plot very slowly (more than 30 seconds).

## Figure 17: The service of produced and consigned serum and vaccine at Central Hygiene Institute, 1930-1937 ##

Time <- rep(c(1930:1937),4)
serum1 <- c(12,150,191,185,245,276,486,644)
serum2 <- c(3,144,163,184,202,246,264,537)
vaccine1 <- c(154,698,1849,2066,2029,2592,2447,4233)
vaccine2 <- c(120,493,1831,1933,1951,2404,2326,3965)
consigned <- c(vaccine2,serum2) + 350
produced <- c(vaccine1,serum1) + 350
Value <- c(consigned,produced )
Group1 <- rep(c("Asi","Serum"), each = 8)
Group2 <- rep(c("sevkedilen","istihsalat"), each = 16)

Center <- data.frame(Time, Value, Group1, Group2)
xcor <- rep(c(1930:1937),2)
label1 <- c("","698","1.849","2.066","2.029","2.592","2.447","4.233",
            "154 - 12","150","191","185","245","276","486","644")
label2 <- c("","493","1.831","1.933","1.951","2.404","2.326","3.965",
            "3 - 120","144","163","184","202","246","264","537")
ek <- seq(0,7)
cizgi <- seq(0,600,40)

ggplot(Center, aes(x=Time, fill = Group1))+
  geom_col(data = subset(Center, Group2 == "istihsalat"), aes(y = -Value), 
           position = "identity",
           colour = "black", size = 1.1, width = 0.8) +
  geom_col(data = subset(Center, Group2 == "sevkedilen"), aes(y = Value), 
           position = "identity",
           colour = "black", size = 1.1, width = 0.8) +
  scale_x_reverse(expand = c(0,0)) +
  coord_flip(clip = 'off')+
  scale_fill_manual(values=c("white","black")) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(), 
        axis.line.x = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.length.x = unit(.15,"cm"),
        axis.text.x = element_text(color = "black", size = 11, face = "bold", vjust = 0.1),
        axis.title.x = element_blank()) +
  scale_y_continuous(breaks = c(-4850, -3350, -2350, -1350, -350, 350, 1350, 
                                2350, 3350, 4350, 4850),
                     labels = c("4.500","3.000","2.000","1.000","0","0","1.000",
                                "2.000","3.000","","4.500"),
                     limits = c(-4850,4850), expand = c(0,0)) +
  theme(plot.margin = unit(c(5,3,1,3), "lines")) +
  theme(legend.position= "none") +
  ##Legend boxes are integrated
  annotate("rect", xmin = 1928.9 , xmax = 1929.2 , ymin = 4200, ymax = 4600, 
           size = 1, colour = "black", fill = "white") +
  annotate("rect", xmin = 1928.9 , xmax = 1929.2 , ymin = -4200, ymax = -4600, 
           size = 1, colour = "black", fill = "black") +
  annotate("rect", xmin = 1929.85 , xmax = 1930.2 , ymin = 400 , ymax = 630 , 
           size = 1,  fill = "white") +
  annotate("rect", xmin = 1929.85 , xmax = 1930.2 , ymin = -400 , ymax = -630 , 
           size = 1,  fill = "white") +
  annotate("text", x = xcor, y = -produced , label = label1, size = 3.8, 
           hjust = 1.3 , fontface = "bold.italic") +
  annotate("text", x = xcor, y = consigned , label = label2, size = 3.8, 
           hjust = -0.09, fontface = "bold.italic") +
  ##Legend titles are integrated
  annotate("text", x = 1929.3, y = 0, label = "Yillar", size = 5) +
  annotate("text", x = 1933.5, y = -4100, label = "ISTIHSALAT   (KILO)", 
           size = 6, angle = 45) +
  annotate("text", x = 1933.5, y = 4100, label = "SEVKEDILEN   (KILO)", 
           size = 6, angle = -45) +
  annotate("text", x = 1929, y = -3850, label = "Serum", 
           size = 4, fontface = "italic") +
  annotate("text", x = 1929, y = 4000, label = "Asi", 
           size = 4, fontface = "italic") +
  #Vertical box integrated
  annotate("rect", ymin = -350, ymax = 350, xmin = 1929.7, xmax = 1937, 
           fill = "white", colour = "black", size = 1) +
  annotate("segment", x = 1930.4, xend = 1931, y = -250 + cizgi, 
           yend = -500 + cizgi, size = 0.5) +
  annotate("segment", x = 1931.4, xend = 1932, y = -250 + cizgi, 
           yend = -500 + cizgi, size = 0.5) +
  annotate("segment", x = 1932.4, xend = 1933, y = -250 + cizgi, 
           yend = -500 + cizgi, size = 0.5) +
  annotate("segment", x = 1933.4, xend = 1934, y = -250 + cizgi, 
           yend = -500 + cizgi, size = 0.5) +
  annotate("segment", x = 1934.4, xend = 1935, y = -250 + cizgi, 
           yend = -500 + cizgi, size = 0.5) +
  annotate("segment", x = 1935.4, xend = 1936, y = -250 + cizgi, 
           yend = -500 + cizgi, size = 0.5) +
  annotate("segment", x = 1936.4, xend = 1937, y = -250 + cizgi, 
           yend = -500 + cizgi, size = 0.5) +
  annotate("rect", ymin = -350, ymax = 350, xmin = 1929.6 + ek, 
           xmax = 1930.41 + ek, fill = "white", colour = "black", size = 1) +
  annotate("text", x = c(1930:1937), y = 0, label = paste(c(1930:1937)), 
           size = 5.5, fontface = "bold") +
  #Changes on horizontal axis
  annotate("segment", x = 1937.7, xend = 1937.7, y = -4850, yend = -350, 
           size = 1.5) +
  annotate("segment", x = 1937.7, xend = 1937.7, y = 350, yend = 4850, 
           size = 1.5) +
  annotate("segment", x = 1937, xend = 1937.7, y = -350, yend = -350, 
           size = 1) +
  annotate("segment", x = 1937, xend = 1937.7, y = 350, yend = 350, 
           size = 1) +
  #Titles and caption
  annotate("text", x = 1927, y = 0, label = "T.C. \n MERKEZ HIFZISSIHHA MUESSESESI", 
           size = 5, fontface = "bold") +
  annotate("text", x = 1927.7, y = 0, label = "Serum ve Asi istihsal ve sevkiyyati faaliyeti", 
           size = 5, fontface = "italic") +
  annotate("text", x = 1928.2, y = 0, label = "1930 \u2013 1937", 
           size = 5) +
  labs(caption = "S     A     Y     I     S     I") +
  theme(plot.caption = element_text(size = 25,  hjust = 0.5))
                                   

## Figure 19: The service of birth and childcare houses, 1926-1937 ##

childinpatient <- c(51,68,59,86,88,290,292,323,454,519,749,1013) + 700
childoutpatient <- c(193.0,295.7,286.4,470.7,767.1,953.8,1181.6,1246.1,
                     1420.6,1697.1,2055.6,2227.5) + 700
womaninpatient <- c(161,366,460,605,1161,1944,2370,2720,3236,3725,
                    4703,4971) + 700
womanoutpatient <- c(639.7,834.5,647.1,812.4,1436.3,2217.7,2216.8,2686.6,
                     3374.3,3424.2,3514.0,4329.4) + 700
inpatient <- c(childinpatient, womaninpatient)
outpatient <- c(childoutpatient,womanoutpatient)
Value <- c(inpatient,outpatient)
Time <- rep(c(1926:1937),4)
Group1 <- rep(c("Cocuk\n","Kadin"), each = length(childinpatient))
Group2 <- rep(c("yatak","ayak"), each = length(outpatient))

childcare <- data.frame(Time, Value, Group1, Group2)

ek <- seq(0,11)

label1 <- c("1 930","2 957","2 864","4 707","1 671","9 538",
            "11 816","12 461","14 206","16.971","20.556","22 275")
label2 <- c("6 397","8 345","6 471","8 124","14 363","22.177",
            "22 168","22.866","33 743","34.242","35.140","43.294")
label3 <- c("51","68","59","86","88","290","292","323","454",
            "519","749","1 013")
label4 <- c("161","366","460","605","1.161","944","2 370",
            "2 720","3 236","3.725","4.703","4 971")

ggplot(childcare, aes(x = Time, fill = Group1)) +
  geom_col(data = subset(childcare, Group2 == "ayak"), aes(y = Value),
           position = position_dodge(width = -0.7), color = "black", 
           size = 1.2, width = 0.7) +
  geom_col(data = subset(childcare, Group2 == "yatak"), aes(y = -Value),
           position = position_dodge(width = -0.7), color = "black", 
           size = 1.2, width = 0.7) +
  scale_x_reverse(expand = c(0,0)) +
  scale_fill_manual(values = c("black","white")) +
  coord_flip(clip = 'off') +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(size = 1.2), axis.ticks.x = element_line(size = 1.2), 
        axis.ticks.length.x = unit(.15,"cm"),
        axis.text.x = element_text(color = "black", size = 15, face = "bold", vjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(breaks = c(-5700, -4700, -3700, -2700, -1700, -700, 700, 
                                1700, 2700, 3700, 4700, 5700),
                     labels = c("5.000","4","3","2","1","0","0","10","20",
                                "30","40","50.000"),
                     limits = c(-5700,5700), expand = c(0,0)) +
  theme(plot.margin = unit(c(2,3,4,3), "lines")) +
  annotate("text", x = c(1926:1937), y = childoutpatient, label = label1, 
           hjust = -0.1, vjust = -0.5, fontface = "bold.italic") +
  annotate("text", x = c(1926:1937), y = womanoutpatient, label = label2, 
           hjust = -0.1, vjust = 1, fontface = "bold.italic") +
  annotate("text", x = c(1926:1937), y = -childinpatient, label = label3, 
           hjust = 1.2, vjust = -0.5, fontface = "bold.italic") +
  annotate("text", x = c(1926:1937), y = -womaninpatient, label = label4, 
           hjust = 1.2, vjust = 1, fontface = "bold.italic") +
  ##Legend boxes are integrated
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 13.5, face = "italic"),
        legend.position = c(-0.04,0.9),
        legend.justification = c(0,1),
        legend.spacing.x = unit(0.2,"cm"),
        legend.direction = 'vertical',
        legend.key = element_rect(size = 12, fill = "white", colour = "white"),
        legend.key.width = unit(1.5,'lines')) +
  #Vertical box integrated
  annotate("rect", ymin = -700, ymax = 700, xmin = 1925.7, xmax = 1937.6, 
           fill = "black") +
  annotate("rect", ymin = -630, ymax = 630, xmin = 1925.72 + ek, xmax = 1926.30 + ek, 
           fill = "white") +
  annotate("text", x = c(1926:1937), y = 0, label = paste(c(1926:1937)), size = 6, 
           fontface = "bold") +
  annotate("text", x = 1925.3, y = 0, label = "YILLAR",fontface = "bold.italic") +
  annotate("text", x = 1932, y = -4700, label = "YATIRILARAK  TEDAVI", angle = 50, 
           size = 6, fontface = "italic") +
  annotate("text", x = 1932, y = 4700, label = "AYAKTA     TEDAVI", angle = -60, 
           size = 6, fontface = "italic") +
  annotate("text", x = 1937, y = -4100, label = "B  I  N  L  E  R", vjust = 4.7, 
           size = 5, fontface = "italic") +
  annotate("text", x = 1937, y = 2700, label = "O N   B  I  N  L  E  R", vjust = 4.7, 
           size = 5, fontface = "italic")+
  labs(title = "DOGUM  VE  COCUK  BAKIM  EVLERI  FAALIYETI\n1926 \U2013 1937\n") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


## Figure 20: The service of the Zonguldak Government hospital, 1924-1937 ##
## Re-interpreted version


Inpatient <- c(598,701,523,376,568,687,774,777,815,1031,1223,1374,1528,1733)
Outpatient <- c(NA,67,69,173,272,291,183,161,690,1531,3203,2443,3661,4432)
Time <- seq(1924,1937)
data <- data.frame("x" = Time, "y1" = Inpatient, "y2" = Outpatient)

ggplot(data=data,aes(x = Time)) +
  geom_line(aes(y = Inpatient), size = 0.8)+
  geom_line(aes(y = Outpatient), linetype = "dashed", size = 0.8)+
  theme_classic() +
  scale_x_continuous(expand = c(0,0), breaks = c(seq(1924,1937,1))) +
  scale_y_continuous(limits = c(0,5000)) +
  ggtitle("ZONGULDAK HASTANESININ FAALIYETI \n 1924 \u2013 1937") +
  theme(axis.title = element_blank(),
        plot.title = element_text(face = "bold", color = "black", 
                                size = 12, family="sans", vjust=0, hjust = 0.5),
        plot.margin = margin(2, 2, 2, 2, "cm")) + 
  annotate("text", x = 1934.5, y = 4200, label = "Ayakda tedavi sayısı",
           size=3.0, fontface = "bold.italic", hjust=0.3)+
  annotate("text", x = 1936, y = 1200, label = "Yatırılarak tedavi sayısı",
           size=3, fontface = "bold.italic", hjust=0.3)+
  annotate("text", x = 1937.15, y = 4432, label = "4432",
           size=3.0, fontface = "bold.italic", hjust=0.1)+
  annotate("text", x = 1937.15, y = 1744, label = "1733",
           size=3, fontface = "bold.italic", hjust=0.1) +
  coord_cartesian(clip = 'off') 
## Warning is just for missing values.

# Note to turn this ggplot2 object into a plotly object, first save the ggplot2 object in 
# a variable and apply the ggplotly function onto it.

## Figure 21: The drugs sent by the Department of Control of Syphilis to cities for treatment, 1926-1937 ##
## Re-interpreted version

Time <- rep(seq(1926,1937),4)
arsenobenzol <- c(NA,17,17,14,32,32,56,10,24,36,25,31)
bizmut <- c(108,25,33,69,84,62,3,5,13,29,38,70)
civa <- c(93,115,72,61,80,41,154,172,173,201,177,202)
iyodur <- c(NA,NA,NA,NA,NA,NA,NA,NA,26,34,35,28)
Value <- c(arsenobenzol,bizmut,civa,iyodur)
Group <- factor(c(rep(c("Arsenobenzol","Bizmut mustahlebi","Civa","Iyodur ve civa merhemi"), 
                      each = length(civa))),
                levels = c("Arsenobenzol","Bizmut mustahlebi","Civa","Iyodur ve civa merhemi"))

syphilis <- data.frame("x" = Time, "y" = Value, Group)

ann_text <- data.frame("Time" = c(1937,1937,1937,1937), "Value"=c(35,75,205,35),
                       "Group" = c("Arsenobenzol","Bizmut mustahlebi","Civa",
                                 "Iyodur ve civa merhemi"), 
                       label = c("31","70","202", "28"))

ggplot(syphilis, aes(x = Time)) +
  geom_line(aes(y = Value), size = 0.5) +
  geom_text(data = ann_text, aes(x = Time, y = Value), size = 3, vjust = -0.1, hjust = 1, 
            label = ann_text$label) +
  facet_wrap(~ Group, ncol = 2) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1926,1937,2)) +
  scale_y_continuous(lim = c(0,210)) +
  theme_classic() +
  ggtitle("Devlet Frengi Mucadele Teskilatı tarafından tedavi icin \n Vilayetlere gonderilen ilaclar \n 1925-1937") +
  labs(caption = "Sayılar kiloyu gostermektedir") +
  theme(plot.title = element_text(face = "bold", size = 12, family = "sans", vjust = 0, hjust = 0.5),
        axis.title = element_blank(),
        plot.caption = element_text(face = "bold.italic", size = 8, vjust = -5, hjust = 0),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "#DCDCDC"),
        plot.margin = margin(1.5, 1.5, 1.5, 1.5, "cm")) +
  coord_cartesian(clip = 'off')                                       

## Warning is just for missing values.


## Figure 22: The service of birth and childcare houses, 1926-1937 ##
## Re-interpreted version

childinpatient <- c(51,68,59,86,88,290,292,323,454,519,749,1013)
womaninpatient <- c(161,366,460,605,1161,1944,2370,2720,3236,3725,4703,4971) 
childoutpatient <- c(193.0,295.7,286.4,470.7,767.1,953.8,1181.6,1246.1,
                     1420.6,1697.1,2055.6,2227.5)*10 
womanoutpatient <- c(639.7,834.5,647.1,812.4,1436.3,2217.7,2216.8,2686.6,
                     3374.3,3424.2,3514.0,4329.4)*10 
Time <- rep(c(1926:1937),1)
inpatient <- data.frame(Time, childinpatient, womaninpatient)
outpatient <- data.frame(Time, childoutpatient, womanoutpatient)

p1<- ggplot(inpatient, aes(x = Time)) +
  geom_line(aes(y = childinpatient)) +
  geom_line(aes(y = womaninpatient), linetype="dashed") +
  scale_x_continuous(expand = c(0,0), breaks = seq(1926,1937,1)) +
  theme_classic() +
  ggtitle("YATIRILARAK TEDAVI") +
  theme(axis.title = element_blank(),
        plot.title = element_text(face = "bold", size= 10),
        plot.margin = margin(1.5, 1.5, 1.5, 1.5, "cm")) +
  annotate("text", x = 1936, y = 1100, label = "Cocuk",
           size = 3.0, fontface = "bold.italic", hjust=1) +
  annotate("text", x = 1935.75, y = 5100, label = "Kadın",
           size = 3, fontface = "bold.italic", hjust=1)+
  annotate("text", x = 1937.15, y = 1080, label = "1013",
           size = 3.0, fontface = "bold.italic", hjust=0.0, vjust = 0.9) +
  annotate("text", x = 1937.15, y = 5080, label = "4971",
           size = 3, fontface = "bold.italic", hjust = 0.1, vjust = 0.9) +
  coord_cartesian(clip = 'off')

p2 <- ggplot(outpatient, aes(x = Time)) +
  geom_line(aes(y=childoutpatient)) +
  geom_line(aes(y=womanoutpatient), linetype="dashed") +
  scale_x_continuous(expand = c(0,0), breaks=seq(1926,1937,1)) +
  scale_y_continuous(limits = c(0,50000)) +
  theme_classic() +
  ggtitle("AYAKTA TEDAVI") +
  theme(axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10),
        plot.margin = margin(1.5, 1.5, 1.5, 1.5, "cm")) +
  annotate("text", x = 1935.5, y = 22990, label = "Cocuk",
           size = 3.0, fontface = "bold.italic", hjust=-0.1) +
  annotate("text", x = 1935.5, y = 44714, label = "Kadın",
           size = 3, fontface = "bold.italic", hjust=-0.1) +
  annotate("text", x = 1937.25, y = 22490, label = "1013",
           size = 3.0, fontface = "bold.italic", hjust=0.0, vjust=1) +
  annotate("text", x = 1937.25, y = 44114, label = "4971",
           size = 3, fontface = "bold.italic", hjust=0.1, vjust=1) +
  coord_cartesian(clip = 'off') 

library(ggpubr)
figure <- ggarrange(p1,p2, nrow = 2)
annotate_figure(figure,
                top = text_grob("DOGUM  VE  COCUK  BAKIM  EVLERI  FAALIYETI\n1926 \U2013 1937\n", 
                                face = "bold", size = 11))


#####  The figures in the Supplementary file ####

## Figure 1: The workload of municipality hospitals, 1928-1937 ##

Inpatient <- c(12188,12746,12810,15625,14600,15757,17121,17213,18004,18193)
Outpatient <- c(30774,30670,34689,57285,66959,84625,126324,145483,159853,177075)
Time <- rep(seq(1928,1937),2)
Value <- c(Inpatient,Outpatient)
Group <- factor(c(rep(c("Yatakda tedavi sayisi", "Ayakda       \"           \" "), 
                      each = length(Inpatient))),
                levels=c("Yatakda tedavi sayisi", "Ayakda       \"           \" "))
data_fig <- data.frame("x" = Time, "y" = Value, "grp" = Group)
xcor <- rep(paste(c(seq(1928,1937)), sep=""), 2)
ycor <- Value + 110
y.label <- c("12.188","12.746","12.810","15.625","14.600","15.757","17.121",
             "17.213","18.004", "18.193","30.774","30.670","34.689","57.285",
             "66.959","84.625", "126.324","145.483","159.853","177.075")
             
ggplot(data = data_fig, aes(x = factor(Time), y = Value, fill = Group, color = Group, 
                          alpha = Group))+
  geom_col(position = "identity", color = "black", size = 1.25, width = 0.85) +
  scale_fill_manual(values = c("black","white")) +
  scale_alpha_manual(values = c(1, 0))+
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), axis.title.y = element_blank(), 
        axis.line.x = element_line(color = "black", size = 1.2), 
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(color = "black", size = 12, face = "bold"),
        axis.title.x = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  scale_y_continuous(expand = c(0,0)) +
  annotate("text", x = xcor, y = ycor, label = y.label, color = "black", size = 3.5, 
           fontface = "bold.italic", angle = 90, hjust = -0.1) +
  coord_cartesian(clip = 'off') +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12, face = "bold.italic"),
        legend.key = element_rect(size = 4, fill = "white", colour = "white"),
        legend.position = c(0,0.90), legend.justification = c(0,1), legend.direction = 'vertical',
        legend.spacing.x = unit(0.15,"cm")) +
  guides(fill = guide_legend(keywidth = 0.68,keyheight = 0.25, default.unit = "inch")) +
  ggtitle("BELEDIYELERE AIT HASTANELER MESAISI \n 1928\u20131937")+
  theme(plot.title = element_text(face = "bold", size = 12, vjust = 0.95, hjust = 0.3, family = "sans"))

## Figure 2: The service of mobile hospitals for Trachoma struggle, 1932-1937

Time <- rep(seq(1932,1937),3)
First <- c(4291,53921,54924,67668,60069,40879)
Second <- c(3798,40499,39407,50964,44286,22113)
Third <- c(96749,420819,587802,531157,606518,669151)

Value <- c(First, Second, Third)
Group <- factor(c(rep(c("Muayene        sayisi","Trahomlu        \"","Ilaclananlar    \""), 
                      each = length(First))),
                levels = c("Muayene        sayisi","Trahomlu        \"","Ilaclananlar    \""))

trachoma <- data.frame(Time, Value, Group)

xcor1 <- seq(1932,1937) - 0.3
xcor2 <- seq(1932,1937)
xcor3 <- seq(1932,1937) + 0.28
ycor1 <- First
ycor2 <- Second
ycor3 <- Third
ycor4 <- Third-90000
annolabel1 <- c("4.291", "53.921","54.924","67.668","60.069","40.879")
annolabel2 <- c("3.798","40.499","39.407","50.964","44.286","22.113")
annolabel3 <- c("96.749","42.0819",NA,NA,NA,NA)
annolabel4 <- c(NA,NA,"587.802","53.1157","60.6518","66.9151")

add <- seq(0,5) #number of bars-1
xline <- c(1931.6) + add
xlinend <- c(1932.4) + add

Line <- seq(0,0.15,0.02) #for vertical lines

ggplot(trachoma, aes(x = Time, y = Value, fill = Group)) +
  geom_col(position = position_dodge(), color = "black", size = 1.05, width = 0.8) +
  scale_fill_manual(values = c("white","black","white")) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.1), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 13, 
                                   face = "bold", vjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1932,1937),expand = c(0,0)) +
  theme(plot.margin = unit(c(5,1,1,1), "lines")) +
  annotate("text", x = xcor1, y = ycor1, label = annolabel1,
           size = 3.6, fontface = "bold.italic", angle = 90, hjust = -0.1) +
  annotate("text", x = xcor2, y = ycor2, label = annolabel2,
           size = 3.6, fontface = "bold.italic", angle = 90, hjust = -0.1) +
  annotate("text", x = xcor3, y = ycor3, label = annolabel3,
           size = 3.6, fontface = "bold.italic", angle = 90, hjust = -0.1) +
  annotate("text", x = xcor3, y = ycor4, label = annolabel4,
           size = 3.6, fontface = "bold.italic", angle = 90, hjust = 0.1, vjust = 0.1) +
  coord_cartesian(clip = 'off') +
  theme (legend.position = "none")+
  ##Vertical lines are integrated
  annotate("segment", x = 1931.7 + Line, xend = 1931.7 + Line, 
           y = 0, yend = 4291, size = 1) +
  annotate("segment", x = 1932.6 + Line, xend = 1932.6 + Line, 
           y = 0, yend = 53921, size = 1) +
  annotate("segment", x = 1932.7 + Line, xend = 1932.7 + Line,
           y = 0, yend = 53921, size = 1) +
  annotate("segment", x = 1933.6 + Line, xend = 1933.6 + Line, 
           y = 0, yend = 54924, size = 1) +
  annotate("segment", x = 1933.7 + Line, xend = 1933.7 + Line, 
           y = 0, yend = 54924, size = 1) +
  annotate("segment", x = 1934.6 + Line, xend = 1934.6 + Line, 
           y = 0, yend = 67668, size = 1) +
  annotate("segment", x = 1934.7 + Line, xend = 1934.7 + Line, 
           y = 0, yend = 67668, size = 1) +
  annotate("segment", x = 1935.6 + Line, xend = 1935.6 + Line, 
           y = 0, yend = 60069, size = 1) +
  annotate("segment", x = 1935.7 + Line, xend = 1935.7 + Line, 
           y = 0, yend = 60069, size = 1) +
  annotate("segment", x = 1936.6 + Line, xend = 1936.6 + Line, 
           y = 0, yend = 40879, size = 1) +
  annotate("segment", x = 1936.7 + Line, xend = 1936.7 + Line, 
           y = 0, yend = 40879, size = 1) +
  #Legend boxes are integrated
  annotate("rect", xmin = 1932.0 , xmax = 1932.2, ymin = 545000, 
           ymax = 565000, colour = "black", fill= "white", size = 1) +
  annotate("rect", xmin = 1932.0 , xmax = 1932.2, ymin = 520000, 
           ymax = 540000, colour = "black", fill= "black", size = 1) +
  annotate("rect", xmin = 1932.0 , xmax = 1932.2, ymin = 495000, 
           ymax = 515000, colour = "black", fill= "white", size = 1) +
  annotate("segment", x = seq(1932.0,1932.2, by = 0.02), 
           xend = seq(1932.0,1932.2,by=0.02), y = 545000, yend = 565000, size = 1) +
  #Legend titles are integrated
  annotate(geom = "text", x = 1932.7, y = 555000, label = "Muayene sayisi", size = 3.5, 
           fontface = "bold.italic") +
  annotate(geom = "text", x = 1932.7, y = 530000,label = "Trahomlu      \" ", size = 3.5, 
           fontface = "bold.italic") +
  annotate(geom = "text", x = 1932.7, y = 505000, label = "Ilaclananlar  \" ", size = 3.5, 
           fontface="bold.italic") +
  #3-lined title integrated
  annotate("text", x = 1933.4, y = 700000, label = "TRAHOM MUCADELESI", size = 5, 
           fontface = "bold") +
  annotate("text", x = 1933.4, y = 670000, label = "Seyyar teskilati faaliyeti", 
           size = 5, fontface = "bold.italic") +
  annotate("text", x = 1933.4, y = 640000, label = "1932 \u2013 1937", size = 5, 
           fontface = "bold")



## Figure 3: Meningococcal vaccine administered in various regions of the country, 1931-1937 ##

Time <- rep(seq(1931,1937),3)
First <- c(813, 62872,86112,25349,10399,533,42853)
Second <- c(813,59137,75361,19780,8943,251,41235)
Third <- c(813,51580,72132,19251,7300,250,34728)

Value <- c(First, Second, Third)
Group <- factor(c(rep(c("Birinci     asi","İkinci        \"","Ucuncu    \""), 
                      each = length(First))),
                levels = c("Birinci     asi","İkinci        \"","Ucuncu    \""))

meningococcal <- data.frame(Time, Value, Group)

xcor1 <- seq(1931,1937) - 0.3
xcor2 <- seq(1931,1937)
xcor3 <- seq(1931,1937) + 0.28
ycor1 <- First
ycor2 <- Second
ycor3 <- Third
annolabel1 <- c("813","62.872","86.112","25.349","10.399","533","42.853")
annolabel2 <- c("813","59.137","75.361","19.780","8.943","251","41.235")
annolabel3 <- c("813","51.580","72.132","19.251","7.300","250","34.728")

add <- seq(0,6) #number of bars-1
xline <- c(1930.6) + add
xlinend <- c(1931.4) + add

Line <- seq(0,0.14,0.07) #for vertical lines

ggplot(meningococcal, aes(x = Time, y = Value, fill = Group)) +
  geom_col(position = position_dodge(), color = "black", size = 1.05, width = 0.8) +
  scale_fill_manual(values=c("black","white","white")) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "bold", vjust = 1),
        axis.title.x = element_blank()) +
  annotate("segment", x = xline, xend = xlinend, y = 0, yend = 0, size = 1.2) + #thicker x-axis
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1930,1937)) +
  theme(plot.margin = unit(c(5,1,1,1), "lines")) +
  annotate("text", x = xcor1, y = ycor1, label = annolabel1,
           size = 3.2, fontface = "bold.italic", angle = 90, hjust = -0.1) +
  annotate("text", x = xcor2, y = ycor2, label = annolabel2,
           size = 3.2, fontface = "bold.italic", angle = 90, hjust = -0.1) +
  annotate("text", x = xcor3, y = ycor3, label = annolabel3,
           size = 3.2, fontface = "bold.italic", angle = 90, hjust = -0.1) +
  coord_cartesian(clip = 'off') +
  theme (legend.position = "none") +
  ##Vertical lines are integrated
  annotate("segment", x = 1931.2 + Line, xend = 1931.2 + Line, y = 0, 
           yend = 813, size = 1) +
  annotate("segment", x = 1932.2 + Line, xend = 1932.2 + Line, y = 0, 
           yend = 51580,size = 1) +
  annotate("segment", x = 1933.2 + Line, xend = 1933.2 + Line, y = 0, 
           yend = 72132,size = 1) +
  annotate("segment", x = 1934.2 + Line, xend = 1934.2 + Line, y = 0, 
           yend = 19251,size = 1) +
  annotate("segment", x = 1935.2 + Line, xend = 1935.2 + Line, y = 0, 
           yend = 7300,size = 1) +
  annotate("segment", x = 1936.2 + Line, xend = 1936.2 + Line, y = 0, 
           yend = 250,size = 1) +
  annotate("segment", x = 1937.2 + Line, xend = 1937.2 + Line, y = 0, 
           yend = 34728, size = 1) +
  #Legend boxes are integrated
  annotate("rect", xmin = 1934.4 , xmax = 1934.6 , ymin = 74600, 
           ymax = 76600, colour = "black", fill= "black", size = 1) +
  annotate("rect", xmin = 1934.4 , xmax = 1934.6 , ymin = 72000, 
           ymax = 74000, colour = "black", fill= "white", size = 1) +
  annotate("rect", xmin = 1934.4 , xmax = 1934.6 , ymin = 69500, 
           ymax = 71500, colour = "black", fill= "white", size = 1) +
  annotate("segment", x = seq(1934.4,1934.6,by=0.02), xend = seq(1934.4,1934.6,by=0.02), 
           y = 69500, yend = 71500, size = 1) +
  #Legend titles are integrated
  annotate(geom = "text", x = 1934.9, y = 76000, label = "Birinci   asi", size = 4, 
           fontface = "bold.italic") +
  annotate(geom = "text", x = 1934.9, y = 73500, label = "Ikinci      \" ", size = 4, 
           fontface = "bold.italic") +
  annotate(geom = "text", x = 1934.9, y = 71000, label = "Ucuncu   \" ", size = 4, 
           fontface="bold.italic") +
  #3-lined title integrated
  annotate("text", x = 1935, y = 90000, label = "Yurdun muhtelif bolgelerinde yapilan", 
           size = 4, fontface = "bold") +
  annotate("text", x = 1935, y = 85000, label = "Menengokok asisi", size = 6, 
           fontface = "bold") +
  annotate("text", x = 1935, y = 80000, label = "1931 \u2013 1937", size = 3.7, 
           fontface = "bold") +
  #Caption integrated
  labs(caption = "Sayilar asi tatbik edilen sahislarin miktarini gosterir.") +
  theme(plot.caption = element_text(face = "bold.italic", size = 12 , hjust = 0.0))


## Figure 4: Diphteria vaccine administered in various regions of the country, 1930-1936 ##

Time <- rep(seq(1930,1936),3)
First <- c(13047,743,2891,4000,14505,22366,1233)
Second <- c(10358,688,2800,2985,13965,13782,1233)
Third <- c(8878,424,2760,2088,11097,8291,1233)
Value <- c(First, Second, Third)
Group <- factor(c(rep(c("Birinci     asi","İkinci        \"","Ucuncu    \""), 
                      each = length(First))),
                levels = c("Birinci     asi","İkinci        \"","Ucuncu    \""))

diphteria <- data.frame(Time, Value, Group)

xcor1 <- seq(1930,1936) - 0.3
xcor2 <- seq(1930,1936)
xcor3 <- seq(1930,1936) + 0.28
ycor1 <- First
ycor2 <- Second
ycor3 <- Third
annolabel1 <- c("13.047","743","2.891","4.000","14.505","22.366","1.233")
annolabel2 <- c("10.358","688","2.800","2.985","13.965","13.782","1.233")
annolabel3 <- c("8.878","424","2.760","2.088","11.097","8.291","1.233")

add <- seq(0,6) #number of bars-1
xline <- c(1929.6) + add
xlinend <- c(1930.4) + add

Line <- seq(0,0.14,0.07) #for vertical lines

ggplot(diphteria, aes(x = Time, y = Value, fill = Group)) +
  geom_col(position = position_dodge(), color = "black", size = 1.05, width = 0.8) +
  scale_fill_manual(values = c("black","white","white")) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "bold", vjust = 1),
        axis.title.x = element_blank()) +
  annotate("segment", x = xline, xend = xlinend, y = 0, yend = 0, size = 1.2) + #thicker x-axis
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1930,1936)) +
  theme(plot.margin = unit(c(5,1,1,1), "lines")) +
  annotate("text", x = xcor1, y = ycor1, label = annolabel1,
           size = 3.2, fontface = "bold.italic", angle = 90, hjust = -0.1) +
  annotate("text", x = xcor2, y = ycor2, label = annolabel2,
           size = 3.2, fontface = "bold.italic", angle = 90, hjust = -0.1) +
  annotate("text", x = xcor3, y = ycor3, label = annolabel3,
           size = 3.2, fontface = "bold.italic", angle = 90, hjust = -0.1) +
  coord_cartesian(clip = 'off') +
  theme (legend.position = "none")+
  ##Vertical lines are integrated
  annotate("segment", x = 1930.2 + Line, xend = 1930.2 + Line, 
           y = 0, yend = 8878, size = 1) +
  annotate("segment", x = 1931.2 + Line, xend = 1931.2 + Line, 
           y = 0, yend = 424, size = 1) +
  annotate("segment", x = 1932.2 + Line, xend = 1932.2 + Line, 
           y = 0, yend = 2760, size = 1) +
  annotate("segment", x = 1933.2 + Line, xend = 1933.2 + Line, 
           y = 0, yend = 2088, size = 1) +
  annotate("segment", x = 1934.2 + Line, xend = 1934.2 + Line, 
           y = 0, yend = 11097, size = 1) +
  annotate("segment", x = 1935.2 + Line, xend = 1935.2 + Line, 
           y = 0, yend = 8291, size = 1) +
  annotate("segment", x = 1936.2 + Line, xend = 1936.2 + Line, 
           y = 0, yend = 1233, size = 1) +
  #Legend boxes are integrated
  annotate("rect", xmin = 1931.1 , xmax = 1931.2 , ymin = 17300, 
           ymax = 17600, colour = "black", fill= "black", size = 1) +
  annotate("rect", xmin = 1931.1 , xmax = 1931.2 , ymin = 16300, 
           ymax = 16600, colour = "black", fill= "white", size = 1) +
  annotate("rect", xmin = 1931.1 , xmax = 1931.2 , ymin = 15300, 
           ymax = 15600, colour = "black", fill= "white", size = 1) +
  annotate("segment", x = seq(1931.1,1931.2,by=0.025), 
           xend = seq(1931.1,1931.2,by=0.025), y = 15300, yend = 15600, 
           size = 1) +
  #Legend titles are integrated
  annotate(geom = "text", x = 1931.6, y = 17500, label = "Birinci   asi", 
           size = 4, fontface = "bold.italic") +
  annotate(geom = "text", x = 1931.6, y=16500, label = "Ikinci      \" ", 
           size = 4, fontface = "bold.italic") +
  annotate(geom = "text", x = 1931.6, y = 15500, label = "Ucuncu  \" ", 
           size = 4, fontface = "bold.italic") +
  #3-lined title integrated
  annotate("text", x = 1931, y = 24500, label = "Yurdun muhtelif bolgelerinde yapilan", 
           size = 4, fontface = "bold") +
  annotate("text", x = 1931, y = 23500, label = "Difteri asisi", 
           size = 6, fontface = "bold") +
  annotate("text", x = 1931, y = 22500, label = "1930 \u2013 1936", 
           size = 3.7 , fontface = "bold") +
  #Caption integrated
  labs(caption = "Sayilar asi tatbik edilen sahislarin miktarini gosterir.")  +
  theme(plot.caption = element_text(face = "bold.italic", size = 10, hjust = 0.0))



## Figure 5: Typhoid vaccine administered in variuos regions of the country, 1927-1937 ##

Time <- rep(seq(1927,1937),3)
First <- c(91431,94320,70005,71045,111012,114590,143650,
           190256,66022,29848,548961)
Second <- c(57443,55797,48810,38561,89604,82093,97657,
            133314,64938,28539,460192)
Third <- c(NA,NA,NA,NA,NA,NA,NA,NA,19162,NA,69963)
Value <- c(First,Second,Third)
Group <- factor(c(rep(c("Birinci     asi","İkinci        \"","Ucuncu    \""), 
                      each = length(First))),
                levels = c("Birinci     asi","İkinci        \"","Ucuncu    \""))

tifo <- data.frame(Time, Value, Group)

xcor1 <- seq(1927,1937) - 0.3
xcor2 <- seq(1927,1937)
xcor3 <- seq(1927,1937) + 0.28
ycor1 <- First
ycor2 <- Second
ycor3 <- Third
annolabel1 <- c("91.431","94 320","70.005","71.045","111.012","114.590",
                "143.650","190.256","66.022","29.848","548 961")
annolabel2 <- c("57.442","55.797","48.810","38.561","89.604","82.093",
                "97.657","133.314","64.938","28.539","460.192")
annolabel3 <- c("","","","","","","","","19.162","","69963")

add <- seq(0,10)  #number of bars-1
xline <- c(1926.6) + add
xlinend <- c(1927.4) + add

Line <- seq(0,0.14,0.07) #for vertical lines

ggplot(tifo, aes(x = Time, y = Value, fill = Group)) +
  geom_col(position = position_dodge(), color = "black", size = 1.25, width = 0.8) +
  scale_fill_manual(values=c("black","white","white")) +
  theme_classic() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "black", size = 11, face = "bold", 
                                   vjust = 1),
        axis.title.x = element_blank()) +
  annotate("segment", x = xline, xend = xlinend, y = 0, yend = 0, 
           size = 1.2) +  #thicker x-axis
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1927,1937)) +
  theme(plot.margin = unit(c(5,1,1,1), "lines")) +
  annotate("text", x = xcor1, y = ycor1, label = annolabel1,
           size = 3.2, fontface = "bold.italic", angle = 90, 
           hjust = -0.1) +
  annotate("text", x = xcor2, y = ycor2, label = annolabel2,
           size = 3.2, fontface = "bold.italic", angle = 90, 
           hjust = -0.1) +
  annotate("text", x = xcor3, y = ycor3, label = annolabel3,
           size = 3.2, fontface = "bold.italic", angle = 90, 
           hjust = -0.1) +
  coord_cartesian(clip = 'off') +
  theme (legend.position = "none")+
  ##Vertical lines are integrated
  annotate("segment", x = 1935.2 + Line, xend = 1935.2 + Line, 
           y = 0, yend = 19162, size = 1) +
  annotate("segment", x = 1937.2 + Line, xend = 1937.2 + Line, 
           y = 0, yend = 69963, size = 1) +
  ###annotate("segment", x = 1930.3, xend = 1930.3, y = 350000, yend = 400500, size = 1) +  ####?
  ##Legend boxes are integrated
  annotate("rect", xmin = 1929.9 , xmax = 1930.2 , ymin = 380000, 
           ymax = 395000, colour = "black", fill= "black", size = 1) +
  annotate("rect", xmin = 1929.9 , xmax = 1930.2 , ymin = 360000, 
           ymax = 375000, colour = "black", fill= "white", size = 1) +
  annotate("rect", xmin = 1929.9 , xmax = 1930.2 , ymin = 340000, 
           ymax = 355000, colour = "black", fill= "white", size = 1) +
  ###annotate("rect", xmin = 1930.25 , xmax = 1932 , ymin = 329000, ymax = 410000, fill= "white") +  ####?
  annotate("segment", x = seq(1929.9,1930.2, 0.04), xend =  seq(1929.9,1930.2, 0.04), 
           y = 340000, yend = 355000, size = 1) +
  ##Legend titles are integrated
  annotate("text", x = 1930.9 ,y = 387500, label = "Birinci   asi", size = 3.7, 
           fontface = "italic") +
  annotate("text", x = 1930.9 ,y = 367500, label = "İkinci      \" ", size = 3.7, 
           fontface = "italic") +
  annotate("text", x = 1930.9 ,y = 347500, label = "Ucuncu   \" ", size = 3.7, 
           fontface = "italic") +
  #3-line title integrated
  annotate("text", x = 1931, y = 500500, label = "Yurdun muhtelif bolgelerinde yapilan", 
           size = 4, fontface = "bold") +
  annotate("text", x = 1931, y = 470000, label = ". Tifo asisi", 
           size = 6, fontface = "bold") +
  annotate("text", x = 1931, y = 440000, label = "1927 \u2013 1937", 
           size = 3.7 , fontface = "bold") +
  #Caption integrated
  labs(caption = "Sayilar asi tatbik edilen sahislarin miktarini gosterir.") +
  theme(plot.caption = element_text(face = "bold.italic", size = 10 , hjust = 0.0))