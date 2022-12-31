library(libstableR)
library(ggthemes)
library(ggplot2)
library(broman)
domain <- seq(-5,5,0.01)

cum.i <- NULL
for(i in seq(1.0,2,0.25)){

this.i <-   data.frame(x=domain, pdf=stable_pdf(domain, pars = c(i,0,1,0)), a=i)
cum.i <- rbind(cum.i, this.i)
}

p <- ggplot(cum.i, aes(x,y=pdf,frame=a, color=factor(myround(a,2)))) +
  geom_line(lwd=1.4) +
  scale_color_brewer(name="alpha", palette="YlOrRd")+
  theme_dark(base_size=20) +
  ggtitle("Cauchy (alpha=1) to Gaussian (alpha=2)")+
  xlab(NULL)+
  ylab("Density")
  


p
ggsave("cauchy2gauss.png", width=6.67, height=6.67, units="in")
ggsave("cauchy2gauss.png", width=16.67, height=16.67, units="in")
## gganimate(p)
