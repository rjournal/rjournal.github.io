###############################
## Do the whole plot again with fancy font

rahlfPlot <- function() {
    
    par(cex.axis=1.1, mai=c(0.75,1.5,0.25,0.5), omi=c(0.5,0.5,1.1,0.5),
        mgp=c(6,1,0), family="Lato Light", las=1)
	
    ## Import data and prepare chart

    colour <- rgb(68, 90, 111, 150, maxColorValue=255)
    myData <- read.csv("data/chile.csv")
    with(myData,
    {
        ## Define chart and other elements

        plot(x, y, axes=F, type="n", xlab="",
             xlim=c(1800,2020), ylim=c(0,14000),
             xpd=T, ylab="million 1990 International Geary-Khamis dollars")
        axis(1,at=pretty(x),col=colour)
        axis(2,at=py <- pretty(y), col=colour, cex.lab=1.2,
             labels=format(py, big.mark=","))

        y <- ts(y,start=1800,frequency=1)
        points(window(y, end=1869))
        lines(window(y, start=1870))

        myShapeColour1 <- rgb(0, 128, 128, 50, maxColorValue=255)
        myShapeColour2 <- rgb(0, 128, 128, 80, maxColorValue=255)
        mySelection <- subset(myData, x >= 1879 & x <= 1884)
        polygon(c(min(mySelection$x), mySelection$x,
                  max(mySelection$x)),
                c(-500, mySelection$y, -500),
                col=myShapeColour2, border=NA)
        text(1860, 2200, adj=0, col=colour, "Pacific War")
        mySelection <- subset(myData, x >= 1940 & x <= 1973)
        polygon(c(min(mySelection$x), mySelection$x, max(mySelection$x)),
                c(-500, mySelection$y, -500),
                col=myShapeColour1,border=NA)
        text(1930, 5000, adj=0, col=colour, "Allende Regime")
        mySelection <- subset(myData, x >= 1973 & x <= 1990)
        polygon(c(min(mySelection$x), mySelection$x, max(mySelection$x)),
                c(-500, mySelection$y, -500),
                col=myShapeColour2, border=NA)
        text(1960, 6800, adj=0, col=colour, "Military Regime")
    })

    ## Titling

    mtext("Gross national product of Chile",
          3, line=2, adj=0, cex=2.4, family="Lato Black", outer=T)
    mtext("Annual figures", 3, line=-0.5, adj=0, cex=1.8, font=3, outer=T)
    mtext(paste("Source: Rolf Lüders,",
                "The Comparative Economic Performance of Chile 1810-1995,",
                "www.ggdc.net/maddison"),
          1, line=3, adj=1.0, cex=0.95, font=3)
}
