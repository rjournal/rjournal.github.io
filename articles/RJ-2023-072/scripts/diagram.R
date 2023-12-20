
## Diagram with circles with linear gradient on the borders!

## https://stackoverflow.com/questions/69755844/is-it-possible-to-draw-the-following-diagram-in-r

## ./original.jpg

## I got pointed here via R-help post
## "[R] Help needed with data visualization in R"
## from bharat rawlley <bharat_m_all@yahoo.co.in>
## 2021-11-02

percent <- c(51, 48, 46, 41, 29, 25, 34, 18, 18)

group <- c("medical school applicants",
           "medical school graduates",
           "residents",
           "faculty",
           "division chiefs",
           "full professors",
           "senior associate deans",
           "department chairs",
           "deans")

o <- order(percent, decreasing=TRUE)

amount <- percent[o]
label <- gsub(" ", "\n", group[o])

diam <- sqrt(amount)
rad <- diam/2
x <- cumsum(diam) - diam/2
y <- rad

w <- sum(diam)
h <- 2*max(diam)
    
library(grid)

grad <- linearGradient(rgb(0, 0, 0, c(.5, .1)),
                       y1=.5, y2=.5)

shadedCircle <- function(x, y, r) {
    pushViewport(viewport(x, y, 2*r, 2*r, default.units="native",
                          mask=rectGrob(width=1.2, height=1.2,
                                        gp=gpar(col=NA, fill=grad))))
    grid.circle(.5, .5, .5, default.units="native",
                gp=gpar(lwd=10))
    upViewport()
}

png("diagram.png", width=800, height=400)

grid.newpage()
pushViewport(viewport(width=.9,
                      layout=grid.layout(widths=w, heights=h, respect=TRUE)),
             viewport(layout.pos.col=1,
                      xscale=c(0, w),
                      yscale=c(0, h)))
grid.rect(gp=gpar(col=NA, fill="grey95"))
## grid.circle(x, y, r=rad, default.units="native")
purple <- colorspace::darken("purple", .5)
for (i in seq_along(x)) {
    shadedCircle(x[i], y[i], rad[i])
    amountGrob <- textGrob(paste0(amount[i], "%"),
                           x[i], y[i], default.units="native",
                           gp=gpar(col=purple,
                                   fontface="bold",
                                   cex=1 + amount[i]/max(amount)))
    labelGrob <- textGrob(toupper(label[i]),
                          x[i],
                          unit(.7, "npc"), ## 1.7*max(diam),
                          default.units="native",
                          just="bottom",
                          gp=gpar(fontface="bold", cex=.7, lineheight=1))
    grid.draw(amountGrob)
    grid.draw(labelGrob)
    grid.segments(unit(x[i], "native"),
                  grobY(amountGrob, 90) + unit(2, "mm"),
                  unit(x[i], "native"),
                  grobY(labelGrob, 270) - unit(2, "mm"),
                  gp=gpar(col=purple, lwd=2))
}

dev.off()
