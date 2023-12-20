library("grid")
library("colorspace")
cellSize <- unit(4, "mm")
ncol <- 10
ns <- 3
gap <- unit(1, "lines")
border <- unit(0, "mm")
bgcol <- "grey80"
bgfill <- "white"
paletteGrid <- function(cols, qual=FALSE) {
    n <- length(cols)
    nrow <- ceiling(n / ncol)
    pushViewport(viewport(width=ncol*cellSize, height=nrow*cellSize,
                          x=0, y=1,
                          just=c("left", "top")))
    x <- rep((1:ncol - 1)/ncol, nrow)[1:n] + .5/ncol
    y <- rep(1 - (1:nrow - 1)/nrow, each=ncol)[1:n] - .5/nrow
    if (qual) {
        ## grid.rect(x=0, width=n/ncol, just="left", gp=gpar(col=NA, fill="grey90"))
        grid.circle(x, y, r=.4/nrow,
                    gp=gpar(col="grey40",
                            fill=cols, lwd=1))
    } else {    
        grid.rect(x, y, 
                  width=unit(1/ncol, "npc"),
                  height=unit(1/nrow, "npc"),
                  gp=gpar(col="grey40",
                          fill=cols, lwd=1,
                          linejoin="mitre"))
    }
    upViewport()
}
paletteGrids <- function(colList, nswatch=ns, qual=FALSE) {
    n <- length(colList)
    qual <- rep(qual, length.out=n)
    nrow <- ceiling(n / nswatch)
    layout <- grid.layout(nrow, nswatch,
                          heights=cellSize + gap,
                          widths=ncol*cellSize + gap,
                          just=c("left", "top"))
    pushViewport(viewport(width=unit(1, "npc") - 2*border,
                          height=unit(1, "npc") - 2*border,
                          layout=layout, name="top"))
    for (i in 1:n) {
        row <- (i-1) %/% nswatch + 1
        col <- (i-1) %% nswatch + 1
        if (!is.null(colList[[i]])) {
            pushViewport(viewport(layout.pos.row=row, layout.pos.col=col))
            grid.text(names(colList)[i], 0, unit(1, "npc") - .7*gap, 
                      just=c("left", "top"),
                      gp=gpar(cex=.7, fontfamily="mono"))
            pushViewport(viewport(y=unit(1, "npc") - gap, just="top"))
            paletteGrid(colList[[i]], qual=qual[i])
            upViewport(2)
        }
    }
    upViewport()
    ## Tiny dot in bottom-right corner so that PDF output does not
    ## remove white space from right side and bottom
    ## (so that swatches are consistent size across figures in PDF output)
    grid.circle(x=.9, y=0, r=unit(.1, "mm"), gp=gpar(col=NA, fill="grey90"))
}
drawBorder <- function(row, y=.5, just="centre",
                       height=unit(1, "npc") + 2*border) {
  pushViewport(viewport(layout.pos.row=row, layout.pos.col=1:3))
  grid.rect(width=unit(1, "npc") + 2*border,
            height=height,
            y=y, just=just,
            gp=gpar(col=bgcol, fill=bgfill),
            name="bg1")
  upViewport()
}
drawBorder <- function(row, 
                       y1=unit(1, "npc") - unit(1, "mm"),
                       y2=unit(0, "npc"),
                       name=NULL) {
  pushViewport(viewport(layout.pos.row=row, layout.pos.col=1:3))
  grid.segments(unit(0, "npc") - border, y1,
                unit(0, "npc") - border, y2,
                gp=gpar(col=bgcol, lwd=7, lineend="butt"),
                name=name)
  upViewport()
}
drawBorder <- function(..., name=NULL) {
  grid.null(name=name)
}
