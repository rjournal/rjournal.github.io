# install.packages("StratigrapheR")

# please use StratigrapheR version 1.2.1

library(StratigrapheR)

# ----

interval <- as.lim(l = c(0,1,2), r = c(0.5,2,2.5), # Make a lim object
                   id = c("Int. 1","Int.2","Int.3"))
interval # print what is in the lim object
#> $l
#> [1] 0 1 2
#> $r
#> [1] 0.5 2.0 2.5
#> $id
#> [1] "Int. 1" "Int.2" "Int.3"
#> $b
#> [1] "[]" "[]" "[]"
# Visualization of the lim object
plot.new()
plot.window(ylim = c(-0.5, 2.5), xlim = c(0, 2.5))
axis(3, pos = 1.5, las = 1)
infobar(ymin = 0, ymax = 1, xmin = interval$l, xmax = interval$r,
        labels = c(interval$id), srt = 0)

# ----

graphical_function <- function() # Graphical function needed by pdfDisplay
{
  opar <- par()$mar # Save initial graphical parameters
  par(mar = c(0,3,0,1)) # Change the margins of the plot
  plot.new() # Open a new plot
  plot.window(xlim = c(-0.2, 1.2), ylim = c(-5, 1)) # Define plot coordinates
  minorAxis(2, at.maj = seq(-5, 1, 0.5), n = 5, las = 1) # Add axis
  points(c(0.25, 0.75), c(0.75, 0.75), pch = 19)
  polygon(c(0.1, 0.25, 0.75, 0.9, 0.75, 0.25, NA,
            0, 0.25, 0.75, 1, 0.75, 0.25),
          c(0.5, 0.25, 0.25, 0.5, 0.4, 0.4, NA,
            0.5, 0, 0, 0.5, 1, 1), lwd = 2)
  lines(x = c(0.5, 0.5, NA, 0, 0.2, 0.5, 0.8, 1, NA,
              0, 0.2, 0.5, 0.9, 1.2),
        y = c(-0.25, -3, NA, -5, -4, -3, -4, -5, NA,
              -2.5, -1.5, -1, -0.75, 0.25), lwd = 2)
  par(mar = opar) # Restore initial graphical parameters
}
pdfDisplay(graphical_function(),"graphical_function", width = 3.5, height = 10)

# ----

i <- c(rep("A1",6), rep("A2",6), rep("A3",6)) # Polygon IDs
x <- c(1,2,3,3,2,1,2,3,4,4,3,2,3,4,5,5,4,3) # x coordinates
y <- c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6) # y coordinates
plot.new()
plot.window(xlim = c(0,6), ylim = c(0,7))
multigons(i, x, y,
          front = "A2", # This gets the polygon A2 in front of all others
          density = c(NA, 5, 10), # Shading line density
          scol = "grey20", # Shading line colour; one value means all polygons
          # are subject to this graphical parameter
          col = c("black", "grey80", "white"), # Background colour
          lwd = 2, # Width of border lines
          slty = 2, slwd = 1) # Shading lines type and width

# ----

i <- c(rep("A1",6), rep("A2",6), rep("A3",6)) # Lines IDs
x <- c(1,2,3,3,2,1,4,5,6,6,5,4,7,8,9,9,8,7) # x coordinates
y <- c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6) # y coordinates
plot.new()
plot.window(xlim = c(0,10), ylim = c(0,7))
multilines(i, x, y,
           j = c("A3", "A1", "A2"), # j controls the order of the graphical
           # parameters applied to each named line:
           lty = c(1,2,3), lwd = 2, # e.g. lty = 1 (solid line) is applied
           # to "A3", the line at the right
           type = c("l", "o", "o"),
           pch = c(NA,21,24), cex = 1, bg = "black")

# ----

svg.file.directory <- tempfile(fileext = ".svg") # Creates temporary file
writeLines(example.ammonite.svg, svg.file.directory) # Writes svg in the file
ammonite.drawing <- pointsvg(file = svg.file.directory) # Read svg
plot.new()
plot.window(xlim = c(-2, 5), ylim = c(-2, 2))
axis(1)
axis(2, las = 2)
centresvg(ammonite.drawing, # Object
          x = c(3,0), y = 0, # Coordinates for centering
          xfac = 2, yfac = 2, # Dimension stretching factors
          col = c("grey","white")) # Graphical parameters

# ----

basic.log <- litholog(l = bed.example$l,  # This creates a data table of
                      r = bed.example$r,  # rectangles coordinates for a
                      h = bed.example$h,  # basic litholog
                      i = bed.example$id)

legend <- data.frame(litho = c("S", "L", "C"),             # This creates a data
                     col = c("grey30", "grey90", "white"), # table for the
                     density = c(30, 0,10),                # symbology
                     angle = c(180, 0, 45), stringsAsFactors = FALSE)
View(legend)

# left_join in the dplyr package merges the symbology with the table of beds:
bed.legend <- dplyr::left_join(bed.example,legend, by = "litho")

plot.new()
plot.window(xlim = c(0,6), ylim = c(-1,77))
minorAxis(2, at.maj = seq(0, 75, 5), n = 5)
# Plotting of the polygons making the litholog,
# with corresponding symbology:
multigons(basic.log$i, x = basic.log$xy, y = basic.log$dt,
          col = bed.legend$col,
          density = bed.legend$density,
          angle = bed.legend$angle)
# Writing the name of beds, only in beds thick enough
bedtext(labels = bed.example$id, l = bed.example$l, r = bed.example$r,
        x = 0.5, # x position where to center the text
        ymin = 3) #  text is not added in beds that thinner than ymin

# ----

# Code repeated from earlier examples ----
basic.log <- litholog(l = bed.example$l, r = bed.example$r,
                      h = bed.example$h, i = bed.example$id)
legend <- data.frame(litho = c("S", "L", "C"),
                     col = c("grey30", "grey90", "white"),
                     density = c(30, 0,10),
                     angle = c(180, 0, 45), stringsAsFactors = FALSE)
bed.legend <- dplyr::left_join(bed.example,legend, by = "litho")
# ----
# Generation of the boundaries, either sinusoidal or from drawings ---
s1 <- sinpoint(5,0,0.5,nwave = 1.5)
s2 <- sinpoint(5,0,1,nwave = 3, phase = 0)
s3 <- framesvg(example.liquefaction, 1, 4, 0, 2, plot = FALSE, output = TRUE)
# Visualizing the s3 boundary, i.e. the liquefaction sedimentary feature ----
plot(s3$x, s3$y, cex.axis = 1.2, lwd = 2,
     type = "l", ylab = "", xlab = "", bty = "n", las = 1)

# Welding the boundaries to the basic litholog ----
final.log <- weldlog(log = basic.log,
                     dt = boundary.example$dt, # Position of the boundaries
                                               # to be changed
                     seg = list(s1 = s1, s2 = s2, s3 = s3), # list of segments
                     j = c("s1","s1","s1","s3", # Attributing the segments to
                           "s2","s2","s1"),     # the respective bed boundaries
                                                # to  be changed
                     warn = F)
# Visualizing the resulting litholog (similarly to earlier code) ----
plot.new()
plot.window(xlim = c(0,6), ylim = c(-1,77))
minorAxis(2, at.maj = seq(0, 75, 5), n = 5, las = 1)
multigons(final.log$i, x = final.log$xy, y = final.log$dt,
          col = bed.legend$col,
          density = bed.legend$density,
          angle = bed.legend$angle)
bedtext(labels = bed.example$id, l = bed.example$l, r = bed.example$r,
        x = 0.75, ymin = 3)

# ----

# Code repeated from earlier examples ----
basic.log <- litholog(l = bed.example$l, r = bed.example$r,
                      h = bed.example$h, i = bed.example$id)
legend <- data.frame(litho = c("S", "L", "C"), density = c(30, 0,10),
                     col = c("grey30", "grey90", "white"),
                     angle = c(180, 0, 45), stringsAsFactors = FALSE)
bed.legend <- dplyr::left_join(bed.example,legend, by = "litho")
s1 <- sinpoint(5,0,0.5,nwave = 1.5)
s2 <- sinpoint(5,0,1,nwave = 3, phase = 0)
s3 <- framesvg(example.liquefaction, 1, 4, 0, 2, plot = FALSE, output = TRUE)
final.log <- weldlog(log = basic.log, dt = boundary.example$dt,
                     seg = list(s1 = s1, s2 = s2, s3 = s3),
                     j = c("s1","s1","s1","s3","s2","s2","s1"), warn = F)

# Visualizing the resulting litholog (similarly to earlier code) ----
plot.new()
plot.window(xlim = c(-1.5,8), ylim = c(-1,81))
minorAxis(2, at.maj = seq(0, 75, 5), n = 5, las = 1)
multigons(final.log$i, x = final.log$xy, y = final.log$dt,
          col = bed.legend$col,
          density = bed.legend$density,
          angle = bed.legend$angle)
bedtext(labels = bed.example$id, l = bed.example$l, r = bed.example$r,
        x = 0.5, ymin = 2)
# Making a data table for the symbology of magnetochrons
legend.chron <- data.frame(polarity = c("N", "R"),
                           bg.col = c("black", "white"),
                           text.col = c("white", "black"),
                           stringsAsFactors = FALSE)
# Merging symbology with a data table of chrons
chron.legend <- dplyr::left_join(chron.example, legend.chron, by = "polarity")
# Plotting the chrons with the given symbology
infobar(-1.5, -1, chron.legend$l, chron.legend$r,
        labels = chron.legend$polarity,
        m = list(col = chron.legend$bg.col),
        t = list(col = chron.legend$text.col),
        srt = 0)
# Adding colour information
colour <- bed.example$colour
colour[colour == "darkgrey"] <- "grey20"
colour[colour == "brown"] <- "tan4"
# Plotting the colour next to the litholog
infobar(-0.25, -0.75, bed.example$l, bed.example$r,
        m = list(col = colour))
text(-0.5, 79, "Colour", srt = 90)
text(-1.25, 79, "Magnetochrons", srt = 90)
axis(4, at = proxy.example$dt, labels = proxy.example$name,
     pos = 6, lwd = 0, lwd.ticks = 1, las = 1)

# ----

# Code repeated from earlier examples ----
basic.log <- litholog(l = bed.example$l, r = bed.example$r,
                      h = bed.example$h, i = bed.example$id)
legend <- data.frame(litho = c("S", "L", "C"),
                     col = c("grey30", "grey90", "white"),
                     density = c(30, 0,10),
                     angle = c(180, 0, 45), stringsAsFactors = FALSE)
bed.legend <- dplyr::left_join(bed.example,legend, by = "litho")
s1 <- sinpoint(5,0,0.5,nwave = 1.5)
s2 <- sinpoint(5,0,1,nwave = 3, phase = 0)
s3 <- framesvg(example.liquefaction, 1, 4, 0, 2, plot = FALSE, output = TRUE)
final.log <- weldlog(log = basic.log, dt = boundary.example$dt,
                     seg = list(s1 = s1, s2 = s2, s3 = s3),
                     j = c("s1","s1","s1","s3","s2","s2","s1"), warn = F)
# ----
opar <- par() # Save initial graphical parameters (IGP)
par(mfrow = c(1,2), # Set two vertical plots along each other
    yaxs = "r", # Default setting, adds 4% more range for y
    mar = c(5.1, 4.1, 4.1, 0.1)) # Change settings for margins
# Visualizing the resulting litholog (similarly to earlier code) ----
plot.new()
plot.window(xlim = c(0,6), ylim = c(-1,77))
minorAxis(2, at.maj = seq(0, 75, 5), n = 5, las = 1)
multigons(final.log$i, x = final.log$xy, y = final.log$dt,
          col = bed.legend$col,
          density = bed.legend$density,
          angle = bed.legend$angle)
bedtext(labels = bed.example$id, l = bed.example$l, r = bed.example$r,
        x = 0.75, ymin = 3)
# Visualizing quantified values along the litholog ----
par(mar = c(5.1, 0.1, 4.1, 4.1)) # Change settings for margins of 2nd plot
plot.new()
plot.window(xlim = c(-2*10^-8,8*10^-8), ylim = c(-1,77)) # ylim similar to
# litholog
minorAxis(4, at.maj = seq(0, 75, 5), n = 5, las = 1) # Repetition of the axis to
# check both sides are matching
lines(proxy.example$ms, proxy.example$dt, type = "o", pch = 19)
axis(1)
title(xlab = "Magnetic Susceptibility")
par(mar = opar$mar, mfrow = opar$mfrow, yaxs = opar$yaxs) # Restore IGP

# ----

legend <- data.frame(litho = c("S", "L", "C"), # Symbology
                     col = c("grey30", "grey90", "white"), # data table
                     density = c(30, 0,10), angle = c(180, 0, 45),
                     stringsAsFactors = FALSE)
f <- function(legend_row) # To simplify coding, we design here a function
  # plotting rectangles with the desired symbology
{
  multigons(i = rep(1, 4), c(-1,-1,1,1), c(-1,1,1,-1),
            col = legend$col[legend_row],
            density = legend$density[legend_row],
            angle = legend$angle[legend_row])
}
opar <- par() # Save initial graphical parameters
par(mar = c(0,0,0,0), mfrow = c(5,1)) # Make 5 plot windows
nlegend(t = "Shale", cex = 2) # The cex parameter controls the size of the text
f(1) # 1 stands for the first row of the symbology data table
nlegend(t = "Limestone", cex = 2)
f(2)
nlegend(t = "Chert", cex = 2)
f(3)
nlegend(t = "Ammonite", cex = 2)
centresvg(example.ammonite, 0,0,xfac = 0.5)
nlegend(t = "Belemnite", cex = 2)
centresvg(example.belemnite, 0,0,xfac = 0.5)
par(mar = opar$mar, mfrow = opar$mfrow) # Restore initial graphical parameters

# ----

# Code repeated from earlier examples ----
basic.log <- litholog(l = bed.example$l, r = bed.example$r,
                      h = bed.example$h, i = bed.example$id)
legend <- data.frame(litho = c("S", "L", "C"),
                     col = c("grey30", "grey90", "white"),
                     density = c(30, 0,10),
                     angle = c(180, 0, 45), stringsAsFactors = FALSE)
bed.legend <- dplyr::left_join(bed.example,legend, by = "litho")
s1 <- sinpoint(5,0,0.5,nwave = 1.5)
s2 <- sinpoint(5,0,1,nwave = 3, phase = 0)
s3 <- framesvg(example.liquefaction, 1, 4, 0, 2, plot = FALSE, output = TRUE)
final.log <- weldlog(log = basic.log, dt = boundary.example$dt,
                     seg = list(s1 = s1, s2 = s2, s3 = s3),
                     j = c("s1","s1","s1","s3","s2","s2","s1"), warn = F)
legend.chron <- data.frame(polarity = c("N", "R"),
                           bg.col = c("black", "white"),
                           text.col = c("white", "black"),
                           stringsAsFactors = FALSE)
chron.legend <- dplyr::left_join(chron.example,legend.chron, by = "polarity")
colour <- bed.example$colour
colour[colour == "darkgrey"] <- "grey20"
colour[colour == "brown"] <- "tan4"
# ----
# Function that will draw a litholog, with personalized coordinates control
log.function <- function(xlim = c(-2.5,7), ylim = c(-1,77))
{
  plot.new()
  plot.window(xlim = xlim, ylim = ylim)
  minorAxis(2, at.maj = seq(0, 75, 5), n = 5, pos = -1.75, las = 1)
  multigons(final.log$i, x = final.log$xy, y = final.log$dt,
            col = bed.legend$col,
            density = bed.legend$density,
            angle = bed.legend$angle)
  bedtext(labels = bed.example$id, l = bed.example$l, r = bed.example$r,
          x = 1, edge = TRUE, ymin = 2)
  centresvg(example.ammonite, 6,
            fossil.example$dt[fossil.example$type == "ammonite"],
            xfac = 0.5)
  centresvg(example.belemnite, 6,
            fossil.example$dt[fossil.example$type == "belemnite"],
            xfac = 0.5)
  infobar(-1.5, -1, chron.legend$l, chron.legend$r,
          labels = chron.legend$id, m = list(col = chron.legend$bg.col),
          t = list(col = chron.legend$text.col))
  infobar(-0.25, -0.75, bed.example$l, bed.example$r,
          m = list(col = colour))
}
# In this gr() function, log.function() is repeated, which plots the
# desired parts of the litholog
gr <- function()
{
  opar <- par() # Save initial graphical parameters
  par(mar = c(1,2,1,2), yaxs = "i")
  ylim <- c(0,40) # Initial range to be plotted
  for(i in 1:0) log.function(ylim = ylim + 40*i) # Iteration of the plotting
  # The drawing range's length is iteratively added to the range already drawn
  par(mar = opar$mar, yaxs = opar$yaxs) # Restore initial graphical parameters
}
# Integration of gr() in pdfDisplay to make PDFs
pdfDisplay(gr(), name = "divided log", width = 3, height = 5)

# The code can be adapted to divide the plot differently,
# and to add other plots along the litholog
gr2 <- function()
{
  opar <- par() # Save initial graphical parameters (IGP)
  low <- c(-5, 25, 55) # Another way of defining the dimensions
  high <- c( 25, 55, 85) # of succesive plotting windows
  for(i in 3:1){ # Inverted order to have them in stratigraphic order
    par(mfrow = c(1,2), yaxs = "i") # Plot in two columns, same yaxs for both
    par(mar = c(5,2,1,0)) # Define margins for first plot (left)
    log.function(ylim = c(low[i], high[i]))
    par(mar = c(5,0,1,1)) # Second plot (right): change only the vertical
    # margins (2nd and 4th)
    plot.new()
    plot.window(xlim = c(-2*10^-8,8*10^-8), ylim = c(low[i], high[i]))
    lines(proxy.example$ms, proxy.example$dt, type = "o", pch = 19)
    axis(1)
    title(xlab = "Magnetic Susceptibility")
  }
  par(mar = opar$mar, yaxs = opar$yaxs, mfrow = opar$mfrow) # Restore IGP
}
pdfDisplay(gr2(), name = "divide in 3", wi = 5, he = 7)

# ----

svg.file.directory <- tempfile(fileext = ".svg") # Creates temporary file
writeLines(example.HB2000.svg, svg.file.directory) # Writes svg in the file
# Log: 1 Humblet and Boulvain 2000 ----
a <- pointsvg(svg.file.directory) # Import the svg
out <- framesvg(a,
                xmin = 0, xmax = 5, # Initial coordinates of the
                ymin = 27, ymax = 36, # rectangle (see SVG file)
                output = T, # This allows to output the changed coordinates
                forget = "P287") # 'forget' removes the rectangle added in the
                                 # svg to serve as a referential in x and y
# Log 2: Code repeated from earlier examples ----
basic.log <- litholog(l = bed.example$l, r = bed.example$r,
                      h = bed.example$h, i = bed.example$id)
legend <- data.frame(litho = c("S", "L", "C"),
                     col = c("grey30", "grey90", "white"),
                     density = c(30, 0,10),
                     angle = c(180, 0, 45), stringsAsFactors = FALSE)
bed.legend <- dplyr::left_join(bed.example,legend, by = "litho")
s1 <- sinpoint(5,0,0.5,nwave = 1.5)
s2 <- sinpoint(5,0,1,nwave = 3, phase = 0)
s3 <- framesvg(example.liquefaction, 1, 4, 0, 2, plot = FALSE, output = TRUE)
final.log <- weldlog(log = basic.log, dt = boundary.example$dt,
                     seg = list(s1 = s1, s2 = s2, s3 = s3),
                     j = c("s1","s1","s1","s3","s2","s2","s1"), warn = F)
# Plotting two logs in front of each other ----
plot.out <- out # Save a version of the svg object
tie.points <- data.frame(l = c(20,35,54,66), # Define points to correlate
                         r.raw = c(29.8,31,32.5,33.25)) # the two sections in
                                                        # their own depth scales
plot.out$x <- 15 - out$x # Change the coordinates for
plot.out$y <- 10*(out$y - 27.5) # second litholog (imported
axs2 <- 10*(28:35 - 27.5) # from Fig. 1), to plot it t
tie.points$r <- 10*(tie.points$r.raw - 27.5) # in front of the first litholog
g <- function()
{
  opar <- par() # Save initial graphical parameters
  par(mar = c(1,4,1,4))
  plot.new()
  plot.window(xlim = c(0,15), ylim = c(0,75))
  minorAxis(2, at.maj = seq(0,75, 5), n = 5, las = 1, cex.axis = 1.2)
  minorAxis(4, at.maj = axs2, labels = 28:35, n = 10, las = 1, cex.axis = 1.2)
  multigons(final.log$i, x = final.log$xy, y = final.log$dt,
            col = bed.legend$col,
            density = bed.legend$density,
            angle = bed.legend$angle)
  placesvg(plot.out, col = "white") # Adding the drawn plot
  ylink(tie.points$l, tie.points$r, 6, 9, ratio = 0.5, # Correlation between
        l = list(lty = c(1,2,2,1), lwd = 2)) # the two plots
  par(mar = opar$mar) # Restore initial graphical parameters
}
pdfDisplay(g(), "Log Correlation")

# ----

window <- in.window(irreg.example$dt, # Depth values
                    w = 30, # Size of the window
                    xout = seq(0, 600, 20), # Center position of windows
                    xy = irreg.example$xy) # Intensity values (or other)
mov.mean <- rowMeans(window$xy, na.rm = TRUE) # Average of the intensity
# values in windows
presence <- matrix(as.integer(!is.na(window$xy)), # Discriminate between NA
                   ncol = ncol(window$xy)) # values and intensity values
amount <- rowSums(presence) # to determine the amount of
# real values in each window
# (example of window calculation)
opar <- par() # Save initial graphical parameters
par(mfrow = c(2,1), mar = c(0,4,0,0))
plot(irreg.example$dt, irreg.example$xy, type = "o", pch = 19,
     xlim = c(0,600), xlab = "dt", ylab = "xy and moving average", axes = F)
lines(window$xout, mov.mean, col = "red", lwd = 2)
axis(2, las = 1)
par(mar = c(5,4,0,0))
plot(window$xout, amount, pch = 19, xlim = c(0,600), ylim = c(0,25),
     xlab = "dt", ylab = "amount of points in the windows", axes = F)
axis(1)
axis(2, las = 1)
par(mar = opar$mar, mfrow = opar$mfrow) # Restore initial graphical parameters

# ----

id <- c("samp1", "samp1", "samp2", "samp3", "samp3", "samp3")
meas <- c( 0.45, 0.55, 5.0, 100, 110, 120)
new_sequence <- nset(id, 2, warn = F)
new_sequence
#> [,1] [,2]
#> samp1 1 2
#> samp2 3 NA
#> samp3 4 5
clean_meas <- matrix(meas[new_sequence], ncol = 2)
row.names(clean_meas) <- unique(id)
clean_meas
#> [,1] [,2]
#> samp1 0.45 0.55
#> samp2 5.00 NA
#> samp3 100.00 110.00











