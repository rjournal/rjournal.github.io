# script for submission


# Installing the released version of OpenLand from CRAN -------
install.packages("OpenLand")


# Loading the OpenLand package ----
library(OpenLand)

# downloading the SaoLourencoBasin multi-layer raster and make it available into R
url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"

temp <- tempfile()
download.file(url, temp, mode = "wb")
load(temp)

# looking on the metadata of the example dataset
SaoLourencoBasin 



# Accumulated changes -----
# the acc_changes() function, with the SaoLourencoBasin dataset
SL_changes <- acc_changes(SaoLourencoBasin)
SL_changes





# accumulate changes plot with tmap (acc_change) ----
install.packages("tmap")
library(tmap)
tmap::tmap_options(max.raster = c(plot = 41711112, view = 41711112))

acc_map <- tmap::tm_shape(SL_changes[[1]]) +
  tmap::tm_raster(
    style = "cat",
    labels = c(
      paste0(SL_changes[[2]]$PxValue[1], " Change", " (", round(SL_changes[[2]]$Percent[1], 2), "%", ")"),
      paste0(SL_changes[[2]]$PxValue[2], " Change", " (", round(SL_changes[[2]]$Percent[2], 2), "%", ")"),
      paste0(SL_changes[[2]]$PxValue[3], " Changes", " (", round(SL_changes[[2]]$Percent[3], 2), "%", ")")
    ),
    palette = c("#ede5cf", "#c1766f", "#541f3f"),
    title = "Changes in the interval \n2002 - 2014"
  ) +
  tmap::tm_legend(
    position = c(0.01, 0.2),
    legend.title.size = 1.2,
    legend.title.fontface = "bold",
    legend.text.size = 0.8
  ) +
  tmap::tm_compass(type = "arrow",
                   position = c("right", "top"),
                   size = 3) +
  tmap::tm_scale_bar(
    breaks = c(seq(0, 40, 10)),
    position = c(0.76, 0.001),
    text.size = 0.6
  ) +
  tmap::tm_credits(
    paste0(
      "São Lourenço River Basin",
      "\nAccumulated changes from 2002 to 2014",
      "\nData created with OpenLand package",
      "\nLUC Sources: Embrapa Pantanal et al. (2015)",
      "\nDatum: SIRGAS 2000"
    ),
    size = 0.7,
    position = c(0.01, -0, 01)
  ) +
  tmap::tm_graticules(
    n.x = 6,
    n.y = 6,
    lines = FALSE,
    #alpha = 0.1
    labels.rot = c(0, 90)
  ) +
  tmap::tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.02))

# the map
acc_map




# contingency table -------
## creating the contingency table
SL_2002_2014 <- contingencyTable(input_raster = SaoLourencoBasin,
                                 pixelresolution = 30)
names(SL_2002_2014)

## editing the category names
SL_2002_2014$tb_legend$categoryName <- factor(c("Ap", "FF", "SA", "SG", "aa", "SF", 
                                                "Agua", "Iu", "Ac", "R", "Im"),
                                              levels = c("FF", "SF", "SA", "SG", "aa", "Ap", 
                                                         "Ac", "Im", "Iu", "Agua", "R"))

## add the colours by the same order of the legend
SL_2002_2014$tb_legend$color <- c("#DDCC77", "#117733", "#44AA99", "#88CCEE",
                                  "#CC6677", "#999933", "#332288", "#AA4499",
                                  "#661100", "#882255", "#6699CC")

# Miscellaneous Non-Spatial Visualization Tools----

barplotLand(dataset = SL_2002_2014$lulc_Multistep, 
            legendtable = SL_2002_2014$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)


netgrossplot(dataset = SL_2002_2014$lulc_Multistep,
             legendtable = SL_2002_2014$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross Changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "#e0c2a2", NG = "#541f3f", NL = "#c1766f"), 
             area_km2 = TRUE)

chordDiagramLand(dataset = SL_2002_2014$lulc_Onestep,
                 legendtable = SL_2002_2014$tb_legend,
                 legposition = c(x = -1.3, y = 0.5),
                 x.margin = c(-1.2, 1),
                 area_km2 = TRUE)



sankeyLand(dataset = SL_2002_2014$lulc_Onestep,
           legendtable = SL_2002_2014$tb_legend)

sankeyLand(dataset = SL_2002_2014$lulc_Multistep,
           legendtable = SL_2002_2014$tb_legend)


# Intensity Analysis -----

testSL <- intensityAnalysis(dataset = SL_2002_2014, category_n = "Ap",
                            category_m = "SG", area_km2 = TRUE)

names(testSL)

# interval level
plot(testSL$interval_lvl,
     labels = c(leftlabel = "Interval change area (%)",
                rightlabel = "Annual change area (%)"),
     marginplot = c(-8, 0), leg_curv = c(x = .3, y = .1),
     color_bar = c(fast = "#541f3f", slow =  "#c1766f", area = "#888888"))





# category level: Gain area
plot(testSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~ km ^ 2 ~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .3), leg_curv = c(x = 1, y = .5))

# category level: Loss area
plot(testSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~ km ^ 2 ~ ")"),
                rightlabel = "Loss Intensity (%)"),
     marginplot = c(.3, .3), leg_curv = c(x = 1, y = .5))



# transition level: Gain of the category `n` Ap
plot(testSL$transition_lvlGain_n,
     labels = c(leftlabel = bquote("Gain of Ap (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain of Ap (%)"),
     marginplot = c(.3, .3), 
     leg_curv = c(x = 1, y = .4))

# transition level: Loss of the category `m` "SG"
plot(testSL$transition_lvlLoss_m,
     labels = c(leftlabel = bquote("Loss of SG (" ~ km^2 ~ ")"), 
                rightlabel = "Intensity Loss of SG (%)"),
     marginplot = c(.3, .3), 
     leg_curv = c(x = .1, y = .4))

