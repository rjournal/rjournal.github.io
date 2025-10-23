#
# Script for generating all figures in the article
# (calls each script in the subdirectory fig/)
#

# directory where the figures will be written to
figprefix <- "fig/"

# directory with the scripts (relative to figprefix)
scriptprefix <- "../scripts/"

# directory with the data (relative to figprefix)
dataprefix <- "../data/"

if (basename(figprefix) != basename(getwd())) setwd(figprefix)

# Figure 1
source(paste(scriptprefix, "plot-volatility-politis.r", sep=""))

# Figure 2
source(paste(scriptprefix, "plot-goetze-max.r", sep=""))

# Figure 3
source(paste(scriptprefix, "plot-pcov-fixed-m.r", sep=""))

# Figure 4
source(paste(scriptprefix, "plot-pcov-databased-m.r", sep=""))

# Figure 5
source(paste(scriptprefix, "plot-mean-comparison.r", sep=""))
