install.packages("remotes")
remotes::install_github("rjournal/rjtools")


library(rjtools)

# initialization

create_article(name = "drip-rjnl")

# on my ubuntu

#pandoc::pandoc_activate(version = '3.1.6')
#rmarkdown::render("/home/yicheng/Dropbox/Research/dripRpkg/rjR2/drip-rjnl.Rmd")

# no long need the above workarounds. Generate the PDF and HTML using the knit button directly.

initial_check_article(path = "/home/yicheng/Dropbox/Research/dripRpkg/rjR3/")

# on my mac

rmarkdown::render("/Users/kangy10/Library/CloudStorage/Dropbox/Research/dripRpkg/rjR2/drip-rjnl.Rmd", 
                  output_format = "pdf_document")
initial_check_article(path = "/Users/kangy10/Library/CloudStorage/Dropbox/Research/dripRpkg/rjR2/")
