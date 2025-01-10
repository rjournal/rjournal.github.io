library(testthat)
library(shinymgr)

# set shinyMgrPath
smp <- "file path to your shinymgr project"
fp <- list.files(paste0(smp, "/tests/testthat"))

test_files(fp)
