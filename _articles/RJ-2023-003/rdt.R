# Script to install end-to-end provenance tools and run examples in 
# the R Journal paper, Making Provenance Work for You

# Note the following libraries must be installed:
# openssl:  apt install libssl-dev
# curl:  apt install libcurl4-openssl-dev
# xml: apt install libxml2-dev
# java: apt install default-jre

cat("Please be sure the following are installed:  libssl-dev, ")
cat("libcurl4-openssl-dev, libxml2-dev, default-jre.")
cat("If missing, they can be installed with apt install")

initial.packages <- loadedNamespaces()
unloadLibraries <- function(lib) {
  loaded <- loadedNamespaces()
  newly.loaded <- setdiff(loaded, initial.packages)
  utils::capture.output(utils::capture.output(lapply(newly.loaded, 
         function (ns) {
           tryCatch (unloadNamespace(ns),
                     error=function(e) {})
         }), type="message"), type="output")
  invisible()
}
#install.packages( pkgs = c("rdtLite", "provSummarizeR", "provDebugR", "provViz", "provExplainR"),
#                  repos = "https://cran.rstudio.com/")

# Listing 2
cat ("Listing 2 output:\n")
library(rdtLite)
prov.run("examples/mtcars_example.R")
prov.summarize()

# A first example
cat ("\nA First Example:\n")
library(provDebugR)
prov.debug()
debug.lineage("cars4Cyl.df", forward = TRUE)
debug.lineage("cars4Cyl.df")

# Collecting provenance with rdtLite
unloadLibraries()
cat ("\nCollecting provenance with rdtLite")
library(rdtLite)
prov.run ("examples/script.R")

unloadLibraries()
library(rdtLite)

# The following example shown in the paper should be run
# interactively, not from within a file.
#prov.init ()
#data <- read.csv ("examples/mydata.csv")
#data$z <- data$x + data$y
#prov.quit ()

# 
cat ("\nprovViz")
library(provViz)
cat ("provViz only works from an interactive environment, not from scripts\n")
cat ("To run it, enter the following command in an interactive environment:\n")
cat ('provViz::prov.visualize.run ("examples/car.R", prov.dir="examples")\n')

# Note to generate the output shown in Figure 2, you must use a command in the 
# interative visualizer.  
# Right-click on the node labeled "3-cars4Cyl.df".
# Select "Show what is computed using this value"

# provDebugR
# tryCatch is used so that the expected error in debugScript does not
# cause this script to fail.
unloadLibraries()
library(rdtLite)
library(provDebugR)
cat ("\nprovDebugR\n")
cat("The output here is slightly different than in the paper.")
cat("In the paper, an error is produced, but that would cause")
cat("this example script to fail, so we catch the error and")
cat("print it instead")
tryCatch (prov.debug.run("examples/debugScript.R"), error = function (e) {print(e)} )
debug.warning()
debug.warning(1)

# Listing 3.  Note that the output that is retrieved from StackOverflow might 
# differ from what the listing shows since what is available on StackOverflow
# might have changed.  We are running with StackOverflow FALSE here because
# it requires standard input to select an entry, which cannot be done from 
# the script.
cat ("\nListing 3:  Note that we are running with StackOverflow FALSE here because")
cat ("\nrunning with StackOverflow TRUE requires standard input\n")
debug.error()

unloadLibraries()
library(rdtLite)
library(provDebugR)
tryCatch (
  prov.debug.run("examples/debugScript4.R"),
  error = function (e) {print (e)}
)
debug.variable(x, showType = TRUE)
debug.lineage(x)
debug.type.changes()
debug.line(4)
debug.state(4)
debug.lineage(x, forward = TRUE)

# provExplainR
cat ("\nprovExplainR\n")
cat("Listing 4:\n")
library(provExplainR)
prov.explain (dir1 = "examples/prov_factorial_2021-03-31T12.01.36EDT", 
              dir2 = "examples/prov_factorial_2021-04-26T16.34.16EDT")

cat ("\nFigure 4:\n")
prov.diff.script (
  dir1 = "examples/prov_factorial_2021-03-31T12.01.36EDT", 
  dir2 = "examples/prov_factorial_2021-04-26T16.34.16EDT")

