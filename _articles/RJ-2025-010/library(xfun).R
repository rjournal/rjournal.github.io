library(xfun)
rmd_files <- list.files(
  path = "_articles/", 
  pattern = "RJ-20\\d{2}-[^/]+\\.Rmd$", 
  recursive = TRUE, 
  full.names = TRUE
)

rmd <- rmd_files[1]
html <- read_utf8(with_ext(rmd, "html"))
if (any(grepl("rjournal.github.io", html, fixed = TRUE))) {
  install.packages(setdiff(renv::dependencies(rmd)$Package, installed.packages()))
  rmarkdown::render(rmd, output_format = "all")
}
