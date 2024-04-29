# run html processing
Rscript -e 'rmarkdown::render("ComparingNLStools.Rmd", "rjtools::rjournal_web_article", output_options=list(), clean=FALSE)'
Rscript -e 'rmarkdown::render("ComparingNLStools.Rmd", "rjtools::rjournal_pdf_article", output_options=list(), clean=FALSE)'
