# Generated by `rjournal_article()` using `knitr::purl()`: do not edit by hand
# Please edit 2018-2.Rmd to modify this file

## d-article > * {

##     grid-column: page;

## }


## ---- results = "asis", echo = FALSE, layout = "l-page"-----------------------
articles <- yaml::read_yaml("2018-2.yml")$articles
contents <- lapply(articles, function(x) {
  if(!is.null(x$heading)) {
    return(paste("\n\n###", x$heading))
  }
  stringr::str_glue(
"[{x$title}]({if(grepl('RJ-\\\\d{4}-\\\\d{2,}', x$slug)) '../../articles/' else '../../news/RJ-2018-2-'}{x$slug})<br>{glue::glue_collapse(x$author, sep = ', ', last = ' and ')} {x$pages[1]}\n\n")
})

cat(do.call(c, contents), sep = "\n")

