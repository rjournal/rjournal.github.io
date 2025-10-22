# ...existing code...
library(xml2)
library(dplyr)
library(bib2df)

parse_doi_xml <- function(xml_file) {
  doc <- read_xml(xml_file)
  ns <- c(x = "http://www.crossref.org/schema/4.8.0")
  
  is_missing <- function(node) inherits(node, "xml_missing")
  txt <- function(node, path) {
    n <- xml_find_first(node, path, ns)
    if (is_missing(n)) return(NA_character_)
    t <- xml_text(n)
    if (identical(t, "")) NA_character_ else t
  }
  issue <- xml_find_first(doc, ".//x:journal_issue", ns)
  volume <- txt(issue, ".//x:volume")
  number <- txt(issue, ".//x:issue")
  
  articles <- xml_find_all(doc, ".//x:journal_article", ns)
  rows <- purrr::map_dfr(articles, function(a) {
    title <- txt(a, ".//x:titles/x:title")
    
    author_nodes <- xml_find_all(a, ".//x:person_name", ns)
    authors <- if (length(author_nodes) == 0) {
      NA_character_
    } else {
      tmp <- vapply(author_nodes, function(p) {
        given <- txt(p, ".//x:given_name")
        surname <- txt(p, ".//x:surname")
        paste(na.omit(c(given, surname)), collapse = " ")
      }, FUN.VALUE = character(1))
      paste(tmp, collapse = " and ")
    }
    
    year <- txt(a, ".//x:publication_date/x:year")
    month <- txt(a, ".//x:publication_date/x:month")
    day <- txt(a, ".//x:publication_date/x:day")
    pub_date <- if (!is.na(year)) paste(na.omit(c(year, month, day)), collapse = "-") else NA_character_
    
    first_page <- txt(a, ".//x:pages/x:first_page")
    last_page  <- txt(a, ".//x:pages/x:last_page")
    pages <- if (!is.na(first_page) || !is.na(last_page)) paste(na.omit(c(first_page, last_page)), collapse = "-") else NA_character_
    
    doi_val <- txt(a, ".//x:doi_data/x:doi")
    if (is.na(doi_val)) doi_val <- txt(a, ".//x:publisher_item/x:identifier[@id_type='doi']")
    resource <- txt(a, ".//x:doi_data/x:resource")
    
    tibble::tibble(
      bibtexkey = stringr::str_extract(resource, "RJ-[0-9\\-]*"),
      title = paste0("{",title,"}"),
      AUTHOR = authors,
      date = pub_date,
      year = format(as.Date(date), "%Y"),
      pages = pages,
      doi = doi_val,
      url = resource,
      journal = "{The R Journal}",
      volume = volume,
      number = number,
      category = "article"
    )
  })
}

issues <- c(paste(2022, 1:4, sep="-"), 
            paste(2023, 1:4, sep="-"), 
            paste(2024, 1:4, sep="-"),
            paste(2025, 1:2, sep="-"))
for (iss in issues[-1]) {
  df <- parse_doi_xml(paste0("_issues/",iss,"/doi.xml"))
  bib2df::df2bib(df, "RJournal.bib", append = TRUE)
}
