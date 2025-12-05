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
    if (is_missing(n)) {
      return(NA_character_)
    }
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
      tmp <- vapply(
        author_nodes,
        function(p) {
          given <- txt(p, ".//x:given_name")
          surname <- txt(p, ".//x:surname")
          paste(na.omit(c(given, surname)), collapse = " ")
        },
        FUN.VALUE = character(1)
      )
      paste(tmp, collapse = " and ")
    }

    year <- txt(a, ".//x:publication_date/x:year")
    first_page <- txt(a, ".//x:pages/x:first_page")
    last_page <- txt(a, ".//x:pages/x:last_page")
    pages <- if (!is.na(first_page) || !is.na(last_page)) {
      paste(na.omit(c(first_page, last_page)), collapse = "-")
    } else {
      NA_character_
    }
    doi_val <- txt(a, ".//x:doi_data/x:doi")
    if (is.na(doi_val)) {
      doi_val <- txt(a, ".//x:publisher_item/x:identifier[@id_type='doi']")
    }
    url <- txt(a, ".//x:doi_data/x:resource")
    key <- stringr::str_extract(url, "RJ-[0-9\\-]*")
    tibble::tibble(
      bibtexkey = key,
      title = paste0("{", title, "}"),
      AUTHOR = authors,
      month = switch(
        number,
        "1" = "mar",
        "2" = "jun",
        "3" = "sep",
        "4" = "dec",
        NA_character_
      ),
      year = year,
      pages = pages,
      doi = doi_val,
      url = url,
      journal = "{The R Journal}",
      volume = volume,
      number = number,
      category = "article"
    )
  })
}

rjbib <- bib2df::bib2df("RJournal.bib")
last_issue <- paste0(rjbib$YEAR, "-", rjbib$NUMBER) |>
  unique() |>
  max()

new_issues <- fs::dir_ls("_issues") |>
  stringr::str_remove("_issues/")
new_issues <- new_issues[new_issues > last_issue]

for (iss in new_issues) {
  df <- parse_doi_xml(paste0("_issues/", iss, "/doi.xml"))
  bib2df::df2bib(df, "RJournal.bib", append = TRUE)
}
