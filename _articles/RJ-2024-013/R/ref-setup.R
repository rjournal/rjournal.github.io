# rjournal
ctv <- function(view) glue("\\ctv{[view]}", .open = "[", .close = "]")
CRANpkg <- function(pkg) glue("\\CRANpkg{[pkg]}", .open = "[", .close = "]")
ref <- function(x) {
  case_when(knitr::is_html_output() ~ glue("\\@ref({x})"),
            TRUE ~ glue("\\ref{[x]}", .open = "[", .close = "]"))
}
secref <- function(x, name) {
  case_when(knitr::is_html_output() ~ glue("[\"{name}\"](#{x})"),
            TRUE ~ glue("\\hyperref[(x)]{(name)}", .open = "(", .close = ")"))
}

url <- function(x) {
  ifelse(knitr::is_html_output(), 
         glue("[{x}]({x})"), 
         glue("\\url{[x]}", .open = "[", .close = "]"))
}
