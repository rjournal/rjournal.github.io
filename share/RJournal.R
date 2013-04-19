
library("tools")
library("utils")

### try to format author names correctly
text2author <- function(x) {
    dm <- delimMatch(x)
    ret <- substr(x, start = dm + 1, stop = dm + attr(dm, "match.length") - 2)
    ret <- gsub("by ", "", ret)
    ret <- gsub("~", " ", ret)
    gsub(",", " and ", ret)
}

### get content of LaTeX commands
tex2text <- function(x) {
    dm <- delimMatch(x)
    ret <- substr(x, start = dm + 1, stop = dm + attr(dm, "match.length") - 2)
}

### transform author names to BibTeX keys
author2key <- function(x) {

    a <- strsplit(x, "and")
    sapply(a, function(x) paste(sapply(x, function(x) {
        tmp <- strsplit(x, " ")[[1]]
        tmp[length(tmp)]
    }), collapse = "+"))
}

### read relevant information from toc file
read_toc <- function(issue) {

    tex <- paste(issue, "tex", sep = ".")
    stopifnot(file.exists(tex))

    texc <- readLines(tex)
    includes <- texc[grep("\\\\include\\{", texc)]
    texfiles <- paste(tex2text(includes), "tex", sep = ".")

    authors <- sapply(texfiles, function(x) {
        tmp <- readLines(x)
        text2author(tmp[grep("^\\\\author", tmp)])
    })

    pdf <- paste(issue, "pdf", sep = ".")
    if (!file.exists(pdf))
        texi2dvi(tex, pdf = TRUE, clean = FALSE)
    stopifnot(file.exists(pdf))

    toc <- paste(issue, "toc", sep = ".")
    stopifnot(file.exists(tex))

    tocc <- readLines(toc)
    chapters <- tocc[grep("\\{chapter\\}", tocc)]
    ss <- strsplit(chapters, "\\}\\{")
    
    title <- sapply(ss, function(x) x[2])
    startpage <- as.integer(sapply(ss, function(x) x[3]))

    data.frame(authors = authors, title = title, 
               startpage = startpage, stringsAsFactors = FALSE)
}

read_info <- function(issue) {

    tex <- paste(issue, "tex", sep = ".")
    stopifnot(file.exists(tex))
    texc <- readLines(tex)

    date <- tex2text(texc[grep("\\\\date\\{", texc)])
    list(volume = tex2text(texc[grep("\\\\volume\\{", texc)]),
         number = tex2text(texc[grep("\\\\volnumber\\{", texc)]),
         month = strsplit(date, " ")[[1]][1],
         year = strsplit(date, " ")[[1]][2])
}

toc2bib <- function(issue) {

    papers <- read_toc(issue)
    info <- read_info(issue)
    key <- paste("RJournal", author2key(papers$authors), info$year, sep = "_")
    pages <- paste(papers$startpage, c(papers$startpage[-1] - 1, max(papers$startpage)), sep = "--")

    x <- vector(mode = "character", length = nrow(papers))

    for (i in 1:length(x)) {
        if (papers$author[i] == "") next

        x[i] <- paste("@article{", key[i], ", \n",
                      "  author = {", papers$authors[i], "}", ", \n",
                      "  title  = {", papers$title[i], "}", ", \n",
                      "  journal = {R News}", ", \n",
                      "  year    = {", info$year, "}", ", \n",
                      "  volume  = {", info$volume, "}", ", \n",
                      "  number  = {", info$number, "}", ", \n",
                      "  pages   = {", pages[i], "}", ", \n",
                      "  month   = {", info$month, "} \n } \n", sep = "")
    }
    x
}

tex2pdf <- function(issue) {

    papers <- read_toc(issue)
    info <- read_info(issue)
    key <- paste("RJournal", author2key(papers$authors), info$year, sep = "_")
    spage <- papers$startpage
    epage <- c(papers$startpage[-1] - 1, max(papers$startpage))

    writeLines(c("\\documentclass[a4]{article}",
                 "%%\\usepackage{Sweave}",
                 "\\usepackage{pdfpages}",
                 "\\pagestyle{empty}",
                 "\\begin{document}",
                 "\\includepdf[pages=\\Sexpr{p1}-\\Sexpr{p2}]{\\Sexpr{pdffile}}",
                 "\\end{document}"), con = "extract.Rnw")

    pdffile <<- paste(issue, "pdf", sep = ".")
    for (i in 1:nrow(papers)) {
        if (papers$author[i] == "") next
        p1 <<- spage[i]
        p2 <<- epage[i]
        Sweave("extract.Rnw")
        texi2dvi("extract.tex", pdf = TRUE, clean = TRUE)
        file.copy("extract.pdf", paste(key[i], "pdf", sep = "."), 
                  overwrite = TRUE)
    }
    #file.remove("extract.Rnw")
    #file.remove("extract.tex")
}

### RJournal_test.tex must exist
### each single paper being included via
### \include{mypaper}

### produce RJournal.bib
writeLines(toc2bib("RJournal_test"), con = "RJournal.bib")

### produce single PDF files
tex2pdf("RJournal_test")
