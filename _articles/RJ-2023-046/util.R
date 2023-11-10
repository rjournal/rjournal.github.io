

isHTML <- knitr::is_html_output()


caption <- function(main, pdfExtra, htmlExtra="") {
    if (isHTML) {
        paste0(
            main,
            htmlExtra)
    } else {
        paste0(
            main, 
            " An interactive version of this figure is available in the supplemental file \\texttt{figures-page.html}.",
            pdfExtra)
    }
}


go <- function(label, widget, state, alt="") {
    if (!isHTML) 
        return(knitr::asis_output(alt))

    command <- paste0(
        "document.getElementById(\"",widget,"\")",
        ".langevitour.setState(",state,");",
        "document.getElementById(\"",widget,"\")",
        ".scrollIntoView({behavior:\"smooth\"})")
    command <- htmltools::htmlEscape(command, attribute=TRUE)

    knitr::asis_output(paste0(
        "<button onclick=\"",command,"\">",
        label,
        "</button>"))
}


goInline <- function(label, widget, state) {
    go(label,widget,state,label)
}

