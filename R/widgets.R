write_widget <- function(widget, from = '/usr/local/lib/R/site-library/', to = NULL, background = 'white', to_save = FALSE, to_raw = TRUE) {
    html <- htmlwidgets:::toHTML(widget, standalone = TRUE, knitrOptions = list())

    rendered <- htmltools::renderTags(html)
    deps <- rendered$dependencies
    deps_rendered <- htmltools::renderDependencies(dependencies = deps, srcType = c('hred', 'file'))
    deps_updated <- if (!is.null(to)) {
        gsub(from, to, deps_rendered)
    } else {
        deps_rendered
    }

    html <- c(
        "<!DOCTYPE html>",
        "<html>",
            "<head>",
                "<meta charset='utf-8'/>",
                deps_updated,
                rendered$head,
            "</head>",
            sprintf("<body style='background-color:%s;'>", htmltools::htmlEscape(background)),
                rendered$html,
            "</body>",
        "</html>"
    )

    if (to_save) {
        file <- tempfile()
        writeLines(html, file, useBytes = TRUE)
        return(file)
    } else {
        if (to_raw) {
            return(charToRaw(paste(html, collapse = '')))
        } else {
            return(paste(html, collapse = ''))
        }
    }
}
