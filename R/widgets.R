update_dep_path <- function(dep, libdir = 'lib') {
    dir <- dep$src$file
    if (!is.null(dep$package))
        dir <- system.file(dir, package = dep$package)

    if (length(libdir) != 1 || libdir %in% c("", "/"))
        stop("libdir must be of length 1 and cannot be '' or '/'")

    target <- if (getOption('htmltools.dir.version', TRUE)) {
        paste(dep$name, dep$version, sep = '-')
    } else {
        dep$name
    }
    dep$src$file <- file.path(libdir, target)
    dep
}

write_widget <- function(widget, libdir = 'lib', cdn = NULL, background = 'white', to_save = FALSE, to_raw = FALSE) {
    html <- htmlwidgets:::toHTML(widget, standalone = TRUE, knitrOptions = list())

    rendered <- htmltools::renderTags(html)
    deps <- rendered$dependencies
    deps_up <- lapply(deps, update_dep_path, libdir = libdir)
    deps_rendered <- htmltools::renderDependencies(dependencies = deps_up, srcType = c('hred', 'file'))
    deps_updated <- if (!is.null(cdn)) {
        gsub(libdir, cdn, deps_rendered)
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
