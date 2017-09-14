library(htmlwidgets)
library(DT)
library(highcharter)

libs <- file.path(getwd(), 'lib')
selfcontained <- FALSE

get_plot <- function(selfcontained = selfcontained, libdir = libs) {
    p <- hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class))
    p$sizingPolicy$browser$padding <- 15
    p$sizingPolicy$browser$fill <- TRUE
    htmlwidgets::saveWidget(p, "plot.html", selfcontained, libdir)
}


get_dt <- function(selfcontained = selfcontained, libdir = libs) {
    ##dt <- datatable(iris, rownames = FALSE)
    dt <- datatable(iris, extensions = 'Responsive', class = 'display compact', rownames = FALSE, options = list(
        dom = 'lftip',
        scrollY = 300,
        scroller = TRUE,
        pageLength = 10,
        lengthMenu = c(10, 15, 20)
    ))
    dt$sizingPolicy$browser$padding <- 10
    dt$sizingPolicy$browser$fill <- TRUE
    htmlwidgets::saveWidget(dt, "table.html", selfcontained, libdir)
}
c("Depends", "Imports", "LinkingTo")


> saveWidget
function (widget, file, selfcontained = TRUE, libdir = NULL,
          background = "white", knitrOptions = list())
{
    html <- toHTML(widget, standalone = TRUE, knitrOptions = knitrOptions)
    if (is.null(libdir)) {
        libdir <- paste(tools::file_path_sans_ext(basename(file)),
                        "_files", sep = "")
    }
    htmltools::save_html(html, file = file, libdir = libdir,
                         background = background)
    if (selfcontained) {
        if (!pandoc_available()) {
            stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\\n",
                 "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
        }
        pandoc_self_contained_html(file, file)
        unlink(libdir, recursive = TRUE)
    }
    invisible(NULL)
}
<environment: namespace:htmlwidgets>


htmltools::save_html
function (html, file, background = "white", libdir = "lib")
{
    dir <- dirname(file)
    oldwd <- setwd(dir)
    on.exit(setwd(oldwd), add = TRUE)
    rendered <- renderTags(html)
    deps <- lapply(rendered$dependencies, function(dep) {
        dep <- copyDependencyToDir(dep, libdir, FALSE)
        dep <- makeDependencyRelative(dep, dir, FALSE)
        dep
    })
    html <- c("<!DOCTYPE html>", "<html>", "<head>", "<meta charset=\\"utf-8\\"/>",
        renderDependencies(deps, c("href", "file")), rendered$head,
        "</head>", sprintf("<body style=\\"background-color:%s;\\">",
            htmlEscape(background)), rendered$html, "</body>",
        "</html>")
    writeLines(html, file, useBytes = TRUE)
}
<bytecode: 0xb9dce58>
<environment: namespace:htmltools>

