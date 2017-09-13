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
