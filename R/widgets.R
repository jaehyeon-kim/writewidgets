#' Convert R bool value into Python bool value as a string
#'
#' \code{convert_bool} converts a R bool value (TRUE or FALSE) into a Python bool value (True or False). This function is not exported.
#'
#' @param bool R bool value
#' @return Python bool value as a string
#' @export
convert_bool <- function(bool) {
    if(bool) 'True' else 'False'
}


# library(htmlwidgets)
# library(DT)
# library(highcharter)
#
# libs <- file.path(getwd(), 'lib')
# selfcontained <- FALSE
#
# get_plot <- function(selfcontained = selfcontained, libdir = libs) {
#     p <- hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class))
#     p$sizingPolicy$browser$padding <- 15
#     p$sizingPolicy$browser$fill <- TRUE
#     htmlwidgets::saveWidget(p, "plot.html", selfcontained, libdir)
# }
#
#
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
# c("Depends", "Imports", "LinkingTo")

# > saveWidget
# function (widget, file, selfcontained = TRUE, libdir = NULL,
#           background = "white", knitrOptions = list())
# {
#     html <- toHTML(widget, standalone = TRUE, knitrOptions = knitrOptions)
#     if (is.null(libdir)) {
#         libdir <- paste(tools::file_path_sans_ext(basename(file)),
#                         "_files", sep = "")
#     }
#     htmltools::save_html(html, file = file, libdir = libdir,
#                          background = background)
#     if (selfcontained) {
#         if (!pandoc_available()) {
#             stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\\n",
#                  "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
#         }
#         pandoc_self_contained_html(file, file)
#         unlink(libdir, recursive = TRUE)
#     }
#     invisible(NULL)
# }
# <environment: namespace:htmlwidgets>

write_widget <- function() {

}

`%||%` <- function(x, y){
    if (is.null(x)) y else x
}

`%+%` <- function(x, y) {
    paste(c(x, y), collapse = '-')
}

get_default_sizes <- function() {
    list(
        DEFAULT_WIDTH = 960,
        DEFAULT_HEIGHT = 500,
        DEFAULT_PADDING = 40,
        DEFAULT_WIDTH_VIEWER = 450,
        DEFAULT_HEIGHT_VIEWER = 350,
        DEFAULT_PADDING_VIEWER = 15
    )
}

resolve_sizing <- function(x, sp) {
    default_sizes <- get_default_sizes()
    return(list(
        runtime = list(
            viewer = list(
                width = x$width %||% sp$viewer$defaultWidth %||% default_sizes$DEFAULT_WIDTH_VIEWER,
                height = x$height %||% sp$viewer$defaultHeight %||% default_sizes$DEFAULT_HEIGHT_VIEWER,
                padding = sp$viewer$padding %||% default_sizes$DEFAULT_PADDING_VIEWER,
                fill = sp$viewer$fill %||% TRUE
            ),
            browser = list(
                width = x$width %||% sp$browser$defaultWidth %||% default_sizes$DEFAULT_WIDTH,
                height = x$height %||% sp$browser$defaultHeight %||% default_sizes$DEFAULT_HEIGHT,
                padding = sp$browser$padding %||% default_sizes$DEFAULT_PADDING,
                fill = sp$browser$fill %||% FALSE
            )
        ),
        width = x$width %||% sp$defaultWidth %||% default_sizes$DEFAULT_WIDTH,
        height = x$height %||% sp$defaultHeight %||% default_sizes$DEFAULT_HEIGHT
    ))
}

create_widget_id <- function(bytes = 10) {

    as.integer(Sys.time()) %+% paste(format(as.hexmode(sample(256, bytes, replace = TRUE)-1), width=2), collapse = "")
}

to_html <- function(x) {
    size_info <- resolve_sizing(x, x$sizingPolicy)

    id <- x$elementId %||% 'htmlwidgets' %+% create_widget_id()

    id
}

# toHTML <- function(x, standalone = FALSE, knitrOptions = NULL) {
#
#     sizeInfo <- resolveSizing(x, x$sizingPolicy, standalone = standalone, knitrOptions = knitrOptions)
#
#     if (!is.null(x$elementId))
#         id <- x$elementId
#     else
#         id <- paste("htmlwidget", createWidgetId(), sep="-")
#
#     w <- validateCssUnit(sizeInfo$width)
#     h <- validateCssUnit(sizeInfo$height)
#
#     # create a style attribute for the width and height
#     style <- paste(
#         "width:", w, ";",
#         "height:", h, ";",
#         sep = "")
#
#     x$id <- id
#
#     container <- if (isTRUE(standalone)) {
#         function(x) {
#             div(id="htmlwidget_container", x)
#         }
#     } else {
#         identity
#     }
#
#     html <- htmltools::tagList(
#         container(
#             htmltools::tagList(
#                 x$prepend,
#                 widget_html(
#                     name = class(x)[1],
#                     package = attr(x, "package"),
#                     id = id,
#                     style = style,
#                     class = paste(class(x)[1], "html-widget"),
#                     width = sizeInfo$width,
#                     height = sizeInfo$height
#                 ),
#                 x$append
#             )
#         ),
#         widget_data(x, id),
#         if (!is.null(sizeInfo$runtime)) {
#             tags$script(type="application/htmlwidget-sizing", `data-for` = id,
#                         toJSON(sizeInfo$runtime)
#             )
#         }
#     )
#     html <- htmltools::attachDependencies(html,
#                                           c(widget_dependencies(class(x)[1], attr(x, 'package')),
#                                             x$dependencies)
#     )
#
#     htmltools::browsable(html)
#
# }

