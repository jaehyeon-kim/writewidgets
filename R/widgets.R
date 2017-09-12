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

