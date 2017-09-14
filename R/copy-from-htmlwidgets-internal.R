DEFAULT_WIDTH <- 960
DEFAULT_HEIGHT <- 500
DEFAULT_PADDING <- 40
DEFAULT_WIDTH_VIEWER <- 450
DEFAULT_HEIGHT_VIEWER <- 350
DEFAULT_PADDING_VIEWER <- 15

#### utils.R
`%||%` <- function(x, y){
    if (is.null(x)) y else x
}

`%-%` <- function(x, y) {
    paste(c(x, y), collapse = '-')
}

#htmlwidgets:::toHTML(dt)

# Copied from shiny 0.14.2
toJSON2 <- function(x, ...,  dataframe = "columns", null = "null", na = "null", auto_unbox = TRUE,
                    digits = getOption("shiny.json.digits", 16), use_signif = TRUE, force = TRUE,
                    POSIXt = "ISO8601", UTC = TRUE, rownames = FALSE, keep_vec_names = TRUE, strict_atomic = TRUE) {
    if (strict_atomic) x <- I(x)
    jsonlite::toJSON(
        x, dataframe = dataframe, null = null, na = na, auto_unbox = auto_unbox,
        digits = digits, use_signif = use_signif, force = force, POSIXt = POSIXt,
        UTC = UTC, rownames = rownames, keep_vec_names = keep_vec_names,
        json_verbatim = TRUE, ...
    )
}

if (requireNamespace('shiny') && packageVersion('shiny') >= '0.12.0') local({
    tryCatch({
        toJSON <- getFromNamespace('toJSON', 'shiny')
        args2 <- formals(toJSON2)
        args1 <- formals(toJSON)
        if (!identical(args1, args2)) {
            warning('Check shiny:::toJSON and make sure htmlwidgets:::toJSON is in sync')
        }
    })
})

toJSON <- function(x) {
    if (!is.list(x) || !('x' %in% names(x))) return(toJSON2(x))
    func <- attr(x$x, 'TOJSON_FUNC', exact = TRUE)
    args <- attr(x$x, 'TOJSON_ARGS', exact = TRUE)
    if (length(args) == 0) args <- getOption('htmlwidgets.TOJSON_ARGS')
    if (!is.function(func)) func <- toJSON2
    res <- if (length(args) == 0) func(x) else do.call(func, c(list(x = x), args))
    # make sure shiny:::toJSON() does not encode it again
    structure(res, class = 'json')
}

JSEvals <- function(list) {
    # the `%||% list()` part is necessary as of R 3.4.0 (April 2017) -- if `evals`
    # is NULL then `I(evals)` results in a warning in R 3.4.0. This is circumvented
    # if we let `evals` be equal to `list()` in those cases
    evals <- names(which(unlist(shouldEval(list)))) %||% list()
    I(evals)  # need I() to prevent toJSON() from converting it to scalar
}

shouldEval <- function(options) {
    if (is.list(options)) {
        if ((n <- length(options)) == 0) return(FALSE)
        # use numeric indices as names (remember JS indexes from 0, hence -1 here)
        if (is.null(names(options)))
            names(options) <- seq_len(n) - 1L
        # Escape '\' and '.' by prefixing them with '\'. This allows us to tell the
        # difference between periods as separators and periods that are part of the
        # name itself.
        names(options) <- gsub("([\\.])", "\\\\\\1", names(options))
        nms <- names(options)
        if (length(nms) != n || any(nms == ''))
            stop("'options' must be a fully named list, or have no names (NULL)")
        lapply(options, shouldEval)
    } else {
        is.character(options) && inherits(options, 'JS_EVAL')
    }
}

#### htmlwidgets.R
widget_html <- function(name, package, id, style, class, inline = FALSE, ...){

    # attempt to lookup custom html function for widget
    fn <- tryCatch(get(paste0(name, "_html"),
                       asNamespace(package),
                       inherits = FALSE),
                   error = function(e) NULL)

    # call the custom function if we have one, otherwise create a div
    if (is.function(fn)) {
        fn(id = id, style = style, class = class, ...)
    } else if (inline) {
        tags$span(id = id, style = style, class = class)
    } else {
        tags$div(id = id, style = style, class = class)
    }
}

widget_dependencies <- function(name, package){
    getDependency(name, package)
}

# Generates a <script type="application/json"> tag with the JSON-encoded data,
# to be picked up by htmlwidgets.js for static rendering.
widget_data <- function(x, id, ...){
    # It's illegal for </script> to appear inside of a script tag, even if it's
    # inside a quoted string. Fortunately we know that in JSON, the only place
    # the '<' character can appear is inside a quoted string, where a Unicode
    # escape has the same effect, without confusing the browser's parser. The
    # repro for the bug this gsub fixes is to have the string "</script>" appear
    # anywhere in the data/metadata of a widget--you will get a syntax error
    # instead of a properly rendered widget.
    #
    # Another issue is that if </body></html> appears inside a quoted string,
    # then when pandoc coverts it with --self-contained, the escaping gets messed
    # up. There may be other patterns that trigger this behavior, so to be safe
    # we can replace all instances of "</" with "\\u003c/".
    payload <- toJSON(createPayload(x))
    payload <- gsub("</", "\\u003c/", payload, fixed = TRUE)
    tags$script(type = "application/json", `data-for` = id, HTML(payload))
}

# Helper function to create payload
createPayload <- function(instance){
    if (!is.null(instance$preRenderHook)){
        instance <- instance$preRenderHook(instance)
        instance$preRenderHook <- NULL
    }
    x <- .subset2(instance, "x")
    list(x = x, evals = JSEvals(x), jsHooks = instance$jsHooks)
}
