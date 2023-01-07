#' Add statistical output
#'
#' @param list A list to which the statistical output will be added.
#' @param stats The statistical output that will be added to the list.
#' @param title A short string describing the statistical output.
#' @param fn An optional function used to format the statistical output.
#' @param labels The column used to label data frame rows (or a character vector
#' with length equal to the number of rows).
#' @param notes A string containing additional information.
#' @param ... Additional arguments passed on to `fn()` (if specified) or
#' `tidy()` and `glance()` from the `broom` package.
#'
#' @return A list containing the statistical output appended to the existing object.
#' @export
#'
#' @examples
#'
add_stats <- function(list, stats, title = NULL, fn = NULL, labels = NULL, notes = NULL, ...) {

    if (!is.list(list)) {
        stop("The 'list' argument must be a list.")
    }

    # Use the name of the R object passed to `stats` as a unique id
    id <- deparse(substitute(stats))

    # Use the function passed to `fn` or format_stats() to format `stats`
    if (!is.null(fn)) {
        formatted_stats <- fn(stats, ...)
    } else {
        formatted_stats <- format_stats(stats, labels = labels, ...)
    }

    if (!is.list(formatted_stats)) {
        stop("The formatted statistical output is not valid.")
    }

    # Add title and notes
    if (!missing(title)) formatted_stats$info$title <- title
    if (!missing(notes)) formatted_stats$info$notes <- notes

    # Add to existing list
    list[[id]] <- formatted_stats

    return(list)

}
