#' Write statistical output
#'
#' @param list A list of statistical output generated using `add_stats()`.
#' @param file A character string naming a file to write the statistical output.
#'
#' @export
#'
#' @examples
write_stats <- function(list, file) {

    if (!is.list(list)) {
        stop("The 'list' argument must be a list.")
    }

    jsonlite::write_json(list, path = file, pretty = TRUE, auto_unbox = TRUE,
        digits = NA)

}
