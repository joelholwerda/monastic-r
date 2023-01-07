
#' Title
#'
#' @param stats The statistical output that will formatted.
#' @param labels The column used to label data frame rows (or a character vector
#' with length equal to the number of rows).
#' @param ... Additional arguments passed on to `tidy()` and `glance()` from the
#' `broom` package.
#'
#' @return A list containing the formatted statistical output.
#' @export
#'
#' @examples
format_stats <- function(stats, labels = NULL, ...) {

    # Check whether `stats` is supported by the insight and broom packages
    stats_class <- base::class(stats)
    stats_methods <- base::c("model_info", "find_formula", "glance", "tidy") |>
        base::sapply(has_method, stats_class)

    # Retrieve model info and tidy statistical output
    formatted_stats <- base::list()

    if (stats_methods[["model_info"]]) {
        formatted_stats$info$model_info <- insight::model_info(stats)
    }

    if (stats_methods[["find_formula"]]) {
        formatted_stats$info$model_formula <- insight::find_formula(stats) |>
            base::format()
    }

    if (stats_methods[["glance"]]) {
        formatted_stats$model_summary <- broom::glance(stats, ...) |>
            base::as.list()
    }

    if (stats_methods[["tidy"]]) {
        formatted_stats$parameter_summary <- broom::tidy(
            stats, conf.int = TRUE, ...
        ) |>
            format_df("term")
    }

    formatted_stats$info$time <- Sys.time() |> format("%d %B %Y %H:%M:%S")

    return(formatted_stats)
}

has_method <- function(fn, class){
    available_methods <- utils::methods(fn)
    class_has_method <- base::paste0(class, "$") |>
        base::grepl(available_methods) |>
        base::any()

    return(class_has_method)
}

format_df <- function(df, labels) {

    if (
        !base::length(labels) == base::nrow(df) &
        !(base::length(labels) == 1 & base::any(labels %in% base::names(df)))
    ) {

        base::stop("Invalid labels.")
    }

    # If column name provided for `labels`, replace with column contents
    if (base::length(labels) == 1) {
        col_name <- labels
        labels <- df[[col_name]]
        df <- df[-base::which(base::names(df) == col_name)]
    }

    list <- base::asplit(df, 1) |> base::lapply(as.list)
    base::names(list) <- labels

    return(list)

}
