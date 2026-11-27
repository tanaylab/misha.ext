#' writes a data frame with a header
#'
#' @param data data frame
#' @param file output file name
#' @param header string with the header
#' @param col_names write column names
#' @param append append to an existing file
#' @param ... additional parameters for fwrite
#'
#' @inheritDotParams tgutil::fwrite
#' @inheritParams data.table::fwrite
#'
#' @examples
#' \dontrun{
#' fwrite_header(mtcars, file = tempfile(), header = "# this is a header", sep = "\\t")
#' }
#'
#' @export
fwrite_header <- function(data, file, header, sep = ",", col_names = FALSE, append = FALSE, scipen = 20, ...) {
    deprecate_soft("fwrite_header", "write the header with `writeLines()` and append the rows with `tgutil::fwrite(append = TRUE)`")
    h1 <- paste(header, collapse = sep)
    if (col_names) {
        h2 <- paste(names(data), collapse = sep)
        write(paste(h1, h2, sep = "\n"), file, append = append)
    } else {
        write(h1, file, append = append)
    }

    fwrite(data, file, sep = sep, append = TRUE, col.names = FALSE, row.names = FALSE, scipen = scipen, ...)
}

#' @export
tgutil::psum

#' @export
tgutil::pmean

#' Transform an intervals set to matrix (deprecated)
#'
#' @description
#' Deprecated. Use [misha::gintervals.to_mat()] instead.
#'
#' @inheritParams misha::gintervals.to_mat
#' @param df intervals set
#' @param remove_intervalID kept for backward compatibility; ignored. The
#'   new function handles intervalID via the matrix's `"intervals"`
#'   attribute, not via column dropping.
#'
#' @return see [misha::gintervals.to_mat()]
#'
#' @export
intervs_to_mat <- function(df, remove_intervalID = TRUE) {
    deprecate_soft("intervs_to_mat", "use `misha::gintervals.to_mat()`")
    misha::gintervals.to_mat(df)
}


#' Transform a matrix to intervals set (deprecated)
#'
#' @description
#' Deprecated. Use [misha::gintervals.from_mat()] instead.
#'
#' @inheritParams misha::gintervals.from_mat
#'
#' @return see [misha::gintervals.from_mat()]
#'
#' @export
mat_to_intervs <- function(mat) {
    deprecate_soft("mat_to_intervs", "use `misha::gintervals.from_mat()`")
    misha::gintervals.from_mat(mat)
}

# misha.ext is retired: every export warns once per session with what to use
# instead. Calls from a package, misha.ext itself included, stay quiet: the
# user cannot fix those.
# ponytail: rlang is already a dependency - no need for lifecycle.
deprecate_soft <- function(old, instead) {
    if (isNamespace(topenv(parent.frame(2)))) {
        return(invisible(NULL))
    }
    rlang::warn(
        sprintf("misha.ext is retired. Instead of `%s()`, %s.", old, instead),
        class = "lifecycle_warning_deprecated",
        .frequency = "once",
        .frequency_id = paste0("misha.ext::", old)
    )
}
