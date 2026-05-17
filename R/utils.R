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
    .Deprecated("misha::gintervals.to_mat")
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
    .Deprecated("misha::gintervals.from_mat")
    misha::gintervals.from_mat(mat)
}
