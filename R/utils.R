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

#' Transform an intervals set to matrix
#'
#' @param df intervals set
#' @param remove_intervalID whether to remove the "intervalID" column generated by a gextract call
#'
#' @return matrix with coordinate rownames (chrom_start_end). if \code{remove_intervalID == FALSE}, also with \code{intervalID} column.
#'
#' @export
intervs_to_mat <- function(df, remove_intervalID = TRUE) {
    mat <- df %>%
        tidyr::unite("coord", chrom:end) %>%
        as.data.frame() %>%
        tibble::remove_rownames() %>%
        tibble::column_to_rownames("coord") %>%
        as.matrix()
    if (remove_intervalID && ("intervalID" %in% colnames(mat))) {
        mat <- mat[, -grep("intervalID", colnames(mat))]
    }
    return(mat)
}


#' Transform a matrix with coordinate rownames to intervals set
#'
#' @param mat matrix with coordinate rownames (chrom_start_end).
#'
#' @return intervals set
#'
#' @export
mat_to_intervs <- function(mat) {
    df <- mat %>%
        as.data.frame() %>%
        tibble::rownames_to_column("coord") %>%
        tidyr::separate(coord, c("chrom", "start", "end"), sep = "_") %>%
        mutate(start = as.numeric(start), end = as.numeric(end))
    return(df)
}
