#' writes a data frame with a header
#' 
#' @param data data frame
#' @param file output file name
#' @param header string with the header
#' @param ... additional parameters for fwrite 
#' 
#' @inheritDotParams tgutil::fwrite
#' 
#' @examples
#' \dontrun{
#' fwrite_header(mtcars, file=tempfile(), header="# this is a header", sep="\t")
#' }
#' 
#' @export
fwrite_header <- function(data, file, header, sep=",", ...) {
  h1 <- paste(header, collapse = sep)
  h2 <- paste(names(data), collapse = sep)
  writeLines(paste(h1, h2, sep = "\n"), file)
  fwrite(data, file, sep = sep, append = TRUE, col.names = FALSE, row.names = FALSE, ...)
}