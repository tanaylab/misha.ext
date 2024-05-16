#' Write IGV file from intervals set data frame
#'
#' @param data data frame with chrom,start,end and additional data columns
#' @param file name of the output IGV file
#' @param type data type for IGV viewer. Available options are: COPY_NUMBER, GENE_EXPRESSION, CHIP, DNA_METHYLATION, ALLELE_SPECIFIC_COPY_NUMBER, LOH, RNAI. Note that if `type` is set other viewer options (in `...`) would be ignored.
#' @param rm_intervalID remove intervalID column from `data`
#' @param feature_name feature name for IGV viewer
#' @param ... track parameters for IGV viewer. See: http://software.broadinstitute.org/software/igv/TrackLine.
#'
#' @examples
#' \dontrun{
#' fwrite_igv(d, "out.igv", graphType = "points")
#' fwrite_igv(d, "out.igv", type = "DNA_METHYLATION")
#' }
#'
#' @export
fwrite_igv <- function(data, file, type = NULL, rm_intervalID = TRUE, feature_name = 1, ...) {
    if (!is.null(type)) {
        header <- glue("#type={type}")
    } else {
        igv_options <- list(...)
        header <- paste0("#track", paste(purrr::imap_chr(igv_options, ~ glue("{.y}={.x}")), collapse = " "))
    }

    if (rm_intervalID) {
        data <- data %>% select(-intervalID)
    }

    data <- data %>%
        mutate(chrom = as.numeric(gsub("chr", "", chrom))) %>%
        mutate(Feature = feature_name) %>%
        select(chrom:end, Feature, everything()) %>%
        mutate(start = start + 1, end = end + 1) %>%
        arrange(chrom, start)

    fwrite_header(data, file = file, header = header, sep = "\t")
}
