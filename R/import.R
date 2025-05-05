#' Wrapper around gtrack.import_mappedseq for (multiple) bam files
#'
#' @param bam_files vector of bam (or sam) files to import
#' @param min_mapq skip alignments with MAPQ smaller than \code{min_mapq}
#' @param ... parameters of gtrack.import_mappedseq
#'
#' @return None
#'
#' @export
#' @inheritDotParams misha::gtrack.import_mappedseq
#' @inheritParams misha::gtrack.import_mappedseq
#' @seealso \link[misha]{gtrack.import_mappedseq}
gtrack.import_mappedseq_bam <- function(bam_files, track, min_mapq = NULL, ...) {
    cat_cmd <- "cat"
    if (length(bam_files) > 1) {
        cat_cmd <- "samtools cat"
    }
    view_cmd <- "samtools view -h "
    if (!is.null(min_mapq)) {
        if (!is.numeric(min_mapq)) {
            stop("min_mapq should be an integer")
        }
        view_cmd <- glue("samtools view -h -q {min_mapq}")
    }

    files <- paste(bam_files, collapse = " ")
    tmp_fifo <- tempfile()
    tryCatch(
        {
            system(glue("mkfifo {tmp_fifo}; {cat_cmd} {files} | {view_cmd} > {tmp_fifo}"), wait = FALSE)
            gtrack.import_mappedseq(file = tmp_fifo, cols.order = NULL, track = track, ...)
        },
        finally = system(glue("rm -f {tmp_fifo}"))
    )
}
