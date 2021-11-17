#' Wrapper around gtrack.import_mappedseq for (multiple) bam files
#'
#' @param bam_files vector of bam (or sam) files to import
#' @param ... parameters of gtrack.import_mappedseq
#'
#' @return
#' @export
#' @seealso \link[misha]{gtrack.import_mappedseq}
gtrack.import_mappedseq_bam <- function(bam_files, ...){
    cat_cmd <- 'cat'
    if (length(bam_files) > 1){
        cat_cmd <- 'samtools cat'
    }
    files <- paste(bam_files, collapse=' ')
    tmp_fifo <- tempfile()
    tryCatch({
        system(glue('mkfifo {tmp_fifo}; {cat_cmd} {files} | samtools view -h > {tmp_fifo}'), wait=FALSE)
        gtrack.import_mappedseq(file=tmp_fifo, cols.order=NULL, ...)
        }, finally=system(glue('rm -f {tmp_fifo}')))
}

