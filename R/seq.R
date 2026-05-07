#' Defunct: use `gtrack.create_kmer` instead
#'
#' `gseq.create_track` has been removed in favour of
#' [gtrack.create_kmer()], which is faster (whole-genome scans in seconds),
#' produces a dense track, and supports multi-kmer summation
#' (e.g. `c("G", "C")` for GC content) plus an optional sliding window.
#'
#' Migration:
#' \preformatted{
#' # Old:
#' gseq.create_track("CG", "seq.CG")
#' # New:
#' gtrack.create_kmer("seq.CG", "CG starts (fwd)",
#'                    kmer = "CG", iterator = 1, strand = 1)
#' }
#'
#' @param ... formerly accepted arguments; now ignored.
#'
#' @seealso [gtrack.create_kmer()]
#'
#' @export
gseq.create_track <- function(...) {
    .Defunct(
        new = "gtrack.create_kmer",
        package = "misha.ext",
        msg = paste(
            "'gseq.create_track' is defunct.",
            "Use 'gtrack.create_kmer' instead.",
            "See ?gtrack.create_kmer for the new API.",
            sep = "\n"
        )
    )
}
