#' Create sequence tracks for misha database
#'
#' Create tracks based on the presence of specific sequences.
#'
#'
#' @param s sequence to look for. can be a regex.
#' @param track name of the track to create
#' @param strand strand of the sequences
#' @param intervals intervals set
#'
#' @examples
#' \dontrun{
#' gseq.create_track("A", "seq.A")
#' gseq.create_track("C", "seq.C")
#' gseq.create_track("T", "seq.T")
#' gseq.create_track("G", "seq.G")
#'
#' gseq.create_track("CG", "seq.CG") # genome CpGs
#' gseq.create_track("[GC]", "seq.G_or_C") # C or G (for GC content calculations)
#'
#' # create all dinucleotide tracks
#' dinucs <- expand.grid(c("T", "C", "G", "A"), c("T", "C", "G", "A"))
#' dinucs <- apply(dinucs, 1, paste, collapse = "")
#' for (dinuc in dinucs) {
#'     message(dinuc)
#'     gseq.create_track(dinuc, paste0("seq.", dinuc))
#' }
#' }
#' @export
gseq.create_track <- function(s, track, strand = 1, intervals = gintervals.all()) {
    if (is.null(getOption("gmax.data.size"))) {
        options(gmax.data.size = 1000000000)
    }

    intervs <- giterator.intervals(iterator = getOption("gmax.data.size") - 1, intervals = intervals)

    pb <- progress::progress_bar$new(total = nrow(intervs), format = "[:bar] :current/:total (:percent)")

    message("Extracting sequences")
    seq_df <- purrr::map_dfr(1:nrow(intervs), ~ {
        interv <- intervs[.x, ] %>% mutate(strand = strand)
        seq <- toupper(gseq.extract(interv))
        pb$tick(1)
        stringr::str_locate_all(seq, s)[[1]] %>%
            as_tibble() %>%
            mutate(
                chrom = interv$chrom,
                start = start + interv$start - 1,
                end = start + 1
            ) %>%
            select(chrom, start, end) %>%
            mutate(value = 1)
    })

    message("Creating track")
    gtrack.create_sparse(track, glue("contains 1 for when sequence (on strand {strand}) starts with {s}"), intervals = seq_df[, 1:3], values = seq_df$value)
}
