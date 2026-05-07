#' Create a dense track that is a sum of misha tracks
#'
#' @description The function creates a dense track that is a sum of misha tracks, by first creating a virtual track for each track with a "sum" function, and then running \code{gtrack.create} with a track expression that uses \code{psum} to sum the tracks.
#'
#'
#' @param out_track name of the track to create
#' @param tracks vector of tracks to sum
#' @param track_regex regular expression of the tracks to sum. You should provide either this or \code{tracks}.
#'
#' @return None.
#'
#' @inheritParams misha::gtrack.create
#'
#' @examples
#' \dontrun{
#' gset_genome("mm10")
#' gtrack.marginal_sum("wt_gastru.marginal", track_regex = "wt_gastru")
#' gtrack.marginal_sum("wt_gastru.marginal", tracks = c("wt_gastru.mc1", "wt_gastru.mc2"))
#' }
#'
#' @export
gtrack.marginal_sum <- function(out_track, tracks = NULL, track_regex = NULL, description = NULL, iterator = 10) {
    if (is.null(tracks)) {
        if (!is.null(track_regex)) {
            tracks <- gtrack.ls(track_regex)
        } else {
            stop("You should provide either tracks or track_regex")
        }
    }

    vtracks <- paste0("vt_", basename(tempfile()), "_", tracks, sep = "")
    purrr::walk2(tracks, vtracks, ~ gvtrack.create(.y, .x, func = "sum"))

    withr::defer(purrr::walk(vtracks, gvtrack.rm))

    track_str <- paste(vtracks, collapse = ", ")
    expr <- paste0("psum(", track_str, ", na.rm = TRUE)")

    description <- description %||% paste("Marginal sum of tracks:", paste(tracks, collapse = ", "))

    gtrack.create(
        expr = expr,
        description = description,
        track = out_track,
        iterator = iterator
    )
}

#' Create a dense track from k-mer occurrences in the genome
#'
#' @description
#' Builds a dense track whose value at each iterator interval is the number
#' (or fraction) of k-mer occurrences anchored inside that interval. Multiple
#' k-mers can be supplied; their counts/fractions are summed (e.g. supplying
#' \code{c("G", "C")} together with \code{mode = "frac"} yields a GC-content
#' track).
#'
#' This is a thin wrapper around the \code{kmer.count} / \code{kmer.frac}
#' virtual tracks (see \code{\link[misha]{gvtrack.create}}). All scanning
#' happens in misha's C++ layer, so a whole-hg38 GC-content track at 20 bp
#' resolution with a 200 bp sliding window completes in roughly 25 seconds
#' (vs. minutes for the regex-based path that this replaces).
#'
#' Replaces the slower \code{gseq.create_track}, which used R-level regex
#' and produced a sparse track.
#'
#' @param track Name of the track to create.
#' @param description Track description.
#' @param kmer Character vector of one or more k-mers, each containing only
#'   A/C/G/T characters (case insensitive). Their counts/fractions are summed.
#' @param iterator Track expression iterator. Either a positive integer
#'   binsize (used over the whole genome) or an intervals data frame
#'   restricting the scope. Mirrors \code{\link[misha]{gtrack.create}}.
#' @param window Optional positive integer. If given, each value reflects the
#'   k-mer count/fraction in a centred sliding window of \code{window} bp
#'   around the iterator anchor. Implemented via
#'   \code{\link[misha]{gvtrack.iterator}} with
#'   \code{sshift = -floor(window/2)} and \code{eshift = floor(window/2)}, so
#'   odd values round down by 1 bp.
#' @param strand Integer; \code{1} = forward, \code{-1} = reverse, \code{0} =
#'   both. Default \code{0}.
#' @param mode One of \code{"count"} or \code{"frac"}.
#'
#' @return None. A new dense track is created in the current misha database.
#'
#' @seealso \code{\link[misha]{gtrack.create}},
#'   \code{\link[misha]{gvtrack.create}}
#'
#' @examples
#' \dontrun{
#' # CpG starts at 1 bp resolution (replaces gseq.create_track("CG", "seq.CG"))
#' gtrack.create_kmer("seq.CG", "CG starts (fwd)",
#'     kmer = "CG", iterator = 1, strand = 1
#' )
#'
#' # CG count per 20 bp bin
#' gtrack.create_kmer("seq.CG_20", "CG count per 20bp",
#'     kmer = "CG", iterator = 20, strand = 1
#' )
#'
#' # GC fraction in 200 bp window at 20 bp resolution
#' gtrack.create_kmer("seq.GC", "GC fraction in 200bp window",
#'     kmer = c("G", "C"), iterator = 20, window = 200,
#'     mode = "frac", strand = 1
#' )
#' }
#'
#' @export
gtrack.create_kmer <- function(track, description, kmer,
                               iterator = 1, window = NULL,
                               strand = 0,
                               mode = c("count", "frac")) {
    mode <- match.arg(mode)

    if (!is.character(kmer) || length(kmer) == 0L) {
        stop("kmer must be a non-empty character vector", call. = FALSE)
    }
    kmer <- toupper(kmer)
    if (any(!nzchar(kmer)) || any(!grepl("^[ACGT]+$", kmer))) {
        stop("kmer must contain only A, C, G, T characters", call. = FALSE)
    }

    strand <- as.integer(strand)
    if (length(strand) != 1L || is.na(strand) || !strand %in% c(-1L, 0L, 1L)) {
        stop("strand must be -1, 0, or 1", call. = FALSE)
    }
    # kmer.count/kmer.frac vtrack params expect numeric on the C++ side
    strand_num <- as.numeric(strand)

    if (!is.null(window)) {
        window <- as.integer(window)
        if (length(window) != 1L || is.na(window) || window < 1L) {
            stop("window must be NULL or a positive integer", call. = FALSE)
        }
    }

    func_name <- if (mode == "count") "kmer.count" else "kmer.frac"

    vtracks <- paste0("vt_", basename(tempfile()), "_kmer_", seq_along(kmer))

    withr::defer({
        for (vt in vtracks) {
            if (vt %in% gvtrack.ls()) {
                suppressWarnings(do.call(gvtrack.rm, list(vtrack = vt)))
            }
        }
    })

    for (i in seq_along(kmer)) {
        gvtrack.create(vtracks[i], NULL, func_name,
            kmer = kmer[i], strand = strand_num
        )
        if (!is.null(window)) {
            half <- as.integer(window %/% 2L)
            gvtrack.iterator(vtracks[i], sshift = -half, eshift = half)
        }
    }

    expr <- paste(vtracks, collapse = " + ")

    gtrack.create(
        track = track,
        description = description,
        expr = expr,
        iterator = iterator
    )
}
