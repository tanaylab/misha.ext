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
