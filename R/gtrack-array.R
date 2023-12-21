#' Extract a virtual track on each array.track column
#'
#' @param track array track
#' @inheritParams misha::gextract
#' @inheritParams misha::gtrack.array.extract
#' @inheritParams misha::gvtrack.create
#'
#' @export
gvtrack.array_extract <- function(track, intervals, iterator, func = NULL, slice = NULL, ...) {
    if (is.null(slice)) {
        slice <- gtrack.array.get_colnames(track)
    }

    vnames <- paste0("v_", stringi::stri_rand_strings(length = 5, n = 1), "_", slice)
    walk2(slice, vnames, ~ {
        gvtrack.create(vtrack = .y, src = track, func = func)
        gvtrack.array.slice(.y, slice = .x)
    })

    on.exit(walk(vnames, gvtrack.rm))

    res <- gextract(vnames, intervals = intervals, iterator = iterator, colnames = slice, ...)

    return(res)
}
