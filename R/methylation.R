#' gextract methylation data
#'
#' @param tracks name of methylation track suffix (without ".cov" or ".meth")
#' @param names sets the columns names in the returned value.
#' @param intervals genomic scope for which the function is applied
#' @param iterator track expression iterator. If "NULL" iterator is based on CpGs
#' @param d_expand smooth the methylation signal \code{d_expand} bp from each side (optional).
#' @param join_intervals add the intervals to the returned data frame (using left_join)
#' @param extract_meth_calls extract also methylation calls (".meth" tracks)
#' @param avg_tracks tracks of average methylation (for tracks that do not have ".cov" and ".meth" tracks)
#' @param avg_tracks_names names of tracks in \code{avg_tracks} (optional)
#' @param annot_tracks other tracks to extract 
#' @param annot_tracks_names names of annotation tracks
#' @param min_cov minimal coverage per track. If the coverage is below this number the methylation values would be changed to NA.
#' @param ... additional arguments to \link[misha]{gextract}
#'
#' @return data frame with a column with average methylation for each track, and another column with ".cov" suffix with every track's coverage. If \code{extract_meth_calls} is TRUE - additional columns with ".meth" suffix with number of methylation calls per track.
#'
#' @seealso \link[misha]{gextract}
#' @export
gextract_meth <- function(tracks, names = NULL, intervals = gintervals.all(), iterator = "intervs.global.seq_CG", d_expand = NULL, join_intervals = FALSE, extract_meth_calls = FALSE, avg_tracks=NULL, avg_tracks_names=NULL, annot_tracks = NULL,
annot_tracks_names = NULL, min_cov=NULL, ...) {
    opt <- options(gmax.data.size = 1e9)
    on.exit(options(opt))

    names <- names %||% tracks
    annot_tracks_names <- annot_tracks_names %||% annot_tracks

    cov_vtracks <- glue("{names}_smoo.cov")
    walk2(cov_vtracks, glue("{tracks}.cov"), ~ gvtrack.create(.x, .y, "sum"))
    meth_vtracks <- glue("{names}_smoo.meth")
    walk2(meth_vtracks, glue("{tracks}.meth"), ~ gvtrack.create(.x, .y, "sum"))

    if (!is.null(d_expand)) {
        walk(cov_vtracks, ~ gvtrack.iterator(.x, sshift = -d_expand, eshift = d_expand))
        walk(meth_vtracks, ~ gvtrack.iterator(.x, sshift = -d_expand, eshift = d_expand))
    }    

    if (join_intervals) {
        func <- misha.ext::gextract.left_join
    } else {
        func <- gextract
    }

    if (extract_meth_calls) {
        if (!is.null(avg_tracks)){
            stop("cannot use 'extract_meth_calls' option with avg_tracks")
        }
        data <- func(c(glue("{meth_vtracks} / {cov_vtracks}"), cov_vtracks, meth_vtracks, annot_tracks), intervals = intervals, iterator = iterator, colnames = c(names, glue("{names}.cov"), glue("{names}.meth"), annot_tracks_names), ...)
    } else {
        if (!is.null(avg_tracks)){
            avg_names <- avg_tracks_names %||% avg_tracks            
            data <- func(c(glue("{meth_vtracks} / {cov_vtracks}"), cov_vtracks, glue("{avg_tracks}.avg"), annot_tracks), intervals = intervals, iterator = iterator, colnames = c(names, glue("{names}.cov"), avg_names, annot_tracks_names), ...)
        } else {            
            data <- func(c(glue("{meth_vtracks} / {cov_vtracks}"), cov_vtracks, annot_tracks), intervals = intervals, iterator = iterator, colnames = c(names, glue("{names}.cov"), annot_tracks_names), ...)
        }
        
    }

    replace_list <- map(glue("{names}.cov"), ~0) %>% set_names(glue("{names}.cov"))
    data <- data %>% tidyr::replace_na(replace = replace_list)

    data <- as_tibble(data)

    if (!is.null(min_cov)){
        for (n in names){
            f <- data[[glue("{n}.cov")]] < min_cov
            data[[n]][f] <- NA
        }
    }    

    return(data)
}
