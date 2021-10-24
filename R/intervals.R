#' Finds neighbors between two sets of intervals (and does not return conflicting column names)
#'
#' @inheritParams misha::gintervals.neighbors
#'
#' @return
#' @export
#' @seealso \link[misha]{gintrevals.neighbors}
gintervals.neighbors1 <- function(intervals1 = NULL,
                                  intervals2 = NULL,
                                  maxneighbors = 1,
                                  mindist = -1e+09,
                                  maxdist = 1e+09,
                                  na.if.notfound = TRUE) {
    res <-
        gintervals.neighbors(
            intervals1 = intervals1,
            intervals2 = intervals2,
            maxneighbors = maxneighbors,
            mindist = mindist,
            maxdist = maxdist,
            na.if.notfound = na.if.notfound
        ) %>%
        tibble::repair_names()

    return(res %>% as_tibble())
}


#' Filter intervals set by distance to another.
#' A wrapper around gintervals.neighbours and filter(dist <= max_distance)
#'
#' @param intervals1 intervals set
#' @param intervals2 intervals set by which to filter intervals1
#' @param max_distance maximal distance of every interval in intervals1 from intervals2 (defualt 0)
#' @param abs_dist take the absolute distance
#' @param bind_intervals2 cbind add intervals2 to result
#' @param ... additional parameters to gintervals.neighbours1
#'
#' @return
#' @export
gintervals.filter <- function(intervals1, intervals2, max_distance = 0, abs_dist = TRUE, bind_intervals2 = FALSE, ...) {
    intervals1_cols <- colnames(intervals1)
    res <- intervals1 %>% gintervals.neighbors1(intervals2, ...)
    if (abs_dist) {
        res$dist <- abs(res$dist)
    }
    res <- res %>% filter(dist <= max_distance)
    if (!bind_intervals2) {
        res <- res %>% select(one_of(intervals1_cols))
    }
    return(res)
}

#' Returns the result of track expressions evaluation for each of the
#' iterator intervals, and cbinds the intervals (instead of intervalID)
#'
#' @inheritParams misha::gextract
#' @param suffix suffix for conflicting column names
#'
#' @return
#' @export
#'
#' @seealso \link[misha]{gextract}
gextract.left_join <- function(expr, intervals = NULL, colnames = NULL, iterator = NULL, band = NULL, file = NULL, intervals.set.out = NULL, suffix = "1") {
    if ("character" %in% class(intervals)) {
        intervals <- gintervals.load(intervals)
    }
    d <- gextract(expr, intervals = intervals, colnames = colnames, iterator = iterator, band = band, file = file, intervals.set.out = intervals.set.out)
    conflict_names <- which(colnames(intervals) %in% colnames(d))
    colnames(intervals)[conflict_names] <- paste0(colnames(intervals)[conflict_names], suffix)
    intervals$intervalID <- 1:nrow(intervals)
    d <- d %>%
        arrange(intervalID) %>%
        left_join(intervals, by = "intervalID") %>%
        select(-intervalID)
    return(d)
}


#' Define promoter regions
#' @param upstream bp upstream to TSS
#' @param downstream bp downstread from tss
#'
#' @export
get_promoters <- function(upstream = 500, downstream = 50) {
    gintervals.load("intervs.global.tss") %>%
        mutate(start = ifelse(strand == 1, start - upstream, start - downstream), end = ifelse(strand == 1, end + downstream, end + upstream)) %>%
        gintervals.force_range()
}
