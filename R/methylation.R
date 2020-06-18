#' gextract methylation data
#'
#' @param tracks name of methylation track suffix (without ".cov" or ".meth")
#' @param names sets the columns names in the returned value.
#' @param d_expand smooth the methylation signal \code{d_expand} bp from each side (optional).
#' @param intervals genomic scope for which the function is applied
#' @param iterator track expression iterator. If "NULL" iterator is based on CpGs
#' @param join_intervals add the intervals to the returned data frame (using left_join)
#' @param ... additional arguments to \code{gextract}
#'
#' @return data frame with a column with average methylation for each track, and another column with ".cov" suffix with every track's coverage
#'
#' @export
gextract_meth <- function(tracks, names = NULL, d_expand = NULL, intervals = gintervals.all(), iterator = "intervs.global.seq_CG", join_intervals = FALSE, ...) {
  cov_vtracks <- glue("{names}_smoo.cov")
  walk2(cov_vtracks, glue("{tracks}.cov"), ~ gvtrack.create(.x, .y, "sum"))
  meth_vtracks <- glue("{names}_smoo.meth")
  walk2(meth_vtracks, glue("{tracks}.meth"), ~ gvtrack.create(.x, .y, "sum"))

  if (!is.null(d_expand)) {
    walk(cov_vtracks, ~ gvtrack.iterator(.x, sshift = -d_expand, eshift = d_expand))
    walk(meth_vtracks, ~ gvtrack.iterator(.x, sshift = -d_expand, eshift = d_expand))
  }

  names <- names %||% tracks

  if (join_intervals) {
    func <- misha.ext::gextract.left_join
  } else {
    func <- gextract
  }

  data <- func(c(glue("{meth_vtracks} / {cov_vtracks}"), cov_vtracks), intervals = intervals, iterator = iterator, colnames = c(names, glue("{names}.cov")), ...)
  replace_list <- map(glue("{names}.cov"), ~0) %>% set_names(glue("{names}.cov"))
  data <- data %>% tidyr::replace_na(replace = replace_list)

  data <- as_tibble(data)

  return(data)
}
