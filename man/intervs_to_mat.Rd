% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{intervs_to_mat}
\alias{intervs_to_mat}
\title{Transform an intervals set to matrix}
\usage{
intervs_to_mat(df, remove_intervalID = TRUE)
}
\arguments{
\item{df}{intervals set}

\item{remove_intervalID}{whether to remove the "intervalID" column generated by a gextract call}
}
\value{
matrix with coordinate rownames (chrom_start_end). if \code{remove_intervalID == FALSE}, also with \code{intervalID} column.
}
\description{
Transform an intervals set to matrix
}
