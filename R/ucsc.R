#' Write USCS genome browser file from intervals set data frame
#'
#' @param intervals misha intervals set. Can have an additional 'name' and 'score' fields.
#' @param file name of the output file
#' @param name track name
#' @param description track description
#' @param type track type. See https://genome.ucsc.edu/FAQ/FAQformat.html
#' @param color color of the track in the genome browser
#' @param append append to an existing file
#' @param rm_intervalID remove intervalID column from `intervals`
#' @param span span parameter for \code{type = "wig"}
#' @param ... list of additional attributes such as group or priority. See: https://genome.ucsc.edu/goldenPath/help/customTrack.html
#'
#' @examples
#' \dontrun{
#' fwrite_ucsc(d, "out.ucsc", type = "broadPeak", color = "red")
#' fwrite_ucsc(d1, "out.ucsc", type = "broadPeak", color = "blue", append = TRUE)
#' fwrite_ucsc(d1, "out.ucsc", type = "broadPeak", color = "green", append = TRUE, list(visibility = 3, offset = 15))
#' }
#'
#' @export
fwrite_ucsc <- function(intervals, file, name, type = NULL, description = "", color = "black", rm_intervalID = TRUE, append = FALSE, sparse = TRUE, span=NULL, ...) {
    color <- paste0(grDevices::col2rgb(color)[, 1], collapse = ",")
    header <- paste0("track ", glue("name={name} description=\"{description}\" color={color}"))

    if (!is.null(type)) {
        header <- glue("{header} type={type}")
    }

    ucsc_options <- list(...)
    if (length(ucsc_options) > 0) {
        header <- paste0(
            header,
            " ",
            paste(purrr::imap_chr(ucsc_options, ~ glue("{.y}={.x}")), collapse = " ")
        )
    }

    if (rm_intervalID) {
        if (rlang::has_name(intervals, "intervalID")){
            intervals <- intervals %>% select(-intervalID)
        }        
    }

    data1 <- intervals %>%
        select(chrom:end)

    if (rlang::has_name(intervals, "name")) {
        data1 <- data1 %>% bind_cols(intervals %>% select(name))
    }

    if (rlang::has_name(intervals, "score")) {
        data1 <- data1 %>%
            bind_cols(intervals %>% select(score)) %>%
            filter(!is.na(score))
    }

    if (!is.null(type) && type == "wig") {
        if (append){
            stop("Unable to use type 'wig' and 'append=TRUE'")
        }
        write(header, file, append = FALSE)
        write_wig(data1, file, span = span)
    } else {
        data1 <- data1 %>%
            mutate(start = start + 1, end = end + 1) %>%
            arrange(chrom, start) %>%
            rename("#chrom" = chrom, chromStart = start, chromEnd = end)

        fwrite_header(data1, file = file, header = header, sep = "\t", quote = FALSE, append = append)
    }
}


write_wig <- function(df, file, span = NULL) {
    
    for (chrom in unique(df$chrom)) {
        data <- df %>%
            filter(chrom == !!chrom) %>%
            mutate(start = start + 1, end = end + 1) %>%
            arrange(start) %>%
            select(start, score)
        if (!is.null(span)) {
            header <- glue("variableStep chrom={chrom} span={span}")
        } else {
            header <- glue("variableStep chrom={chrom}")
        }
        
        fwrite_header(data, file = file, header = header, sep = "\t", quote = FALSE, col_names = FALSE, append = TRUE)
    }
}
