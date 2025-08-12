#' Write USCS genome browser file from intervals set data frame
#'
#' @param intervals misha intervals set. Can have an additional 'name' and 'score' fields. When 'type' equals to 'barChart', the intervals should have also a 'strand', 'category' and 'name2' fields.
#' @param file name of the output file
#' @param name track name
#' @param description track description
#' @param type track type. See https://genome.ucsc.edu/FAQ/FAQformat.html
#' @param color color of the track in the genome browser
#' @param append append to an existing file
#' @param rm_intervalID remove intervalID column from `intervals`
#' @param span span parameter for \code{type = "wig"}
#' @param categories categories order for 'barChart' type
#' @param ... list of additional attributes such as group or priority. See: https://genome.ucsc.edu/goldenPath/help/customTrack.html
#'
#' @examples
#' \dontrun{
#' gset_genome("mm9")
#' intervals <- gintervals(1, c(3025716, 3052742, 3181668), c(3026216, 3053242, 3182168))
#' intervals$score <- c(0.2, 0.5, 1)
#' fwrite_ucsc(intervals, "out.ucsc",
#'     name = "clust1", type = "bedGraph",
#'     graphType = "bar", color = "red", viewLimits = "0:1",
#'     autoScale = "off"
#' )
#' intevals2 <- gintervals(1, c(3671488, 3903482, 3943609), c(3671988, 3903982, 3944109))
#' intevals2$score <- c(1, 0.2, 0.45)
#' fwrite_ucsc(intervals2, "out.ucsc",
#'     name = "clust2", type = "bedGraph",
#'     graphType = "bar", color = "blue", viewLimits = "0:1",
#'     autoScale = "off", append = TRUE
#' )
#' }
#'
#' @export
fwrite_ucsc <- function(intervals, file, name, type = NULL, description = "", color = "black", rm_intervalID = TRUE, append = FALSE, span = NULL, categories = NULL, ...) {
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
        if (rlang::has_name(intervals, "intervalID")) {
            intervals <- intervals %>% select(-intervalID)
        }
    }

    ext <- span %||% 0

    intervals <- intervals %>%
        mutate(temp_id = 1:n())

    data1 <- intervals %>%
        select(chrom:end, temp_id) %>%
        left_join(gintervals.all() %>% rename(chrstart = start, chrend = end), by = "chrom") %>%
        filter(end + span <= chrend)

    intervals <- intervals[data1$temp_id, ]

    data1 <- data1 %>%
        select(-temp_id)

    if (rlang::has_name(intervals, "name")) {
        data1 <- data1 %>% bind_cols(intervals %>% select(name))
    }

    if (rlang::has_name(intervals, "score")) {
        data1 <- data1 %>%
            bind_cols(intervals %>% select(score)) %>%
            filter(!is.na(score))
    }

    if (!is.null(type) && type == "wig") {
        if (append) {
            stop("Unable to use type 'wig' and 'append=TRUE'")
        }
        write(header, file, append = FALSE)
        write_wig(data1, file, span = span)
    } else if (!is.null(type) && type == "barChart") {
        categories <- categories %||% unique(intervals$category)
        header <- paste(header, glue("barChartBars=\"{paste(categories, collapse = ' ')}\""))

        data1 <- intervals %>%
            mutate(category = factor(category, levels = categories)) %>%
            mutate(score = as.character(score)) %>%
            tidyr::complete(category, tidyr::nesting(chrom, start, end, strand, name, name2), , fill = list(score = "")) %>%
            arrange(chrom, start, end, category) %>%
            group_by(chrom, start, end, strand, name, name2) %>%
            summarise(expScores = paste(score, collapse = ",")) %>%
            ungroup()

        data1 <- data1 %>%
            mutate(start = start + 1, end = end + 1) %>%
            arrange(chrom, start) %>%
            mutate(strand = ifelse(strand == 1, "+", "-")) %>%
            mutate(score = 999, expCount = length(categories)) %>%
            rename(chromStart = start, chromEnd = end) %>%
            select(chrom, chromStart, chromEnd, name, score, strand, name2, expCount, expScores, everything()) %>%
            rename("#chrom" = chrom, "string name" = name) %>%
            mutate(`_dataOffset` = 0, `_dataLen` = 0)


        fwrite_header(data1, file = file, header = header, sep = "\t", quote = FALSE, append = append)
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
