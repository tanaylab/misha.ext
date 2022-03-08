#'
#' @export
#' @rdname  gset_genome
find_params_yaml <- function() {
    if (file.exists(".misha.yaml")) {
        return(".misha.yaml")
    }

    home_dir_yaml <- file.path(Sys.getenv("HOME"), ".misha.yaml")
    if (file.exists(home_dir_yaml)) {
        return(home_dir_yaml)
    }

    if (!is.null(Sys.getenv("MISHA_GENOMES"))) {
        return(Sys.getenv("MISHA_GENOMES"))
    }

    file.copy(system.file("config", "misha_params.yaml", package = "misha.ext"), to = home_dir_yaml)
    message(glue("{home_dir_yaml} does not exist.\nAn empty template was created, please edit it and reload the package.\n"))
    return(NULL)
}

init_config <- function(params_yaml) {
    suppressWarnings(tgconfig::register_params(params_yaml, package = "misha.ext", override = TRUE))
}

#' Set misha root based on genome name
#'
#' @param genome name of the genome (e.g. hg19)
#' @param params_yaml path to .misha.yaml parameters file. By default, \code{misha.ext} would look for such file
#' first at the current directory, then at the user's home directory, then at an environment variable called "MISHA_GENOMES"
#' and if none of the above exist - a new file would be created at the user's home directory. You can run \code{find_params_yaml}
#' to see the current location of your configuration file.
#'
#' @examples
#' \dontrun{
#' gset_genome("hg19")
#' }
#'
#' @export
gset_genome <- function(genome, params_yaml = find_params_yaml()) {
    groot <- get_genome(genome, params_yaml)
    if (is.null(groot)) {
        stop("no genome named ", genome, " in params yaml")
    }
    gsetroot(groot)
}

get_genome <- function(genome, params_yaml) {
    init_config(params_yaml)
    genomes <- tgconfig::get_param("genome", package = "misha.ext")
    groot <- genomes[[genome]]
    return(groot)
}


#' Does a genome db exist at the config file
#'
#' @inheritParams gset_genome
#' @export
genome_exists <- function(genome, params_yaml = find_params_yaml()) {
    return(!is.null(get_genome(genome, params_yaml)))
}
