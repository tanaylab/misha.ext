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

    env_var <- Sys.getenv("MISHA_GENOMES")
    if (!is.null(env_var) && env_var != "") {
        return(env_var)
    }

    if (!interactive()) {
        answer <- 1
    } else {
        answer <- utils::menu(c("Yes", "No"), title = glue::glue("{home_dir_yaml} does not exist. Would you like to create it?"))
    }

    if (answer == 1) {
        file.copy(system.file("config", "misha_params.yaml", package = "misha.ext"), to = home_dir_yaml)
        message(glue("An empty template was created at {home_dir_yaml}, please edit it and rerun the function\n"))
        return(home_dir_yaml)
    }

    stop(glue("Please create a file called {home_dir_yaml}, or set the environment variable MISHA_GENOMES to the path of such file."))
}

init_config <- function(params_yaml) {
    suppressWarnings(tgconfig::register_params(params_yaml, package = "misha.ext", override = TRUE))
}

#' Set misha root based on genome name
#'
#' @param genome name of the genome (e.g. hg19)
#' @param params_yaml path to .misha.yaml parameters file. By default, \code{misha.ext} would look for such file
#' @param force force gsetroot call, overwriting exsiting memoized genome
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
gset_genome <- function(genome, params_yaml = find_params_yaml(), force=FALSE) {
    groot <- get_genome(genome, params_yaml)
    if (is.null(groot)) {
        stop("no genome named ", genome, " in params yaml")
    }
    if(!exists("global_groots", envir = .misha)){
        global_groots = list()
    }
    if(is.null(global_groots[[genome]]) || force){
        gsetroot(groot)
        global_groots[[genome]] = list(ALLGENOME=.misha$ALLGENOME, GROOT=.misha$GROOT, GWD=.misha$GWD, GTRACKS=.misha$GTRACKS, GINTERVS=.misha$GINTERVS)
    } else {
        assign("ALLGENOME", global_groots[[genome]][['ALLGENOME']], envir = .misha)
        assign("GROOT", global_groots[[genome]][['GROOT']], envir = .misha)
        assign("GWD", global_groots[[genome]][['GWD']], envir = .misha)
        assign("GTRACKS", global_groots[[genome]][['GTRACKS']], envir = .misha)
        assign("GINTERVS", global_groots[[genome]][['GINTERVS']], envir = .misha)
    }
    assign("global_groots", global_groots, envir = .misha)
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
