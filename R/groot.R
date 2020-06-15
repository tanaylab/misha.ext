find_params_yaml <- function() {
  if (file.exists(".misha.yaml")) {
    return(".misha.yaml")
  }

  home_dir_yaml <- file.path(Sys.getenv("HOME"), ".misha.yaml")
  if (file.exists(home_dir_yaml)) {
    return(home_dir_yaml)
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
#' @param params_yaml path to .misha.yaml parameters file
#'
#' @examples
#' \dontrun{
#' gset_genome("hg19")
#' }
#'
#' @export
gset_genome <- function(genome, params_yaml = find_params_yaml()) {
  init_config(params_yaml)
  genomes <- tgconfig::get_param("genome", package = "misha.ext")
  groot <- genomes[[genome]]
  if (is.null(groot)) {
    stop("no genome named ", genome, " in params yaml")
  }
  gsetroot(groot)
}
