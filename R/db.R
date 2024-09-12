#' Create and Load a Genome Database
#'
#' This function downloads, extracts, and loads a misha genome database for the specified genome.
#'
#' @param genome A character string specifying the genome to download. Supported genomes are "mm10" and "hg38".
#' @param path A character string specifying the directory where the genome will be extracted. Defaults to genome name in the current working directory.
#' @param tmpdir A character string specifying the directory for storing temporary files. Defaults to the system's temporary directory.
#'
#' @details
#' The function checks if the specified genome is available. If tmpdir, it constructs the download URL, downloads the genome file,
#' extracts it to the specified directory, and loads the genome database using \code{gsetroot}. The function also calls \code{gdb.reload} to reload the genome database.
#'
#' @return None.
#'
#' @examples
#' \dontrun{
#' gdb.create_genome("mm10", path = "/path/to/genome")
#' }
#'
#' @export
gdb.create_genome <- function(genome, path = getwd(), tmpdir = tempdir()) {
    # Supported genomes
    supported_genomes <- c("mm10", "hg38")

    # Check if the genome is supported
    if (!genome %in% supported_genomes) {
        stop(paste("The genome", genome, "is not available yet. Available genomes are:", paste(supported_genomes, collapse = ", ")))
    }

    # Construct the download URL
    base_url <- "https://misha-genome.s3.eu-west-1.amazonaws.com/"
    url <- paste0(base_url, genome, ".tar.gz")

    # Create a temporary file to store the download
    temp_file <- tempfile(fileext = ".tar.gz", tmpdir = tmpdir)

    # Download the genome file
    message("Downloading ", genome, " genome...")
    download.file(url, temp_file, mode = "wb")

    # Create the destination directory if it doesn't exist
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    # Extract the tar.gz file
    message("Extracting ", genome, " genome...")
    untar(temp_file, exdir = path)

    # Remove the temporary file
    unlink(temp_file)

    message("Loading misha root...")
    gsetroot(file.path(path, genome))
    gdb.reload()

    message(genome, " genome has been successfully downloaded and extracted to ", file.path(path, genome))
}
