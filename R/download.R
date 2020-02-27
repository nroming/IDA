#' Downloads a file to disk from an url and logs the ural and the date of
#' download to disk.
#'
#' @param url Url of a file.
#' @param log Name of the log file to write to (default 'log.txt').
#' @param folder Folder where to dowload the file and write the log to (default
#'   'data-raw').
#' @return None
#' @export
download_source_file <- function(url, log = "log.txt", folder = "data-raw"){

  directory <- dirname(url)
  file <- basename(url)

  cat("Downloading ", file, ".\n", sep="")

  download.file(url, file.path(folder, file))

  cat(file, " downloaded from \n", directory, " on ", as.character(Sys.time()),
      ".\n \n", file = file.path(folder, log), sep ="", append = TRUE)
}
