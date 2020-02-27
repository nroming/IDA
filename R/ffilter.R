#' Improves filtering data by using cached results if filtering conditions stay the same
#'
#' this function computes a md5-hash of the filtering arguments supplied and
#' uses this hash as a file name for caching the filtering results. If filtering
#' arguments change, a new cached file will be created
#'
#' @param x A compatible dataframe
#' @param ...  arguments passed on to \code{\link[dplyr]{filter}}
#' @return A compatible dataframe
#' @import dplyr digest
#' @export
ffilter <- function(x, ...){
  if(!dir.exists("cache/ffilter")){
    dir.create("cache/ffilter", recursive = TRUE)
  }

  arguments <- as.list(substitute(list(...)))

  filename <- file.path("cache", "ffilter", paste0(digest::digest(arguments), ".rda"))

  if(file.exists(filename)){
    message("Loading data from disk.")
    result <- readRDS(filename)
  } else {
    message("Filtering")
    result <- dplyr::filter(x, ...)
    saveRDS(result, file = filename, compress = FALSE)
  }
return(result)
}
