#' Shows all variables present in a dataframe
#'
#' This function is just a convenient wrapper
#'
#' @param df A compatible dataframe
#' @return A factor
#' @import dplyr
#' @export
show_variables <- function(df){
  return(unique(df$variable))
}
