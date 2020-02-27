#' Computes compound annual/periodical growth rates
#'
#' @param df A compatible dataframe
#' @param vars Variables to compute growth rates for
#' @return A dataframe containing the compoud periodical growth rates
#' @export
get_growth_rates <- function(df, vars){
  # create appropriate subset
  df <- filter(df, variable %in% vars)

  df <- group_by_(df, .dots =names(df)[!(names(df) %in% c("temporal", "value"))]) %>%
    mutate(value = (lag(value, n = 0, order_by = temporal) / lag(value, n = 1, order_by = temporal))^(1/(lag(temporal, n = 0, order_by = temporal) - lag(temporal, n = 1, order_by = temporal))) - 1) %>% ungroup()

  df$variable <- paste0(df$variable, "|CAGR")

  df$unit <- "1"

  return(df)
}
