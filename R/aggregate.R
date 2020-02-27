#' Aggregates a dataset over space
#'
#' @param df A compatible dataset.
#' @param variable One or several variables to aggregate
#' @param orig Current spatial encoding, see \code{\link[countrycode]{countrycode}}.
#' @param dest Target spatial encoding.
#' @return A dataframe containing the aggregated values from original dataframe.
#' @export
aggregate_space <- function(df, vars, orig, dest, add){
  # create appropriate subset
  df <- dplyr::filter(df, variable %in% vars)

  # apply mapping
  df$spatial <- countrycode::countrycode(df$spatial, origin = orig,
                                       destination = dest, addendum = add)

  # remove entries for which no match was found
  df <- na.omit(df)

  # aggregate
  df <- group_by2(df) %>%
          dplyr::summarize(value = sum(value, na.rm = TRUE))

  return(df)
}


#' Aggregates a dataset over variables
#'
#' @param df A compatible dataset
#' @param new_var Name of the aggregated variable
#' @param vars Vector of variable names to be aggregated
#' @param append Append the aggregated data to the original dataset (default
#'   FALSE)?
#' @param absolute For e.g. IEA data, many flows are expressed as negative
#'   values. Setting this to TRUE enables a check for that and returns the
#'   absolute values if all values are smaller than (or equal to) zero.
#' @return A dataframe containing the aggregated values from the original dataframe.
#' @export
aggregate_variable <- function(df, new_var, vars, append = FALSE, absolute = FALSE){

  if(append) df_orig <- df

  df <- dplyr::filter(df, variable %in% vars)

  if (max(range(df$value, na.rm = TRUE)) == 0 && absolute){
    df$value <- abs(df$value)
  }

  df <- group_by2(df, non_grouping = "variable") %>%
  summarize(value = sum(value, na.rm = TRUE), variable = new_var) %>% ungroup()

  if(append) df <- rbind(df_orig, df)

  return(df)
}

