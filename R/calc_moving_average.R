#' Calculation of a moving average of arbitrary lenght
#'
#' @param df A compatible dataframe
#' @param vars Variables for which the calculation should be carried out (default all variables present in df)
#' @param window The width of the rolling window (default 3)
#' @param append should the moving average be added to the original data (default TRUE)
#' @return A compatible dataframe
#' @export
calc_moving_average <- function(df, vars = unique(df$variable), window = 3,
                                append = TRUE){
  if(append) df_orig = df

  df <- filter(df, variable %in% vars)

  df <- group_by(df, source_id, model, scenario, spatial, variable, unit) %>%
    mutate(value = rollmean(x = value, k = window, fill = NA)) %>%
    ungroup() %>%
    mutate(variable = paste0(variable, "|MA", window))

  if(append) df <- bind_rows(df_orig, df)

  return(df)
}
