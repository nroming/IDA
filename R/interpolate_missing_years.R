#' Interpolation of missing years in a dataset
#'
#' @param df A compatible dataframe
#' @param stepwidth A unit for the output stepwidth in years
#' @param method One of 'linear' or 'spline' (see \code{\link[zoo]{na.approx}})
#' @return A compatible dataframe
#' @export
#' @examples
#' tmp <- filter(idata, scenario == "myo_L15_BC_a", variable == "GDP")
#' tmp <- interpolate_missing_years(tmp)
interpolate_missing_years <- function(df, stepwidth = 1, method = "linear"){
  # detect length of temporal data
  if (any(nchar(df$temporal)) > 4){
    stop("Temporal dimension does not seem to be in years!")
  }

  time_target <- seq(min(df$temporal, na.rm = TRUE), max(df$temporal, na.rm = TRUE), by = stepwidth)

  # preallocate empty target dataframe containing all possible combinations of
  # column values in the original dataframe except the original values
  tmp <- expand.grid(source_id = unique(df$source_id),
                     model = unique(df$model),
                     scenario = unique(df$scenario),
                     variable = unique(df$variable),
                     temporal = as.integer(time_target),
                     spatial = unique(df$spatial),
                     unit = unique(df$unit),
                     stringsAsFactors = FALSE)

  # find out the maximum and minimum values for the temporal variable
  df_dim <- group_by(df, source_id, model, scenario, spatial, variable,
                     unit) %>%
  summarise(year_min = min(temporal, na.rm = TRUE),
            year_max = max(temporal, na.rm = TRUE)) %>%
    ungroup()

  # expand the original dataframe with the new years
  df <- left_join(tmp, df, by = c("source_id", "model", "scenario", "variable",
                                  "temporal", "spatial", "unit"))

  # below is necessary as otherwise interpolation does not work (issue #51)
  # attach information on specific minimumn and maximum year for each
  # combination of source, model, sceanario, spatial, variable, unit ...
  df <- right_join(df, df_dim, by = c("source_id", "model", "scenario",
                                      "variable", "spatial", "unit"))

  # ... and filter out accordingly
  df <- filter(df, temporal >= year_min)
  df <- filter(df, temporal <= year_max)

  # drop the helper columns
  df <- select(df, -year_min, -year_max)

  # get into the right order so that the interpolations functions from the
  # zoo-package work
  df <- arrange(df, source_id, model, scenario, spatial, variable, temporal)

  if (method == "linear"){
    # linear approximation of missing values
    df <- group_by(df, source_id, model, scenario, spatial, variable, unit) %>%
      filter(!all(is.na(value))) %>% ungroup()
    df <- mutate(df, value = zoo::na.approx(value))

    # df <- group_by(df, source_id, model, scenario, spatial, variable, unit) %>%
    #     mutate(value = zoo::approx(value)) %>% ungroup()
  } else if (method == "spline"){
    df <- group_by(df, source_id, model, scenario, spatial, variable, unit) %>%
      ungroup()
    df <- mutate(df, value = zoo::na.spline(value))
  } else {
    stop("No valid method of interpolation provided (must be one of 'linear' or 'spline')!")
  }

  return(df)
}
