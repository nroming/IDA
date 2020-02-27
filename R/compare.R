compare_2_scenarios <- function(df, var, scen1, scen2,
                              space, time){
  # create appropriate subset
  df <- filter(df, variable = var, spatial = space, temporal = time)

  if(length(unique(df$unit))>1){
    stop("Units are not harmonized! Comparison is impossible.")
  }

  df <- dcast(temporal ~ scenario)


}
