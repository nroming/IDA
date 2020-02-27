#' Calculates the year at which emissions get zero
#'
#' Since some variables never actually get zero, this function uses the year
#' where the absoloute value is smallest as a proxy.
#'
#' @param df A compatible dataframe
#' @param append Append the aggregated data to the original dataset (default
#'   FALSE)?
#' @return A compatible dataframe
#' @import dplyr
#' @export
calc_extreme_timesteps <- function(df, append = FALSE){
  if(append) df_orig <- df

  df <- interpolate_missing_years(df)

  df <- group_by2(df, non_grouping = "temporal") %>%
    filter(value == min(abs(value))) %>%
    mutate(value = temporal, temporal = NA)  %>% ungroup() %>%
    mutate(variable = paste0(variable, "|Year minimum"))

  if(append) df <- rbind(df_orig, df)

  return(df)
}

#' Calculates the 10, 20, ..., 90 percentiles of data
#'
#' @param df A compatible dataframe
#' @param append Append the aggregated data to the original dataset (default
#'   FALSE)?
#' @param ... further arguments passed on to \code{\link[dplyr]{filter}} #'
#' @return A compatible dataframe
#' @import dplyr
#' @export
calc_percentiles <- function(df, append = FALSE, ...){
  df <- filter(df, ...)

  if(append) df_orig <- df

  df <- group_by2(df, non_grouping = "scenario") %>%
    summarize(PCTL10 = quantile(value, 0.1, na.rm = TRUE),
              PCTL20 = quantile(value, 0.2, na.rm = TRUE),
              PCTL30 = quantile(value, 0.3, na.rm = TRUE),
              PCTL40 = quantile(value, 0.4, na.rm = TRUE),
              PCTL50 = quantile(value, 0.5, na.rm = TRUE),
              PCTL60 = quantile(value, 0.6, na.rm = TRUE),
              PCTL70 = quantile(value, 0.7, na.rm = TRUE),
              PCTL80 = quantile(value, 0.8, na.rm = TRUE),
              PCTL90 = quantile(value, 0.9, na.rm = TRUE),
              MEAN    = mean(value, na.rm = TRUE),
              MIN     = min(value, na.rm = TRUE),
              MAX     = max(value, na.rm = TRUE)
    ) %>% ungroup()

  df <- melt(df, id.vars = c("source_id", "model", "spatial", "temporal", "variable", "unit"),
             variable.name = "percentile")

  df <- mutate(df, variable = paste(df$variable, df$percentile, sep = "|")) %>%
    select(-percentile)

  if(append) df <- rbind(df_orig, df)

  return(df)
}

#' Calculate cumulated emissions (or other things), aka budgets, over time
#'
#' @param df A compatible dataframe
#' @param gases Variables to be aggregated
#' @param t_start First time-step of the period over which to cumulate
#' @param t_end Last time-step over which to cumulate
#' @param append Append the aggregated data to the original dataset (default
#'   FALSE)?
#' @return A compatible dataframe
#' @import dplyr
#' @export
calc_cumulated_time <- function(df, gases, t_start, t_end, append = FALSE){

  if(append) df_orig <- df

  df <- filter(df, variable %in% gases)

  df <- interpolate_missing_years(df)

  df <- filter(df, temporal >= t_start, temporal <= t_end)

  df <- group_by2(df, non_grouping = "temporal") %>%
    summarize(value = sum(value)) %>% ungroup()

  df <- mutate(df, variable = paste0("Cumulative ", variable, "|", t_start, "-",
                                     t_end),
               unit = gsub("/yr", "", unit, fixed = TRUE),
               temporal = NA)

  if(append) df <- rbind(df_orig, df)

  return(df)
}

#' Harmonize one or several scenarios compared to a reference scenario
#'
#' This function was initially developed to harmonize IAM emissions data to the
#' RCP scenarios, but can of course be used in other circumstances, as well. The
#' three possible methodologies are described in the paper by Rogelj et. al
#' \url{http://iopscience.iop.org/1748-9326/6/2/024002}
#'
#' @param df A compatible dataframe
#' @param scen_ref Scenario used as reference (e.g. a RCP scenario)
#' @param scens Scenarios to be harmonized
#' @param vars Variables for which harmonization should take place
#' @param method One of 'uniform', 'tapered' or 'offset'
#' @param t_zero Period in which harmonization should start
#' @param t_match Period in which harmonization should end
#' @param append Append the aggregated data to the original dataset (default
#'   FALSE)?
#' @return A compatible dataframe. The 'scenario' column is a combination of the
#'   'scens' and the 'scen_ref', e.g. 'myo_L15_BC_a' and 'RCP3PD' becomes
#'   'myo_L15_BC_a'|'RCP3PD'
#' @import dplyr zoo
#' @export
harmonize_scenario <- function(df, scen_ref, scens, vars, method, t_zero = 2005,
                               t_match = 2050, append = FALSE, ...){

  if(append) df_orig <- df

  df <- filter(df, ...)

  df <- filter(df, scenario %in% c(scen_ref, scens), variable %in% vars)

  df_wide <- dcast(df, spatial + temporal + variable + unit ~ scenario)

  switch(method,

         uniform = {
           stop("Sorry, not implemented yet!")
         },

         tapered = {
           df_ref <- filter(df, scenario == scen_ref, variable %in% vars)

           # sanity check: reference scenario must be unique with regard to source and model
           if(dim(unique(df["source_id", "model"]))[1] != 1){
             stop("Multiple sources and/or models for reference scenario!")
           }

           # data to be harmonized
           df <- filter(df, scenario %in% scens, variable %in% vars)

           # harmonize spatial resolution
           regions <- intersect(df_ref$spatial, df$spatial) #common spatial entities
           df_ref <- filter(df_ref, spatial %in% regions)
           df <- filter(df, spatial %in% regions)

           # harmonize temporal resolution
           periods <- intersect(df_ref$temporal, df$temporal) #common periods
           df_ref <- filter(df_ref, temporal %in% periods)
           df <- filter(df, temporal %in% periods)

           # order data frames
           df_ref <- arrange(df_ref, source_id, model, scenario, spatial, temporal, variable, unit)
           df <- arrange(df, source_id, model, scenario, spatial, temporal, variable, unit)

           # drop column no longer needed from reference data
           df_ref <- select(df_ref, spatial, temporal, variable, unit, value)
           df_ref <- rename(df_ref, ref = value)
           df <- left_join(df, df_ref)

           df <- group_by(df, source_id, model, scenario, spatial, temporal, variable, unit) %>%
             mutate(scaling_factor = ref/value) %>% ungroup()

           df[df$temporal > c(t_zero), "scaling_factor"] <- NA
           df[df$temporal > t_match, "scaling_factor"] <- 1

           df <- group_by(df, source_id, model, scenario, variable, spatial) %>%
             mutate(scaling_factor = as.numeric(na.approx(zoo(scaling_factor,
                                                              order.by = temporal))),
                    value_n = value * scaling_factor) %>% ungroup() %>%
             mutate(scenario = paste0(scenario, "_harm_", scen_ref))

           df <- select(df, -ref, -scaling_factor, -value) %>% rename(value = value_n)
         },

         offset = {
           stop("Sorry, not implemented yet!")
         },

         {
           stop("No valid harmonization method chosen. Possibilities are
                'uniform', 'tapered' or 'offset'.")
         })

  if(append) df <- rbind(df_orig, df)

  return(df)
}







