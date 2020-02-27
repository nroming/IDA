#' Creates an output file in excel format that can be used as SIAMESE downscaling input
#'
#' @param df, A compatible dataframe
#' @param filename path of the Excel file where to write to (e.g. "test.xlsx")
#' @param source source_id from which scenario should be taken (e.g. "AR5")
#' @param scen Name of the scenario for which to output the data
#' @param scale_scen Name of the scenario whose GDP and population data should
#'   be used for downscaling
#' @param region Region for which the downscaling should be done
#' @param subregions A named list with the name of the list entry being the name
#'   of the subregion and the actual entries of the list being the iso3c codes
#'   of the countries. All countries not explicitely mentioned will end up in
#'   ROR (Rest of Region).
#' @param share Output absolute values or regional shares (default: shares)
#' @return NONE
#' @import openxlsx dplyr
#' @export
write_SIAMESE_input <- function(df, filename, source, scen, scale_scen = "SSP2",
                                region, subregions, share = TRUE){

  tmp <- filter(df, source_id %in% c(source, "SSP", "IEA_2014"))

  # turn the list of countries by subregions into a vectors of country codes
  countries <- filter(map_reg, MESSAGE == region) %>% select(IDA) %>%
                  unlist(use.names = TRUE)

    # variable vector (used for renaming)
  vars <- c("Pop"    = "Population",
            "Y"      = "GDP",
            "Q_bio"  = "Primary Energy|Biomass",
            "Q_coal" = "Primary Energy|Coal",
            "Q_gas"  = "Primary Energy|Gas",
            "Q_oil"  = "Primary Energy|Oil",
            "Q_nuc"  = "Primary Energy|Nuclear",
            "Q_ren"  = "Primary Energy|Non-Biomass Renewables")

  tmp$variable <- gsub("|Total", "", tmp$variable, fixed = TRUE)

  # filter IAM data
  df_iam <- filter(tmp, source_id == source, scenario == scen,
                   spatial == region, variable %in% vars)

  if(!("Primary Energy|Non-Biomass Renewables" %in% unique(df_iam$variable))){
    df_iam_re <- aggregate_variable(filter(tmp, scenario == scen, spatial == region),
                                    new_var <- "Primary Energy|Non-Biomass Renewables",
                                    vars = c("Primary Energy|Wind",
                                             "Primary Energy|Geothermal",
                                             "Primary Energy|Hydro",
                                             "Primary Energy|Solar"))

    df_iam <- rbind(df_iam, df_iam_re)
  }

  # filter IEA energy data
  df_energy <- filter(tmp, source_id == "IEA_2014", variable %in% vars,
                      spatial %in% countries)

  # filter SSP population and GDP data
  df_ssp <- filter(tmp, source_id == "SSP", scenario == scale_scen,
                   variable %in% vars,
                   model %in% c("OECD Env-Growth", "IIASA-WiC POP"),
                   spatial %in% countries)

  # remove temporary data
  rm(tmp)

  # reduce the regional map so that it only contains the region of interest with
  # its countries
  map_reg <- select(map_reg, IDA, MESSAGE) %>% filter(MESSAGE == region)

  # rename variables
  for(v in 1:length(vars)){
    try(df_iam <- rename_var(df_iam, var = vars[v], new_name = names(vars)[v]), silent = TRUE)
    try(df_ssp <- rename_var(df_ssp, var = vars[v], new_name = names(vars)[v]), silent = TRUE)
    try(df_energy <- rename_var(df_energy, var = vars[v], new_name = names(vars)[v]), silent = TRUE)
  }

  # get country/or sub-region level data
  df_country <- rbind(df_ssp, df_energy)
  df_country <- filter(df_country, spatial %in% map_reg$IDA, temporal %in% seq(2010, 2100, 10))

  if(share){
    # we need to compute region totals in order to compute country or subregion
    # shares
    df_region <- group_by(df_country, temporal, variable) %>%
    summarise(total = sum(value, na.rm = TRUE)) %>% ungroup()

    df_country <- inner_join(df_country, df_region) %>%
                    mutate(value = value/total) %>%
                    select(-total)

    reg_values <- rename(df_iam, region = value) %>% select(temporal, variable, region)
    df_country <- inner_join(df_country, reg_values) %>%
                    mutate(value = value * region) %>%
                    select(-region)
  }

  # include another column to categorise the countries based on whether they
  # belong to the subregion of interest or the remainder of the region
  df_country$subregion <- NA

  for(reg in names(subregions)){
    df_country[df_country$spatial %in% subregions[reg][[1]], "subregion"] <- reg
  }

  # # if NAs remain, these are filled with "ROR" - Rest Of Region
  df_country[is.na(df_country$subregion), "subregion"] <- "ROR"

  df_country <- group_by(df_country, temporal, subregion, variable) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>% ungroup()

  df_country <- dcast(df_country, subregion + temporal ~ variable)
  df_country <- rename(df_country, time = temporal)

  # fill up with zeros - just because in the current (2016-05) state, that is what
  # the emulator expects
  df_country[is.na(df_country)] <- 0

  df_iam <- filter(df_iam, temporal %in% seq(2010, 2100, 10))

  df_iam <- dcast(df_iam, temporal ~ variable)
  df_iam <- rename(df_iam, time = temporal)

  # create openxlsx workbook
  wb <- createWorkbook()

  # add worksheet
  addWorksheet(wb, sheetName = "metadata")
  addWorksheet(wb, sheetName = region)

  for(reg in names(subregions)){
    addWorksheet(wb, sheetName = reg)
  }

  addWorksheet(wb, sheetName = "ROR")

  # write metadata
  meta <- data.frame(name = c("scenario", "scaling scenario", "share", "region", "subregions"),
                     item = c(scen, scale_scen, share, region, " "))

  # add subregions
  for(reg in names(subregions)){
    meta <- rbind(meta, data.frame(name = reg,
                                   item = paste(unlist(subregions[reg],
                                                       use.names = FALSE), collapse = ", ")))
  }

  # find countries in ROR
  countries_ror <- setdiff(countries, unlist(subregions, use.names = FALSE))

  # add ROR information
  meta <- rbind(meta, data.frame(name = "ROR", item = paste0(countries_ror, collapse = ", ")))

  writeData(wb, sheet = "metadata", x = meta, colNames = FALSE)

  message <- paste("File generated by", Sys.info()["user"], "on", Sys.info()["nodename"], "at", date())
  writeData(wb, sheet = "metadata", x = message,
            startRow = dim(meta)[1] + 2)

  # write data
  writeData(wb, sheet = region, x = df_iam)

  for(reg in names(subregions)){
    writeData(wb, sheet = reg,
          x = filter(df_country, subregion == reg) %>% select(-subregion))
  }

  writeData(wb, sheet = "ROR", x = filter(df_country, subregion == "ROR") %>% select(-subregion))

  # write out to excel file
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}
