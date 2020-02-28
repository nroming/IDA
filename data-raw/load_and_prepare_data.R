rm(list = ls())

start.time <- Sys.time()

library(tidyverse)
library(countrycode)
library(reshape2)
library(zoo)
library(openxlsx)
library(readxl)

# load some necessary functions
source("R/import.R")
source("R/utilities.R")

# create necessary directories
if(!dir.exists("data")){
  dir.create("data")
}

# conversions factors -----
# energy (source: https://www.iea.org/statistics/resources/unitconverter/)
conv <- list(Mtoe2PJ = 41.868,
             Mtoe2EJ = 0.041868,
             ktoe2EJ = 0.000041868,
             TWh2EJ = 3600 * 1e-6)

# AR5 global warming potentials ----
# AR5, page 1302
GWP <- list("CO2"       = 1,
            "CH4"       = 21,
            "N2O"       = 310,
            "HFC125"    = 2800,
            "HFC134a"   = 1300,
            "HFC143a"    = 3800,
            "HFC152a"   = 140,
            "HFC227ea"  = 2900,
            "HFC23"     = 11700,
            "HFC236fa"  = 6300,
            "HFC245fa"  = 560,
            "HFC32"     = 650,
            "HFC365mfc" = 1000,
            "HFC43-10"  = 1300,
            "C2F6"      = 9200,
            "C3F8"      = 7000,
            "C4F10"     = 7000,
            "C5F12"     = 7500,
            "C6F14"     = 7400,
            "C7F16"     = 7400,
            "C4F8"      = 8700,
            "CF4"       = 6500,
            "SF6"       = 23900
)

# atomic weights
# http://www.ciaaw.org/atomic-weights.htm
AW <- list("C" = 12.011,
           "H" = 1.008,
           "N" = 14.007,
           "O" = 15.999,
           "S" = 32.06)

## region mapping ----
map_reg <- read_excel("data-raw/mappings.xlsx", sheet = "regions")

idata <- data.frame()
scenarios <- data.frame()

# World Development Indicators -------------------------------------------------
# 2015 edition
cat("Reading in WDI 2015 data.\n")
WDI_2015 <- read_csv(unz("data-raw/WDI_csv_2015.zip", "WDI_Data.csv"))

WDI_vars <- filter(read_excel("data-raw/Variables.xlsx", sheet = "WDI_2015"),
                   !(is.na(variable)))

# merge WDI data with variable selection
WDI_2015 <- left_join(WDI_vars, WDI_2015)

# rename 'Country Code' column to 'wb' to make comparisons with the
# codelist_panel dataframe from the countrycode package more easy
names(WDI_2015)[names(WDI_2015) == "Country Code"] <- "wb"

# only keep country and global values, drop regional aggregates
WDI_2015 <- filter(WDI_2015, wb %in% c(codelist_panel$wb, "WLD"))

# drop columns
WDI_2015 <- select(WDI_2015, variable, is_unit, target_unit, conversion, wb, 9:63)

# rename
WDI_2015 <- rename(WDI_2015, spatial = wb)

# melt dataframe
WDI_2015 <- melt(WDI_2015, id.vars = c("spatial", "variable", "is_unit", "target_unit",
                             "conversion"),
            variable.name = "temporal")

# so far, temporal is a factor, not good
WDI_2015 <- mutate(WDI_2015, temporal = as.numeric(as.character(temporal)))

# drop NAs
WDI_2015 <- filter(WDI_2015, !(is.na(value)))

# harmonize units
WDI_2015 <- group_by(WDI_2015, spatial, variable, is_unit, target_unit, conversion, temporal) %>%
                mutate(value = eval(parse(text = paste(value, conversion)))) %>%
                rename(unit = target_unit) %>% ungroup()

WDI_2015 <- select(WDI_2015, spatial, temporal, variable, unit, value)

WDI_2015 <- mutate(WDI_2015, model = "WDI_2015",
                   scenario = "history",
                   source_id = "WDI_2015")

idata <- bind_rows(idata, WDI_2015)

rm(WDI_2015)

# 2016 edition
cat("Reading in WDI 2016 data.\n")
WDI_2016 <- read_csv(unz("data-raw/WDI_csv_2016.zip", "WDI_Data.csv"))

WDI_vars <- filter(read_excel("data-raw/Variables.xlsx", sheet = "WDI_2016"),
                   !(is.na(variable)))

# merge WDI data with variable selection
WDI_2016 <- left_join(WDI_vars, WDI_2016)

# rename 'Country Code' column to 'wb' to make comparisons with the
# codelist_panel dataframe from the countrycode package more easy
names(WDI_2016)[names(WDI_2016) == "Country Code"] <- "wb"

# only keep country and global values, drop regional aggregates
WDI_2016 <- filter(WDI_2016, wb %in% c(codelist_panel$wb, "WLD"))

# drop columns
WDI_2016 <- select(WDI_2016, variable, is_unit, target_unit, conversion, wb, 9:64)

# rename
WDI_2016 <- rename(WDI_2016, spatial = wb)

# melt dataframe
WDI_2016 <- melt(WDI_2016, id.vars = c("spatial", "variable", "is_unit", "target_unit",
                             "conversion"),
            variable.name = "temporal")

# so far, temporal is a factor, not good
WDI_2016 <- mutate(WDI_2016, temporal = as.numeric(as.character(temporal)))

# drop NAs
WDI_2016 <- filter(WDI_2016, !(is.na(value)))

# harmonize units
WDI_2016 <- group_by(WDI_2016, spatial, variable, is_unit, target_unit, conversion, temporal) %>%
                mutate(value = eval(parse(text = paste(value, conversion)))) %>%
                rename(unit = target_unit) %>% ungroup()

WDI_2016 <- select(WDI_2016, spatial, temporal, variable, unit, value)

WDI_2016 <- mutate(WDI_2016, model = "WDI_2016",
                   scenario = "history",
                   source_id = "WDI_2016")

idata <- bind_rows(idata, WDI_2016)

rm(WDI_2016)

## UN World Population Prospects 2015 ------------------------------------------
cat("Reading in UN World Population Prospects data.\n")
WPP2015 <- read_csv(unz("data-raw/WPP2015_DB02_Populations_Annual.zip",
                        "WPP2015_DB02_Populations_Annual.csv"))

# drop columns not needed/wanted
WPP2015 <- select(WPP2015, -Location, -VarID, -MidPeriod, -GrowthRate,
                  -PopDensity) %>% rename(spatial = LocID, temporal = Time,
                                          scenario = Variant)

# convert UN numerical country codes into ISO 3-character codes
WPP2015 <- mutate(WPP2015, spatial = countrycode(spatial, "un", "iso3c"))

# convert to long dataframe
WPP2015 <- melt(WPP2015, id.vars = c("scenario", "spatial", "temporal"))

# rename variables
WPP2015 <- mutate(WPP2015, variable = gsub("Pop", "Population|", variable),
                           variable = gsub("|Total", "", variable, fixed = TRUE))

# unit conversions
WPP2015 <- mutate(WPP2015, value = value / 1e3,
                  unit = "million",
                  model = "WPP_2015",
                  source_id = "WPP_2015")

idata <- bind_rows(idata, WPP2015)

rm(WPP2015)

## AR5 database ----------------------------------------------------------------
cat("Reading in AR5 data.\n")
AR5 <- read_csv(unz("data-raw/ar5_public_version102_compare_20150629-130000.csv.zip",
                    "ar5_public_version102_compare_compare_20150629-130000.csv"),
                progress = TRUE)

names(AR5) <- tolower(names(AR5))

AR5 <- rename(AR5, spatial = region)

AR5 <- melt(AR5, id.vars = c("model", "scenario", "spatial", "variable", "unit"),
            variable.name = "temporal")

AR5 <- mutate(AR5, value = as.numeric(value))

AR5 <- filter(AR5, !(is.na(value)))

AR5 <- mutate(AR5, source_id = "AR5")

# unit renaming
AR5 <- mutate(AR5, unit = if_else(unit == "US$2005/t CO2", "USD2005/t CO2", unit),
              unit = if_else(unit == "US$2005/t CO2", "USD2005/t CO2", unit),
              unit = if_else(unit == "billion US$2005/yr", "bn USD2005/yr", unit),
              unit = if_else(unit == "?C", "degree C", unit))

# unit conversions
AR5 <- mutate(AR5, value = if_else(unit == "kt N2O/yr", value/1e3, value),
              unit = if_else(unit == "kt N2O/yr", "Mt N2O/yr", unit))

# column transformation
AR5 <- mutate(AR5, temporal = as.integer(as.character(temporal)))

idata <- bind_rows(idata, AR5)

rm(AR5)

# AR5 scenario information
AR5_scens <- read_excel("data-raw/AR5DB_scenario_classification_final.xlsx")

names(AR5_scens) <- tolower(names(AR5_scens))

AR5_scens <- melt(AR5_scens, id.vars = c("model", "scenario"))

AR5_scens <- mutate(AR5_scens, temporal = NA,
                    spatial = "World",
                    source_id = "AR5")

scenarios <- bind_rows(scenarios, AR5_scens)

rm(AR5_scens)

# LIMITS scenario database ----
cat("Reading in LIMITS data.\n")

LIMITS <- read_csv(unz("data-raw/LIMITSPUBLIC_2014-10-13.csv.zip",
                    "LIMITSPUBLIC_2014-10-13.csv"),
                progress = TRUE)

names(LIMITS) <- tolower(names(LIMITS))

LIMITS <- rename(LIMITS, spatial = region)

LIMITS <- melt(LIMITS, id.vars = c("model", "scenario", "spatial", "variable", "unit"),
            variable.name = "temporal")

LIMITS <- mutate(LIMITS, value = as.numeric(value))

LIMITS <- filter(LIMITS, !(is.na(value)))

LIMITS <- mutate(LIMITS, source_id = "LIMITS")

# change unit for GDP based on variable
LIMITS <- mutate(LIMITS, unit = if_else(variable == "GDP|MER", "bn USD2005/yr", unit),
                  unit = if_else(variable == "GDP|PPP", "bn PPP2005/yr", unit))

# unit renaming
LIMITS <- mutate(LIMITS, unit = if_else(unit == "US$2005/t CO2", "USD2005/t CO2", unit),
                 # this is just an addition to the change of units based above
                 # for all variables except GDP (e.g. policy cost)
                 unit = if_else(unit == "billion US$2005/yr", "bn USD2005/yr", unit),
                 unit = if_else(unit == "?C", "degree C", unit))

# unit conversions
LIMITS <- mutate(LIMITS, value = if_else(unit == "kt N2O/yr", value/1e3, value),
                 unit = if_else(unit == "kt N2O/yr", "Mt N2O/yr", unit))

# column type conversion
LIMITS <- mutate(LIMITS, temporal = as.integer(as.character(temporal)))

idata <- bind_rows(idata, LIMITS)

rm(LIMITS)

## AMPERE scenario database ----
cat("Reading in AMPERE data.\n")

AMPERE <- read_csv(unz("data-raw/AmperePublic_WP2+3_2014-10-09.csv.zip",
                    "AmperePublic_WP2+3_2014-10-09.csv"),
                progress = TRUE)

names(AMPERE) <- tolower(names(AMPERE))

AMPERE <- rename(AMPERE, spatial = region)

AMPERE <- melt(AMPERE, id.vars = c("model", "scenario", "spatial", "variable", "unit"),
            variable.name = "temporal")

AMPERE <- mutate(AMPERE, value = as.numeric(value))

AMPERE <- filter(AMPERE, !(is.na(value)))

AMPERE <- mutate(AMPERE, source_id = "AMPERE")

# change unit for GDP based on variable
AMPERE <- mutate(AMPERE, unit = if_else(variable == "GDP|MER", "bn USD2005/yr", unit),
                 unit = if_else(variable == "GDP|PPP", "bn PPP2005/yr", unit))

# unit renaming
AMPERE <- mutate(AMPERE, unit = if_else(unit == "US$2005/t CO2", "USD2005/t CO2", unit),
                 # this is just an addition to the change of units based above
                 # for all variables except GDP (e.g. policy cost)
                unit = if_else(unit == "billion US$2005/yr", "bn USD2005/yr", unit),
                unit = if_else(unit == "?C", "degree C", unit))

# unit conversions
AMPERE <- mutate(AMPERE, value = if_else(unit == "kt N2O/yr", value/1e3, value),
                 unit = if_else(unit == "kt N2O/yr", "Mt N2O/yr", unit))

# column type conversion
AMPERE <- mutate(AMPERE, temporal = as.integer(as.character(temporal)))

idata <- bind_rows(idata, AMPERE)

rm(AMPERE)

## SSP database ----------------------------------------------------------------
cat("Reading in SSP database.\n")
SSP <- read_csv(unz("data-raw/SspDb_country_data_2013-06-12.csv.zip",
                    "SspDb_country_data_2013-06-12.csv"), progress = TRUE)

# load PPP-MER exchange rates
ppp_mer <- read_excel("data-raw/OECD-WB PPP-MER2005_conversion_rates.xlsx",
                      sheet = "OECD-WB PPP-MER Conversionrates")

names(ppp_mer) <- c("spatial", "xr")

names(SSP) <- tolower(names(SSP))

SSP <- rename(SSP, spatial = region)

SSP <- melt(SSP, id.vars = c("model", "scenario", "spatial", "variable", "unit"),
            variable.name = "temporal")

SSP <- mutate(SSP, value = as.numeric(value))

SSP <- na.omit(SSP)

SSP <- mutate(SSP, source_id = "SSP")

# unit conversions
SSP <- mutate(SSP, unit = if_else(unit == "billion US$2005/yr", "bn PPP2005/yr", unit))

# calculate GDP in MER
mer <- filter(SSP, variable == "GDP|PPP")
mer <- inner_join(mer, ppp_mer, by = "spatial")

mer <- mutate(mer, value = value * xr, unit = "bn USD2005/yr",
              variable = "GDP|MER") %>%
          select(-xr)

SSP <- bind_rows(SSP, mer)

rm(mer)

# only keep necessary data:
# scenario group 'SSPX_v9_130325' (replace 'X' with 1 through 5) contains GDP and Population data from the OECD.
# Since they used IIASA total population numbers as input, we only keep the GDP
# numbers from these group of scenarios
# scenario group 'SSPX_v9_130115' (replace 'X' with 1 through 5) contains GDP,
# total population and detailed population structure by sex and education from
# IIASA and urbanizations scenarios from NCAR. We drop the GDP numbers since the
# OECD GDP numbers are the SSP reference scenarios used in the scientific
# community
gdp <- filter(SSP, model == "OECD Env-Growth", variable %in% c("GDP|PPP", "GDP|MER"))
pop <- filter(SSP, model %in% c("NCAR", "IIASA-WiC POP"), !(variable %in% c("GDP|PPP", "GDP|MER")))

SSP <- bind_rows(gdp, pop)

rm(gdp, pop)

# clean scenario names: remove version and submission information
SSP <- mutate(SSP, scenario = substr(scenario, start = 1, stop = 4))

# column type conoverions
SSP <- mutate(SSP, temporal = as.integer(as.character(temporal)))

idata <- bind_rows(idata, SSP)

rm(SSP)

# SSP <- filter(SSP, grepl("130325", SCENARIO))

## EDGAR emissions data --------------------------------------------------------
## TODO: For EDGAR data

# cat("Reading in EDGAR CO2 emissions data")
EDGAR_CO2 <- read_excel("data-raw/EDGARv42FT2012_CO2_excl_biomass_short_and_long.xls",
                    skip = 7)
EDGAR_CH4 <- read_excel("data-raw/EDGARv42FT2012_CH4.xls", skip = 7)
EDGAR_N2O <- read_excel("data-raw/EDGARv42FT2012_N2O.xls", skip = 7)
EDGAR_GHG <- read_excel("data-raw/EDGARv42FT2012_GHG.xls", skip = 8)

# drop the first column containing the country names
EDGAR_CO2 <- select(EDGAR_CO2, 2:dim(EDGAR_CO2)[2])
EDGAR_CH4 <- select(EDGAR_CH4, 2:dim(EDGAR_CH4)[2])
EDGAR_N2O <- select(EDGAR_N2O, 2:dim(EDGAR_N2O)[2])
EDGAR_GHG <- select(EDGAR_GHG, 2:dim(EDGAR_GHG)[2])

# convert to long format
EDGAR_CO2 <- melt(EDGAR_CO2, id.vars = "Country code", variable.name = "temporal")
EDGAR_CH4 <- melt(EDGAR_CH4, id.vars = "Country code", variable.name = "temporal")
EDGAR_N2O <- melt(EDGAR_N2O, id.vars = "Country code", variable.name = "temporal")
EDGAR_GHG <- melt(EDGAR_GHG, id.vars = "Country code", variable.name = "temporal")

# add variable
EDGAR_CO2$variable <- "Emissions|CO2"
EDGAR_CH4$variable <- "Emissions|CH4"
EDGAR_N2O$variable <- "Emissions|N2O"
EDGAR_GHG$variable <- "Emissions|GHG"

# extract units
EDGAR_CO2$unit <- as.character(read_excel("data-raw/EDGARv42FT2012_CO2_excl_biomass_short_and_long.xls")[2, 2])
EDGAR_CH4$unit <- as.character(read_excel("data-raw/EDGARv42FT2012_CH4.xls")[2, 2])
EDGAR_N2O$unit <- as.character(read_excel("data-raw/EDGARv42FT2012_N2O.xls")[2, 2])
EDGAR_GHG$unit <- as.character(read_excel("data-raw/EDGARv42FT2012_GHG.xls")[2, 2])

# extract scenario
EDGAR_CO2$scenario <- as.character(read_excel("data-raw/EDGARv42FT2012_CO2_excl_biomass_short_and_long.xls")[1, 2])
EDGAR_CH4$scenario <- as.character(read_excel("data-raw/EDGARv42FT2012_CH4.xls")[1, 2])
EDGAR_N2O$scenario <- as.character(read_excel("data-raw/EDGARv42FT2012_N2O.xls")[1, 2])
EDGAR_GHG$scenario <- as.character(read_excel("data-raw/EDGARv42FT2012_GHG.xls")[1, 2])

EDGAR <- bind_rows(EDGAR_CO2, EDGAR_CH4, EDGAR_N2O, EDGAR_GHG)

rm(EDGAR_CO2, EDGAR_CH4, EDGAR_N20, EDGAR_GHG)

names(EDGAR)[names(EDGAR) == "Country code"] <- "spatial"

# # unit conversions
# # CO2 emissions
EDGAR[EDGAR$unit == "kton (Gg) CO2 /yr", "value"] <-
  EDGAR[EDGAR$unit == "kton (Gg) CO2 /yr", "value"] / 1e3
EDGAR[EDGAR$unit == "kton (Gg) CO2 /yr", "unit"] <- "Mt CO2/yr"

# CH4 emissions
EDGAR[EDGAR$unit == "kton (Gg) CH4 /yr", "value"] <-
  EDGAR[EDGAR$unit == "kton (Gg) CH4 /yr", "value"] / 1e3
EDGAR[EDGAR$unit == "kton (Gg) CH4 /yr", "unit"] <- "Mt CH4/yr"

# N2O emissions
EDGAR[EDGAR$unit == "kton (Gg) N2O /yr", "value"] <-
  EDGAR[EDGAR$unit == "kton (Gg) N2O /yr", "value"] / 1e3
EDGAR[EDGAR$unit == "kton (Gg) N2O /yr", "unit"] <- "Mt N2O/yr"

# CO2e emissions
EDGAR[EDGAR$unit == "kton (Gg) CO2eq /yr", "value"] <-
  EDGAR[EDGAR$unit == "kton (Gg) CO2eq /yr", "value"] / 1e3
EDGAR[EDGAR$unit == "kton (Gg) CO2eq /yr", "unit"] <- "Mt CO2-equiv/yr"

EDGAR <- na.omit(EDGAR)

# # unit conversions
# CO2 emissions
EDGAR[EDGAR$unit == "kton (Gg) CO2 /yr", "value"] <-
  EDGAR[EDGAR$unit == "kton (Gg) CO2 /yr", "value"] / 1e3
EDGAR[EDGAR$unit == "kton (Gg) CO2 /yr", "unit"] <- "Mt CO2/yr"

# CH4 emissions
EDGAR[EDGAR$unit == "kton (Gg) CH4 /yr", "value"] <-
  EDGAR[EDGAR$unit == "kton (Gg) CH4 /yr", "value"] / 1e3
EDGAR[EDGAR$unit == "kton (Gg) CH4 /yr", "unit"] <- "Mt CH4/yr"

# N2O emissions
EDGAR[EDGAR$unit == "kton (Gg) N2O /yr", "value"] <-
  EDGAR[EDGAR$unit == "kton (Gg) N2O /yr", "value"] / 1e3
EDGAR[EDGAR$unit == "kton (Gg) N2O /yr", "unit"] <- "Mt N2O/yr"

# CO2e emissions
EDGAR[EDGAR$unit == "kton (Gg) CO2eq /yr", "value"] <-
  EDGAR[EDGAR$unit == "kton (Gg) CO2eq /yr", "value"] / 1e3
EDGAR[EDGAR$unit == "kton (Gg) CO2eq /yr", "unit"] <- "Mt CO2-equiv/yr"

EDGAR$model <- "EDGARv4"
EDGAR$source_id <- "EDGARv4"

EDGAR <- na.omit(EDGAR)

# column type conversion
EDGAR <- mutate(EDGAR, temporal = as.integer(temporal))

idata <- bind_rows(idata, EDGAR)

rm(EDGAR)

# WEO 2016 ----
source <- "data-raw/AnnexA_WEO2016.xlsx"

# get sheet names
# sheets <- excel_sheets("data-raw/WEO2015_AnnexA.xls")
sheets <- excel_sheets(source)

# drop sheets that are not needed
sheets <- sheets[!(sheets %in% c("Contents", "Notes"))]

# preallocate dataframe for results
weo_2016 <- data.frame()

for(sheet in sheets){
  x <- read_excel(source, sheet = sheet, col_names = FALSE)

  if(sheet == "Fossil fuel supply"){
    cat("Reading in of 'Fossil fuel supply' not yet implemented. Skipping it for now.")
  } else {
    # detect region
    region <- strsplit(sheet, split = "_")[[1]][[1]]

    # detect sheet type
    type <- strsplit(sheet, split = "_")[[1]][[2]]

    if(type == "Balance"){
      # Find the row numbers where the actual data begins and ends.
      # Unknown how robust this is. 'grep' seems to treat the whole
      # dataframe as a vector
      first_row <- which(x == "Energy demand (Mtoe)", arr.ind = TRUE, useNames = FALSE)[1]
      last_row <- which(x == "Source: World Energy Outlook 2016", arr.ind = TRUE, useNames = FALSE)[1]-1

      # limit the data to that part containing actual useful data
      x <- x[first_row:last_row, ]

      # fill up missing values
      x[1, 2:dim(x)[2]] <- na.locf(t(x[1, 2:dim(x)[2]])) # 'Energy demand (Mtoe)' etc
      x[3, 2:dim(x)[2]] <- na.locf(t(x[3, 2:dim(x)[2]])) # 'New Policies Scenario' etc

      # use first row ('Energy demand (Mtoe)') as column names ...
      colnames(x) <- x[1,]

      # ... and drop the first row since it is not needed anymore
      x <- x[-1, ]

      # drop unneeded columns
      # IMPORTANT: this also drops the second column where the variable name -
      # i.e. 'TPED', 'Coal', ... - is repeated; without intending it
      x <- x[!(colnames(x) %in% c("CAAGR (%)", "Shares (%)"))]

      # now, also the column names are not needed anymore and are replaced with
      # the respective year
      colnames(x) <- x[1, ]

      # drop row with years
      x <- x[-1, ]

      # transpose data
      x <- as.data.frame(t(x), stringsAsFactors = FALSE)

      # use new first row containing 'TPED' etc. as column names ...
      colnames(x) <- as.character(x[1, ])

      # ... and drop it
      x <- x[-1, ]

      # convert all columns except the first one (column name 'NA') into numeric
      x[!(is.na(colnames(x)))] <-
        as.numeric(as.character(unlist(x[!(is.na(colnames(x)))])))

      colnames(x)[is.na(colnames(x))] <- "scenario"

      # define categories
      # each of these has subcategories that should add up, i.e. 'TPED' (Total
      # primary energy demand) has the subcategories 'Coal', 'Oil', 'Gas',
      # 'Nuclear', 'Hydro', 'Bioenergy' and 'Other renewables'
      # the 'World' region does also contain 'Transport|Oil|Bunkers', other
      # regions do not
      if(region == "World"){
      categories <- list("TPED" = c("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                    "Bioenergy", "Other renewables"),
                         "Power generation" = c("Coal", "Oil", "Gas", "Nuclear",
                                                "Hydro", "Bioenergy",
                                                "Other renewables"),
                         "Other energy sector" = "Electricity",
                         "TFC" = c("Coal", "Oil", "Gas", "Electricity", "Heat",
                                   "Bioenergy", "Other renewables"),
                         "Industry" = c("Coal", "Oil", "Gas", "Electricity",
                                        "Heat", "Bioenergy", "Other renewables"),
                         "Transport" = c("Oil",  "Oil|Bunkers",
                                         "Electricity", "Biofuels",
                                         "Other fuels"),
                         "Buildings" = c("Coal", "Oil", "Gas", "Electricity",
                                         "Heat", "Bioenergy", "Other renewables"),
                         "Other" =c())
      } else {
              categories <- list("TPED" = c("Coal", "Oil", "Gas", "Nuclear", "Hydro",
                                    "Bioenergy", "Other renewables"),
                         "Power generation" = c("Coal", "Oil", "Gas", "Nuclear",
                                                "Hydro", "Bioenergy",
                                                "Other renewables"),
                         "Other energy sector" = "Electricity",
                         "TFC" = c("Coal", "Oil", "Gas", "Electricity", "Heat",
                                   "Bioenergy", "Other renewables"),
                         "Industry" = c("Coal", "Oil", "Gas", "Electricity",
                                        "Heat", "Bioenergy", "Other renewables"),
                         "Transport" = c("Oil", "Electricity", "Biofuels",
                                         "Other fuels"),
                         "Buildings" = c("Coal", "Oil", "Gas", "Electricity",
                                         "Heat", "Bioenergy", "Other renewables"),
                         "Other" =c())
      }


      # search and replace shitty "Of which:" occurences
      for (i in 2:length(colnames(x))){
        if(grepl("Of which: ", colnames(x)[i])){
          colnames(x)[i] <- gsub("Of which: ", paste0(colnames(x)[i-1], "|"), colnames(x)[i],   fixed = TRUE)
        }
      }

      # loop over categories
      for (i in 1:length(categories)){
        # get the actual category name
        current_category <- names(categories)[i]

        # find the right column
        num_cat <- grep(paste0("^", current_category, "$"), colnames(x))

        # count the number of subcategories present
        num_sub_cats <-  length(categories[i][[1]])

        # the last category 'Other' has no subcategories
        if(num_sub_cats > 0){
          # join the categories and the subcategories
          colnames(x)[(num_cat+1):(num_cat+num_sub_cats)] <-
            paste(current_category,
                  colnames(x)[(num_cat+1):(num_cat+num_sub_cats)], sep = "|")
        }
      }

      # add 'TFC' to 'Industry', 'Buildings' and 'Transport'
      colnames(x) <- gsub("Industry", "TFC|Industry", colnames(x))
      colnames(x) <- gsub("Transport", "TFC|Transport", colnames(x))
      colnames(x) <- gsub("Buildings", "TFC|Buildings", colnames(x))

      # transform row names into a column containing the temporal information
      x$temporal <- floor(as.numeric(row.names(x)))

      # bring the dataframe into IDA compatible format
      x <- melt(x, id.vars = c("scenario", "temporal"))

      # convert the unit to EJ
      # x <- mutate(x, value = value * conv$Mtoe2EJ)

      # attach spatial information, unit, model, source
      x$spatial <- region
      x$unit <- "Mtoe/yr"
      x$model <- "WEM"
      x$source_id <- "WEO_2016"

      weo_2016 <- bind_rows(weo_2016, x)
    } else if(type == "Elec"){
      # electricity generation
      gen <- x[3:17, ]

      # fill up variable and scenario
      gen[1, 2:dim(gen)[2]] <- t(na.locf(t(gen[1, 2:dim(gen)[2]])))
      gen[3, 2:dim(gen)[2]] <- t(na.locf(t(gen[3, 2:dim(gen)[2]])))

      # use first row as colnames ...
      colnames(gen) <- gen[1, ]

      # ... and drop them
      gen <- gen[-1, ]

      # drop shares and growth rates
      gen <- gen[!(colnames(gen) %in% c("CAAGR (%)", "Shares (%)"))]

      # use first row (containing temporal information) as colnames ....
      colnames(gen) <-gen[1, ]

      # ... and drop it
      gen <- gen[-1, ]

      # transpose
      gen <- as.data.frame(t(gen), stringsAsFactors = FALSE)

      # use first row (containing variables) as colnames ...
      colnames(gen) <- gen[1, ]

      # ... and delete the row
      gen <- gen[-1, ]

      # add temporal column (since they were made unique by e.g. adding '.1' as
      # in '2030.1' we need 'floor'-function)
      gen$temporal <- floor(as.numeric(rownames(gen)))

      # add name for scenario column
      colnames(gen)[1] <- "scenario"

      colnames(gen)[!(colnames(gen) %in% c("scenario", "temporal"))] <-
        paste0("Secondary Energy|Electricity|", colnames(gen)[!(colnames(gen) %in% c("scenario", "temporal"))])

      # melt the data
      gen <- melt(gen, id.vars = c("temporal", "scenario"))

      # add unit
      gen <- mutate(gen, unit = "EJ/yr", value = as.numeric(value) * conv$TWh2EJ)

      # power generation capacity
      cap <- x[19:33, ]

      # fill up variable and scenario
      cap[1, 2:dim(cap)[2]] <- t(na.locf(t(cap[1, 2:dim(cap)[2]])))
      cap[3, 2:dim(cap)[2]] <- t(na.locf(t(cap[3, 2:dim(cap)[2]])))

      # use first row as colnames ...
      colnames(cap) <- cap[1, ]

      # ... and drop them
      cap <- cap[-1, ]

      # also drop empty second column
      cap <- cap[, -2]

      # drop shares and growth rates
      cap <- cap[!(colnames(cap) %in% c("CAAGR (%)", "Shares (%)"))]

      # use first row (containing temporal information) as colnames ....
      colnames(cap) <-cap[1, ]

      # ... and drop it
      cap <- cap[-1, ]

      # transpose
      cap <- as.data.frame(t(cap), stringsAsFactors = FALSE)

      # use first row (containing variables) as colnames ...
      colnames(cap) <- cap[1, ]

      # ... and delete the row
      cap <- cap[-1, ]

      # add temporal column (since they were made unique by e.g. adding '.1' as
      # in '2030.1' we need 'floor'-function)
      cap$temporal <- floor(as.numeric(rownames(cap)))

      # add name for scenario column
      colnames(cap)[1] <- "scenario"

      colnames(cap)[!(colnames(cap) %in% c("scenario", "temporal"))] <-
        paste0("Electricity|Capacity|", colnames(cap)[!(colnames(cap) %in% c("scenario", "temporal"))])

      # melt the data
      cap <- melt(cap, id.vars = c("temporal", "scenario"))

      # add unit
      cap <- mutate(cap, unit = "GW", value = as.numeric(value))

      # electricity CO2 emissions
      # 'World' sheet has different dimensions since it also contains emissions
      # from Bunkers
      if(region == "World"){
        emi <- x[35:51, ]

        # replace tricky entries for 'Oil' demand
        emi[15, 1] <- "Oil|Transport"
        emi[16, 1] <- "Oil|Transport|Bunkers"

      } else {
        emi <- x[35:50,]

        # replace tricky entries for 'Oil' demand
        emi[15, 1] <- "TFC|Oil|Transport"
      }

      # fill up variable and scenario
      emi[1, 2:dim(emi)[2]] <- t(na.locf(t(emi[1, 2:dim(emi)[2]])))
      emi[3, 2:dim(emi)[2]] <- t(na.locf(t(emi[3, 2:dim(emi)[2]])))

      # use first row as colnames ...
      colnames(emi) <- emi[1, ]

      # ... and drop them
      emi <- emi[-1, ]

      # also drop empty second column
      emi <- emi[, -2]

      # drop shares and growth rates
      emi <- emi[!(colnames(emi) %in% c("CAAGR (%)", "Shares (%)"))]

      # use first row (containing temporal information) as colnames ....
      colnames(emi) <-emi[1, ]

      # ... and drop it
      emi <- emi[-1, ]

      # transpose
      emi <- as.data.frame(t(emi), stringsAsFactors = FALSE)

      # use first row (containing variables) as colnames ...
      colnames(emi) <- emi[1, ]

      # ... and delete the row
      emi <- emi[-1, ]

      # add name for scenario column
      colnames(emi)[1] <- "scenario"

      # define categories and subcategories
      # different for 'World' (incl. Bunkers) and regions
      if(region == "World"){
        categories <- list("Total CO2" = c("Coal", "Oil", "Gas"),
                           "Power generation" = c("Coal", "Oil", "Gas"),
                           "TFC" = c("Oil|Transport", "Oil|Transport|Bunkers", "Gas"))
      } else {
        categories <- list("Total CO2" = c("Coal", "Oil", "Gas"),
                           "Power generation" = c("Coal", "Oil", "Gas"),
                           "TFC" = c("Oil|Transport", "Gas"))
      }

       # loop over categories
      for (i in 1:length(categories)){
        # get the actual category name
        current_category <- names(categories)[i]

        # find the right column
        num_cat <- grep(paste0("^", current_category, "$"), colnames(emi))

        # count the number of subcategories present
        num_sub_cats <-  length(categories[i][[1]])

        # the last category 'Other' has no subcategories
        if(num_sub_cats > 0){
          # join the categories and the subcategories
          colnames(emi)[(num_cat+1):(num_cat+num_sub_cats)] <-
            paste(current_category,
                  colnames(emi)[(num_cat+1):(num_cat+num_sub_cats)], sep = "|")
        }
      }

      colnames(emi)[!(colnames(emi) %in% c("scenario", "temporal"))] <-
        paste0("Emissions|CO2|", colnames(emi)[!(colnames(emi) %in% c("scenario", "temporal"))])

      # add temporal column (since they were made unique by e.g. adding '.1' as
      # in '2030.1' we need 'floor'-function)
      emi$temporal <- floor(as.numeric(rownames(emi)))

      # melt the data
      emi <- melt(emi, id.vars = c("temporal", "scenario"))

      # add unit
      emi <- mutate(emi, unit = "Mt CO2/yr", value = as.numeric(value))

      y <- bind_rows(gen, cap, emi)

      # attach spatial information, unit, model, source
      y$spatial <- region
      y$model <- "WEM"
      y$source_id <- "WEO_2016"

      weo_2016 <- bind_rows(weo_2016, y)
    }


  }
}

# harmonize variable names to match those contained in idata
weo_2016 <- mutate(weo_2016, variable = gsub("TPED", "Primary Energy",
                                             variable, fixed = TRUE))
weo_2016 <- mutate(weo_2016, variable = gsub("TFC", "Final Energy",
                                             variable, fixed = TRUE))

# shorten scenario names
weo_2016[weo_2016$scenario == "Current Policies Scenario", "scenario"] <-
  "CPS"
weo_2016[weo_2016$scenario == "New Policies Scenario", "scenario"] <-
  "NPS"
weo_2016[weo_2016$scenario == "450 Scenario", "scenario"] <-
  "450"

weo_2016[weo_2016$unit == "Mtoe/yr", "value"] <- weo_2016[weo_2016$unit == "Mtoe/yr", "value"] * conv$Mtoe2EJ
weo_2016[weo_2016$unit == "Mtoe/yr", "unit"] <- "EJ/yr"

idata <- bind_rows(idata, weo_2016)

rm(weo_2016)

## Joeri's MESSAGE 1p5 scenarios ----
cat("Reading in Joeri's 1.5degree C scenarios from MESSAGE")
untar("data-raw/MES_1.5deg_scenario.tar.xz", compressed = "xz", exdir = "data-raw") # uncompress data
joeris <- read_csv2("data-raw/MES_1.5deg_scenario/MES_1.5deg_scenario/data.csv", na = c("..", "x"))

names(joeris) <- tolower(names(joeris))

joeris <- rename(joeris, spatial = region)

joeris <- melt(joeris, id.vars = c("model", "scenario", "spatial", "variable", "unit"),
            variable.name = "temporal")

joeris$value <- as.numeric(joeris$value)

joeris <- filter(joeris, !(is.na(value)))

joeris$source_id <- "Joeri_1p5"
joeris$model <- "MESSAGE"

# change unit for GDP based on variable
joeris[joeris$variable == "GDP|Total|MER", "unit"] <- "bn USD2005/yr"
joeris[joeris$variable == "GDP|Total|PPP", "unit"] <- "bn PPP2005/yr"

joeris[joeris$variable == "GDP|Total|MER", "variable"] <- "GDP|MER"
joeris[joeris$variable == "GDP|Total|PPP", "variable"] <- "GDP|PPP"

# unit renaming
joeris[joeris$unit == "US$2005/t CO2", "unit"] <- "USD2005/t CO2"
# this is just an addition to the change of units based above for all variables
# except GDP (e.g. policy cost)
joeris[joeris$unit == "billion US$2005/yr", "unit"] <- "bn USD2005/yr"
joeris[joeris$unit == "?C", "unit"] <- "degree C"

# variable renaming
joeris[joeris$variable == "Population|Total", "variable"] <- "Population"

# unit conversions
joeris[joeris$unit == "kt N2O/yr", "value"] <-
  joeris[joeris$unit == "kt N2O/yr", "value"] / 1e3
joeris[joeris$unit == "kt N2O/yr", "unit"] <- "Mt N2O/yr"

# read in MAGICC output for Joeri's scenarios
magicc <- read_excel("data-raw/MES_1.5deg_scenario/MES_1.5deg_scenario/Illustrative_1p5CSCENs_exceedanceProbs.xls",
                     sheet = "MAGICC_output",
                     col_names = c("SCEN-file",
                                   "scenario",
                                   "Exceedance_Max_1p5C",
                                   "Exceedance_2100_1p5C",
                                   "Exceedance_Max_2C",
                                   "Exceedance_2100_2C",
                                   "IIASADB source model",
                                   "IIASADB target model"), skip = 2)


unlink("data-raw/MES_1.5deg_scenario/", recursive = TRUE) # remove uncompressed data

magicc <- select(magicc, scenario, Exceedance_Max_1p5C,
                 Exceedance_2100_1p5C, Exceedance_Max_2C, Exceedance_2100_2C)

magicc <- melt(magicc, id.vars = "scenario")

magicc$spatial <- "World"
magicc$temporal <- NA
magicc$model <- "MESSAGE"
magicc$source_id <- "Joeri_1p5"
magicc$unit <- "-"

magicc[grep("2100", magicc$variable), "temporal"] <- 2100

magicc <- rename_var(magicc, "Exceedance_Max_1p5C",
                     "Temperature|Exceedance Probability|1.5 degC|MAGICC6|Max")
magicc <- rename_var(magicc, "Exceedance_2100_1p5C",
                     "Temperature|Exceedance Probability|1.5 degC|MAGICC6")
magicc <- rename_var(magicc, "Exceedance_Max_2C",
                     "Temperature|Exceedance Probability|2.0 degC|MAGICC6|Max")
magicc <- rename_var(magicc, "Exceedance_2100_2C",
                     "Temperature|Exceedance Probability|2.0 degC|MAGICC6")

# column type conversion
joeris <- mutate(joeris, temporal = as.integer(as.character(temporal)))

idata <- bind_rows(idata, joeris, magicc)

rm(joeris, magicc)

# RCP3 data from the MAGICC wiki ----
# http://wiki.magicc.org/index.php?title=Creating_MAGICC_Scenario_Files&oldid=46
# files are stored in Malte Meinshausen's home directory on the PIK webserver
rcp3pd <- read_magicc("data-raw/RCP3PD.SCEN")
rcp45 <- read_magicc("data-raw/RCP45.SCEN")
rcp6 <- read_magicc("data-raw/RCP6.SCEN")
rcp85 <- read_magicc("data-raw/RCP85.SCEN")

rcp <- bind_rows(rcp3pd, rcp45, rcp6, rcp85)

rm(rcp3pd, rcp45, rcp6, rcp85)

rcp$model <- "MAGICC6"
rcp$source_id <- "RCP"

rcp[rcp$spatial == "WORLD", "spatial"] <- "World"
rcp[rcp$spatial == "R5ASIA", "spatial"] <- "ASIA"
rcp[rcp$spatial == "R5LAM", "spatial"] <- "LAM"
rcp[rcp$spatial == "R5MAF", "spatial"] <- "MAF"
rcp[rcp$spatial == "R5OECD", "spatial"] <- "OECD1990"
rcp[rcp$spatial == "R5REF", "spatial"] <- "REF"

idata <- bind_rows(idata, rcp)

rm(rcp)

## IEA energy data ----
cat("Reading IEA energy data (2014 edition).")
untar("data-raw/IEA_2014.tar.xz", compressed = "xz", exdir = "data-raw") # uncompress IEA data
iea <- read_csv("data-raw/IEA_2014/IEA_2014/IEA2014.csv", na = c("..", "x"))
unlink("data-raw/IEA_2014/", recursive = TRUE) # delete uncompressed IEA data

iea <- mutate(iea, TIME = gsub("2013E", "2013", TIME),
              TIME = as.integer(TIME))

map_reg <- read_excel("data-raw/mappings.xlsx", sheet = "regions")

# primary energy
# some PE types need aggregation
coal <- c("HARDCOAL", "BROWN", "ANTCOAL", "COKCOAL", "BITCOAL", "SUBCOAL",
          "LIGNITE", "PATFUEL", "OVENCOKE", "GASCOKE", "COALTAR", "BKB",
          "GASWKSGS", "COKEOVGS", "BLFURGS", "OGASES", "PEAT", "PEATPROD")
oil <- c("CRNGFEED", "CRUDEOIL", "NGL", "REFFEEDS", "ADDITIVE", "NONCRUDE")
oil_products <- c("REFINGAS", "ETHANE", "LPG", "NONBIOGASO", "AVGAS", "JETGAS",
         "NONBIOJETK", "OTHKERO", "NONBIODIES", "RESFUEL", "NAPHTA", "WHITESP",
         "LUBRIC", "BITUMEN", "PARWAX", "PETCOKE", "ONONSPEC")
renewables <- c("HYDRO", "GEOTHERM", "SOLARPV", "SOLARTH", "TIDE", "WIND")
biomass <- c("MUNWASTER", "PRIMSBIO", "BIOGASES", "BIOGASOL", "BIODIESEL",
             "OBIOLIQ", "RENEWNS", "CHARCOAL")

pe <- filter(iea, FLOW == "TPES", PRODUCT %in% c(coal, oil, oil_products, "NATGAS",
                                                  "NUCLEAR", renewables,
                                                  biomass))

# add variable column which can also be used for aggregation
pe$variable <- NA
pe[pe$PRODUCT %in% coal, "variable"] <- "Primary Energy|Coal"
pe[pe$PRODUCT %in% c(oil, oil_products), "variable"] <- "Primary Energy|Oil"
pe[pe$PRODUCT == "NATGAS", "variable"] <- "Primary Energy|Gas"
pe[pe$PRODUCT == "NUCLEAR", "variable"] <- "Primary Energy|Nuclear"
pe[pe$PRODUCT %in% renewables, "variable"] <- "Primary Energy|Non-Biomass Renewables"
pe[pe$PRODUCT %in% biomass, "variable"] <- "Primary Energy|Biomass"

pe <- group_by(pe, COUNTRY, TIME, variable) %>%
        summarise(value = sum(ktoe, na.rm = TRUE)) %>% ungroup() %>%
        mutate(value = value * conv$ktoe2EJ, unit = "EJ/yr", source_id = "IEA_2014",
               model = "IEA", scenario = "history") %>%
        rename(temporal = TIME, IEA_country = COUNTRY)

# special treatment of nuclear, which is contained in the data with its heat content
pe[pe$variable == "Primary Energy|Nuclear", "value"] <- pe[pe$variable == "Primary Energy|Nuclear", "value"] / 3

pe <- inner_join(pe, select(map_reg, IDA, IEA_country)) %>% select(-IEA_country) %>% rename(spatial = IDA)

idata <- bind_rows(idata, pe)

rm(pe)

# final energy
fe <- filter(iea, FLOW == "TFC", PRODUCT == "TOTAL") %>%
        mutate(variable = "Final Energy|Total",
                ktoe = ktoe * conv$ktoe2EJ,
                unit = "EJ/yr",
                model = "IEA",
                scenario = "history",
                source_id = "IEA_2014") %>%
        select(-PRODUCT, -FLOW) %>%
        rename(value = ktoe, temporal = TIME, IEA_country = COUNTRY)

fe <- inner_join(fe, select(map_reg, IDA, IEA_country)) %>% select(-IEA_country) %>% rename(spatial = IDA)

idata <- bind_rows(idata, fe)

rm(fe)

# coal used for electricity
# no autoproducers (AUTOELEC, AUTOCHP)
coal_elec <- filter(iea, FLOW %in% c("MAINELEC",  "MAINCHP"), PRODUCT %in% coal)

# values are all negative
coal_elec <- group_by(coal_elec, COUNTRY, TIME) %>%
                summarise( value = sum(ktoe, na.rm = TRUE)) %>%
                mutate(value = value  * -conv$ktoe2EJ,
                       variable = "Primary Energy|Coal|w/o CCS|Electricity",
                       unit = "EJ/yr",
                       model = "IEA",
                       scenario = "history",
                       source_id = "IEA_2014") %>%
                rename(temporal = TIME, IEA_country = COUNTRY) %>% ungroup()

coal_elec <- inner_join(coal_elec, select(map_reg, IDA, IEA_country)) %>% select(-IEA_country) %>% rename(spatial = IDA)

# attach to data
idata <- bind_rows(idata, coal_elec)

rm(coal_elec)

# including autoproducers (AUTOELEC, AUTOCHP)
coal_elec1 <- filter(iea, FLOW %in% c("MAINELEC",  "MAINCHP", "AUTOELEC", "AUTOCHP"), PRODUCT %in% coal)

# values are all negative
coal_elec1 <- group_by(coal_elec1, COUNTRY, TIME) %>%
                summarise( value = sum(ktoe, na.rm = TRUE)) %>%
                mutate(value = value  * -conv$ktoe2EJ,
                       variable = "Primary Energy|Coal|w/o CCS|Electricity_1",
                       unit = "EJ/yr",
                       model = "IEA",
                       scenario = "history",
                       source_id = "IEA_2014") %>%
                rename(temporal = TIME, IEA_country = COUNTRY) %>% ungroup()

coal_elec1 <- inner_join(coal_elec1, select(map_reg, IDA, IEA_country)) %>% select(-IEA_country) %>% rename(spatial = IDA)

# attach to data
idata <- bind_rows(idata, coal_elec1)

rm(coal_elec1)

# final energy aggregation to the usual classes
# this mapping is taken from Antoine Levesque's code for PIK's energy demand
# generator (EDGE)
iea_liquid = c("CRUDEOIL", "NGL", "CRNGFEED",
               "BIODIESEL", "NONBIODIES", "OTHKERO",
               "RESFUEL", "AVGAS", "JETGAS",
               "NONBIOJETK", "REFINGAS",
               "PARWAX", "ONONSPEC", "WHITESP", "NAPHTHA",
               "BITUMEN", "LUBRIC", "NONBIOGASO",
               "BIOGASOL", "OBIOLIQ", "COALTAR","REFFEEDS", "NONCRUDE",
               "LPG", "ADDITIVE")
iea_heat = c("HEAT", "GEOTHERM", "SOLARTH")
iea_gas = c("NATGAS", "GASWKSGS", "COKEOVGS","ETHANE",
            "BLFURGS", "OGASES", "BIOGASES")
iea_solid = c("HARDCOAL", "BROWN", "PATFUEL",
              "MUNWASTER", "MUNWASTEN", "INDWASTE",
              "RENEWNS", "BKB", "ANTCOAL", "COKCOAL",
              "BITCOAL", "SUBCOAL", "LIGNITE","PETCOKE",
              "OVENCOKE", "PEAT", "PEATPROD",
              "PRIMSBIO", "CHARCOAL", "OILSHALE",
              "GASCOKE")
iea_elec = c("ELECTR")

iea_agriculture <- c("AGRICULT", "FISHING")
iea_industry <- c("TOTIND")
iea_services <- c("COMMPUB")

iea_fe_by_sector <- filter(iea, FLOW %in% c(iea_agriculture, iea_industry,
                                            iea_services),
                           PRODUCT %in% c(iea_solid, iea_liquid, iea_gas,
                                          iea_heat, iea_elec))

# merge subsectors
iea_fe_by_sector <- mutate(iea_fe_by_sector,
                           FLOW = gsub("FISHING", "AGRICULT", FLOW))

# rename sectors
iea_fe_by_sector[iea_fe_by_sector$FLOW == "AGRICULT", "FLOW"] <- "Agriculture"
iea_fe_by_sector[iea_fe_by_sector$FLOW == "TOTIND", "FLOW"] <- "Industry"
iea_fe_by_sector[iea_fe_by_sector$FLOW == "COMMPUB", "FLOW"] <- "Services"


# replace the FE subtypes
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_solid, "PRODUCT"] <- "Solids"
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_liquid, "PRODUCT"] <- "Liquids"
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_gas, "PRODUCT"] <- "Gases"
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_heat, "PRODUCT"] <- "Heat"
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_elec, "PRODUCT"] <- "Electricity"

# aggregation of FE types by sector, country, year
iea_fe_by_sector <- group_by(iea_fe_by_sector, COUNTRY, TIME, FLOW, PRODUCT) %>%
  summarise(value = sum(ktoe, na.rm = TRUE) * conv$ktoe2EJ) %>%
  rename(temporal = TIME) %>%
  mutate(variable = paste("Final Energy", FLOW, PRODUCT, sep = "|"),
         unit = "EJ/yr",
         source_id = "IEA_2014",
         model = "IEA",
         scenario = "history") %>%
  ungroup()

iea_fe_by_sector <- mutate(iea_fe_by_sector,
                           spatial = countrycode(COUNTRY, "country.name", "iso3c")) %>%
  filter(!is.na(spatial)) %>%
  select(-COUNTRY, -FLOW, -PRODUCT)

idata <- bind_rows(idata, iea_fe_by_sector)

rm(iea_fe_by_sector)

rm(iea, iea_fe_by_sector)

# SSP IAM database ----
untar("data-raw/SSP_DB_2017-03-01.tar.gz", exdir = "data-raw")

SSP_files <- dir("data-raw/SSP_DB_2017-03-01", pattern = "*.xlsx",
                 full.names = TRUE)

ssp_iam <- data.frame()

for(file in SSP_files){
  tmp <- read.xlsx(file, sheet = "data")

  tmp <- select(tmp, -Notes) %>%
    filter(!is.na(Region))

  ssp_iam <- bind_rows(ssp_iam, tmp)

  rm(tmp)
}

unlink("data-raw/SSP_DB_2017-03-01/", recursive = TRUE)

ssp_iam <- melt(ssp_iam,
             id.vars = c("Model", "Scenario", "Region", "Variable", "Unit"),
             variable.name = "temporal")

ssp_iam <- mutate(ssp_iam, temporal = as.integer(as.character(temporal)))

names(ssp_iam) <- tolower(names(ssp_iam))

ssp_iam <- rename(ssp_iam, spatial = region) %>%
  mutate(source_id = "SSP-IAM")

# rename variables and units
ssp_iam <- mutate(ssp_iam,
                  unit = if_else(variable == "GDP|PPP" &
                                   unit == "billion US$2005/yr",
                                 "bn PPP2005/yr", unit))

idata <- bind_rows(idata, ssp_iam)

rm(ssp_iam)

# Rogelj(2018) data ----

cat("Reading in data from Rogelj et al. (2018).\n")

# read in data
rogelj2018 <- read_excel("data-raw/SSP_19_snapshot_NOT4DISTRIBUTION_or_EXTERNAL_USE.xls",
                   sheet = "snapshot", skip = 1)

# lowercase column names make for easier referencing
colnames(rogelj2018) <- tolower(colnames(rogelj2018))

# convert to long dataframe
rogelj2018 <- gather(rogelj2018, key = "temporal", value = "value", 6:25) %>%
  filter(!is.na(value)) %>%
  mutate(temporal = as.integer(temporal),
         source_id = "Rogelj2018") %>%
  rename(spatial = region)

idata <- bind_rows(idata, rogelj2018)

rm(rogelj2018)

## AME database ----
untar("data-raw/AME_DB_2017-10-16.tar.gz", exdir = "data-raw")

AME_files <- dir("data-raw/AME_DB_2017-10-16", pattern = "*.xlsx",
                 full.names = TRUE)

ame <- data.frame()

for(file in AME_files){
  tmp <- read.xlsx(file, sheet = "data")

  tmp <- select(tmp, -Notes) %>%
    filter(!is.na(Region))

  ame <- bind_rows(ame, tmp)

  rm(tmp)
}

unlink("data-raw/AME_DB_2017-10-16/", recursive = TRUE)

ame <- melt(ame,
             id.vars = c("Model", "Scenario", "Region", "Variable", "Unit"),
             variable.name = "temporal")

ame <- mutate(ame, temporal = as.integer(as.character(temporal)),
              value = as.numeric(value)) %>%
  filter(!is.na(value))

names(ame) <- tolower(names(ame))

ame <- rename(ame, spatial = region) %>%
  mutate(source_id = "AME")

idata <- bind_rows(idata, ame)

# ETP 2017 ----
source <- "data-raw/ETP2017_scenario_summary.xlsx"

# get sheet names
# sheets <- excel_sheets("data-raw/WEO2015_AnnexA.xls")
sheets <- excel_sheets(source)

# drop sheets that are not needed
regions <- sheets[!(sheets %in% c("Information", "Graph"))]

# preallocate dataframe for results
etp_2017 <- data.frame()

for(region in regions){
  x <- read_excel(source, sheet = region, col_names = FALSE)

  # remove empty rows
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  # # remove emtpy columns
  # # https://stackoverflow.com/questions/22104962/how-to-remove-empty-columns-in-r#22106157
  # x <- Filter(function(y)!all(is.na(y)), x)

  # fill up missing values
  x[1, ] <- na.locf(t(x[1, ]), na.rm = FALSE)
  x[, 1] <- na.locf(x[, 1], na.rm = FALSE)

  x_rts <- x[2:dim(x)[1], 1:11]
  x_2ds <- x[2:dim(x)[1], c(1:2, 16:24)]
  x_b2ds <- x[2:dim(x)[1], c(1:2, 29:37)]

  rm(x) # not needed anymore

  # use first row, containing years, as column names ...
  colnames(x_rts) <- c("var1", "var2", x_rts[1, 3:11])
  colnames(x_2ds) <- c("var1", "var2", x_2ds[1, 3:11])
  colnames(x_b2ds) <- c("var1", "var2", x_b2ds[1, 3:11])

  # ... and drop it
  x_rts  <- x_rts[-1, ]
  x_2ds  <- x_2ds[-1, ]
  x_b2ds <- x_b2ds[-1, ]

  # add scenario information
  x_rts$scenario  <- "RTS"
  x_2ds$scenario  <- "2DS"
  x_b2ds$scenario <- "B2DS"

  x <- bind_rows(x_rts, x_2ds, x_b2ds)

  rm(x_rts, x_2ds, x_b2ds)

  x <- x[complete.cases(x), ]

  # add spatial information
  x$spatial <- region

  etp_2017 <- bind_rows(etp_2017, x)

  rm(x)
}

etp_2017 <- gather(etp_2017, key = "temporal", value = "value", -scenario,
                   -spatial, -var1, -var2) %>%
  mutate(temporal = as.integer(temporal),
         value = as.numeric(value))

# unit conversion and adding units (with PJ as default as this is most common)
etp_2017$unit <- "PJ"

etp_2017[etp_2017$var1 == "Gross electricity generation (TWh)", "value"] <-
  etp_2017[etp_2017$var1 == "Gross electricity generation (TWh)", "value"] * conv$TWh2EJ
etp_2017[etp_2017$var1 == "Gross electricity generation (TWh)", "unit"] <- "EJ"

etp_2017[etp_2017$var1 == "Gross electricity capacity (GW)", "unit"] <- "GW"
etp_2017[etp_2017$var1 %in% c("Direct CO2 emissions (Mt CO2)", "CO2 captured (Mt CO2)", "BECCS, CO2 captured (Mt CO2)"), "unit"] <- "Mt CO2/yr"

etp_2017[etp_2017$unit == "PJ", "value"] <- etp_2017[etp_2017$unit == "PJ", "value"] / 1000
etp_2017[etp_2017$unit == "PJ", "unit"] <- "EJ"

# rename 'var1'
etp_2017[etp_2017$var1 == "Total primary energy demand (PJ)", "var1"] <- "Primary Energy"

etp_2017[etp_2017$var1 == "Final energy demand (PJ)", "var1"] <- "Final Energy"
etp_2017[etp_2017$var1 == "Final energy demand industry sector (PJ)", "var1"] <- "Final Energy|Industry"
etp_2017[etp_2017$var1 == "Final energy demand transport sector (PJ)", "var1"] <- "Final Energy|Transportation"
etp_2017[etp_2017$var1 == "Final energy demand residential sector (PJ)", "var1"] <- "Final Energy|Residential"
etp_2017[etp_2017$var1 == "Final energy demand services sector (PJ)", "var1"] <- "Final Energy|Commercial"
etp_2017[etp_2017$var1 == "Final energy demand agriculture, fisheries and forestry sector (PJ)", "var1"] <- "Final Energy|Agricultural"
etp_2017[etp_2017$var1 == "Final energy demand non-energy use (PJ)", "var1"] <- "Final Energy|Other Sector" # What is this supposed to be?

etp_2017[etp_2017$var1 == "Gross electricity generation (TWh)", "var1"] <- "Secondary Energy|Electricity"

etp_2017[etp_2017$var1 == "Gross electricity capacity (GW)", "var1"] <- "Capacity|Electricity"

etp_2017[etp_2017$var1 == "Direct CO2 emissions (Mt CO2)", "var1"] <- "Emissions|CO2"
etp_2017[etp_2017$var1 == "CO2 captured (Mt CO2)", "var1"] <- "Emissions|CO2"
etp_2017[etp_2017$var1 == "Direct CO2 emissions (Mt CO2)", "var1"] <- "Emissions|CO2|CCS"
etp_2017[etp_2017$var1 == "BECCS, CO2 captured (Mt CO2)", "var1"] <- "Emissions|CO2|BECCS"

# there is not good match in the IPCC naming scheme for this one without the
# information from 'var2' which can only be added later
etp_2017[etp_2017$var1 == "Fuel input electricity and heat generation (PJ)", "var1"] <- "Primary Energy|Electricity and Heat"

# rename 'var2'
etp_2017[etp_2017$var2 == "Natural gas", "var2"] <- "Gas"
etp_2017[etp_2017$var2 == "Biomass and waste", "var2"] <- "Biomass"
etp_2017[etp_2017$var2 == "Biomass and waste", "var2"] <- "Biomass"
etp_2017[etp_2017$var2 == "Solar PV", "var2"] <- "Solar|PV"
etp_2017[etp_2017$var2 == "Solar CSP", "var2"] <- "Solar|CSP"
etp_2017[etp_2017$var2 == "Coal with CCS", "var2"] <- "Coal|w/ CCS"
etp_2017[etp_2017$var2 == "Natural gas with CCS", "var2"] <- "Gas|w/ CCS"
etp_2017[etp_2017$var2 == "Biomass with CCS", "var2"] <- "Biomass|w/ CCS"
etp_2017[etp_2017$var2 == "Wind onshore", "var2"] <- "Wind|Onshore"
etp_2017[etp_2017$var2 == "Wind offshore", "var2"] <- "Wind|Offshore"
etp_2017[etp_2017$var2 == "Other transformation", "var2"] <- "Other"

etp_2017$variable <- paste(etp_2017$var1, etp_2017$var2, sep = "|")

etp_2017 <- select(etp_2017, -var1, -var2)

# rename regions
etp_2017 <- mutate(etp_2017, spatial = if_else(spatial == "WORLD", "World", spatial))

etp_2017$source_id <- "ETP2017"
etp_2017$model <- "WEM"

idata <- bind_rows(idata, etp_2017)

rm(etp_2017)

# reshuffle the columns
idata <- select(idata, source_id, model, scenario, spatial, temporal, variable,
                unit, value)

# convert column types
# # FIXME: dplyr solution below does not work. Why?
# data <- mutate(data, temporal = as.integer(temporal), variable = as.factor(variable),
#                      unit = as.factor(unit), model = as.factor(model))

## create a tidy dataset-----
# idata_tidy <- dcast(idata,
#                     source_id + model + scenario + spatial + temporal ~ variable)

# variable naming clean-up
idata <- mutate(idata, variable = gsub("\\|Total$", "", variable))

# final manipulations of idata ----
idata$source_id <- as.factor(idata$source_id)
idata$spatial <- as.factor(idata$spatial)
idata$scenario <- as.factor(idata$scenario)
idata$temporal <- as.integer(idata$temporal)
idata$variable <- as.factor(idata$variable)
idata$unit <- as.factor(idata$unit)
idata$model <- as.factor(idata$model)

# variable mapping for translation between SIAMESE and IDA
map_var <- data.frame(SIAMESE = c("p_bio",
                                   "p_coal",
                                   "p_gas",
                                   "p_nuc",
                                   "p_oil",
                                   "p_ren",
                                   "CC",
                                   "I",
                                   "K",
                                   "Q_bio",
                                   "Q_coal",
                                   "Q_gas",
                                   "Q_nuc",
                                   "Q_oil" ,
                                   "Q_ren" ,
                                   "Y",
                                   "TFP" ),

                       IDA     = c("Price Index|Primary Energy|Biomass",
                                   "Price Index|Primary Energy|Coal",
                                   "Price Index|Primary Energy|Gas",
                                   "Price Index|Primary Energy|Nuclear",
                                   "Price Index|Primary Energy|Oil",
                                   "Price Index|Primary Energy|Non-Biomass Renewables",
                                   "Consumption",
                                   "Investment",
                                   "Capital Stock",
                                   "Primary Energy|Biomass",
                                   "Primary Energy|Coal",
                                   "Primary Energy|Gas",
                                   "Primary Energy|Nuclear",
                                   "Primary Energy|Oil",
                                   "Primary Energy|Non-Biomass Renewables",
                                   "GDP|MER",
                                   "TFP"),
                       units =  c("1",
                                  "1",
                                  "1",
                                  "1",
                                  "1",
                                  "1",
                                  "bn USD2005",
                                  "bn USD2005",
                                  "bn USD2005",
                                  "EJ/yr",
                                  "EJ/yr",
                                  "EJ/yr",
                                  "EJ/yr",
                                  "EJ/yr",
                                  "EJ/yr",
                                  "bn USD2005",
                                  "1"),
                       stringsAsFactors = FALSE
)

save(map_reg, file = "data/map_reg.rda")
save(idata, file = "data/idata.rda")
save(scenarios, file = "data/scenarios.rda")
save(AW, file = "data/AW.rda")
save(GWP, file = "data/GWP.rda")
save(map_var, file = "data/map_var.rda")
save(conv, file = "data/conv.rda")

print(Sys.time() - start.time)
