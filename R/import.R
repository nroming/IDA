#' Read in data from an PRIMAP excel sheet
#'
#' @param path Path to the excel file
#' @param sheet Name of the sheet to read in
#' @return A compatible dataframe
read_primap_excel_sheet <- function(path, sheet){

  # read in the full sheet
  tmp <- read_excel(path, sheet, col_names = FALSE)

  # get the row where the actual data starts
  first_data_row <- as.integer(tmp[which(tmp[,1] == "SHEET_FIRSTDATAROW"), 2])

  # extract the actual data
  data <- tmp[first_data_row:dim(tmp)[1], ]

  # assign column names
  names(data) <- c("spatial", as.integer(tmp[first_data_row-1, 2:dim(tmp)[2]]))

  # melt the dataframe
  data <- na.omit(melt(data, id.vars = names(data)[1],
                  variable.name = "temporal"))

  # convert from factor to integer
  data$temporal <- as.integer(data$temporal)

  # add variable
  data$variable <- as.character(tmp[which(tmp[,1] == "SHEET_CATEGORY"), 2])

  # add source_id
  data$source_id <- as.character(tmp[which(tmp[,1] == "SHEET_SOURCE"), 2])

  # add model and model version
  data$model <- paste(tmp[1,1], tmp[2,2])

  # add unit
  data$unit <- as.character(tmp[which(tmp[,1] == "SHEET_UNIT"), 2])

  # add scenario
  data$scenario <- as.character(tmp[which(tmp[,1] == "SHEET_SCENARIO"), 2])

  return(data)
}


# read_primap_excel <- function(path){
#   # get all sheet names
#   sheets <- excel_sheets(path)
#
#   # remove 'Sheet1', 'Sheet2', ...
#   sheets <- sheets[!grepl("Sheet", sheets)]
# }

#' Reads MAGICC scen-files
#'
#' @param filename Full path of file to be read in
#' @return An IDA compatible dataframe
#' @import readr reshape2 dplyr
#' @export
read_magicc <- function(filename){
  # details on the data are contained in the first five lines of the scen file
  # see http://wiki.magicc.org/index.php?title=Creating_MAGICC_Scenario_Files&oldid=46
  characteristics <- read_lines(filename, n_max = 5)

  # determine number of timesteps
  n_t <- as.integer(characteristics[1])

  # determine regional resolution
  if(as.integer(characteristics[2]) == 11){
    n_reg <- 1
  } else if(as.integer(characteristics[2]) == 21){
    n_reg <- 5
  } else if(as.integer(characteristics[2]) == 31){
    n_reg <- 6
  } else if(as.integer(characteristics[2]) == 41){
    n_reg <- 7
  } else stop("Unknown regional resolution.")

  # determine scenario name
  scen <- gsub(" ", "", characteristics[3])

  # preallocate empty dataframe
  df <- data.frame()

  # read in variables and units
  # this is done only once since they are the same in the whole file
  variables <- strsplit(read_lines(filename, skip = 7, n_max = 1), split = " ")[[1]]
  variables <- variables[variables != ""]
  variables <- variables[2:length(variables)]
  units <- strsplit(read_lines(filename, skip = 8, n_max = 1), split = " ")[[1]]
  units <- units[units != ""]
  units <- units[2:length(units)]

  units <- data.frame(variable = variables, unit = units)

  skip_count <- 9

  empties <- fwf_empty(filename, skip = skip_count)

  # loop over regions
  for(r in 1:n_reg){
    region <- gsub(" ", "", read_lines(filename, skip = skip_count - 3, n_max = 1))

    tmp <- read_fwf(filename, col_positions = empties, skip = skip_count,
                    n_max = n_t)

    names(tmp)[1] <- "temporal"
    names(tmp)[2:dim(tmp)[2]] <- variables

    tmp$spatial <- region

    tmp <- melt(tmp, id.vars = c("spatial", "temporal"))

    df <- rbind(df, tmp)

    skip_count = skip_count + n_t + 5
  }

  df <- inner_join(df, units)

  df <- mutate(df, scenario = scen,
                   unit = as.character(unit))

  # convert to IDA units
  # CO2
  df <- change_variable(df, old_name = "FossilCO2",
                        new_name = "Emissions|CO2|Fossil fuels and Industry",
                        old_unit = "GtC", new_unit = "Mt CO2/yr", multiplier = (AW$C+AW$O*2)/AW$C*1000)

  df <- change_variable(df, old_name = "OtherCO2",
                        new_name = "Emissions|CO2|Land-Use Change",
                        old_unit = "GtC", new_unit = "Mt CO2/yr", multiplier = (AW$C+AW$O*2)/AW$C*1000)

  # CH4
  df <- change_variable(df, old_name = "CH4",
                        new_name = "Emissions|CH4|Total",
                        old_unit = "MtCH4", new_unit =  "Mt CH4/yr")

  # N20
  df <- change_variable(df, old_name = "N2O",
                        new_name = "Emissions|N20|Total",
                        old_unit = "MtN2O-N", new_unit = "Mt N20/yr",
                        multiplier = (AW$N*2+AW$O)/AW$N)

  # SO2 (SOx)
  df <- change_variable(df, old_name = "SOx",
                        new_name = "Emissions|SO2|Total",
                        old_unit = "MtS", new_unit = "Mt SO2/yr",
                        multiplier = (AW$S + AW$O*2)/AW$S)
  # CO
  df <- change_variable(df, old_name = "CO",
                        new_name = "Emissions|CO|Total",
                        old_unit = "Mt CO", new_unit = "Mt COE/yr")

  # BC
  df <- change_variable(df, old_name = "BC",
                        new_name = "Emissions|BC|Total",
                        old_unit = "Mt", new_unit = "Mt BCE/yr")

  # OC
  df <- change_variable(df, old_name = "OC",
                        new_name = "Emissions|OC|Total",
                        old_unit = "Mt", new_unit = "Mt OCE/yr")

  # NH3
  df <- change_variable(df, old_name = "NH3",
                        new_name = "Emissions|NH3|Total",
                        old_unit = "MtN", new_unit = "Mt NH3/yr",
                        multiplier = (AW$N + AW$H*3)/AW$N)

  # NMVOC
  df <- change_variable(df, old_name = "NMVOC",
                        new_name = "Emissions|VOC|Total",
                        old_unit = "Mt", new_unit = "Mt VOC/yr")

  # NOx
  df <- change_variable(df, old_name = "NOx",
                        new_name = "Emissions|NOx|Total",
                          old_unit = "Mt", new_unit = "Mt NOx/yr")

  # CF4
  df <- change_variable(df, old_name = "CF4",
                        new_name = "Emissions|GHG|CF4",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$CF4 /1000)

  # C2F6
  df <- change_variable(df, old_name = "C2F6", new_name = "Emissions|GHG|C2F6",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$C2F6/1000)

  # C6F14
  df <- change_variable(df, old_name = "C6F14", new_name = "Emissions|GHG|C6F14",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$C6F14/1000)

  # HFC23
  df <- change_variable(df, old_name = "HFC23", new_name = "Emissions|GHG|HFC23",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$HFC23/1000)

  # HFC32
  df <- change_variable(df, old_name = "HFC32", new_name = "Emissions|GHG|HFC32",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$HFC32/1000)

  # HFC43-10
  df <- change_variable(df, old_name = "HFC43-10", new_name = "Emissions|GHG|HFC43-10",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$HFC43-10/1000)

  # HFC125
  df <- change_variable(df, old_name = "HFC125", new_name = "Emissions|HFC|HFC125",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$HFC125/1000)

  # HFC134a
  df <- change_variable(df, old_name = "HFC134a", new_name = "Emissions|HFC|HFC134a",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$HFC134a/1000)

  # HFC143a
  df <- change_variable(df, old_name = "HFC143a", new_name = "Emissions|HFC|HFC143a",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$HFC143a/1000)

  # HFC227ea
  df <- change_variable(df, old_name = "HFC227ea", new_name = "Emissions|HFC|HFC227ea",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$HFC227ea/1000)

  # HFC245fa
  df <- change_variable(df, old_name = "HFC245fa", new_name = "Emissions|HFC|HFC245fa",
                        old_unit = "kt", new_unit = "Mt CO2eq/yr",
                        multiplier = GWP$HFC245fa/1000)

  # SF6
  df <- change_variable(df, old_name = "SF6", new_name = "Emissions|SF6|Total",
                        old_unit = "kt", new_unit = "kt SF6/yr")

 return(df)
}
