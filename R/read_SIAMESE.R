#' reads in SIAMESE output excel sheet
#'
#' This function reads in SIAMESE output in excel format. Some manual edits
#' might be necessary, depending on the version of SIAMESE. For instance, each
#' column in the SIAMESE excel needs an entry in each cell of the firts row -
#' this needs to be added prior to attempted read-in manually.
#'
#' @param path Path to the excel sheet
#' @param region IAM region which was downscaled using SIAMESE
#' @param scen IAM scenario
#' @param sheet_name Excel sheet which actually contains the output data
#' @param model Model name which is included in the output file
#' @param project Project name, is used as "source_id"
#' @return An IDA compatible dataframe
#' @import openxlsx dplyr reshape2
#' @export
read_SIAMESE <- function(path, region, scen, sheet_name = "Sheet1",
                         model = "SIAMESE",
                         project = "unknown"){

  tmp <- read.xlsx(path, sheet = sheet_name, colNames = FALSE)

  tmp <- as.data.frame(t(tmp), stringsAsFactors = FALSE)

  # replace zeros with NAs
  tmp[tmp == 0] <- NA

  names(tmp) <- c("variable", "spatial", tmp[1, 3:dim(tmp)[2]])

  tmp <- tmp[-1, ]

  # only keep that part of the dataframe where there is some data in the second
  # (region) column
  tmp <- filter(tmp, !is.na(spatial))

  # remove columns containing only NA (sometimes the third row in the xlsx
  # contains zeros instead of NAs)
  tmp <- tmp[, colSums(is.na(tmp)) != nrow(tmp)]

  # remove NAs in first column
  tmp <- mutate(tmp, variable = na.locf(variable))

  tmp <- melt(tmp, id.vars = c("spatial", "variable"), variable.name = "temporal")

  tmp <- mutate(tmp, temporal = as.integer(as.character(temporal)),
                value = as.numeric(value))

  tmp$unit <- NA

  for(var in unique(map_var$SIAMESE)){
    tmp[tmp$variable == var, "unit"] <- map_var[map_var$SIAMESE == var, "units"]
    tmp$variable <- gsub(var, map_var[map_var$SIAMESE == var, "IDA"], tmp$variable, fixed = TRUE)
  }

  tmp[tmp$spatial == "Rest", "spatial"] <- paste0("ROR_", region)
  tmp$scenario = scen
  tmp$model = model
  tmp$source_id = project

  return(tmp)
}
