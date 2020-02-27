#' Writes a customizable excel file
#'
#' @param df A compatible dataframe
#' @param file File to write to.
#' @param sheet Name of the sheet where data should be written to
#' @param vars Variables to extract and write out
#' @param time Time for which information should be written out
#' @param space Countries or regions for which to write out data
#' @return NONE
#' @import openxlsx dplyr
#' @export
write_excel <- function(df, filename, sheet_name = "data",
                        vars = unique(df$variable), time = 1990:2030,
                            space = unique(df$spatial)){

  # create styles
  # excel colours from this site: http://dmcritchie.mvps.org/excel/colors.htm
  # the blue below is colour 49 as in the default excel styles
  h1 <- createStyle(fontSize = 15, fontColour = "#003366",
                  textDecoration = "Bold", border = "Bottom",
                  borderColour = "#003366", borderStyle = "medium")

  h2 <- createStyle(fontSize = 13, fontColour = "#003366",
                  textDecoration = "Bold", border = "Bottom",
                  borderColour = "#003366", borderStyle = "medium")


  # create appropriate subset
  df <- filter(df, variable %in% vars, temporal %in% time, spatial %in% space)

  # convert to wide format
  df <- dcast(df, variable + source_id + model + scenario + spatial + unit ~ temporal)

  # create openxlsx workbook
  wb <- createWorkbook()

  # add worksheet
  addWorksheet(wb, sheetName = sheet_name)

  # initialize row counter
  row_count <- 1

  for(v in unique(df$variable)){
    tmp <- filter(df, variable == v) %>% select(-variable)

    # order by spatial information
    tmp <- arrange(tmp, spatial)

    # write variable name
    writeData(wb, sheet = sheet_name, x = v, startRow = row_count)
    addStyle(wb, sheet = sheet_name, style = h1, row = row_count, cols = 1:dim(tmp)[2])

    # increment row count
    row_count <- row_count + 1

    # write data
    writeData(wb, sheet = sheet_name,  x = tmp, startRow = row_count)
    addStyle(wb, sheet = sheet_name, style = h2, row = row_count, cols = 1:dim(tmp)[2])

    # increment row count (add one extra row of spacing)
    row_count <- row_count + dim(tmp)[1] + 2
  }

  # write out to excel file
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}

#' Writes a csv file in standard IDA format
#'
#' Takes an IDA compatible dataframe and writes it to disk as csv. As default,
#' the whole dataframe is written out.
#'
#' @param df, A compatible dataframe
#' @param filename, file to write the file to
#' @param format "long" or "wide"?
#' @param ... further arguments passed on to \code{\link[dplyr]{filter}}
#' @return None
#' @import dplyr readr reshape2
#' @export
write_IDA_csv <- function(df = idata, filename, format = "long", ...){

  df <- filter(df, ...)

  switch(format,
          "long" = {
            df <- df
          },
          "wide" = {
            df <- dcast(df, source_id + model + scenario + spatial + variable + unit ~ temporal)
          }
  )

  if(interactive() == TRUE){
    if(dim(df)[1] > 10^4){
      divisor <- 1000
      if(dim(df)[1] > 10^5){
        divisor <- 10000
      }

      df_sample <- sample_n(df, size = dim(df)[1] %/% divisor)

      test <- paste(capture.output(write.csv(df_sample)), collapse="\n")

      cat("Writing", capture.output(print(object.size(test)*divisor,
                                          units="auto")), "to disk. \n")
    }
  }

  write_csv(df, path = filename)
}


#' Writes a xlsx file in standard IDA format
#'
#' Takes an IDA compatible dataframe and writes it to disk as xlsx. As default,
#' the whole dataframe is written out.
#'
#' @param df, A compatible dataframe
#' @param filename, file to write the file to
#' @param format "long" or "wide"?
#' @param filter Shall Excel-filters  already be included (default: TRUE)
#' @param ... further arguments passed on to \code{\link[dplyr]{filter}}
#' @return None
#' @import dplyr openxlsx reshape2
#' @export
write_IDA_xlsx <- function(df = idata, filename, format = "long", filter = TRUE,...){
  df <- filter(df, ...)

  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "data")

  switch(format,
         "long" = {
           df <- df
           writeData(wb, "data", x = df, withFilter = filter)
         },
         "wide" = {
           df <- dcast(df, source_id + model + scenario + spatial + variable + unit ~ temporal)

           # No filter should ever be added to the year columns in wide format.
           # Therefore the process of writing the data to the worksheet is split
           # in two.
           # First phase: write data with optional filter
           writeData(wb, "data", x = select(df, source_id, model, scenario,
                                            spatial, variable, unit),
                     withFilter = filter)
           # Second phase: write year columns without filter
           # ATTENTION: harcoded start column
           writeData(wb, "data", x = select(df, -source_id, -model, -scenario,
                                            -spatial, -variable, -unit),
                     startCol = 7, withFilter = FALSE)
         }
        )
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}
