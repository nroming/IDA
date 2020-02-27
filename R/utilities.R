#' Searches for patterns in a column of a dataframe
#'
#' @param df A compatible dataframe.
#' @param column Name of the column to search in; one of: source_id, model,
#'   scenario, spatial, temporal, variable, unit
#' @param string Searchstring.
#' @param fixed see \code{\link[base]{grep}}
#' @return A vector of matches.
#' @export
search_in_columns <- function(df, column, string, fixed = TRUE){
  return(grep(string, unique(df[, column]), fixed = fixed, value = TRUE))
}

#' Searches for patterns in a column of a dataframe
#'
#' @param df A compatible dataframe.
#' @param column Name of the column to search in; one of: source_id, model,
#'   scenario, spatial, temporal, variable, unit
#' @param string Searchstring (quoted).
#' @param fixed see \code{\link[base]{grep}}
#' @param ignore.case see \code{\link[base]{grep}}
#' @return A vector of matches.
#' @export
search_column <- function(df, column, string, fixed = TRUE,
                          ignore.case = TRUE){
  df <- dplyr::select_(df, gsub('"', '', deparse(substitute(column))))
  df <- distinct(df)

  return(grep(string, df[[1]], fixed = fixed, value = TRUE,
              ignore.case = ignore.case))
}


#' Returns the memory footprint of an object
#'
#' @param x An R object
#' @return None
#' @export
get_size <- function(x){
  return(format(object.size(x), units = "auto"))
}

#' Renames a variable
#'
#' @param df A compatible dataframe
#' @param var Name of the variable to be renamed
#' @param new_name New name of the variable
#' @return A dataframe with the new instead of the old variable name
#' @export
rename_var <- function(df, var, new_name){
  tmp <- filter(df, variable == var)
  df <- filter(df, !(variable == var))

  tmp$variable <- new_name

  df <- rbind(df, tmp)

  return(df)
}


#' Automatic determination of spatial coding used in a dataset.
#'
#' @param df A compatible dataframe
#' @return A dataframe containing the source_id and the spatial coding, see
#'   \code{\link[countrycode]{countrycode}}. If multiple matches for a specific
#'   source are found, all of the are returned.
#' @export
determine_spatial_codes <- function(df){
  # preallocate result dataframe
  result <- data.frame(source_id = unique(df$source_id), code = NA)

  for (id in unique(df$source_id)){
    tmp <- dplyr::filter(df, source_id == id)

    source <- as.character(unique(tmp$spatial))

    matches <- data.frame(code = names(countrycode::countrycode_data),
                          match_count = NA)

    for (col in colnames(countrycode::countrycode_data)){
      target = as.character(countrycode::countrycode_data[[col]])

      matches[matches$code == col, "match_count"] <- length(intersect(source, target))
    }
    result[result$source_id == id, "code"] <- paste(matches[matches$match_count == max(matches$match_count),"code"], collapse = ", ")
  }
  return(result)
}

#' Returns the longest common substring (at the beginning) of a vector of
#' strings Found on
#' \url{http://stackoverflow.com/questions/26285010/r-find-largest-common-substring-starting-at-the-beginning}
#'
#' @param strings A vector of strings
#' @return A character string
get_longest_common_substring <- function(strings){
  strings <- as.character(strings)
  strings.split <- strsplit(strings, '')
  strings.split <- lapply(strings.split, `length<-`, max(nchar(strings)))
  strings.mat <- do.call(rbind, strings.split)
  common.substr.length <- which.max(apply(strings.mat, 2, function(col) !length(unique(col)) == 1)) - 1
  return(substr(strings[1], 1, common.substr.length))
}

#' Returns unique values present for a column
#'
#' @param df A compatible dataframe
#' @param col A column name
#' @param ... further argument passed on to \code{\link[dplyr]{filter}}
#' @return A one-column dataframe containing the unique values of df for the specified column.
#' @export
get_unique <- function(df, column, ...){
  df <- dplyr::filter(df, ...)

  df <- dplyr::select_(df, deparse(substitute(column)))
  df <- unique(df)
  return(df)
}

#' Groups a dataframe by all columns except those in 'non-grouping'
#'
#' This function enhances the functionality of \code{\link[dplyr]{group_by}} by
#' making it possible to group a dataframe by all columns except those mentioned
#' explicitely
#'
#' @param df A compatible dataframe
#' @param non_grouping A vector of column names to be excluded from the grouping
#' @param ... further argument passed on to \code{\link[dplyr]{filter}}
#' @import dplyr
#' @return A compatible dataframe, grouped by all columns except those contained
#'   in 'non_grouping'
#' @export
group_by2 <- function(df, non_grouping = NULL, ...){
  df <- filter(df, ...)

  # never group over value!
  non_grouping <- c(non_grouping, "value")

  grouping <- setdiff(names(df), non_grouping)

  df <- group_by_(df, .dots = grouping)

  return(df)
}

#' Renames variables, changes their units and multiplies them
#'
#'@param df A compatible dataframe
#'@param var Variable which should be renamed
#'@param old_name new_name Old and new name of the variable, respectively
#'@param old_unit new_unit The original and new unit, respectively
#'@param multiplier Value with which the variables value should be multiplied (default = 1)
#'@param replace Append the changed data to the original dataset (default
#'   TRUE)?
#'@return A compatible dataframe in which the respective variable is changed accordingly
#'@import dplyr
#'@export
change_variable <- function(df, old_name, new_name = old_name, old_unit,
                            new_unit = old_unit,
                            multiplier = 1, replace = TRUE){
  # copy original data
  df_orig <- df

  # create apprpriate subset and remove the same data from the original data for
  # later reattachment of changed data
    df <- filter(df, variable == old_name, unit == old_unit)

    # remove old variable
    if(replace){
      df_orig <- filter(df_orig, variable != old_name | unit != old_unit)
    }

  df <- mutate(df, value = value * multiplier, unit = new_unit,
               variable = new_name)

  df <- rbind(df, df_orig)

  return(df)
}

#' Renames scenarios, e.g. to get nicer names for plots
#'
#' @param df A compatible dataframe
#' @param old_name new_name Old and new scenario name, repectively
#' @return A compatible dataframe with the respective scenario changes accordingly
#' @import dplyr
#' @export
change_scenario <- function(df, old_name, new_name){
  # copy original data
  df_orig <- df

  # create apprpriate subset and remove the same data from the original data for
  # later reattachment of changed data
  df <- filter(df, scenario == old_name)
  df_orig <- filter(df_orig, scenario != old_name)

  df <- mutate(df, scenario = new_name)

  df <- rbind(df, df_orig)

  return(df)
}

#' #' Converts data to be compatible with magicc
#' #'
#' #' This function is used for conversion between
#' convert_for_magicc <- function(df, scen, ref_scen){
#'   df <-
#' }
