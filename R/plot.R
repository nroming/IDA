#' Plots a single variable as line plot
#'
#' @param df A compatible dataframe
#' @param var Variable to be plotted.
#' @param space Country or region for which variable should be plotted
#' @param time Time periods for which data should be plotted (default: all
#'   contained in df)
#' @param x_label x-axis label (default: year)
#' @param save Save the plot to disk (default: FALSE)
#' @param ... further arguments passed on to \code{\link[ggplot2]{ggsave}} in
#'   case \code{save = TRUE}, especially you need to specify a \code{filename}
#' @return A ggplot plot
#' @import dplyr ggplot2
#' @export
plot_single_var <- function(df, var, space, time = unique(df$temporal),
                            x_label = "year", save = FALSE, ...){

  if(length(var)>1) stop("You must select ONE variable!")

  # create appropriate subset
  df <- filter(df, variable == var, spatial == space, temporal %in% time)

  p <- ggplot(df, aes(x = temporal, y = value, group = scenario, colour = scenario))
  p <- p + geom_line()
  p <- p + ylab(unique(df$unit))
  p <- p + xlab(x_label)

  if(save){
    ggsave(plot = p, ...)
  }

  return(p)
}



#' Prepares generic plot title and variable names from dataframe
#'
#' @param df A compatible dataframe
#' @return A list containing a ggplot2 plot object (accessible via
#'   listname$plot) and a dataframe with modified 'variable' names (accessible
#'   via listname$data)
#' @keywords internal
#' @import ggplot2
prep_plot <- function(df){
  # Extract the title
  title <- get_longest_common_substring(unique(df$variable))

  if(title != ""){
    # shorten variable names accordingly
    df$variable <- gsub(title, "", df$variable, fixed = TRUE)

    # clean the title
    title <- sub("[|]$", "", title)
  } else {
    title = unique(df$variable)
  }

  p <- ggplot()
  p <- p + ggtitle(title)
  p <- p + ylab(unique(df$unit))

  return(list("plot" = p, "data" = df))
}

#' Creates a stacked bar plot
#'
#' @param df A compatible dataframe
#' @param x_axis Which variable should be plotted on the x-axis. Possible
#'   choices are 'scenario' and 'model'.
#' @param save Save the plot to disk (default: FALSE)
#' @param ... ... further arguments passed on to \code{\link[ggplot2]{ggsave}} in
#'   case \code{save = TRUE}, especially you need to specify a \code{filename}
#' @return a ggplot2 plot
#' @import ggplot2
#' @export
plot_stacked_bar <- function(df, x_axis = "scenario", save = FALSE, ...){
  # Sanity checks
  if(length(unique(as.character(df$unit)))>1){
    stop("More than one unit found. Variables seem to be from different
         categories, this is not covered (yet).")
  }

  # prepare plot title, variable names
  prep <- prep_plot(df)

  # prep_plot returns a list
  p <- prep$plot
  df <- prep$data

  switch(x_axis,
         scenario = {
           p <- p + geom_bar(data = df,
                                      aes(x = scenario, y = value,
                                          fill = variable, order = variable), stat = "identity",
                                      position = "stack")
           p <- p + facet_grid(model ~ temporal)
         },
         model = {p <- p + geom_bar(data = df,
                                      aes(x = model, y = value,
                                          fill = variable, order = variable), stat = "identity",
                                      position = "stack")
           p <- p + facet_wrap(scenario ~ temporal)

         }
  )

  p <- p + theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))

  if(save){
    ggsave(plot = p, ...)
  }

  return(p)
}

#' Creates a box plot over time
#'
#' Box plots are grouped by scenarios and by time. Different regions are plotted
#' in separate plots automatically.
#'
#' @param df A compatible dataframe
#' @param save Save the plot to disk (default: FALSE)
#' @param ... ... further arguments passed on to \code{\link[ggplot2]{ggsave}}
#'   in case \code{save = TRUE}, especially you need to specify a
#'   \code{filename}
#' @return a ggplot2 plot
#' @import ggplot2
#' @export
plot_box <- function(df, save = FALSE, ...){
  df$temporal <- as.factor(df$temporal)

    # prepare plot title, variable names
  prep <- prep_plot(df)

  # prep_plot returns a list
  p <- prep$plot
  df <- prep$data

  p <- p + ggeom_boxplot(data = df,
                         aes(x = temporal, y = value,fill = scenario),
                         position=position_dodge(0.9))

  p <- p + facet_wrap(~ spatial)

  if(save){
    ggsave(plot = p, ...)
  }

  return(p)
}

#' Creates (and saves a scatter plot)
#'
#' @param df A compatible dataframe
#' @param x_var Variable to be plotted on the x-axis
#' @param y_axis Variable to be plotted in the y-axis
#' @param save Save the plot to disk (default: FALSE)
#' @param ... ... further arguments passed on to \code{\link[ggplot2]{ggsave}} in
#'   case \code{save = TRUE}, especially you need to specify a \code{filename}
#' @return a ggplot2 plot
#' @import dplyr reshape2 ggplot2
#' @export
plot_scatter <- function(df, x_var, y_var, save = FALSE, ...){
  df <- filter(df, variable %in% c(x_var, y_var), ...)

  # prepare plot title, variable names
  # prep <- prep_plot(df)

  # prep_plot returns a list
  # p <- prep$plot
  # df <- prep$data

  df <- dcast(df, source_id + model + scenario + spatial + temporal ~ variable)

  p <- p + geom_point(data = df,
                      aes(x = x_var, y = y_var, colour = scenario))

  p <- p + facet_wrap(~ spatial)

  if(save){
    ggsave(plot = p, ...)
  }

  return(p)
}
