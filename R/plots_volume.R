#' Quickly create a bar chart which shows volume over time.
#'
#' You can use the `time_unit` argument to display counts for different time frames, e.g. `time = "day"` will give a chart where each bar represents the volume counts for a day. To give the bars new colours, enter a string into `bar_colour =  `, the string should be a hexcode e.g. "#440154FF" or the name of a colour e.g. "midnightblue"
#'
#' @param data Data Frame or Tibble object
#' @param date_var Date column
#' @param plot_type Should the plot be shown as a line or a bar chart?
#' @param colour Colour of line/bars - string, name of hexcode
#' @param time_unit select unit e.g. ("day", "week", "month", "quarter", "year")
#' @param smooth Add a smoothing line (geom_smooth) to the plot?
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' DisplayR::disp_example %>% dr_plot_vot(date = date, time_unit = "day")
#' DisplayR::disp_example %>% dr_plot_vot(date = date, colour = "midnightblue", time_unit = "week")
dr_plot_vot <- function(data, date_var = date, plot_type = c("line", "bar"), colour = "#440154FF", time_unit = c("day", "week", "month", "quarter", "year"), smooth = FALSE) {

  #Match the unit and error check
  time_unit <- match.arg(time_unit)
  plot_type <- match.arg(plot_type)

  #Quick error checking, other parts taken care of by match.arg
  stopifnot(
    is.character(colour),
    is.logical(smooth)
)

  #Date variable for tidy evaluate and then error check quickly:
  date_sym <- rlang::ensym(date_var)
  date_string <- rlang::as_string(date_sym)
  if(!date_string %in% colnames(data)) {
    stop(paste0("Date variable: ", date_string, " not in data frame's column names."))
    }

  #Create a list which is dependent on the time_unit input with the vot_unit_data helper function
  unit_data <- vot_unit_data(time_unit = time_unit, vot_variable = "Volume of Mentions", unit = "count")

  #Get the right unit mapping value - then call in scale_x_date,  from the list according to the input of time_unit
  date_breaks <- unit_data$date_breaks
  date_labels <- unit_data$date_labels
  title <- unit_data$title
  yaxis <- unit_data$yaxis

  #Bin the date variable
  data <- data %>% dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = time_unit))

  #Count the data and start the plotting process
  plot <- data %>%
    dplyr::count(plot_date) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n))

  #Render a line chart if asked to do so, if not a bar chart
  if(plot_type == "line"){
    plot <- plot +
      ggplot2::geom_line(colour = colour, linewidth = 1)
  } else {
    plot <- plot +
      ggplot2::geom_col(fill = colour)
  }

  #Style the plot
  plot <- plot +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    ggplot2::theme(
                   plot.title.position = "plot",
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(linewidth = 0.5)
                   ) +
    ggplot2::labs(
      x = NULL,
      y = yaxis,
      title = title
      )

  #Add a smoothing bar with standard error aesthetic set to false, should maybeoffer options for colour? Used to do so by feeding ellipsis to geom_smooth, but most users not comfortable enough with ellipsis.
  if(smooth){
    plot <- plot + ggplot2::geom_smooth(se = FALSE)
    return(plot)
  }else{
    return(plot)
  }
}


#' Quickly make a line plot of volume over time for a grouping variable
#'
#' @param data Data frame or tibble
#' @param group_var grouping variable e.g. country, cluster, topic etc.
#' @param date_var Variable which contains date information (can be datetime too I think)
#' @param time_unit A single unit of time fed into lubridate::floor_date  "week", "day", "month","quarter", "year"

#'
#' @return ggplot object
#' @export
dr_plot_vot_group <- function(data, group_var, date_var = date, time_unit = c("day", "week", "month", "quarter", "year")){

  time_unit <- match.arg(time_unit)

  #Get tidy evaluate supplied variables
  date_sym <- rlang::ensym(date_var)
  group_sym <- rlang::ensym(group_var)

  #Create a list which is dependent on the time_unit input with the vot_unit_data helper function
  unit_data <- vot_unit_data(time_unit = time_unit, vot_variable = "Volume of Mentions", unit = "count")

  #Get the right unit mapping value - then call in scale_x_date,  from the list according to the input of time_unit
  date_breaks <- unit_data$date_breaks
  date_labels <- unit_data$date_labels
  title <- unit_data$title
  yaxis <- unit_data$yaxis

  #Prepare date variable
  data <- data %>% dplyr::mutate(
    plot_date = lubridate::floor_date(!!date_sym, unit = time_unit))

  #Get the number of colours for the viridis palette
  n_colours <- data %>% dplyr::pull(!!group_sym) %>% unique() %>% length()
  colours <- viridis::viridis_pal()(n_colours)

  #Set the plot up
  plot <- data %>%
    dplyr::count(plot_date, !!group_sym) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, colour = !!group_sym)) +
    ggplot2::geom_line()

  #Style the plot
  plot <- plot +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 0.5)
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = yaxis,
      colour = NULL)

  return(plot)
}


#' Get unit-specific data for plotting and switch o reduce complexity
#'
#' This function returns a list of values used for configuring the ggplot2 plot
#' depending on the time unit. This includes date breaks, date labels, plot title,
#' and y-axis label. The 'variable' and 'unit' parameters allow for flexibility in
#' describing the data being plotted.
#'
#' @param time_unit A character string specifying the time unit.
#' One of "day", "week", "month", "quarter", or "year".
#' @param unit A character string used to describe the unit of the data in the y-axis label.
#' Default is "Count".
#' @param vot_variable Will form part of plot's title, should describe type of plot
#'
#' @return A list with elements 'date_breaks', 'date_labels', 'title', 'yaxis'
#'
vot_unit_data <- function(time_unit, vot_variable = "Volume of Mentions", unit = "Count"){
  unit_mapping <- list(
    day = list(
      date_breaks = "1 weeks",
      date_labels = "%d-%m-%y",
      title = paste0(vot_variable, " per Day"),
      yaxis = paste0(unit, " per day")
    ),
    week = list(
      date_breaks = "1 weeks",
      date_labels = "%d-%m-%y",
      title = paste0(vot_variable, " per Week"),
      yaxis = paste0(unit, " per week")
    ),
    month = list(
      date_breaks = "1 months",
      date_labels = "%b-%Y",
      title = paste0(vot_variable, " per Month"),
      yaxis = paste0(unit, " per month")
    ),
    quarter = list(
      date_breaks = "3 months",
      date_labels = "%b-%Y",
      title = paste0(vot_variable, " per Quarter"),
      yaxis = paste0(unit, " per quarter")
    ),
    year = list(
      date_breaks = "1 years",
      date_labels = "%Y",
      title = paste0(vot_variable, " per Year"),
      yaxis = paste0(unit, " per year")
    )
  )

  # Get the values from the list according to the input of time_unit
  unit_data <- unit_mapping[[time_unit]]

  return(unit_data)
}
