#' Quickly create a bar chart which shows volume over time.
#'
#' You can use the `time_unit` argument to display counts for different time frames, e.g. `time = "day"` will give a chart where each bar represents the volume counts for a day. To give the bars new colours, enter a string into `bar_colour =  `, the string should be a hexcode e.g. "#440154FF" or the name of a colour e.g. "midnightblue"
#'
#' @param data Data Frame or Tibble object
#' @param date Date column
#' @param plot_type Should the plot be shown as a line or a bar chart?
#' @param colour Colour of line/bars - string, name of hexcode
#' @param time_unit select unit e.g. ("day", "week", "month", "quarter", "year")
#' @param smooth Add a smoothing line (geom_smooth) to the plot?
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' DisplayR::disp_example %>% disp_plot_vot(date = date, time_unit = "day")
#' DisplayR::disp_example %>% disp_plot_vot(date = date, colour = "midnightblue", time_unit = "week")
disp_plot_vot <- function(data, date = date, plot_type = c("line", "bar"), colour = "#440154FF", time_unit = c("day", "week", "month", "quarter", "year"), smooth = FALSE){

  #Match the unit and error check
  time_unit <- match.arg(time_unit)
  plot_type <- match.arg(plot_type)

  #Quick error checking, other parts taken care of by match.arg
  stopifnot(
    is.character(colour),
    is.logical(smooth)
)

  #Date variable for tidy evaluate and then error check quickly:
  date_sym <- rlang::ensym(date)
  date_string <- rlang::as_string(date_sym)
  if(!date_string %in% colnames(data)) {stop(paste0("Date variable: ",date_string, " not in data frame's column names."))}

  #Create a list which is dependent on the time_unit input
  unit_mapping <- list(
    day = list(
      date_breaks = "1 weeks",
      date_labels = "%d-%m-%y",
      title = "Daily Volume of Mentions",
      yaxis = "Count per day"
    ),
    week = list(
      date_breaks = "1 weeks",
      date_labels = "%d-%m-%y",
      title = "Weekly Volume of Mentions",
      yaxis = "Count per week"
    ),
    month = list(
      date_breaks = "1 months",
      date_labels = "%b-%Y",
      title = "Monthly Volume of Mentions",
      yaxis = "Count per month"
    ),
    quarter = list(
      date_breaks = "3 months",
      date_labels = "%b-%Y",
      title = "Quarterly Volume of Mentions",
      yaxis = "Count per quarter"
    ),
    year = list(
      date_breaks = "1 years",
      date_labels = "%Y",
      title = "Yearly Volume of Mentions",
      yaxis = "Count per year"
    )
  )

  #Get the right unit mapping value - then call in scale_x_date,  from the list according to the input of time_unit
  unit_data <- unit_mapping[[time_unit]]
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
    ggplot2::scale_x_date(date_breaks = date_breaks,
                          date_labels = date_labels) +
    ggplot2::theme(
                   plot.title.position = "plot",
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(linewidth = 0.5)) +
    ggplot2::labs(x = NULL,
                  y = yaxis,
                  title = title)

  #Add a smoothing bar with standard error aesthetic set to false, should maybeoffer options for colour? Used to do so by feeding ellipsis to geom_smooth, but most users not comfortable enough with ellipsis.
  if(smooth){
    plot <- plot + ggplot2::geom_smooth(se = FALSE)
    return(plot)
  }else{
    return(plot)
  }
}

