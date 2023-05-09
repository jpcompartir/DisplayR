#' Display Time Series Chart of Data Volume
#'
#' This function generates a time series chart of data volume over the specified time period.
#' The chart is created using the mschart package and is based on the provided data, date column, time period, and date format.
#' data should be unsummarised, as there is counting under the hood, i.e. your data should be in long form and the date column should not already be counted.
#'
#' @param data A data frame containing the data to be plotted.
#' @param date The name of the date column in the data frame.
#' @param time A character vector specifying the time unit to be used for aggregation (default is "day").
#'             Supported values are "day", "week", "month", "quarter", and "year".
#' @param date_format A character string specifying the format for displaying dates on the x-axis (default is "d/m/yy").
#'
#' @return A mschart object representing the time series chart of data volume over the specified time period.
#' @export
#'
disp_ms_vot <-function(data,
                       date,
                       time =c( "week","day", "month", "quarter", "year"),
                       date_format = "d/m/yy"){

  #Make sure our time and plot_type arguments are input correctly
  time <- match.arg(time)

  #Tidy-evaluate ready variables
  date_sym <- rlang::ensym(date)
  date_string <- rlang::as_string(date_sym)

  plotting_data <- data %>%
    dplyr::mutate(plot_date = as.Date({{date}}),
                  plot_date = lubridate::floor_date(x = plot_date, unit = time))

  plotting_data <- plotting_data %>%
    dplyr::count(plot_date)

  chart <- plotting_data %>%
    mschart::ms_linechart(x = "plot_date", y = "n") %>% mschart::chart_settings(style = "line") %>%
    mschart::chart_ax_x(num_fmt = date_format)

  return(chart)
}
