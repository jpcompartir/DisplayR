globals <- utils::globalVariables(c("plot_date", "n", "percent", "fp_text", ":=", "volume", "Sentiment x Time", "Neutral", "Volume x Time", "sentiment", "df", "group", "positive", "neutral", "negative", "Positive", "Negative", "Volume", "group_n"))


#' Get unit-specific data for plotting and switch o reduce complexity
#'
#' This function returns a list of values used for configuring the ggplot2 plot
#' depending on the time unit. This includes date breaks, date labels, plot title,
#' and y-axis label. The 'variable' and 'unit' parameters allow for flexibility in
#' describing the data being plotted.
#'
#' @param time_unit A character string specifying the time unit.
#' One of "day", "week", "month", "quarter", or "year".
#' @param variable A character string used to describe the data in the plot title.
#' Default is "Volume of Mentions".
#' @param unit A character string used to describe the unit of the data in the y-axis label.
#' Default is "Count".
#'
#' @return A list with elements 'date_breaks', 'date_labels', 'title', 'yaxis'
#'
#' @examples
#' get_unit_data(time_unit = "day")
#' get_unit_data(time_unit = "week", variable = "Sentiment Analysis", unit = "Score")
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
