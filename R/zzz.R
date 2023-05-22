globals <- utils::globalVariables(c("plot_date", "n", "percent", "fp_text", ":=", "volume", "Sentiment x Time", "Neutral", "Volume x Time", "sentiment", "df", "group", "positive", "neutral", "negative", "Positive", "Negative", "Volume", "group_n"))


get_unit_data <- function(time_unit, variable = "Volume of Mentions", unit = "Count"){
  unit_mapping <- list(
    day = list(
      date_breaks = "1 weeks",
      date_labels = "%d-%m-%y",
      title = paste0(variable, " per Day"),
      yaxis = paste0(unit, " per day")
    ),
    week = list(
      date_breaks = "1 weeks",
      date_labels = "%d-%m-%y",
      title = paste0(variable, " per Week"),
      yaxis = paste0(unit, " per week")
    ),
    month = list(
      date_breaks = "1 months",
      date_labels = "%b-%Y",
      title = paste0(variable, " per Month"),
      yaxis = paste0(unit, " per month")
    ),
    quarter = list(
      date_breaks = "3 months",
      date_labels = "%b-%Y",
      title = paste0(variable, " per Quarter"),
      yaxis = paste0(unit, " per quarter")
    ),
    year = list(
      date_breaks = "1 years",
      date_labels = "%Y",
      title = paste0(variable, " per Year"),
      yaxis = paste0(unit, " per year")
    )
  )

  # Get the values from the list according to the input of time_unit
  unit_data <- unit_mapping[[time_unit]]

  return(unit_data)
}
