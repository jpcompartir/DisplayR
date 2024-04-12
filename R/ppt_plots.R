#' Display Time Series Chart of Data Volume
#'
#' This function generates a time series chart of data volume over the specified time period.
#' The chart is created using the mschart package and is based on the provided data, date column, time period, and date format.
#' data should be unsummarised, as there is counting under the hood, i.e. your data should be in long form and the date column should not already be counted.
#'
#' @param data A data frame containing the data to be plotted.
#' @param date The name of the date column in the data frame.
#' @param time A character vector specifying the time unit to be used for aggregation (default is "week").
#'             Supported values are "day", "week", "month", "quarter", and "year".
#' @param date_format A character string specifying the format for displaying dates on the x-axis (default is "d/m/yy").
#'
#' @return A mschart object representing the time series chart of data volume over the specified time period.
#' @export
#'
disp_ms_vot <- function(data,
                        date,
                        time = c("week", "day", "month", "quarter", "year"),
                        date_format = "d/m/yy") {

  # Tidy-evaluate ready variables
  date_sym <- rlang::ensym(date)

  # input validation ----
  time <- match.arg(time)

  if (!tibble::is_tibble(data) && !is.data.frame(data)) {
    stop("Input 'data' must be a tibble or a data frame.")
  }

  if (!rlang::has_name(data, deparse(substitute(date_sym)))) {
    stop("Column specified by 'date = ' not found in 'data'.")
  }

  stopifnot(is.character(date_format)) # I should probably add some more robust tests here
  # ----

  plotting_data <- data %>%
    dplyr::mutate(
      plot_date = as.Date(!!date_sym),
      plot_date = lubridate::floor_date(x = plot_date, unit = time)
    )

  plotting_data <- plotting_data %>%
    dplyr::count(plot_date)

  chart <- plotting_data %>%
    mschart::ms_linechart(x = "plot_date", y = "n") %>%
    mschart::chart_settings(style = "line") %>%
    mschart::chart_data_smooth(values = c(n = 0)) %>%
    mschart::chart_ax_x(num_fmt = date_format)

  return(chart)
}


#' Display Time Series Chart of Grouped Data Volume
#'
#' This function generates a time series chart of grouped data volume over the specified time period.
#' The chart is created using the mschart package and is based on the provided data, date column, time period, and date format.
#' data should be unsummarised, as there is counting under the hood, i.e. your data should be in long form and the date and group column should not already be counted.
#'
#' @param data A data frame containing the data to be plotted.
#' @param date The name of the date column in the data frame.
#' @param group_var The name of the variable you wish to group by, e.g. sentiment or topic.
#' @param time A character vector specifying the time unit to be used for aggregation (default is "week").
#'             Supported values are "day", "week", "month", "quarter", and "year".
#' @param date_format A character string specifying the format for displaying dates on the x-axis (default is "d/m/yy").
#'
#' @return A mschart object representing the time series chart of data volume over the specified time period.
#' @export
#'
disp_ms_vot_grouped <- function(data,
                                date,
                                group_var,
                                time = c("week", "day", "month", "quarter", "year"),
                                date_format = "yyyy/dd/mm") {

  # Tidy-evaluate ready variables
  date_sym <- rlang::ensym(date)
  date_string <- rlang::as_string(date_sym)
  group_sym <- rlang::ensym(group_var)
  group_string <- rlang::as_string(group_sym)


  # input validation ----
  if (!tibble::is_tibble(data) && !is.data.frame(data)) {
    stop("Input 'data' must be a tibble or a data frame.")
  }

  if (!rlang::has_name(data, deparse(substitute(date_sym)))) {
    stop("Column specified by 'date = ' not found in 'data'.")
  }

  if (!rlang::has_name(data, deparse(substitute(group_sym)))) {
    stop("Column specified by 'group_var = ' not found in 'data'.")
  }

  time <- match.arg(time)
  stopifnot(is.character(date_format)) # I should probably add some more robust tests here
  # ----

  plotting_data <- data %>%
    dplyr::mutate(
      !!date_sym := as.Date(!!date_sym),
      !!date_sym := lubridate::floor_date(x = !!date_sym, unit = time)
    )

  plotting_data <- plotting_data %>%
    dplyr::count(!!date_sym, !!group_sym)

  chart <- plotting_data %>%
    mschart::ms_linechart(x = date_string, y = "n", group = group_string) %>%
    mschart::chart_settings(style = "line") %>%
    mschart::chart_ax_x(num_fmt = date_format)

  return(chart)
}


#' Display a Stacked Bar Chart of Sentiment Grouped by a Variable
#'
#' This function takes a data frame, a sentiment variable, and a group variable, and creates a stacked bar chart representing the distribution of sentiment. within each group, with the bars displayed horizontally and the percentage of each sentiment shown.
#'
#' @param data A data frame containing the sentiment and group variables.
#' @param sentiment_var The name of the sentiment variable in the data frame (as a string or unquoted symbol).
#' @param group_var The name of the group variable in the data frame (as a string or unquoted symbol).
#' @param plot_type Where raw volumes or percentages should be plotted.
#'
#' @return A \code{mschart::ms_barchart} object representing the stacked bar chart.
#' @export

disp_ms_sent_grouped <- function(data,
                                 sentiment_var,
                                 group_var,
                                 plot_type = c("percent", "volume")) {

  sentiment_sym <- rlang::ensym(sentiment_var)
  sentiment_string <- rlang::as_string(sentiment_sym)

  group_sym <- rlang::ensym(group_var)
  group_string <- rlang::as_string(group_sym)

  # input validation ----
  if (!tibble::is_tibble(data) && !is.data.frame(data)) {
    stop("Input 'data' must be a tibble or a data frame.")
  }

  if (!rlang::has_name(data, deparse(substitute(sentiment_sym)))) {
    stop("Column specified by 'sentiment_var = ' not found in 'data'.")
  }

  if (!rlang::has_name(data, deparse(substitute(group_sym)))) {
    stop("Column specified by 'group_var = ' not found in 'data'.")
  }

  plot_type <- match.arg(plot_type)
  # ----

  plotting_data <- data %>%
    dplyr::mutate(!!sentiment_sym := tolower(!!sentiment_sym)) %>%
    dplyr::filter(!is.na(!!sentiment_sym)) %>%
    dplyr::count(!!group_sym, !!sentiment_sym) %>%
    dplyr::mutate(percent = n / sum(n), .by = !!group_sym) %>%
    dplyr::mutate(percent = round(percent, 2))

  # browser()

  plot <- plotting_data %>%
    mschart::ms_barchart(x = group_string, y = "n", group = sentiment_string)

  if (plot_type == "percent") {
    plot <- plot %>%
      mschart::as_bar_stack(dir = "horizontal", percent = TRUE)
  } else {
    plot <- plot %>%
      mschart::as_bar_stack(dir = "horizontal", percent = FALSE)
  }

  plot <- plot %>%
    mschart::chart_data_fill(values = c(
      "negative" = "#D83B01",
      "neutral" = "#FFB900",
      "positive" = "#107C10"
    )) %>%
    mschart::chart_data_labels(show_val = TRUE) %>%
    mschart::chart_labels_text(values = officer::fp_text(color = "white"))

  if (plot_type == "percent") {
    plot <- plot %>%
      mschart::chart_labels(ylab = "%", xlab = "Category", title = "Stacked horizontal sentiment distribution")
  } else if (plot_type == "volume") {
    plot <- plot %>%
      mschart::chart_labels(ylab = "n", xlab = "Category", title = "Stacked horizontal sentiment distribution")
  }


  return(plot)
}


#' Add a new slide to a PowerPoint presentation and insert a chart
#'
#' This function creates a new slide with a specified layout and inserts a specified chart into the PowerPoint presentation using the officer package.
#'
#' @param presentation An officer::pptx object representing the PowerPoint presentation.
#' @param chart An mschart::ms_chart object representing the chart to be inserted.
#' @param layout A character string specifying the slide layout to be used (default: "Title and Content").
#' @param master A character string specifying the master slide to be used (default: "Office Theme").
#'
#' @return A modified officer::pptx object with the new slide and chart added.
#' @export
disp_add_slide <- function(presentation,
                           chart,
                           layout = "Title and Content",
                           master = "Office Theme") {

  # input validation ----
  if (!inherits(presentation, "rpptx")) {
    stop("'presentation' must be an officer::pptx object representing the PowerPoint presentation.")
  }

  if (class(chart)[2] != "ms_chart") {
    stop("'chart' must be an mschart::ms_chart object representing the chart to be inserted")
  }

  stopifnot(is.character(layout), is.character(master))
  # ----

  presentation %>%
    officer::add_slide(layout = layout, master = master) %>%
    officer::ph_with(value = chart, location = officer::ph_location_type(type = "body"))
}

#' Create a stacked bar chart for Microsoft's Making My Life Better classification
#'
#' Generates a stacked bar chart using 'mschart' with custom colors for 'yes' and 'no' groups.
#'
#' @param data A dataframe containing the states and their corresponding 'yes' and 'no' counts.
#' @param colours A named vector of colors to apply to the 'yes' groups in the chart.
#' @param state_var The column with the state being plotted (x axis)
#' #'
#' @return An object of class 'ms_barchart' representing the stacked bar chart.
#' @export
#'
#' @examples
#' df <- dplyr::tribble(
#'   ~state, ~yes, ~no,
#'   "Brings me joy", 4861, 167,
#'   "Empowers me", 4715, 313,
#'   "Feels connected", 922, 4106,
#'   "Inspires new ideas", 4906, 122,
#'   "Simplifies tech", 3954, 1074
#' )
#'
#' colours <- c("#0078D4", "#107C10", "#FFB900", "#008575", "#D83B01")

#' chart <- dr_mmlb_barchart(df, colours, state_var = state)
#' print(chart, preview = TRUE)
#'
dr_mmlb_barchart <- function(data,
                             colours,
                             state_var) {

  # input validation ----
  if (!tibble::is_tibble(data) && !is.data.frame(data)) {
    stop("Input 'data' must be a tibble or a data frame.")
  }

  stopifnot(is.character(colours))

  if (!rlang::has_name(data, deparse(substitute(state_var)))) {
    stop("Column specified by 'state_var = ' not found in 'data'.")
  }

  # ----

  state_sym <- rlang::ensym(state_var)  # Convert string to symbol

  # Ensure that the colours vector has names
  state_names <- unique(data[[rlang::as_string(state_sym)]])  # Use double brackets and as_string
  names(colours) <- state_names

  # Pivot the data and prepare for mschart
  data_long <- data %>%
    tidyr::pivot_longer(cols = c("yes", "no"),
                        names_to = "class",
                        values_to = "n") %>%
    dplyr::mutate(group = paste(!!state_sym, class, sep = "_")) %>%
    dplyr::arrange(!!state_sym, dplyr::desc(class)) %>%
    dplyr::mutate(group = factor(group, levels = unique(as.character(group))))

  # Create the fill values dynamically using the colours vector
  yes_colours <- stats::setNames(colours, paste(state_names, "yes", sep = "_"))
  no_colours <- stats::setNames(rep("grey90", length(state_names)), paste(state_names, "no", sep = "_"))
  fill_values <- c(yes_colours, no_colours)

  # Generate the mschart barchart
  chart <- data_long %>%
    mschart::ms_barchart(x = rlang::as_string(state_sym),
                         y = "n",
                         group = "group") %>%
    mschart::as_bar_stack() %>%
    mschart::chart_data_fill(values = fill_values)

  return(chart)
}
