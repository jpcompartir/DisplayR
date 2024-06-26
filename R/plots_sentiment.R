#' Create a sentiment distribution bar chart
#'
#' We prefer bars over pie charts or donuts, end.
#'
#' @param data Data Frame or Tibble object
#' @param sentiment_var Variable containing 'positive', 'negative', 'neutral' sentiment classification
#' @param sentiment_colours Colour mapping for the sentiment categories
#' @param bar_labels Data labels for the bars, percent or volume
#' @return ggplot object of sentiment distribution
#' @export
#'
#' @examples {
#' DisplayR::disp_example %>% dr_plot_sent(sentiment_var =
#' sentiment,
#' bar_labels = "percent")
#' }
dr_plot_sent <- function(data,
                         sentiment_var = sentiment,
                         bar_labels = c("percent", "volume", "none"),
                         sentiment_colours = c("positive" = "#107C10", "negative" = "#D83B01", "neutral" = "#FFB900")){

  # input validation ----
  stopifnot(is.character(sentiment_colours),
            is.data.frame(data))

  # #Set bar_labels and raise error if not appropriate
  bar_labels <- match.arg(bar_labels)

  #Get var for tidy eval
  sentiment_sym <- rlang::ensym(sentiment_var)
  sentiment_string <- rlang::as_string(sentiment_sym)
  if(!sentiment_string %in% colnames(data)) {stop(paste0(sentiment_string, " not in data"))}

  # ----

  #Prep data for plot
  data <- data %>%
    dplyr::filter(!!sentiment_sym %in% c("positive", "negative", "neutral", "POSITIVE", "NEGATIVE", "NEUTRAL",
                                           "Neutral", "Negative", "Positive")) %>%
    dplyr::count(!!sentiment_sym) %>%
    dplyr::mutate(!!sentiment_sym := tolower(!!sentiment_sym),
                  percent = n / sum(n) * 100,
                  percent_character = paste0(round(percent, digits = 1), "%"))

  #Initialise plot
  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = !!sentiment_sym, y = n, fill = !!sentiment_sym)) +
    ggplot2::geom_col()

  #Style plot
  plot <- plot +
    ggplot2::theme_minimal(base_family = "Helvetica") +
    ggplot2::theme(
      legend.position = "none",
      plot.title.position = "plot",
      panel.grid.major = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 0.5)) +
    ggplot2::scale_fill_manual(values = sentiment_colours) +
    ggplot2::labs(x = NULL,
                  y = "Count per category")

  # Add labels
  if (bar_labels == "percent") {
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(label = percent_character),
                         colour = "white",
                         position = ggplot2::position_stack(0.9),
                         check_overlap = TRUE
      )
  }
  if (bar_labels == "volume") {
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(label = scales::comma(n)),
                         colour = "white",
                         position = ggplot2::position_stack(0.9),
                         check_overlap = TRUE
      )}

  return(plot)

}


#' Plot sentiment grouped by a certain variable.
#'
#' @param data A data frame that includes the grouping and sentiment variables.
#' @param group_var The variable to group by. Default is "topic".
#' @param sentiment_var The sentiment variable. Default is "sentiment".
#' @param plot_type The type of plot. Default is "percent".
#' @param bar_labels The type of labels to display on bars. Default is "volume".
#' @param sentiment_colours Colour mapping for the sentiment categories
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples {
#' DisplayR::disp_example %>% dr_plot_sent_group(group_var = topic,
#' sentiment_var = sentiment,
#' plot_type = "percent",
#' bar_labels = "volume")
#'
#' DisplayR::disp_example %>% dr_plot_sent_group(group_var = topic,
#'  sentiment_var = sentiment,
#'  plot_type = "volume",
#'  bar_labels = "none")
#' }
dr_plot_sent_group <- function(data,
                            group_var = topic,
                            sentiment_var = sentiment,
                            plot_type = c("percent", "volume"),
                            bar_labels = c( "volume", "none", "percent"),
                            sentiment_colours = c("positive" = "#107C10", "negative" = "#D83B01", "neutral" = "#FFB900")) {

  # Get variables for tidy evalute
  group_sym <- rlang::ensym(group_var)
  sentiment_sym <- rlang::ensym(sentiment_var)

  group_string <- rlang::as_string(group_sym)
  sentiment_string <- rlang::as_string(sentiment_sym)

  # input validation ----
  stopifnot(is.data.frame(data))

  if(!group_string %in% colnames(data)){
    stop(paste0("Cannot find '", group_string, "' in the data frame, are you sure you have entered the correct column name?"))
  }

  if(!sentiment_string %in% colnames(data)){
    stop(paste0("Cannot find '", sentiment_string, "' in the data frame, are you sure you have entered the correct column name?"))
  }

  plot_type <- match.arg(plot_type)
  bar_labels <- match.arg(bar_labels)

  if(!is.character(sentiment_colours)) {
    stop("sentiment_colours = should be a character vector containing the colour mapping for positive, negative and neutral")
  }

  # ----

  #Summarise data for plotting
  data <- data %>%
    dplyr::filter(!is.na(!!sentiment_sym)) %>%
    dplyr::count({{ group_var }}, {{ sentiment_var }}) %>%
    dplyr::add_count({{ group_var }}, wt = n, name = ".total") %>%
    dplyr::mutate(
      percent = n / .total * 100,
      percent_character = paste0(round(percent, digits = 1), "%")
    )

  # Generate the plot based on the chosen type
  plot <- switch(plot_type,
                 "percent" = data %>%
                   ggplot2::ggplot(ggplot2::aes(x = stats::reorder(!!group_sym, n), y = percent, fill = {{ sentiment_var }})) +
                   ggplot2::geom_col() +
                   ggplot2::labs(fill = NULL, y = NULL, x = "% of Posts"),
                 "volume" = data %>%
                   ggplot2::ggplot(ggplot2::aes(x = stats::reorder(!!group_sym, n), y = n, fill = {{ sentiment_var }})) +
                   ggplot2::geom_col() +
                   ggplot2::labs(fill = NULL, y = NULL, x = "Number of Posts")
  )

  #Style the plot
  plot <- plot +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2:: element_blank()) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = sentiment_colours)

  # Add bar labels based on the chosen type
  plot <- switch(bar_labels,
                 "percent" = plot +
                   ggplot2::geom_text(ggplot2::aes(label = percent_character),
                                      colour = "white",
                                      position = ggplot2::position_stack(0.5),
                                      check_overlap = TRUE),
                 "volume" = plot +
                   ggplot2::geom_text(ggplot2::aes(label = scales::comma(n)),
                                      colour = "white",
                                      position = ggplot2::position_stack(0.5),
                                      check_overlap = TRUE),
                 plot
  )
  return(plot)
}


#' Plot a bar or line chart of sentiment over time
#'
#'
#' @param data A data frame that includes the grouping and sentiment variables.
#' @param sentiment_var The sentiment variable. Default is "sentiment".
#' @param date_var The variable to date by. Default is "date".
#' @param sentiment_colours Colour mapping for the sentiment categories
#' @param plot_type The type of plot. Default is "percent".
#' @param time_unit A single unit of time fed into lubridate::floor_date  "week", "day", "month","quarter", "year"
#'
#' @return ggplot object
#' @export
#'
#' @examples {
#' DisplayR::disp_example %>% dr_plot_sent_vot(plot_type = "bar",
#' time_unit = "day")
#' DisplayR::disp_example %>% dr_plot_sent_vot(plot_type = "line",
#' time_unit = "day")
#' }

dr_plot_sent_vot <- function(data,
                             sentiment_var = sentiment,
                             date_var = date,
                             sentiment_colours = c("positive" = "#107C10","negative" = "#D83B01", "neutral" = "#FFB900"),
                             plot_type = c("bar", "line"),
                             time_unit = c("week", "day","month", "quarter", "year")){

  # input validation ----
  stopifnot(is.character(sentiment_colours),
            is.data.frame(data))

  #Error checking and argument setting
  time_unit <- match.arg(time_unit)
  plot_type <- match.arg(plot_type)

  #Tidy evaluate variables
  sent_sym <- rlang::ensym(sentiment_var)
  date_sym <- rlang::ensym(date_var)
  sent_string <- rlang::as_string(sent_sym)
  date_string <- rlang::as_string(date_sym)

  if(!sent_string %in% colnames(data)){
    stop(paste0("Cannot find '", sent_string, "' in the data frame, did you mean `sentiment_var = sentiment`?"))
  }
  if(!date_string %in% colnames(data)){
    stop(paste0("Cannot find '", date_string, "' in the data frame, did you mean `date_var = date`?"))
  }
  # ----

  #Create a list which is dependent on the time_unit input with the vot_unit_data helper function
  unit_data <- vot_unit_data(time_unit = time_unit, vot_variable = "Volume of Sentiment", unit = "count")

  #Get the right unit mapping value - then call in scale_x_date,  from the list according to the input of time_unit
  date_breaks <- unit_data$date_breaks
  date_labels <- unit_data$date_labels
  title <- unit_data$title
  yaxis <- unit_data$yaxis

  #Summarise the data
  data <- data %>% dplyr:: mutate(
    plot_date = as.Date(!!date_sym),
    plot_date = lubridate::floor_date(plot_date, unit = time_unit)
    ) %>%
    dplyr::count(plot_date,!!sent_sym)

  #Initialise plot
  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = !!sent_sym, colour = !!sent_sym))

  plot <- switch(
    plot_type,
    "bar" = plot +
      ggplot2::geom_col(colour = "white"),
    "line" = plot +
      ggplot2::geom_line(linewidth = 1),
    plot
  )

  #Style plot
  plot <- plot +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    ggplot2::scale_fill_manual(aesthetics = c("fill", "colour"),
                               values = sentiment_colours) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(linewidth = 0.5)) +
    ggplot2::labs(y = yaxis,
                  title = title,
                  x = NULL,
                  fill = NULL)

  return(plot)
}
