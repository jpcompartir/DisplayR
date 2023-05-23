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
dr_plot_sent <- function(data, sentiment_var = sentiment, bar_labels = c("percent", "volume", "none"),
                           sentiment_colours = c("positive" = "#107C10", "negative" = "#D83B01", "neutral" = "#FFB900")){


  stopifnot(is.character(sentiment_colours))

  # #Set bar_labels and raise error if not appropriate
  bar_labels <- match.arg(bar_labels)

  #Get var for tidy eval
  sentiment_sym <- rlang::ensym(sentiment_var)
  sentiment_string <- rlang::as_string(sentiment_sym)
  if(!sentiment_string %in% colnames(data)) {stop(paste0(sentiment_string, " not in data"))}

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
    ggplot2::ggplot(aes(x = !!sentiment_sym, y = n, fill = !!sentiment_sym)) +
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
