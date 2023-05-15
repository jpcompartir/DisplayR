#' Create a flextable for volume + sentiment with a gradient fill.
#'
#' This function takes a data frame, the grouping variable, and the sentiment variable to create a summary table and display it as a flextable. The table contains the count and percentage of positive, negative, and neutral sentiments per group. The positive and negative columns are filled with a gradient color.
#'
#' The output of this function can be exported directly to PowerPoint.
#'
#' The unique values within the sentiment variable should be c("positive", "negative", "neutral"), the function will try to remove NAs, and lower case the sentiment variable, but if you have values such as 'pos', 'neg', 'neu', you'll need to replace them with the values the function expects.
#'
#' @param data A data frame containing the group variable and sentiment variable.
#' @param group_var The name of the column in the data frame used for grouping.
#' @param sentiment_var The name of the column in the data frame containing sentiment values.
#' @param positive_colour (Optional) The colour for the positive gradient. Default is "#107C10".
#' @param negative_colour (Optional) The colour for the negative gradient. Default is "#D83B01".
#'
#' @return A flextable object displaying the summary table with gradient colouring for positive and negative sentiments.
#' @export
#'
disp_flextable <- function(data, group_var, sentiment_var, positive_colour = "#107C10", negative_colour = "#D83B01") {
  group_sym <- rlang::ensym(group_var)
  sentiment_sym <- rlang::ensym(sentiment_var)

  # Create a summary table with columns group_var, volume, negative, neutral, positive
  summary_table <- data %>%
    # Lower case to keep things simple
    dplyr::mutate({{ sentiment_var }} := tolower(!!sentiment_sym)) %>%
    # Remove NA's at sentiment to keep things simple
    dplyr::filter(!is.na(!!sentiment_sym)) %>%
    # Get counts of sentiment per category in group
    dplyr::count(!!group_sym, !!sentiment_sym) %>%
    # Create a % column and round it for later, group by group_var
    dplyr::mutate(percent = round(100 * n / sum(n), 1), .by = !!group_sym) %>%
    # Get volume from a weighted sum of group n
    dplyr::add_count(!!group_sym, name = "volume", wt = n) %>%
    # Reposition volume for table
    dplyr::relocate(volume, .after = 1) %>%
    # Remove within group sentiment n
    dplyr::select(-n) %>%
    # Pivot for table viz
    tidyr::pivot_wider(
      names_from = {{ sentiment_var }},
      values_from = percent
    ) %>%
    dplyr::arrange(dplyr::desc(volume))

  # Extract the min & max values for positive & negative for colouring
  min_positive <- min(summary_table$positive)
  max_positive <- max(summary_table$positive)
  min_negative <- min(summary_table$negative)
  max_negative <- max(summary_table$negative)

  # Create scales for positive & negative columns
  positive_colourer <- scales::col_numeric(palette = c("transparent", positive_colour), domain = c(min_positive, max_positive))
  negative_colourer <- scales::col_numeric(palette = c("transparent", negative_colour), domain = c(min_negative, max_negative))


  # Reformat the names to be title case
  names(summary_table) <- stringr::str_to_title(names(summary_table))

  # Hacky workaround to add tbales whilst maintaining gradient fill
  total_volume <- scales::comma(sum(summary_table$Volume))
  footer_values <- c("Total", paste0(total_volume), "-", "-", "-")

  # Create the flextable and add the colouring
  ft <- summary_table %>%
    flextable::flextable() %>%
    flextable::bg(
      bg = positive_colourer(summary_table$Positive),
      j = "Positive",
      part = "body"
    ) %>%
    flextable::bg(
      bg = negative_colourer(summary_table$Negative),
      j = "Negative",
      part = "body"
    ) %>%
    # add the total of the volume column as a footer
    flextable::add_body_row(footer_values, colwidths = rep(1, 5), top = FALSE)

  return(ft)
}
