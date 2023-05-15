#' Create a Summary Table for gt Summary Process
#'
#' This function creates a summary table as one step of the gt summary process. The table summarizes the data based on a group variable and a sentiment variable.
#'
#' @param data The input data frame.
#' @param group_var The name of the variable used for grouping the data.
#' @param sentiment_var The name of the variable representing the sentiment values.
#'
#' @return A data frame containing the summary table.
#' @keywords internal
#'
make_gt_summary_table <- function(data,
                                  group_var,
                                  sentiment_var){
  #Tidy evaluate supplied variables
  group_sym <- rlang::ensym(group_var)
  sent_sym <- rlang::ensym(sentiment_var)


  summary_table <- data %>%
    dplyr::filter(!is.na(!!sent_sym)) %>%
    dplyr::count(!!group_sym,
                 !!sent_sym) %>%
    dplyr::add_count(!!group_sym, wt = n, name = "group_n") %>%
    dplyr::mutate(volume = sum(n),
                  percent = n / sum(n) * 100,
                  .by =  !!group_sym) %>%
    dplyr::mutate(!!sent_sym := tolower(!!sent_sym)) %>% #convert to lower case, can convert back later
    dplyr::arrange(dplyr::desc(group_n)) %>%
    dplyr::select(-n, -group_n) %>%
    tidyr::pivot_wider(names_from = !!sent_sym,
                       values_from = percent)

  return(summary_table)
}

#JP - 11th May, this function is a mess. Too big, too complicated.

#' Create a gt_summary table fit for use in client briefs
#'
#' Function calls various helper functions, @seealso
#' `make_gt_summary_table`. This function prepares a summary of a given dataset
#' grouped by a variable of interest and summarised based on sentiment and time
#' categories. The summary is presented in a 'gt' table format. Additional
#' summary columns may be created with user-specified icons. The table may be
#' customised with a specified title and source note.
#'
#' @param data A data frame.
#' @param sentiment_var The variable in the data frame representing sentiment.
#' @param group_var The variable in the data frame to be used for grouping.
#' @param date_var The variable in the data frame representing the date.
#' @param time_unit Time unit for grouping. Default choices are "month", "day",
#'                  "week", "year", "quarter". Default is "month".
#' @param sentiment_max_colours List of colours for positive and negative sentiments.
#' @param icons Optional vector of icons to be added to the summary table.
#' @param table_title Title of the table. Default is "Test".
#' @param source_note Note to be added at the bottom of the table indicating the source.
#'
#' @return A gt object representing the summary table.
#' @export
#' @examples
#' \dontrun{
#' disp_gt_summary(my_data, "sentiment", "group", "date", "month",
#'                 list("negative" = "#C00000","positive" = "#107C10"),
#'                 NULL, "My Summary", "source_note")
#' }
disp_gt<- function(data,
                            sentiment_var,
                            group_var,
                            date_var,
                            time_unit =
                              c("month","day","week","year","quarter"),
                            sentiment_max_colours = list("negative" = "#C00000","positive" = "#107C10"),
                            icons = NULL,
                            table_title = "Test",
                            source_note = "source_note ="){

  #Match the time unit argument (name of this should be standardised across all functions in package & in other packages for consistency...)
  time_unit <- match.arg(time_unit)

  #Get symbols for tidy evaluate
  date_sym <- rlang::ensym(date_var)
  group_sym <- rlang::ensym(group_var)
  sent_sym <- rlang::ensym(sentiment_var)

  #Get strings from symbols
  sent_string <- rlang::as_string(sent_sym)
  date_string <- rlang::as_string(date_sym)
  group_string <- rlang::as_string(group_sym)

  #Stop if columns not in data frame and provide more informative error messages
  if(!sent_string %in% colnames(data)){
    stop(paste0("Cannot find '", sent_string, "' in the data frame, did you mean `sentiment_var = sentiment`?"))
  }

  if(!date_string %in% colnames(data)){
    stop(paste0("Cannot find '", date_string, "' in the data frame, did you mean `date_var = date`?"))
  }

  if(!group_string %in% colnames(data)){
    if(!group %in% colnames(data)){
      stop(paste0("Cannot find '", group_string, "' in the data frame, specify the correct column with 'group_var = ' `?"))
    }
  }

  #Create the summary table with the make_gt_summary_table function
  summary <- data %>%
    dplyr::filter(!is.na(!!sent_sym)) %>%
  make_gt_summary_table(group_var = !!group_sym,
                          sentiment_var = !!sent_sym)

  #Rename the columns so that they look prettier later on + add blank columns for volume + sentiment x time plots.
  summary <- summary %>%
    dplyr::select({{group_var}}, Volume = volume, Positive = positive, Neutral = neutral, Negative = negative) %>%
    dplyr::mutate(`Volume x Time` = "",
                  `Sentiment x Time` = "")

  # Space for adding icons here
  if(!is.null(icons)){
    dplyr::mutate(summary, icons = icons, .before = 1)
  }

  table <- summary %>%
    gt::gt()

  #There is a chance that group_splitting this way doesn't work if the factors are re-ordered, I think?
  #Generate list of plots for volume and sentiment
  splits <-  dplyr::group_split(data, {{group_var}})
  vol_plot_list <-purrr::map(splits, ~.x %>%
                               disp_gt_vot(date_var = {{date_var}}, time_unit = time_unit))

  sent_plot_list <- purrr::map(splits, ~ .x %>%
                                 disp_gt_sent_time(date_var = {{date_var}}, sentiment_var = {{sentiment_var}}, time_unit = time_unit))

  #Manual implementation isn't pretty but is better than over-complicated implementation for arbitrary # of columns.

  #Extract the min & max values for positive & negative for colouring
  min_positive <- min(summary$Positive)
  max_positive <- max(summary$Positive)
  min_negative <- min(summary$Negative)
  max_negative <- max(summary$Negative)

  positive_colour <- sentiment_max_colours$positive
  negative_colour <- sentiment_max_colours$negative

  #Create scales for positive & negative columns
  positive_colorer <- scales::col_numeric(palette = c("transparent", positive_colour), domain = c(min_positive, max_positive), alpha = 0,75)
  negative_colorer <- scales::col_numeric(palette = c("transparent", negative_colour), domain = c(min_negative, max_negative), alpha = 0.75)

  #Break this up for debugging - the long pipe is no good for people new to function. summary_rows functions differently in gt 0.9, as do cells_grand_summary & cells_stub_grand_summary
  table <- table %>%
    gt::data_color(columns = Positive,
                   fn = positive_colorer) %>%
    gt::data_color(columns = Negative,
                   fn = negative_colorer)

  #Use grand_summary_rows instead of summary_rows as no grouping. Use named list formula syntax in fns = and fmt_number usinf formulate syntax too.
  table <-  table %>%
    gt::grand_summary_rows(
      columns = c(Volume),
      fns = list(Total ~ sum(.)),
      fmt = ~ fmt_number(.x, use_seps = TRUE, decimals = 0))

  #Add the Volume x Time and Sentiment x Time plots (TODO)
  table <- table %>%
    gt::text_transform(
      locations =
        gt::cells_body(columns = `Volume x Time`),
      fn = function(x){
        vol_plot_list %>%
          gt::ggplot_image(height = gt::px(80),
                           aspect_ratio = 2)}) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = `Sentiment x Time`),
      fn = function(x){
        sent_plot_list %>%
          gt::ggplot_image(height = gt::px(80),
                           aspect_ratio = 2)})

  #Format titles and numbers + align
  table <- table %>%
    gt::tab_header(title = table_title) %>%
    gt::tab_source_note(source_note = source_note) %>%
    gt::fmt_number(columns = Volume, sep_mark = ",", decimals = 0) %>%
    gt::fmt_percent(columns = c(Positive, Negative, Neutral), decimals = 1, scale_values = FALSE) %>%
    gt::opt_table_font("Segoe UI") %>%
    gt::cols_align(align = "center")

  #Restyle table's structural elements
  table <- table %>%
    gt::tab_options(
      column_labels.border.top.width = gt::px(3),
      column_labels.border.top.color = "transparent",
      #Remove border around table
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      #Adjust font sizes and alignment
      source_notes.font.size = 12,
      heading.align = "left")

  #Restyle table's text elements
  table <- table %>%
    gt::tab_style(style = gt::cell_text(weight = "600"),
                  locations = gt::cells_title(groups = "title")) %>%
    gt::tab_style(style = gt::cell_text(color = "grey50",
                                        transform = "capitalize"),
                  locations = gt::cells_column_labels(tidyselect::everything())) %>%
    gt::tab_style(style = gt::cell_text(style = "italic"),
                  locations = gt::cells_stub_grand_summary(tidyselect::everything())) %>%
    gt::tab_style(style = gt::cell_text(style = "italic"),
                  locations = gt::cells_source_notes()) %>%
    gt::tab_style(style = gt::cell_text(weight = "600"),
                  locations = gt::cells_grand_summary(tidyselect::everything()))

  return(table)
}

#' quickly add re-usable theme elements for gt plot funcs
#'
#' @return list of ggplot boiler plate theme options
#' @keywords internal
#'
disp_gt_theme <- function(){

  ggplot_theme <- ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.position = "null",
    axis.title = ggplot2::element_blank(),
    strip.text = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 30),
    axis.text.y = ggplot2::element_text(angle = 45,
                                        hjust = 1),
    plot.margin = ggplot2::margin(30, 0, 0, 0)
  )
}

#' Create volume over time charts for use in gt tables
#'
#' The plot this function output appears in the cells of a gt table. There is custom styling applied to remove much of the detail, the output is not
#'
#' @param data The input data frame.
#' @param date_var The name of the variable in the data frame containing the dates.
#' @param time_unit The time unit to group the data by. Default is "week".
#' @param bar_colour The color of the bars in the plot. Default is "#628EFD".
#' @param date_breaks The breaks to be used on the x-axis for the date scale. Default is "4 months".
#' @param date_labels The format of the labels on the x-axis for the date scale. Default is "%b".
#'
#' @return A ggplot object representing the bar plot.
#' @keywords internal
disp_gt_vot <- function(data, date_var,  time_unit = c("week", "day", "month", "quarter", "year"), bar_colour =  "#628EFD", date_breaks = "4 months",date_labels = "%b"){

  unit <- match.arg(time_unit)
  date_sym <- rlang::ensym(date_var)

  data <- data %>%
    dplyr::mutate(plot_date = as.Date(!!date_sym),
                  plot_date = lubridate::floor_date(plot_date, unit = unit))

  plot <- data %>%
    dplyr::count(plot_date) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n)) +
    ggplot2::geom_col(fill = bar_colour) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    disp_gt_theme()

  return(plot)
}

#' Display Sentiment Over Time
#'
#' This function generates a chart showing the sentiment distribution over time based on a data frame, styled for gt.
#'
#' @param data The input data frame.
#' @param sentiment_var The name of the variable in the data frame containing the sentiment values.
#' @param date_var The name of the variable in the data frame containing the dates.
#' @param chart_type The type of chart to be plotted. Default is "lines".
#' @param time_unit The time unit to group the data by. Default is "week".
#' @param date_breaks The breaks to be used on the x-axis for the date scale. Default is "4 months".
#' @param date_labels The format of the labels on the x-axis for the date scale. Default is "%b".
#'
#' @return A ggplot object representing the plotted chart.
#' @keywords internal
disp_gt_sent_time <- function(data,
                              sentiment_var = sentiment,
                              date_var = date,
                              chart_type = c("lines", "bars"),
                              time_unit = c("week", "day","month", "quarter", "year"),
                              date_breaks = "4 months",
                              date_labels = "%b"
){

  unit <- match.arg(time_unit)
  chart_type <- match.arg(chart_type)

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

  data <- data %>% dplyr:: mutate(
    plot_date = as.Date(!!date_sym),
    plot_date = lubridate::floor_date(plot_date, unit = unit),
    !!sent_sym := tolower(!!sent_sym))

  plot <- data %>%
    dplyr::count(plot_date,!!sent_sym) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = !!sent_sym, colour = !!sent_sym))

  if(chart_type == "lines"){
    plot <- plot +
      ggplot2::geom_line()
  } else { plot <- plot +
    ggplot2::geom_col()
  }
  plot <- plot +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    ggplot2::scale_fill_manual(aesthetics = c("colour", "fill"),
                               values = c("positive" = "#1b7837",
                                          "negative" = "#c00000",
                                          "neutral" = "black")) +
    disp_gt_theme()

  return(plot)
}
