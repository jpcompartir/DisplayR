#' theme_black
#'
#' Makes the plot dark theme - suitable for Microsoft projects.
#' @export
dr_theme_black <- function(){

  output <- ggplot2::theme(plot.background = ggplot2::element_rect(fill = "black", colour = "black"),
                          strip.text = ggplot2::element_text(colour = "white"),
                          strip.background = ggplot2::element_rect(fill = "black"),
                          panel.background = ggplot2::element_rect(fill = 'black'),
                          axis.text = ggplot2::element_text(colour = 'white'),
                          axis.title = ggplot2::element_text(colour = 'white'),
                          axis.line = ggplot2::element_line(colour = 'white'),
                          legend.text = ggplot2::element_text(colour = "white"),
                          legend.background = ggplot2::element_rect(fill = 'black'),
                          legend.title = ggplot2::element_text(colour = 'white'),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank())

  return(output)

}

#' Apply Microsoft Themed Colour Scales and Aesthetics to ggplot2 Plots
#'
#' This function provides a convenient way to apply Microsoft themed colour scales and aesthetics to ggplot2 plots.
#' The user can specify whether they want a continuous or discrete theme by providing the `scale_type` argument.
#'
#' @param scale_type A character string specifying the type of scale, either "continuous" or "discrete". Default is "discrete".
#' @param index An optional numeric index or vector of indices to select specific colours from the Microsoft colour palette.
#' @param direction An optional numeric value (1 or -1) specifying the direction of the colour gradient for continuous scales. Default is 1.
#' @param guide An optional character string specifying the type of guide to use for continuous scales. Default is 'legend'.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font.
#'
#' @return A list containing the ggplot2 theme, fill scale, and colour scale.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Example with continuous theme
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = qsec)) +
#'   geom_point() +
#'   theme_microsoft(scale_type = "continuous")
#'
#' # Example with discrete theme
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   theme_microsoft(scale_type = "discrete")
#'   }
dr_theme_microsoft <- function(scale_type = c("discrete", "continuous"),
                            index = NULL,
                            direction = 1,
                            guide = 'legend',
                            fallback_font = "sans") {

  scale_type <- match.arg(scale_type)

  # Use the fallback font if we're in the RMarkdown check environment
  if (identical(Sys.getenv("R_CHECK_ENVIRON"), "true")) {
    font_family <- fallback_font
  } else {
    font_family <- "Segoe UI Regular"
  }

  #Return the continuous function if necessary and if not discrete
  if (scale_type == "continuous") {

    # The continuous code block from theme_microsoft_continuous
    return(theme_microsoft_continuous(index, direction, guide, font_family = font_family))

  } else if (scale_type == "discrete") {

    # The discrete code block from theme_microsoft_discrete
    return(theme_microsoft_discrete(index, font_family = font_family))

  } else {
    stop("Invalid scale_type argument. Must be either 'continuous' or 'discrete'.")
  }

}

#' theme_microsoft_continous
#'
#' Adds Microsoft colours and font to continous plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide The type of legend. Use "colourbar", "legend" or FALSE.

#' @keywords internal
theme_microsoft_continuous <- function(index = NULL, direction = 1, guide = 'legend', font_family = "Segoe UI Regular"){

  palette = c(
    "#D83B01", # Orange
             "#FFB900", # Yellow
             "#107C10", # Green
             "#008575", # Teal
             "#0078D4", # Blue
             "#8661C5", # Purple,
             "#FF9349", # Light Orange
             "#243a5e", # Dark Blue
             "#9BF00B", # Light Green,
             "#30E5D0", # Light Teal,
             "#50E6FF", # Light Blue
             "#D59DFF", # Light Purple
             "#6b2929", # Dark Orange
             "#6a4b16", # Dark Yellow
             "#054b17", # Dark Green
             "#274b47", # Dark Teal
             "#3b2e58" # Dark Purple
  )

  if (is.numeric(index)){
    values <- palette[index]
  } else {
    values <- c("#ffb900",
                         "#107c10",
                         "#0078d4")
  }

  if (direction == 1){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = values,
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = values,
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  } else if (direction == -1){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = rev(values),
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = rev(values),
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  }

  list(ggplot2::theme_minimal(base_family = font_family),
       fill_scale,
       colour_scale)

}

#' theme_microsoft_discrete
#'
#' Adds Microsoft colours and font to discrete plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @keywords internal
theme_microsoft_discrete <- function(index = NULL, font_family = 'Segoe UI Regular'){

  values = c(
    "#D83B01", # Orange
             "#FFB900", # Yellow
             "#107C10", # Green
             "#008575", # Teal
             "#0078D4", # Blue
             "#8661C5", # Purple,
             "#FF9349", # Light Orange
             "#243a5e", # Dark Blue
             "#9BF00B", # Light Green,
             "#30E5D0", # Light Teal,
             "#50E6FF", # Light Blue
             "#D59DFF", # Light Purple
             "#6b2929", # Dark Orange
             "#6a4b16", # Dark Yellow
             "#054b17", # Dark Green
             "#274b47", # Dark Teal
             "#3b2e58" # Dark Purple
  )

  if (is.numeric(index)){
    values <- values[index]
  }

  list(ggplot2::theme_minimal(base_family = 'Segoe UI Regular'),
       ggplot2::scale_discrete_manual(aesthetics = c('fill', 'colour'),
                                      values = values))

}


#' Apply SAMY Themed Colour Scales and Aesthetics to ggplot2 Plots
#'
#' This function provides a convenient way to apply Microsoft themed colour scales and aesthetics to ggplot2 plots.
#' The user can specify whether they want a continuous or discrete theme by providing the `scale_type` argument.
#'
#' @param scale_type A character string specifying the type of scale, either "continuous" or "discrete". Default is "discrete".
#' @param index An optional numeric index or vector of indices to select specific colours from the SAMY colour palette.
#' @param direction An optional numeric value (1 or -1) specifying the direction of the colour gradient for continuous scales. Default is 1.
#' @param guide An optional character string specifying the type of guide to use for continuous scales. Default is 'legend'.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font.
#'
#' @return A list containing the ggplot2 theme, fill scale, and colour scale.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(ggplot2)
#'
#' # Example with continuous theme
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = qsec)) +
#'   geom_point() +
#'   theme_samy(scale_type = "continuous")
#'
#' # Example with discrete theme
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   theme_samy(scale_type = "discrete")
#'}
dr_theme_samy <- function(scale_type = c("discrete", "continuous"),
                       index = NULL,
                       direction = 1,
                       guide = 'legend',
                       fallback_font = "sans"){
  scale_type <- match.arg(scale_type)

  if (identical(Sys.getenv("R_CHECK_ENVIRON"), "true") || !'Montserrat Regular' %in% names(pdfFonts())) {
    font_family <- fallback_font
  } else {
    font_family <- 'Montserrat Regular'
  }


  if (scale_type == "continuous") {

    # The continuous code block from theme_samy_continuous
    return(theme_samy_continuous(index, direction, guide, font_family = font_family))

  } else if (scale_type == "discrete") {

    # The discrete code block from theme_samy_discrete
    return(theme_samy_discrete(index, font_family = font_family))

  } else {
    stop("Invalid scale_type argument. Must be either 'continuous' or 'discrete'.")
  }

}


#' theme_samy_continuous
#'
#' Adds Samy colours and font to continuous plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide The type of legend. Use "colourbar", "legend" or FALSE.
#'
#' @keywords internal
theme_samy_continuous <- function(index = NULL, direction = 1, guide = 'legend',  font_family = 'Montserrat Regular'){

  palette <- c("#3fbbbb",
                        "#ff5b51",
                        "#ffcf0e",
                        "#7b7b7c",
                        "#c1e5e2",
                        "#f15c54",
                        "#ffe9ab",
                        "#aaabaa",
                        "#f8aa95",
                        "#dfe0e1")

  if (is.numeric(index)){
    values <- palette[index]
  } else {
    values <- c("#ffcf0e",
                         "#f15c54",
                         "#3fbbbb")
  }

  if (direction == 1){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = values,
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = values,
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  } else if (direction == -1){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = rev(values),
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = rev(values),
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  }

  list(ggplot2::theme_minimal(base_family = font_family),
       fill_scale,
       colour_scale)

}




#' theme_samy_discrete
#'
#' Adds Samy colours and font to discrete plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @keywords internal
theme_samy_discrete <- function(index = NULL, font_family = 'Montserrat Regular'){

  values <- c("#3fbbbb",
                       "#ff5b51",
                       "#ffcf0e",
                       "#7b7b7c",
                       "#c1e5e2",
                       "#f15c54",
                       "#ffe9ab",
                       "#aaabaa",
                       "#f8aa95",
                       "#dfe0e1")

  if (is.numeric(index)){
    values <- values[index]
  }

  list(ggplot2::theme_minimal(base_family = font_family),
       ggplot2::scale_discrete_manual(aesthetics = c('fill', 'colour'),
                                      values = values))

}


#' Apply share Themed colour Scales and Aesthetics to ggplot2 Plots
#'
#' This function provides a convenient way to apply share themed colour scales and aesthetics to ggplot2 plots.
#' The user can specify whether they want a continuous or discrete theme by providing the `scale_type` argument.
#'
#' @param scale_type A character string specifying the type of scale, either "continuous" or "discrete". Default is "discrete".
#' @param index An optional numeric index or vector of indices to select specific colours from the share colour palette.
#' @param direction An optional numeric value (1 or -1) specifying the direction of the colour gradient for continuous scales. Default is 1.
#' @param guide An optional character string specifying the type of guide to use for continuous scales. Default is 'legend'.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font.
#'
#' @return A list containing the ggplot2 theme, fill scale, and colour scale.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Example with continuous theme
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = qsec)) +
#'   geom_point() +
#'   theme_share(scale_type = "continuous")
#'
#' # Example with discrete theme
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   theme_share(scale_type = "discrete")
#'   }
dr_theme_share <- function(scale_type = c("discrete", "continuous"),
                        index = NULL,
                        direction = 1,
                        guide = 'legend',
                        fallback_font = "sans") {

  scale_type <- match.arg(scale_type)

  if (identical(Sys.getenv("R_CHECK_ENVIRON"), "true")) {
    font_family <- fallback_font
  } else {
    font_family <- "Neue Haas Grotesk Text Pro 55 Roman"
  }

  if (scale_type == "continuous") {

    # The continuous code block from theme_share_continuous
    return(theme_share_continuous(index, direction, guide, font_family = font_family))

  } else if (scale_type == "discrete") {

    # The discrete code block from theme_share_discrete
    return(theme_share_discrete(index, font_family = font_family))

  } else {
    stop("Invalid scale_type argument. Must be either 'continuous' or 'discrete'.")
  }

}


#' theme_share_continuous
#'
#' Adds SHARE colours and font to continuous plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide The type of legend. Use "colourbar", "legend" or FALSE.
#'
#' @keywords internal
theme_share_continuous <- function(index = NULL, direction = 1, guide = 'legend', font_family = "Neue Haas Grotesk Text Pro 55 Roman"){

  values <- c("#0f50d2",
                       "#7800c6",
                       "#d80a83",
                       "#ffb600",
                       "#ff4e00",
                       "#bb7fe2",
                       "#ffda74",
                       "#ffa67f",
                       "#eb84c1")

  if (is.numeric(index)){
    values <- palette[index]
  } else {
    values <- c("#ffb600",
                         "#d80a83",
                         "#0f50d2")
  }

  if (direction == 1){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = values,
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = values,
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  } else if (direction == -1){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = rev(values),
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = rev(values),
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  }

  list(ggplot2::theme_minimal(base_family = font_family),
       fill_scale,
       colour_scale)

}


#' theme_share_discrete
#'
#' Adds SHARE colours and font to discrete plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @keywords internal
theme_share_discrete <- function(index = NULL, font_family = "Neue Haas Grotesk Text Pro 55 Roman"){

  values <- c("#0f50d2",
                       "#7800c6",
                       "#d80a83",
                       "#ffb600",
                       "#ff4e00",
                       "#bb7fe2",
                       "#ffda74",
                       "#ffa67f",
                       "#eb84c1")

  if (is.numeric(index)){
    values <- values[index]
  }

  list(
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 15,
        hjust = 0.5,
        vjust = 1,
        margin = margin(b = half_line)
      ),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white",
                                               colour = NA),
      axis.line = ggplot2::element_line(colour = "grey20"),
      axis.ticks = ggplot2::element_line(colour = "grey20"),
      axis.text = ggplot2::element_text(colour = "grey30",
                                        size = rel(0.8)),
      axis.title = ggplot2::element_text(colour = "grey30"),
      panel.grid = ggplot2::element_line(colour = "grey92"),
      panel.grid.minor = ggplot2::element_line(linewidth = rel(0.5)),
      strip.background = ggplot2::element_rect(fill = "grey85",
                                               colour = "grey20"),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      complete = TRUE
    ),ggplot2::scale_discrete_manual(aesthetics = c('fill', 'colour'),
                                      values = values))

}

#' Apply Capture Intelligence themed colour scales and aesthetics to ggplot2 plots
#'
#' This function provides a convenient way to apply Capture themed colour scales and aesthetics to ggplot2 plots.
#' The user can specify whether they want a continuous or discrete theme by providing the `scale_type` argument.

#' @param scale_type
#' @param index
#' @param direction
#' @param guide
#' @param fallback_font
#'
#' @return
#' @export
#'
#' @examples
dr_theme_capture <- function(scale_type = c("discrete", "continuous"),
                             index = NULL,
                             direction = 1,
                             guide = 'legend',
                             aesthetics = "fill",
                             fallback_font = "sans") {

  scale_type <- match.arg(scale_type)

  if (identical(Sys.getenv("R_CHECK_ENVIRON"), "true")) {
    font_family <- fallback_font
  } else {
    font_family <- "GT Walsheim Pro"
  }

  if (scale_type == "continuous") {

    # The continuous code block from theme_share_continuous
    return(theme_capture_continuous(direction, guide, font_family = font_family))

  } else if (scale_type == "discrete") {

    # The discrete code block from theme_share_discrete
    return(theme_capture_discrete(direction, font_family = font_family))

  } else {
    stop("Invalid scale_type argument. Must be either 'continuous' or 'discrete'.")
  }

}

#' theme_capture_continuous
#'
#' Adds Capture colours and font to continuous plot.
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide The type of legend. Use "colourbar", "legend" or FALSE.
#'
#' @keywords internal
theme_capture_continuous <- function(direction = 1, guide = 'legend', font_family = "GT Walsheim Pro"){

    fill_scale <- ggplot2::scale_fill_viridis_c(labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide,
                                                direction = direction)
    colour_scale <- ggplot2::scale_colour_viridis_c(labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide,
                                                    direction = direction)

    base_size = 11
    base_family = "GT Walsheim Pro"
    base_line_size = base_size / 22
    base_rect_size = base_size / 22
    half_line <- base_size / 2

    list(
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 15,
          hjust = 0.5,
          vjust = 1,
          margin = margin(b = half_line)
        ),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white",
                                                 colour = NA),
        axis.line = ggplot2::element_line(colour = "grey20"),
        axis.ticks = ggplot2::element_line(colour = "grey20"),
        axis.text = ggplot2::element_text(colour = "grey30",
                                          size = rel(0.8)),
        axis.title = ggplot2::element_text(colour = "grey30"),
        panel.grid = ggplot2::element_line(colour = "grey92"),
        panel.grid.minor = ggplot2::element_line(linewidth = rel(0.5)),
        strip.background = ggplot2::element_rect(fill = "grey85",
                                                 colour = "grey20"),
        legend.key = ggplot2::element_rect(fill = "white", colour = NA),
        legend.position = "bottom",
        complete = TRUE
      ),
      fill_scale,
      colour_scale
    )

}


#' theme_capture_discrete
#'
#' Adds Capture colours and font to discrete plot.
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide The type of legend. Use "colourbar", "legend" or FALSE.
#'
#' @keywords internal
theme_capture_discrete <- function(direction, font_family = "Neue Haas Grotesk Text Pro 55 Roman"){

 fill_scale <- ggplot2::scale_fill_viridis_d(direction = direction)
  colour_scale <- ggplot2::scale_colour_viridis_d(direction = direction)

  base_size = 11
  base_family = "GT Walsheim Pro"
  base_line_size = base_size / 22
  base_rect_size = base_size / 22
  half_line <- base_size / 2

  list(
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 15,
        hjust = 0.5,
        vjust = 1,
        margin = margin(b = half_line)
      ),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white",
                                               colour = NA),
      axis.line = ggplot2::element_line(colour = "grey20"),
      axis.ticks = ggplot2::element_line(colour = "grey20"),
      axis.text = ggplot2::element_text(colour = "grey30",
                                        size = rel(0.8)),
      axis.title = ggplot2::element_text(colour = "grey30"),
      panel.grid = ggplot2::element_line(colour = "grey92"),
      panel.grid.minor = ggplot2::element_line(linewidth = rel(0.5)),
      strip.background = ggplot2::element_rect(fill = "grey85",
                                               colour = "grey20"),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      complete = TRUE
    ),
    fill_scale,
    colour_scale
    )
}
