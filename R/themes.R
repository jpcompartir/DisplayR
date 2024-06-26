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
#' @param scale_type A character string specifying the type of scale, either "continuous" or "discrete". Default is "discrete". Simply, is the data mapped to the colour or fill aesthetic discrete or continuous.
#' @param index An optional numeric index or vector of indices to select specific colours from the Microsoft colour palette.
#' @param direction A character string, either "forwards" or "backwards" specifying the direction of the colour or fill gradient for continuous scales. Default is "forwards".
#' @param guide An optional character string specifying the type of guide to use for continuous scales. Default is "colourbar", but other values include "legend", and "none". A rule of thumb is if the data are continuous, "colourbar" should be used, and if the data are discrete then "legend" should be used.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font (Segoe UI).
#'
#' @return A list containing the ggplot2 theme, fill scale, and colour scale.
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' # Example with continuous theme
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = qsec)) +
#'   geom_point() +
#'   dr_theme_microsoft(scale_type = "continuous")
#'
#' # Example with discrete theme
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   dr_theme_microsoft(scale_type = "discrete")
#'   }

dr_theme_microsoft <- function(scale_type = c("discrete", "continuous"),
                            index = NULL,
                            direction = c("forwards", "backwards"),
                            guide = 'colourbar',
                            fallback_font = "sans") {

  # input validation ----
  scale_type <- match.arg(scale_type)
  direction <- match.arg(direction)

  stopifnot(is.null(index) | is.numeric(index),
            is.character(guide),
            is.character(fallback_font))

  # ----



  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Segoe UI" only in user environments where it is available
  if (interactive() && "Segoe UI" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "Segoe UI"
  }

  #Return the continuous function if necessary and if not discrete
  if (scale_type == "continuous") {

    # The continuous code block from theme_microsoft_continuous
    return(theme_microsoft_continuous(index, direction, guide, fallback_font = font_family))

  } else {

    # The discrete code block from theme_microsoft_discrete
    return(theme_microsoft_discrete(index, fallback_font = font_family))

  }

}

#' theme_microsoft_continous
#'
#' Adds Microsoft colours and font to continous plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide The type of legend. Use "colourbar", "legend" or FALSE.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font (Segoe UI)

#' @keywords internal
theme_microsoft_continuous <- function(index = NULL,
                                       direction = c("forwards", "backwards"),
                                       guide = c('legend', 'colourbar', 'colorbar', 'none'),
                                       fallback_font = "sans"){

  # input validation ----
  direction <- match.arg(direction)
  guide <- match.arg(guide)

  stopifnot(is.null(index) | is.numeric(index),
            is.character(fallback_font))
  # ----

  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Segoe UI" only in user environments where it is available
  if (interactive() && "Segoe UI" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "Segoe UI"
  }

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

  if (direction == "forwards"){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = values,
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = values,
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  } else if (direction == "backwards"){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = rev(values),
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = rev(values),
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  }

  if (guide == "colourbar") {

    guides_scale <- ggplot2::guides(colour = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5),
                                    fill = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5))

  } else if (guide == "legend") {

    guides_scale <- ggplot2::guides(colour = ggplot2::guide_legend(title.position="top", title.hjust = 0.5),
                                    fill = ggplot2::guide_legend(title.position="top", title.hjust = 0.5))

  } else {

    guides_scale <- list(colour = NULL, fill = NULL)

  }

  list(
    theme_boilerplate(font_family = font_family),
    fill_scale,
    colour_scale,
    guides_scale)

}

#' theme_microsoft_discrete
#'
#' Adds Microsoft colours and font to discrete plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @param guide An optional character string specifying the type of guide to use for discrete scales. Either is "legend", which is default, or "none" to remove the legend.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font (Segoe UI)
#' @keywords internal
theme_microsoft_discrete <- function(index = NULL,
                                     guide = c("legend", "none"),
                                     fallback_font = 'sans'){

  # input validation ----
  guide <- match.arg(guide)

  stopifnot(is.null(index) | is.numeric(index),
            is.character(fallback_font))
  # ----


  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Segoe UI" only in user environments where it is available
  if (interactive() && "Segoe UI" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "Segoe UI"
  }

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

  if (guide == "legend") {

    guides_scale <-     ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5),
                                        colour = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5))

  } else {

    guides_scale <- ggplot2::guides(fill = "none",
                                    colour = "none")

  }

  list(
    theme_boilerplate(font_family = font_family),
    ggplot2::scale_discrete_manual(aesthetics = c('fill', 'colour'),
                                   values = values),
    guides_scale
  )

}


#' Apply SAMY Themed Colour Scales and Aesthetics to ggplot2 Plots
#'
#' This function provides a convenient way to apply Microsoft themed colour scales and aesthetics to ggplot2 plots.
#' The user can specify whether they want a continuous or discrete theme by providing the `scale_type` argument.
#'
#' @param scale_type A character string specifying the type of scale, either "continuous" or "discrete". Default is "discrete". Simply, is the data mapped to the colour or fill aesthetic discrete or continuous.
#' @param index An optional numeric index or vector of indices to select specific colours from the SAMY colour palette.
#' @param direction A character string, either "forwards" or "backwards" specifying the direction of the colour or fill gradient for continuous scales. Default is "forwards".
#' @param guide An optional character string specifying the type of guide to use for continuous scales. Default is "colourbar", but other values include "legend", and "none". A rule of thumb is if the data are continuous, "colourbar" should be used, and if the data are discrete then "legend" should be used.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font (Montserrat Regular).
#'
#' @return A list containing the ggplot2 theme, fill scale, and colour scale.
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # Example with continuous theme
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = qsec)) +
#'   geom_point() +
#'   dr_theme_samy(scale_type = "continuous")
#'
#' # Example with discrete theme
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   dr_theme_samy(scale_type = "discrete")

dr_theme_samy <- function(scale_type = c("discrete", "continuous"),
                       index = NULL,
                       direction = c("forward", "backwards"),
                       guide = 'colourbar',
                       fallback_font = "sans"){

  # input validation ----
  scale_type <- match.arg(scale_type)
  direction <- match.arg(direction)

  stopifnot(is.null(index) | is.numeric(index),
            is.character(fallback_font),
            is.character(guide))
  # ----


  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Montserrat Regular" only in user environments where it is available
  if (interactive() && "Montserrat Regular" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "Montserrat Regular"
  }

  if (scale_type == "continuous") {

    # The continuous code block from theme_samy_continuous
    return(theme_samy_continuous(index, direction, guide, fallback_font = font_family))

  } else {

    # The discrete code block from theme_samy_discrete
    return(theme_samy_discrete(index, fallback_font = font_family))

  }

}


#' theme_samy_continuous
#'
#' Adds Samy colours and font to continuous plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide The type of legend. Use "colourbar", "legend" or FALSE.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font (Montserrat Regular).
#'
#' @keywords internal
theme_samy_continuous <- function(index = NULL,
                                  direction = c("forwards", "backwards"),
                                  guide = c("legend", "colourbar", "colorbar", "none"),
                                  fallback_font = 'sans'){

  # input validation ----
  direction <- match.arg(direction)
  guide <- match.arg(guide)

  stopifnot(is.null(index) | is.numeric(index),
            is.character(fallback_font))
  # ----


  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Montserrat Regular" only in user environments where it is available
  if (interactive() && "Montserrat Regular" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "Montserrat Regular"
  }

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

  if (direction == "forwards"){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = values,
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = values,
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  } else if (direction == "backwards"){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = rev(values),
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = rev(values),
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  }

  if (guide == "colourbar") {

    guides_scale <- ggplot2::guides(colour = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5),
                                    fill = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5))

  } else if (guide == "legend") {

    guides_scale <- ggplot2::guides(colour = ggplot2::guide_legend(title.position="top", title.hjust = 0.5),
                                    fill = ggplot2::guide_legend(title.position="top", title.hjust = 0.5))

  } else {

    guides_scale <- list(colour = NULL, fill = NULL)

  }

  list(
    theme_boilerplate(font_family = font_family),
  fill_scale,
  colour_scale,
  guides_scale)

}

#' theme_samy_discrete
#'
#' Adds Samy colours and font to discrete plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @param guide An optional character string specifying the type of guide to use for discrete scales. Either is "legend", which is default, or "none" to remove the legend.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font (Montserrat Regular).
#' @keywords internal
theme_samy_discrete <- function(index = NULL,
                                guide = c("legend", "none"),
                                fallback_font = 'sans'){

  # input validation ----
  guide <- match.arg(guide)

  stopifnot(is.null(index) | is.numeric(index),
            is.character(fallback_font))
  # ----

  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Montserrat Regular" only in user environments where it is available
  if (interactive() && "Montserrat Regular" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "Montserrat Regular"
  }

  guide <- match.arg(guide)

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

  if (guide == "legend") {

    guides_scale <- ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5),
                                        colour = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5))

  } else {

    guides_scale <- ggplot2::guides(fill = "none",
                                    colour = "none")

  }

  list(
    theme_boilerplate(font_family = font_family),
    ggplot2::scale_discrete_manual(aesthetics = c('fill', 'colour'),
                                   values = values),
    guides_scale
  )
}


#' Apply share Themed colour Scales and Aesthetics to ggplot2 Plots
#'
#' This function provides a convenient way to apply share themed colour scales and aesthetics to ggplot2 plots.
#' The user can specify whether they want a continuous or discrete theme by providing the `scale_type` argument.
#'
#' @param scale_type A character string specifying the type of scale, either "continuous" or "discrete". Default is "discrete". Simply, is the data mapped to the colour or fill aesthetic discrete or continuous.
#' @param index An optional numeric index or vector of indices to select specific colours from the SHARE colour palette.
#' @param direction A character string, either "forwards" or "backwards" specifying the direction of the colour or fill gradient for continuous scales. Default is "forwards".
#' @param guide An optional character string specifying the type of guide to use for continuous scales. Default is "colourbar", but other values include "legend", and "none". A rule of thumb is if the data are continuous, "colourbar" should be used, and if the data are discrete then "legend" should be used.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font (NeueHaasGroteskText Pro Md).
#'
#' @return A list containing the ggplot2 theme, fill scale, and colour scale.
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # Example with continuous theme
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = qsec)) +
#'   geom_point() +
#'   dr_theme_share(scale_type = "continuous")
#'
#' # Example with discrete theme
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   dr_theme_share(scale_type = "discrete")

dr_theme_share <- function(scale_type = c("discrete", "continuous"),
                        index = NULL,
                        direction = c("forwards", "backwards"),
                        guide = 'colourbar',
                        fallback_font = "sans") {

  # input validation ----
  scale_type <- match.arg(scale_type)
  direction <- match.arg(direction)

  stopifnot(is.null(index) | is.numeric(index),
            is.character(fallback_font),
            is.character(guide))
  # ----

  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Neue Haas Grotesk Text Pro 55 Roman" only in user environments where it is available
  if (interactive() && "NeueHaasGroteskText Pro Md" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "NeueHaasGroteskText Pro Md"
  }

  if (scale_type == "continuous") {

    # The continuous code block from theme_share_continuous
    return(theme_share_continuous(index, direction, guide, fallback_font = font_family))

  } else {

    # The discrete code block from theme_share_discrete
    return(theme_share_discrete(index, fallback_font = font_family))

  }

}


#' theme_share_continuous
#'
#' Adds SHARE colours and font to continuous plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide The type of legend. Use "colourbar", "legend" or FALSE.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font (NeueHaasGroteskText Pro Md).
#'
#' @keywords internal
theme_share_continuous <- function(index = NULL,
                                   direction = c("forwards", "backwards"),
                                   guide = c('legend', 'colourbar', 'colorbar', 'none'),
                                   fallback_font = "sans"){

  # input validation ----
  direction <- match.arg(direction)
  guide <- match.arg(guide)

  stopifnot(is.null(index) | is.numeric(index),
            is.character(fallback_font))
  # ----

  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Neue Haas Grotesk Text Pro 55 Roman" only in user environments where it is available
  if (interactive() && "NeueHaasGroteskText Pro Md" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "NeueHaasGroteskText Pro Md"
  }

  # stopifnot(direction %in% c(-1, 1),
  #           guide %in% c('colourbar', 'colorbar', 'legend', 'none'))

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

  if (direction == "forwards"){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = values,
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = values,
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  } else if (direction == "backwards"){
    fill_scale <- ggplot2::scale_fill_gradientn(colours = rev(values),
                                                labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide)
    colour_scale <- ggplot2::scale_colour_gradientn(colours = rev(values),
                                                    labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide)
  }


  if (guide == "colourbar") {

    guides_scale <- ggplot2::guides(colour = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5),
                                    fill = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5))

  } else if (guide == "legend") {

    guides_scale <- ggplot2::guides(colour = ggplot2::guide_legend(title.position="top", title.hjust = 0.5),
                                    fill = ggplot2::guide_legend(title.position="top", title.hjust = 0.5))

  } else {

    guides_scale <- list(colour = NULL, fill = NULL)

  }

  list(
    theme_boilerplate(font_family = font_family),
  fill_scale,
  colour_scale,
  guides_scale)

}


#' theme_share_discrete
#'
#' Adds SHARE colours and font to discrete plot.
#' @param index Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)
#' @param guide An optional character string specifying the type of guide to use for discrete scales. Either is "legend", which is default, or "none" to remove the legend.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font (NeueHaasGroteskText Pro Md).
#'
#' @keywords internal
theme_share_discrete <- function(index = NULL,
                                 guide = c("legend", "none"),
                                 fallback_font = "sans"){

  # input validation ----
  guide <- match.arg(guide)

  stopifnot(is.null(index) | is.numeric(index),
            is.character(fallback_font))
  # ----

  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Neue Haas Grotesk Text Pro 55 Roman" only in user environments where it is available
  if (interactive() && "NeueHaasGroteskText Pro Md" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "NeueHaasGroteskText Pro Md"
  }

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

  if (guide == "legend") {

    guides_scale <-     ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5),
                                        colour = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5))

  } else {

    guides_scale <- ggplot2::guides(fill = "none",
                                    colour = "none")

  }

  list(
    theme_boilerplate(font_family = font_family),
    ggplot2::scale_discrete_manual(aesthetics = c('fill', 'colour'),
                                      values = values),
    guides_scale
  )

}

#' Apply Capture Intelligence themed colour scales and aesthetics to ggplot2 plots
#'
#' This function provides a convenient way to apply Capture themed colour scales and aesthetics to ggplot2 plots.
#' The user can specify whether they want a continuous or discrete theme by providing the `scale_type` argument.
#'
#' @param scale_type A character string specifying the type of scale, either "continuous" or "discrete". Default is "discrete". Simply, is the data mapped to the colour or fill aesthetic discrete or continuous.
#' @param direction A character string, either "forwards" or "backwards" specifying the direction of the colour or fill gradient for continuous scales. Default is "forwards". Exercise caution when opting for "backwards" as it reverses the colour sequence, causing smaller data values in the plot to appear lighter. This may not be visually intuitive for the audience, as typically brighter colours, such as yellows, are conventionally associated with higher data values.
#' @param guide An optional character string specifying the type of guide to use for continuous scales. Default is "colourbar", but other values include "legend", and "none". A rule of thumb is if the data are continuous, "colourbar" should be used, and if the data are discrete then "legend" should be used.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font.
#'
#' @return A list containing the ggplot2 theme, fill scale, and colour scale.
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # Example with continuous theme
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = qsec)) +
#'   geom_point() +
#'   dr_theme_capture(scale_type = "continuous")
#'
#' # Example with discrete theme
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   dr_theme_capture(scale_type = "discrete")


dr_theme_capture <- function(scale_type = c("discrete", "continuous"),
                             direction = c("forwards", "backwards"),
                             guide = 'colourbar',
                             fallback_font = "sans") {

  # input validation ----
  scale_type <- match.arg(scale_type)
  direction <- match.arg(direction)

  stopifnot(is.character(fallback_font),
            is.character(guide))
  # ----

  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Neue Haas Grotesk Text Pro 55 Roman" only in user environments where it is available
  if (interactive() && "Helvetica" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "Helvetica"
  }

  if (scale_type == "continuous") {

    # The continuous code block from theme_share_continuous
    return(theme_capture_continuous(direction, guide, fallback_font = font_family))

  } else {

    # The discrete code block from theme_share_discrete
    return(theme_capture_discrete(direction, fallback_font = font_family))

  }

}

#' theme_capture_continuous
#'
#' Adds Capture colours and font to continuous plot.
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide The type of legend. Use "colourbar", "legend" or FALSE.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font.
#'
#' @keywords internal
theme_capture_continuous <- function(direction = c("forwards", "backwards"),
                                     guide = c("colourbar", "colorbar", "legend", "none"),
                                     fallback_font = "sans"){

  # input validation ----
  guide <- match.arg(guide)
  direction <- match.arg(direction)

  stopifnot(is.character(fallback_font))
  # ----

  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Neue Haas Grotesk Text Pro 55 Roman" only in user environments where it is available
  if (interactive() && "Helvetica" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "Helvetica"
  }

  if (direction == "forwards") {

    fill_scale <- ggplot2::scale_fill_viridis_c(labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide,
                                                direction = 1)
    colour_scale <- ggplot2::scale_colour_viridis_c(labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide,
                                                    direction = 1)
  }

  else {

    fill_scale <- ggplot2::scale_fill_viridis_c(labels = scales::comma,
                                                breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                guide = guide,
                                                direction = -1)
    colour_scale <- ggplot2::scale_colour_viridis_c(labels = scales::comma,
                                                    breaks = function(x) round(stats::quantile(x, seq(0, 1, 0.25))),
                                                    guide = guide,
                                                    direction = -1)
  }


    if (guide == "colourbar" | guide == "colorbar") {

      guides_scale <- ggplot2::guides(colour = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5),
                                     fill = ggplot2::guide_colourbar(title.position="top", title.hjust = 0.5))

      } else if (guide == "legend") {

        guides_scale <- ggplot2::guides(colour = ggplot2::guide_legend(title.position="top", title.hjust = 0.5),
                                     fill = ggplot2::guide_legend(title.position="top", title.hjust = 0.5))

      } else {

        guides_scale <- ggplot2::guides(fill = "none",
                                        colour = "none")

      }


    list(
      theme_boilerplate(font_family = font_family),
      fill_scale,
      colour_scale,
      guides_scale
    )

}

#' theme_capture_discrete
#'
#' Adds Capture colours and font to discrete plot.
#' @param direction The direction of the colours in the scale. Set to -1 to reverse them.
#' @param guide An optional character string specifying the type of guide to use for discrete scales. Either is "legend", which is default, or "none" to remove the legend.
#' @param fallback_font Adds a fallback font of 'sans' in case user does not have required font.
#'
#' @keywords internal
theme_capture_discrete <- function(direction = c("forwards", "backwards"),
                                   guide = c("legend", "none"),
                                   fallback_font = "sans"){

  # input validation ----
  guide <- match.arg(guide)
  direction <- match.arg(direction)

  stopifnot(is.character(fallback_font))
  # ----

  # Default to fallback font
  font_family <- fallback_font

  # Switch to "Neue Haas Grotesk Text Pro 55 Roman" only in user environments where it is available
  if (interactive() && "Helvetica" %in% as.data.frame(sysfonts::font_files())$family) {
    font_family <- "Helvetica"
  }

  if (direction == "forwards"){
    fill_scale <- ggplot2::scale_fill_viridis_d(direction = 1)
    colour_scale <- ggplot2::scale_colour_viridis_d(direction = 1)

  }

  else {

    fill_scale <- ggplot2::scale_fill_viridis_d(direction = -1)
    colour_scale <- ggplot2::scale_colour_viridis_d(direction = -1)

  }

  if (guide == "legend") {

    guides_scale <- ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5),
                                    colour = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5))

  } else {

    guides_scale <- ggplot2::guides(fill = "none",
                                    colour = "none")

  }


  list(
    theme_boilerplate(font_family = font_family),
    fill_scale,
    colour_scale,
    guides_scale
       )
}

### helper function

theme_boilerplate <- function(font_family = "sans",
                              base_size = 11) {

  # input validation ----
  stopifnot(is.character(font_family), is.numeric(base_size))
  # ----

  half_line <- base_size / 2

  stopifnot(is.numeric(base_size) && base_size > 5)

  output <- ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = 15,
      hjust = 0.5,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    text = ggplot2::element_text(family = font_family),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white",
                                             colour = NA),
    axis.line = ggplot2::element_line(colour = "grey20"),
    axis.ticks = ggplot2::element_line(colour = "grey20"),
    axis.text = ggplot2::element_text(colour = "grey30",
                                      size = base_size * 0.8),
    axis.title = ggplot2::element_text(colour = "grey30"),
    panel.grid = ggplot2::element_line(colour = "grey92"),
    panel.grid.minor = ggplot2::element_line(linewidth = ggplot2::rel(0.5)),
    strip.background = ggplot2::element_rect(fill = "grey85",
                                             colour = "grey20"),
    legend.key = ggplot2::element_rect(fill = "white", colour = NA),
    legend.text = ggplot2::element_text(family = font_family,
                                        colour = "grey30",
                                        size = base_size * 0.8),
    legend.title = ggplot2::element_text(colour = "grey30"),
    legend.position = "bottom",
    complete = TRUE
  )

  list(
    output,
    ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5),
                    colour = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5))
  )

}

