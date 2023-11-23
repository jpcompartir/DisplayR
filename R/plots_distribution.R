#' Quickly create a raincloud plot
#'
#' Create a raincloud plot to provide maximal statistical information while preserving the desired ‘inference at a glance’ nature of barplots and other similar visualisation devices.
#'
#' @param data A data frame that includes the grouping and continuous variable.
#' @param grouping_variable The variable we want to map colours to.
#' @param continuous_variable The continious variable we want to be displaying the distribution of.
#' @param smoothness A measure, which must be > 0, representing the smoothness of the density plot (i.e. how closely you want the density to match the distribution). Default is 0.3, but too high values can oversmooth the data, and too low values can cause undersmoothing. Generally it is recommended to stick with values between 0.3-1.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' ## Smoothness default value of 0.3
#' iris %>%
#'   dr_plot_raincloud(grouping_variable = Species,
#'                     continuous_variable = Sepal.Length,
#'                     smoothness = 0.3)
#'
#' ## Smoothness changed to 0.8
#' iris %>%
#'   dr_plot_raincloud(grouping_variable = Species,
#'                     continuous_variable = Sepal.Length,
#'                     smoothness = 0.8)

dr_plot_raincloud <- function(data, grouping_variable, continuous_variable, smoothness = 0.3) {

  stopifnot(is.numeric(smoothness),
            smoothness > 0)

  # Get var for tidy eval
  group_sym <- rlang::ensym(grouping_variable)
  group_string <- rlang::as_string(group_sym)
  if(!group_string %in% colnames(data)) {stop(paste0(group_sym, " not in data"))}

  continuous_sym <- rlang::ensym(continuous_variable)
  continuous_string <- rlang::as_string(continuous_sym)
  if(!continuous_string %in% colnames(data)) {stop(paste0(continuous_sym, " not in data"))}

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!group_sym, y = !!continuous_sym, colour = !!group_sym, fill = !!group_sym))

  set.seed(42)

  plot_raincloud <- plot +
    ggplot2::geom_boxplot(
      width = 0.1,
      fill = "white",
      outlier.shape = NA
    ) +
    ggdist::stat_halfeye(
      adjust = smoothness,
      width = 0.5,
      color = NA, ## remove slab interval
      position = ggplot2::position_nudge(x = 0.15)
    ) +
    gghalves::geom_half_point(
      side = "l",
      range_scale = 0.3,
      alpha = 0.5
    ) +
    theme_boilerplate()

  return(plot_raincloud)
}
