#' Create a raincloud plot to provide maximal statistical information while preserving the desired ‘inference at a glance’ nature of barplots and other similar visualisation devices.
#'
#' @param data A data frame that includes the grouping and continuous variable.
#' @param grouping_variable The variable we want to map colours to.
#' @param continuous_variable The continious variable we want to be displaying the distribution of.
#' @param bandwidth A measure, ranging from 0-1, representing the smoothness of the density plot (i.e. how closely you want the density to match the distribution). Default is 0.3, but too high values can underrepresent the data, and too low values can overfit the density.
#'
#' @return
#' @export
#'
#' @examples
#' iris %>%
#' dr_plot_raincloud(grouping_variable = Species, independent_variable = Sepal.Length)

dr_plot_raincloud <- function(data, grouping_variable, continuous_variable, bandwidth = 0.3) {

  stopifnot(is.numeric(bandwidth),
            bandwidth > 0,
            bandwidth < 1)

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
      adjust = bandwidth,
      width = 0.5,
      color = NA, ## remove slab interval
      position = ggplot2::position_nudge(x = 0.15)
    ) +
    gghalves::geom_half_point(
      side = "l",
      range_scale = 0.3,
      alpha = 0.5
    ) +
    DisplayR:::theme_boilerplate()

  return(plot_raincloud)
}
