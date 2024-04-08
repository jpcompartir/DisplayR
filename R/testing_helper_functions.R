theme_edits_legend <- function(theme){ # need to add more here

  theme_func <- get(theme)

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Species,
                            fill = Species
                          )) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_func()

  # Colour guide correct
  expect_equal(plot_test$guides$guides$colour$params$theme$legend.title.position, "top", info= paste0("Theme generating failure: ", theme))
  expect_equal(plot_test$guides$guides$colour$params$theme$legend.title$hjust, 0.5, info= paste0("Theme generating failure: ", theme))

  # Fill guide correct
  expect_equal(plot_test$guides$guides$fill$params$theme$legend.title.position, "top", info= paste0("Theme generating failure: ", theme))
  expect_equal(plot_test$guides$guides$fill$params$theme$legend.title$hjust, 0.5, info= paste0("Theme generating failure: ", theme))
}

theme_accepts_direction_args <- function(theme){

  theme_func <- get(theme)

  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length,
                                       y = Sepal.Width,
                                       colour = Species)) +
    ggplot2::geom_point()

  # invalid direction - should throw error
  expect_error(plot + theme_func(direction = "a"),
               regexp = "\'arg' should be one of \"forwards\"")

  # valid directions
  expect_no_error(plot + theme_func(direction = "forwards"))
  expect_no_error(plot + theme_func(direction = "backwards"))

}


