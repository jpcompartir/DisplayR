edit_legend_titles_check <- function(theme){

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




