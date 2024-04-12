globals <- utils::globalVariables(c("plot_date", "n", "percent", "fp_text", ":=",
                                    "volume", "Sentiment x Time", "Neutral", "Volume x Time",
                                    "sentiment", "df", "group", "positive", "neutral",
                                    "negative", "Positive", "Negative", "Volume", "group_n",
                                    "percent_character", "topic", ".total", "palette", "pdfFonts",
                                    "Sepal.Length", "Sepal.Width", "iris", "Species"))


test_plot <- function() {plot <-
  plot <- ggplot2::ggplot(data = datasets::iris,
                  ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
  ggplot2::geom_point()

  return(plot)
}

# testing_helper functions ----

theme_edits_legend <- function(theme){ # need to add more here

  theme_func <- get(theme)

  if (grepl("continuous", theme)){ # continuous scale
    legend_options <- c("colourbar", "legend") # two legend options

    plot <- ggplot2::ggplot(data = iris,
                            ggplot2::aes(
                              x = Sepal.Length,
                              y = Sepal.Width,
                              colour = Sepal.Width,
                              fill = Sepal.Width
                            )) +
      ggplot2::geom_point()
  } else {  # discrete scale
    legend_options <- "legend"  # one legend option

    plot <- ggplot2::ggplot(data = iris,
                            ggplot2::aes(
                              x = Sepal.Length,
                              y = Sepal.Width,
                              colour = Species,
                              fill = Species
                            )) +
      ggplot2::geom_point()
  }

  # create plots for legend options
  test_plots <- purrr::map(legend_options, ~ {
    plot_test <- plot + theme_func(guide = .x)
  })

  purrr::map(test_plots, ~ { # test over all values of test_plots
    plot_test <- .

    # Colour guide correct
    testthat::expect_equal(plot_test$guides$guides$colour$params$theme$legend.title.position, "top", info= paste0("Theme generating failure: ", theme))
    testthat::expect_equal(plot_test$guides$guides$colour$params$theme$legend.title$hjust, 0.5, info= paste0("Theme generating failure: ", theme))

    # Fill guide correct
    testthat::expect_equal(plot_test$guides$guides$fill$params$theme$legend.title.position, "top", info= paste0("Theme generating failure: ", theme))
    testthat::expect_equal(plot_test$guides$guides$fill$params$theme$legend.title$hjust, 0.5, info= paste0("Theme generating failure: ", theme))
  })
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
  testthat::expect_error(plot + theme_func(direction = "a"),
               regexp = "\'arg' should be one of \"forwards\"",
               info= paste0("Theme generating failure: ", theme))

  # valid directions
  testthat::expect_no_error(plot + theme_func(direction = "forwards"))
  testthat::expect_no_error(plot + theme_func(direction = "backwards"))

}

theme_edits_palette_direction <- function(theme, colour_forward, colour_backward){

  theme_func <- get(theme)

  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Sepal.Width,
                            fill = Sepal.Width
                          )) +
    ggplot2::geom_point()

  plot_direction_forward <- plot +
    theme_func(direction = "forwards")

  plot_direction_backward <- plot +
    theme_func(direction = "backwards")

  # plot_direction_forward and plot_direction_backward first colours are different
  testthat::expect_equal(ggplot2::layer_data(plot_direction_forward)$colour[1], colour_forward, info= paste0("Theme generating failure: ", theme))
  testthat::expect_equal(ggplot2::layer_data(plot_direction_backward)$colour[1], colour_backward, info= paste0("Theme generating failure: ", theme))

  # plot_direction_forward and plot_direction_backward first fills are different
  testthat::expect_equal(ggplot2::layer_data(plot_direction_forward)$fill[1], colour_forward, info= paste0("Theme generating failure: ", theme))
  testthat::expect_equal(ggplot2::layer_data(plot_direction_backward)$fill[1], colour_backward, info= paste0("Theme generating failure: ", theme))
}

