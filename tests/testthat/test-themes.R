### dr_theme_capture

test_that("dr_theme_capture returns a list by default", {

  p <- dr_theme_capture()
  expect_type(p, "list")

})

test_that("Valid scale types return a list", {

  plot_discrete <- dr_theme_capture(scale_type = "discrete")
  plot_continuous <- dr_theme_capture(scale_type = "continuous")
  expect_type(plot_discrete, "list")
  expect_type(plot_continuous, "list")

})

test_that("Invalid scale type throws an error", {

  expect_error(dr_theme_capture(scale_type = "abcd"), "arg' should be one of “discrete”, “continuous”")

})

test_that("Valid guide returns a list", {

  plot_colourbar <- dr_theme_capture(guide = "colourbar")
  plot_legend <- dr_theme_capture(guide = "legend")
  expect_type(plot_colourbar, "list")
  expect_type(plot_legend, "list")
})

test_that("Invalid guide throws error", {

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
    ggplot2::geom_point()

  expect_error(plot + dr_theme_capture(scale_type = "continuous",
                                       guide = "abcd"), regexp = "guide %in%")

})

### theme_capture_continuous()

test_that("theme_capture_continuous returns a list", {

  p <- theme_capture_continuous(direction = 1, guide = 'colourbar', font_family = "GT Walsheim Pro")
  expect_type(p, "list")

})

test_that("theme_capture_continuous gives error when direction isn't 1 or -1", {

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
    ggplot2::geom_point()

  expect_error(plot +
                 theme_capture_continuous(direction = "a"), regexp = "direction %in%")
})

test_that("theme_capture_continuous correctly changes direction of palette", {

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width, fill = Sepal.Width)) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_capture_continuous(direction = 1)

  plot_test_2 <- plot +
    theme_capture_continuous(direction = -1)

  expect_equal(ggplot2::layer_data(plot_test)$colour[1], "#36AD7F")
  expect_equal(ggplot2::layer_data(plot_test_2)$colour[1], "#30728D")
  expect_equal(ggplot2::layer_data(plot_test)$fill[1], "#36AD7F")
  expect_equal(ggplot2::layer_data(plot_test_2)$fill[1], "#30728D")

})

test_that("theme_capture_continuous correctly edits legend title", {

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width, fill = Sepal.Width)) +
    ggplot2::geom_point()

  plot_test_colourbar <- plot +
    theme_capture_continuous()

  expect_equal(plot_test_colourbar$guides$colour$title.position, "top")
  expect_equal(plot_test_colourbar$guides$colour$title.hjust, 0.5)
  expect_equal(plot_test_colourbar$guides$fill$title.position, "top")
  expect_equal(plot_test_colourbar$guides$fill$title.hjust, 0.5)

  plot_test_legend <- plot +
    theme_capture_continuous(guide = "legend")

  expect_equal(plot_test_legend$guides$colour$title.position, "top")
  expect_equal(plot_test_legend$guides$colour$title.hjust, 0.5)
  expect_equal(plot_test_legend$guides$fill$title.position, "top")
  expect_equal(plot_test_legend$guides$fill$title.hjust, 0.5)
})

test_that("theme_capture_continuous correctly removes legend if guide = 'none'", {

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width, fill = Sepal.Width)) +
    ggplot2::geom_point()

  p <- theme_capture_continuous(guide = "none")

  expect_equal(p[[4]]$color, NULL)
  expect_equal(p[[4]]$fill, NULL)

})

test_that("theme_capture_continuous fails if guide isn't colourbar, colorbar, legend, or none", {

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width, fill = Sepal.Width)) +
    ggplot2::geom_point()

  expect_error(plot +
                 theme_capture_continuous(guide = "a"), regexp = "guide %in%")

})

### theme_capture_discrete()

test_that("theme_capture_discrete returns a list", {

  p <- theme_capture_discrete(direction = 1, font_family = "GT Walsheim Pro")
  expect_type(p, "list")

})

test_that("theme_capture_discrete gives error when direction isn't 1 or -1", {

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
    ggplot2::geom_point()

  expect_error(plot +
                 theme_capture_discrete(direction = "a"), regexp = "direction %in%")
})

test_that("theme_capture_discrete correctly changes direction of palette", {

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Species, fill = Species)) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_capture_discrete(direction = 1)

  plot_test_2 <- plot +
    theme_capture_discrete(direction = -1)

  expect_equal(ggplot2::layer_data(plot_test)$colour[1], "#440154FF")
  expect_equal(ggplot2::layer_data(plot_test_2)$colour[1], "#FDE725FF")
  expect_equal(ggplot2::layer_data(plot_test)$fill[1], "#440154FF")
  expect_equal(ggplot2::layer_data(plot_test_2)$fill[1], "#FDE725FF")

})

test_that("theme_capture_discrete correctly edits legend title", {

  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Species, fill = Species)) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_capture_discrete()

  expect_equal(plot_test$guides$colour$title.position, "top")
  expect_equal(plot_test$guides$colour$title.hjust, 0.5)
  expect_equal(plot_test$guides$fill$title.position, "top")
  expect_equal(plot_test$guides$fill$title.hjust, 0.5)
})

### Testing theme_boilerplate

test_that("theme_boilerplate() returns list output when font_family is not provided",
          {
            p <- theme_boilerplate()
            expect_type(p, "list")
            expect_true(p$text$family == "sans")

          })

test_that("theme_boilerplate() returns list output when font_family is provided",
          {
            p <- theme_boilerplate(font_family = "mono")
            expect_type(p, "list")
            expect_true(p$text$family == "mono")

          })

test_that("theme_boilerplate() has desired behaviour", {
  x <- theme_boilerplate()
  #Should return an object of class theme / gg
  expect_true(inherits(x, "gg"))
  expect_s3_class(x, "theme")

  expect_length(x, 15)
  expect_true(all(
    c(
      "text",
      "axis.title",
      "axis.text",
      "axis.line",
      "legend.key",
      "legend.text",
      "legend.title",
      "legend.position",
      "panel.background",
      "panel.border",
      "panel.grid",
      "panel.grid.minor",
      "plot.title",
      "strip.background"
    )
    %in% names(x)
  ))

})

test_that("theme_boilerplate() returns ggplot object", {
  data <- data.frame(x = 1:10, y = 1:10)
  #
  plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_boilerplate()

  expect_true(ggplot2::is.ggplot(plot_test))

})

test_that("theme_boilerplate() has desired behaviour related to text", {
  # Should take a plot and make the plot.title correct
  data <- data.frame(x = 1:10, y = 1:10)
  #
  plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_boilerplate()

  # title size
  expect_true(plot_test$theme$plot.title$size == 15)
  # title hjust
  expect_true(plot_test$theme$plot.title$hjust == 0.5)
  # title vjust
  expect_true(plot_test$theme$plot.title$vjust == 1)
  # title margin
  # expect_equal(plot_test$theme$plot.title$margin[3], unit(5.5, "points"))
  # text family
  expect_false(is.null(plot_test$theme$text))
  expect_true(plot_test$theme$plot.title$size == 15)
  # axis text size and colour
  expect_true(plot_test$theme$axis.text$size == 11* 0.8)
  expect_true(plot_test$theme$axis.text$colour == "grey30")
  # axis title colour
  expect_true(plot_test$theme$axis.title$colour == "grey30")
  # legend.text colour and size
  expect_true(plot_test$theme$legend.text$colour == "grey30")
  expect_true(plot_test$theme$legend.text$size == 11 * 0.8)
  # legend title colour
  expect_true(plot_test$theme$legend.title$colour == "grey30")

})

test_that("theme_boilerplate() has desired behaviour towards panel aesthetics",
          {
            # Should take a plot and make the plot.title correct
            data <- data.frame(x = 1:10, y = 1:10)
            #
            plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y))

            plot_test <- plot +
              theme_boilerplate()

            # panel.border
            expect_type(plot_test$theme$panel.border, "list")
            # pane.background
            expect_true(plot_test$theme$panel.background$fill == "white")
            expect_true(is.na(plot_test$theme$panel.background$colour))

          })

test_that("theme_boilerplate() has desired behaviour towards axis aesthetics",
          {
            # Should take a plot and make the plot.title correct
            data <- data.frame(x = 1:10, y = 1:10)
            #
            plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y))

            plot_test <- plot +
              theme_boilerplate()

            expect_type(plot_test$theme$axis.ticks, "list")
            expect_true(plot_test$theme$axis.ticks$colour == "grey20")
            expect_true(plot_test$theme$axis.line$colour == "grey20")

          })

test_that("theme_boilerplate() has desired behaviour towards legend aesthetics",
          {
            data <- data.frame(x = 1:10, y = 1:10)
            #
            plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y))

            plot_test <- plot +
              theme_boilerplate()

            expect_type(plot_test$theme$legend.key, "list")
            expect_true(is.na(plot_test$theme$legend.key$colour))
            expect_true(plot_test$theme$legend.key$fill == "white")
            expect_true(plot_test$theme$legend.position == "bottom")

          })
