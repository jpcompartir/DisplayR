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

