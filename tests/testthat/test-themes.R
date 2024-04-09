### Testing theme_boilerplate ----

test_that("theme_boilerplate() returns list output when default arguments used",
          {
            theme <- theme_boilerplate()
            expect_type(theme, "list")

            expected_names <- c(
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
              "strip.background",
              "axis.ticks"
            )
            expect_setequal(expected_names, names(theme[[1]]))

})

test_that("theme_boilerplate() returns ggplot object when theme applied", {
  # Create testing plots
  plot <-
    ggplot2::ggplot(data = iris,
                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
    ggplot2::geom_point()

  plot_boilerplate <- plot +
    theme_boilerplate()

  expect_true(ggplot2::is.ggplot(plot_boilerplate))

})

test_that("theme_boilerplate checks arguments are valid",{

  plot <- test_plot()

  expect_error(plot +
                 theme_boilerplate(base_size = 0))

  expect_error(plot +
                 theme_boilerplate(base_size = -2))
})

# test_that("font_family argument behaves as expected with correct inputs", {
#   plot <- test_plot()
#
#   plot_boilerplate <- plot +
#     theme_boilerplate()
#
#   expect_equal(plot_boilerplate$theme$text$family, "sans") # More specific
#   # plot_serif <- plot +
#   #   theme_boilerplate(font_family = "serif")
#   # expect_equal(plot_serif$theme$text$family, "serif")
# })

test_that("base_size argument accepts new inputs", {

  plot <- test_plot()
  plot_base <- plot +
    theme_boilerplate(base_size = 10)

  # Is this intended to be tested by?
  # expect_equal(plot_boilerplate$theme$plot.title$margin[3], unit(5.5, "points"))

})

test_that("theme_boilerplate() has desired behaviour related to text", {
  # Create testing plots
  plot <-
    ggplot2::ggplot(data = iris,
                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
    ggplot2::geom_point()

  plot_boilerplate <- plot +
    theme_boilerplate()

  # title size
  expect_true(plot_boilerplate$theme$plot.title$size == 15)
  # title hjust
  expect_true(plot_boilerplate$theme$plot.title$hjust == 0.5)
  # title vjust
  expect_true(plot_boilerplate$theme$plot.title$vjust == 1)
  # title margin
  ## expect_equal(plot_boilerplate$theme$plot.title$margin[3], unit(5.5, "points"))
  # expect_true(plot_boilerplate$theme$plot.title$size == 15) #Dupe
  # axis text size and colour
  expect_true(plot_boilerplate$theme$axis.text$size == 11 * 0.8)
  expect_true(plot_boilerplate$theme$axis.text$colour == "grey30")
  # axis title colour
  expect_true(plot_boilerplate$theme$axis.title$colour == "grey30")
  # legend.text colour and size
  expect_true(plot_boilerplate$theme$legend.text$colour == "grey30")
  expect_true(plot_boilerplate$theme$legend.text$size == 11 * 0.8)
  # legend title colour
  expect_true(plot_boilerplate$theme$legend.title$colour == "grey30")

})

test_that("theme_boilerplate() has desired behaviour towards panel aesthetics",
          {
            # Create testing plots
            plot <-
              ggplot2::ggplot(data = iris,
                              ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
              ggplot2::geom_point()

            plot_boilerplate <- plot +
              theme_boilerplate()

            # panel.border
            # expect_type(plot_boilerplate$theme$panel.border, "list")
            expect_contains(attributes(plot_boilerplate$theme$panel.border)$class, "element_blank")


            # panel.background
            expect_true(plot_boilerplate$theme$panel.background$fill == "white")
            expect_true(is.na(plot_boilerplate$theme$panel.background$colour))

          })

test_that("theme_boilerplate() has desired behaviour towards axis aesthetics",
          {
            # Create testing plots
            plot <-
              ggplot2::ggplot(data = iris,
                              ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
              ggplot2::geom_point()

            plot_boilerplate <- plot +
              theme_boilerplate()

            # axis ticks
            expect_type(plot_boilerplate$theme$axis.ticks, "list")
            # axis ticks colour
            expect_true(plot_boilerplate$theme$axis.ticks$colour == "grey20")
            # axis line colour
            expect_true(plot_boilerplate$theme$axis.line$colour == "grey20")

          })

test_that("theme_boilerplate() has desired behaviour towards legend aesthetics",
          {
            # Create testing plots
            plot <-
              ggplot2::ggplot(data = iris,
                              ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
              ggplot2::geom_point()

            plot_boilerplate <- plot +
              theme_boilerplate()

            # legend key
            expect_type(plot_boilerplate$theme$legend.key, "list")
            # legend key colour
            expect_true(is.na(plot_boilerplate$theme$legend.key$colour))
            # legend key fill
            expect_true(plot_boilerplate$theme$legend.key$fill == "white")
            # legend position
            expect_true(plot_boilerplate$theme$legend.position == "bottom")

          })

