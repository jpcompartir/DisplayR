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

  testthat::expect_error(plot +
                 theme_boilerplate(base_size = 0))

  testthat::expect_error(plot +
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

  expect_equal(as.numeric(plot_base$theme$plot.title$margin[3]), 5)

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

# test dr_*, *_continuous and *_discrete themes ----
test_that("themes correctly edit legend title and aesthetics", {

  theme_options <- c(
    "share",
    "capture", "samy", "microsoft")
  scale_options <- c("_discrete", "_continuous")
  theme_combinations <- expand.grid(theme_options, scale_options)
  themes <- paste0("theme_", theme_combinations$Var1, theme_combinations$Var2)

  for (theme in themes){
    theme_edits_legend(theme = theme)
  }

})

test_that("themes return list with default arguments", {

  theme_options <- c(
    "share",
    "capture", "samy", "microsoft")
  scale_options <- c("_discrete", "_continuous")
  theme_combinations <- expand.grid(theme_options, scale_options)
  themes <- c(paste0("theme_", theme_combinations$Var1, theme_combinations$Var2),
              paste0("dr_theme_", theme_options))

  for (theme in themes){
    theme_func <- get(theme)
    expect_equal(class(theme_func()), "list", info = paste0("Theme generating failure: ", theme))
  }

})

test_that("direction arguments accept valid inputs and don't accept invalid", {

  theme_options <- c(
    "theme_share_continuous",
    "theme_capture_continuous", "theme_capture_discrete",
    "theme_samy_continuous",
    "theme_microsoft_continuous"
  )

  for (theme in theme_options){
    theme_accepts_direction_args(theme = theme)
  }

})

test_that("themes correctly change palette direction", {

  theme_options <- c(
    "theme_share_continuous",
    "theme_capture_continuous", "theme_capture_discrete",
    "theme_samy_continuous",
    "theme_microsoft_continuous"
  )

  colour_forward <- c("#BC2E96", "#36AD7F",  "#36AD7F", "#D87C6D", "#327B4A")
  colour_backward <- c("#E54E71", "#30728D", "#30728D", "#F67C4C", "#618D0E")

  for (i in length(theme_options)){
    theme_edits_palette_direction(theme = theme_options[i], colour_forward[i], colour_backward[i])
  }
})

test_that("guide arguments accept valid inputs and don't accept invalid", {

  theme_options <- c("share", "capture", "samy", "microsoft")
  scale_options <- "_continuous"
  theme_combinations <- expand.grid(theme_options, scale_options)
  themes <- c(paste0("theme_", theme_combinations$Var1, theme_combinations$Var2),
              paste0("dr_theme_", theme_options))

  for (i in 1:length(themes)){

    theme <- themes[i]
    theme_func <- get(theme)

    if (i < 5){ # explicitly continuous themes
      testthat::expect_error(theme_func(guide = "a"),
                   regexp = "^\'arg\' should be one of",
                   info = paste0("Theme generating failure: ", theme))
    } else { # need to specify continuous scales for dr_* functions
      testthat::expect_error(theme_func(scale_type = "continuous",
                              guide = "a"),
                   regexp = "^\'arg\' should be one of",
                   info = paste0("Theme generating failure: ", theme))
    }

    # Valid guide type returns list
    expect_equal(class(theme_func(guide = "colourbar")), "list", info = paste0("Theme generating failure: ", theme))
    expect_equal(class(theme_func(guide = "legend")), "list", info = paste0("Theme generating failure: ", theme))

  }

})

test_that("scale_type arguments accept valid inputs and don't accept invalid inputs", {

  theme_options <- c("share", "capture", "samy", "microsoft")
  themes <- paste0("dr_theme_", theme_options)

  for (theme in themes){

    theme_func <- get(theme)

    # Invalid scale_type throws an error
    testthat::expect_error(
      theme_func(scale_type = "abcd"),
      "should be one of \"discrete\", \"continuous\"",
      info = paste0("Theme generating failure: ", theme)
    )

    # Valid scale_type argument return a list
    discrete_scale_type <- theme_func(scale_type = "discrete")
    expect_equal(class(discrete_scale_type), "list", info = paste0("Theme generating failure: ", theme))

    continuous_scale_type <- theme_func(scale_type = "continuous")
    expect_equal(class(continuous_scale_type), "list", info = paste0("Theme generating failure: ", theme))
  }


})

