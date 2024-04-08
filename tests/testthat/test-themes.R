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

### theme_capture_discrete() ----

test_that("theme_capture_discrete returns a list with default arguments", {

  theme <- theme_capture_discrete()
  expect_type(theme, "list")

})


test_that("direction arguments accept valid inputs and don't accept invalid",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
              ggplot2::geom_point()

            # invalid direction - should throw error
            expect_error(plot + theme_capture_discrete(direction = "a"), regexp = "\'arg' should be one of \"forwards\"")

            # valid directions
            expect_no_error(plot + theme_capture_discrete(direction = "forwards"))
            expect_no_error(plot + theme_capture_discrete(direction = "backwards"))

          })

test_that("theme_capture_discrete correctly changes direction of palette",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(
                                      x = Sepal.Length,
                                      y = Sepal.Width,
                                      colour = Species,
                                      fill = Species
                                    )) +
              ggplot2::geom_point()

            plot_direction_forward <- plot +
              theme_capture_discrete(direction = "forwards")

            plot_direction_backward <- plot +
              theme_capture_discrete(direction = "backwards")

            # plot_direction_forward and plot_direction_backward first colours are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$colour[1], "#440154FF")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$colour[1], "#FDE725FF")

            # plot_direction_forward and plot_direction_backward first fills are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$fill[1], "#440154FF")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$fill[1], "#FDE725FF")

          })

test_that("theme_capture_discrete correctly edits legend title", {

  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Species,
                            fill = Species
                          )) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_capture_discrete()

  # Colour guide correct
  expect_equal(plot_test$guides$colour$title.position, "top")
  expect_equal(plot_test$guides$colour$title.hjust, 0.5)

  # Fill guide correct
  expect_equal(plot_test$guides$fill$title.position, "top")
  expect_equal(plot_test$guides$fill$title.hjust, 0.5)

})

### theme_capture_continuous() ----

test_that("theme_capture_continuous returns a list with default arguments", {

  theme <- theme_capture_continuous()
  expect_type(theme, "list")

})

test_that("direction arguments accept valid inputs and don't accept invalid",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
              ggplot2::geom_point()

            # invalid direction - should throw error
            expect_error(plot + theme_capture_continuous(direction = "a"), regexp = "\'arg' should be one of \"forwards\"")

            # valid directions
            expect_no_error(plot + theme_capture_continuous(direction = "forwards"))
            expect_no_error(plot + theme_capture_continuous(direction = "backwards"))

          })

test_that("theme_capture_continuous correctly changes direction of palette",
          {
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
              theme_capture_continuous(direction = "forwards")

            plot_direction_backward <- plot +
              theme_capture_continuous(direction = "backwards")

            # plot_direction_forward and plot_direction_backward first colours are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$colour[1], "#36AD7F")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$colour[1], "#30728D")

            # plot_direction_forward and plot_direction_backward first fills are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$fill[1], "#36AD7F")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$fill[1], "#30728D")

          })

test_that("guide arguments accept valid inputs and don't accept invalid",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(
              data = iris,
              ggplot2::aes(
                x = Sepal.Length,
                y = Sepal.Width,
                colour = Sepal.Width,
                fill = Sepal.Width
              )
            ) +
              ggplot2::geom_point()

            # Invalid guide type throws error
            expect_error(theme_capture_continuous(guide = "a"), regexp = "\'arg' should be one of \"colourbar\"")

            # Valid guide type returns list
            expect_type(theme_capture_continuous(guide = "colourbar"), "list")
            expect_type(theme_capture_continuous(guide = "legend"), "list")
            expect_type(theme_capture_continuous(guide = "none"), "list")
          })

test_that("theme_capture_continuous correctly edits legend aesthetics", {
  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Sepal.Width,
                            fill = Sepal.Width
                          )) +
    ggplot2::geom_point()

  # Guide = colourbar
  plot_continuous_colourbar <- plot +
    theme_capture_continuous(guide = "colourbar")

  # colour
  expect_equal(plot_continuous_colourbar$guides$colour$title.position, "top")
  expect_equal(plot_continuous_colourbar$guides$colour$title.hjust, 0.5)

  # fill
  expect_equal(plot_continuous_colourbar$guides$fill$title.position, "top")
  expect_equal(plot_continuous_colourbar$guides$fill$title.hjust, 0.5)

  # Guide = legend
  plot_continuous_legend <- plot +
    theme_capture_continuous(guide = "legend")

  # colour
  expect_equal(plot_continuous_legend$guides$colour$title.position, "top")
  expect_equal(plot_continuous_legend$guides$colour$title.hjust, 0.5)

  # fill
  expect_equal(plot_continuous_legend$guides$fill$title.position, "top")
  expect_equal(plot_continuous_legend$guides$fill$title.hjust, 0.5)

  # Guide = none
  theme_no_guide <- theme_capture_continuous(guide = "none")

  # colour
  expect_equal(theme_no_guide[[4]]$colour, "none")
  # fill
  expect_equal(theme_no_guide[[4]]$fill, "none")
})

### dr_theme_capture ----

test_that("dr_theme_capture returns a list by default", {
  p <- dr_theme_capture()
  expect_type(p, "list")

})

test_that("scale_type arguments accept valid inputs and don't accept invalid inputs",
          {
            # Invalid scale_type throws an error
            expect_error(
              dr_theme_capture(scale_type = "abcd"),
              "should be one of \"discrete\", \"continuous\""
            )

            # Valid scale_type argument return a list
            discrete_scale_type <-
              dr_theme_capture(scale_type = "discrete")
            expect_type(discrete_scale_type, "list")

            continuous_scale_type <-
              dr_theme_capture(scale_type = "continuous")
            expect_type(continuous_scale_type, "list")

          })

test_that("guide arguments accept valid inputs and don't accept invalid inputs",
          {
            # Invalid guide argument throws an error
            expect_error(plot + dr_theme_capture(scale_type = "continuous",
                                                 guide = "abcd"),
                         regexp = "\'arg\' should be one of \"colourbar\"")

            # Make testing plot
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
              ggplot2::geom_point()

            # Valid guide argument returns a list
            colourbar_guide <- dr_theme_capture(guide = "colourbar")
            expect_type(colourbar_guide, "list")

            legend_guide <- dr_theme_capture(guide = "legend")
            expect_type(legend_guide, "list")

          })

### theme_samy_discrete() ----

test_that("theme_samy_discrete returns a list with default arguments", {

  theme <- theme_samy_discrete()
  expect_type(theme, "list")

})


test_that("theme_samy_discrete correctly edits legend title", {

  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Species,
                            fill = Species
                          )) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_samy_discrete()

  # Colour guide correct
  expect_equal(plot_test$guides$colour$title.position, "top")
  expect_equal(plot_test$guides$colour$title.hjust, 0.5)

  # Fill guide correct
  expect_equal(plot_test$guides$fill$title.position, "top")
  expect_equal(plot_test$guides$fill$title.hjust, 0.5)

})

### theme_samy_continuous() ----

test_that("theme_samy_continuous returns a list with default arguments", {

  theme <- theme_samy_continuous()
  expect_type(theme, "list")

})

test_that("direction arguments accept valid inputs and don't accept invalid",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
              ggplot2::geom_point()

            # invalid direction - should throw error
            expect_error(plot + theme_samy_continuous(direction = "a"), regexp = "\'arg\' should be one of \"forwards\"")

            # valid directions
            expect_no_error(plot + theme_samy_continuous(direction = "forwards"))
            expect_no_error(plot + theme_samy_continuous(direction = "backwards"))

          })

test_that("theme_samy_continuous correctly changes direction of palette",
          {
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
              theme_samy_continuous(direction = "forwards")

            plot_direction_backward <- plot +
              theme_samy_continuous(direction = "backwards")

            # plot_direction_forward and plot_direction_backward first colours are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$colour[1], "#D87C6D")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$colour[1], "#F67C4C")

            # plot_direction_forward and plot_direction_backward first fills are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$fill[1], "#D87C6D")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$fill[1], "#F67C4C")

          })

test_that("guide arguments accept valid inputs and don't accept invalid",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(
              data = iris,
              ggplot2::aes(
                x = Sepal.Length,
                y = Sepal.Width,
                colour = Sepal.Width,
                fill = Sepal.Width
              )
            ) +
              ggplot2::geom_point()

            # Invalid guide type throws error
            expect_error(theme_samy_continuous(guide = "a"), regexp = "\'arg\' should be one of \"legend\"")

            # Valid guide type returns list
            expect_type(theme_samy_continuous(guide = "colourbar"), "list")
            expect_type(theme_samy_continuous(guide = "legend"), "list")
            expect_type(theme_samy_continuous(guide = "none"), "list")
          })

test_that("theme_samy_continuous correctly edits legend aesthetics", {
  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Sepal.Width,
                            fill = Sepal.Width
                          )) +
    ggplot2::geom_point()

  # Guide = colourbar
  plot_continuous_colourbar <- plot +
    theme_samy_continuous(guide = "colourbar")

  # colour
  expect_equal(plot_continuous_colourbar$guides$colour$title.position, "top")
  expect_equal(plot_continuous_colourbar$guides$colour$title.hjust, 0.5)

  # fill
  expect_equal(plot_continuous_colourbar$guides$fill$title.position, "top")
  expect_equal(plot_continuous_colourbar$guides$fill$title.hjust, 0.5)

  # Guide = legend
  plot_continuous_legend <- plot +
    theme_samy_continuous(guide = "legend")

  # colour
  expect_equal(plot_continuous_legend$guides$colour$title.position, "top")
  expect_equal(plot_continuous_legend$guides$colour$title.hjust, 0.5)

  # fill
  expect_equal(plot_continuous_legend$guides$fill$title.position, "top")
  expect_equal(plot_continuous_legend$guides$fill$title.hjust, 0.5)

  # Guide = none
  theme_no_guide <- theme_samy_continuous(guide = "none")

  # colour
  expect_equal(theme_no_guide[[4]]$colour, NULL)
  # fill
  expect_equal(theme_no_guide[[4]]$fill, NULL)
})

### dr_theme_samy ----

test_that("dr_theme_samy returns a list by default", {
  p <- dr_theme_samy()
  expect_type(p, "list")

})

test_that("scale_type arguments accept valid inputs and don't accept invalid inputs",
          {
            # Invalid scale_type throws an error
            expect_error(
              dr_theme_samy(scale_type = "abcd"),
              "should be one of \"discrete\", \"continuous\""
            )

            # Valid scale_type argument return a list
            discrete_scale_type <-
              dr_theme_samy(scale_type = "discrete")
            expect_type(discrete_scale_type, "list")

            continuous_scale_type <-
              dr_theme_samy(scale_type = "continuous")
            expect_type(continuous_scale_type, "list")

          })

test_that("guide arguments accept valid inputs and don't accept invalid inputs",
          {
            # Invalid guide argument throws an error
            expect_error(plot + dr_theme_samy(scale_type = "continuous",
                                              guide = "abcd"),
                         regexp = "\'arg\' should be one of \"legend\"")

            # Make testing plot
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
              ggplot2::geom_point()

            # Valid guide argument returns a list
            colourbar_guide <- dr_theme_samy(guide = "colourbar")
            expect_type(colourbar_guide, "list")

            legend_guide <- dr_theme_samy(guide = "legend")
            expect_type(legend_guide, "list")

          })

### theme_share_discrete() ----

test_that("theme_share_discrete returns a list with default arguments", {

  theme <- theme_share_discrete()
  expect_type(theme, "list")

})


test_that("theme_share_discrete correctly edits legend title", {

  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Species,
                            fill = Species
                          )) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_share_discrete()

  # Colour guide correct
  expect_equal(plot_test$guides$colour$title.position, "top")
  expect_equal(plot_test$guides$colour$title.hjust, 0.5)

  # Fill guide correct
  expect_equal(plot_test$guides$fill$title.position, "top")
  expect_equal(plot_test$guides$fill$title.hjust, 0.5)

})

### theme_share_continuous() ----

test_that("theme_share_continuous returns a list with default arguments", {

  theme <- theme_share_continuous()
  expect_type(theme, "list")

})

test_that("direction arguments accept valid inputs and don't accept invalid",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
              ggplot2::geom_point()

            # invalid direction - should throw error
            expect_error(plot + theme_share_continuous(direction = "a"), regexp = "\'arg\' should be one of")

            # valid directions
            expect_no_error(plot + theme_share_continuous(direction = "forwards"))
            expect_no_error(plot + theme_share_continuous(direction = "backwards"))

          })

test_that("theme_share_continuous correctly changes direction of palette",
          {
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
              theme_share_continuous(direction = "forwards")

            plot_direction_backward <- plot +
              theme_share_continuous(direction = "backwards")

            # plot_direction_forward and plot_direction_backward first colours are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$colour[1], "#BC2E96")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$colour[1], "#E54E71")

            # plot_direction_forward and plot_direction_backward first fills are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$fill[1], "#BC2E96")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$fill[1], "#E54E71")

          })

test_that("guide arguments accept valid inputs and don't accept invalid",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(
              data = iris,
              ggplot2::aes(
                x = Sepal.Length,
                y = Sepal.Width,
                colour = Sepal.Width,
                fill = Sepal.Width
              )
            ) +
              ggplot2::geom_point()

            # Invalid guide type throws error
            expect_error(theme_share_continuous(guide = "a"), regexp = "\'arg\' should be one of")

            # Valid guide type returns list
            expect_type(theme_share_continuous(guide = "colourbar"), "list")
            expect_type(theme_share_continuous(guide = "legend"), "list")
            expect_type(theme_share_continuous(guide = "none"), "list")
          })

test_that("theme_share_continuous correctly edits legend aesthetics", {
  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Sepal.Width,
                            fill = Sepal.Width
                          )) +
    ggplot2::geom_point()

  # Guide = colourbar
  plot_continuous_colourbar <- plot +
    theme_share_continuous(guide = "colourbar")

  # colour
  expect_equal(plot_continuous_colourbar$guides$colour$title.position, "top")
  expect_equal(plot_continuous_colourbar$guides$colour$title.hjust, 0.5)

  # fill
  expect_equal(plot_continuous_colourbar$guides$fill$title.position, "top")
  expect_equal(plot_continuous_colourbar$guides$fill$title.hjust, 0.5)

  # Guide = legend
  plot_continuous_legend <- plot +
    theme_share_continuous(guide = "legend")

  # colour
  expect_equal(plot_continuous_legend$guides$colour$title.position, "top")
  expect_equal(plot_continuous_legend$guides$colour$title.hjust, 0.5)

  # fill
  expect_equal(plot_continuous_legend$guides$fill$title.position, "top")
  expect_equal(plot_continuous_legend$guides$fill$title.hjust, 0.5)

  # Guide = none
  theme_no_guide <- theme_share_continuous(guide = "none")

  # colour
  expect_equal(theme_no_guide[[4]]$colour, NULL)
  # fill
  expect_equal(theme_no_guide[[4]]$fill, NULL)
})

### dr_theme_share ----

test_that("dr_theme_share returns a list by default", {
  p <- dr_theme_share()
  expect_type(p, "list")

})

test_that("scale_type arguments accept valid inputs and don't accept invalid inputs",
          {
            # Invalid scale_type throws an error
            expect_error(
              dr_theme_share(scale_type = "abcd"),
              "should be one of \"discrete\", \"continuous\""
            )

            # Valid scale_type argument return a list
            discrete_scale_type <-
              dr_theme_share(scale_type = "discrete")
            expect_type(discrete_scale_type, "list")

            continuous_scale_type <-
              dr_theme_share(scale_type = "continuous")
            expect_type(continuous_scale_type, "list")

          })

test_that("guide arguments accept valid inputs and don't accept invalid inputs",
          {
            # Invalid guide argument throws an error
            expect_error(plot + dr_theme_share(scale_type = "continuous",
                                               guide = "abcd"),
                         regexp = "\'arg\' should be one of")

            # Make testing plot
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
              ggplot2::geom_point()

            # Valid guide argument returns a list
            colourbar_guide <- dr_theme_share(guide = "colourbar")
            expect_type(colourbar_guide, "list")

            legend_guide <- dr_theme_share(guide = "legend")
            expect_type(legend_guide, "list")

          })

### theme_microsoft_discrete() ----

test_that("theme_microsoft_discrete returns a list with default arguments", {

  theme <- theme_microsoft_discrete()
  expect_type(theme, "list")

})


test_that("theme_microsoft_discrete correctly edits legend title", {

  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Species,
                            fill = Species
                          )) +
    ggplot2::geom_point()

  plot_test <- plot +
    theme_microsoft_discrete()

  # Colour guide correct
  expect_equal(plot_test$guides$colour$title.position, "top")
  expect_equal(plot_test$guides$colour$title.hjust, 0.5)

  # Fill guide correct
  expect_equal(plot_test$guides$fill$title.position, "top")
  expect_equal(plot_test$guides$fill$title.hjust, 0.5)

})

### theme_microsoft_continuous() ----

test_that("theme_microsoft_continuous returns a list with default arguments", {

  theme <- theme_microsoft_continuous()
  expect_type(theme, "list")

})

test_that("direction arguments accept valid inputs and don't accept invalid",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
              ggplot2::geom_point()

            # invalid direction - should throw error
            expect_error(plot + theme_microsoft_continuous(direction = "a"), regexp = "\'arg\' should be one of \"forwards\", \"backwards\"")

            # valid directions
            expect_no_error(plot + theme_microsoft_continuous(direction = "forwards"))
            expect_no_error(plot + theme_microsoft_continuous(direction = "backwards"))

          })

test_that("theme_microsoft_continuous correctly changes direction of palette",
          {
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
              theme_microsoft_continuous(direction = "forwards")

            plot_direction_backward <- plot +
              theme_microsoft_continuous(direction = "backwards")

            # plot_direction_forward and plot_direction_backward first colours are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$colour[1], "#327B4A")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$colour[1], "#618D0E")

            # plot_direction_forward and plot_direction_backward first fills are different
            expect_equal(ggplot2::layer_data(plot_direction_forward)$fill[1], "#327B4A")
            expect_equal(ggplot2::layer_data(plot_direction_backward)$fill[1], "#618D0E")

          })

test_that("guide arguments accept valid inputs and don't accept invalid",
          {
            # Create testing plots
            plot <- ggplot2::ggplot(
              data = iris,
              ggplot2::aes(
                x = Sepal.Length,
                y = Sepal.Width,
                colour = Sepal.Width,
                fill = Sepal.Width
              )
            ) +
              ggplot2::geom_point()

            # Invalid guide type throws error
            expect_error(theme_microsoft_continuous(guide = "a"), regexp = "\'arg\' should be one of \"legend\"")

            # Valid guide type returns list
            expect_type(theme_microsoft_continuous(guide = "colourbar"), "list")
            expect_type(theme_microsoft_continuous(guide = "legend"), "list")
            expect_type(theme_microsoft_continuous(guide = "none"), "list")
          })

test_that("theme_microsoft_continuous correctly edits legend aesthetics", {
  # Create testing plots
  plot <- ggplot2::ggplot(data = iris,
                          ggplot2::aes(
                            x = Sepal.Length,
                            y = Sepal.Width,
                            colour = Sepal.Width,
                            fill = Sepal.Width
                          )) +
    ggplot2::geom_point()

  # Guide = colourbar
  plot_continuous_colourbar <- plot +
    theme_microsoft_continuous(guide = "colourbar")

  # colour
  expect_equal(plot_continuous_colourbar$guides$colour$title.position, "top")
  expect_equal(plot_continuous_colourbar$guides$colour$title.hjust, 0.5)

  # fill
  expect_equal(plot_continuous_colourbar$guides$fill$title.position, "top")
  expect_equal(plot_continuous_colourbar$guides$fill$title.hjust, 0.5)

  # Guide = legend
  plot_continuous_legend <- plot +
    theme_microsoft_continuous(guide = "legend")

  # colour
  expect_equal(plot_continuous_legend$guides$colour$title.position, "top")
  expect_equal(plot_continuous_legend$guides$colour$title.hjust, 0.5)

  # fill
  expect_equal(plot_continuous_legend$guides$fill$title.position, "top")
  expect_equal(plot_continuous_legend$guides$fill$title.hjust, 0.5)

  # Guide = none
  theme_no_guide <- theme_microsoft_continuous(guide = "none")

  # colour
  expect_equal(theme_no_guide[[4]]$colour, NULL)
  # fill
  expect_equal(theme_no_guide[[4]]$fill, NULL)
})

### dr_theme_microsoft ----

test_that("dr_theme_microsoft returns a list by default", {
  p <- dr_theme_microsoft()
  expect_type(p, "list")

})

test_that("scale_type arguments accept valid inputs and don't accept invalid inputs",
          {
            # Invalid scale_type throws an error
            expect_error(
              dr_theme_microsoft(scale_type = "abcd"),
              "should be one of \"discrete\", \"continuous\""
            )

            # Valid scale_type argument return a list
            discrete_scale_type <-
              dr_theme_microsoft(scale_type = "discrete")
            expect_type(discrete_scale_type, "list")

            continuous_scale_type <-
              dr_theme_microsoft(scale_type = "continuous")
            expect_type(continuous_scale_type, "list")

          })

test_that("guide arguments accept valid inputs and don't accept invalid inputs",
          {
            # Invalid guide argument throws an error
            expect_error(plot + dr_theme_microsoft(scale_type = "continuous",
                                                   guide = "abcd"),
                         regexp = "\'arg\' should be one of \"legend\"")

            # Make testing plot
            plot <- ggplot2::ggplot(data = iris,
                                    ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
              ggplot2::geom_point()

            # Valid guide argument returns a list
            colourbar_guide <- dr_theme_microsoft(guide = "colourbar")
            expect_type(colourbar_guide, "list")

            legend_guide <- dr_theme_microsoft(guide = "legend")
            expect_type(legend_guide, "list")

          })

