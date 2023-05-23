#These tests are largely unit tests, failures should indicate that either the example data has changed, or changes to the functions have resulted in differing functionality.
test_that("dr_vot_plots input variables raise errors when they should and don't when they shouldn't and lines are default geom", {

  #Get some data for plotting
  df <- DisplayR::disp_example

  #No date2 column so raise error
  expect_error(df %>% dr_plot_vot(date_var = date2))

  #Check data entered correctly renders a plot and date can be string
  plot <- df %>% dr_plot_vot(date_var = "date")

  #Check we've got lines not bars
  plot_geom <- plot$layers[[1]]$geom

  expect_false("GeomCol" %in% class(plot_geom))
  expect_true("GeomLine" %in% class(plot_geom))

  # Check for other errors in inputs aside from date_var = date. Ensure date can be input as symbol.
  expect_error(df %>% dr_plot_vot(date_var = date,plot_type = "beans"))
  expect_error(df %>% dr_plot_vot(date_var = date,bar_colour = "beans"))
  expect_error(df %>% dr_plot_vot(date_var = date, time_unit = "second"))
  expect_error(df %>% dr_plot_vot(date_var = date, smooth = "True"))
})

test_that("dr_plot_vot bars are working", {

  plot <- DisplayR::disp_example %>%
    dplyr::filter(!is.na(sentiment)) %>%
    dr_plot_vot(date = date, time_unit = "day", smooth = FALSE, plot_type = "bar")

  plot_geom <- plot$layers[[1]]$geom

  expect_true("GeomCol" %in% class(plot_geom))
  expect_false("GeomLine" %in% class(plot_geom))

})

test_that("dr_plot_vot_group fails and succeeds when it ought to", {
  plot <- DisplayR::disp_example %>%
    dr_plot_vot_group(group_var = topic, date_var = date, time_unit = "day")
  expect_s3_class(plot, "ggplot")

  #Check the first value of n == 6,if not function behaviour or example data has changed
  expect_true(plot$data[["n"]][[1]] == 6)

  #Function takes symbol and string for group and date_var arguments
  plot_two <- DisplayR::disp_example %>%
    dr_plot_vot_group(group_var = "topic", date_var = "date", time_unit = "day")
  expect_s3_class(plot_two, "ggplot")

  #Check this doesn't change behaviourl and that no stochasticity in plot (why would there be...?)
  expect_true(all(plot_two$data$n == plot$data$n))

  #Check errors are raised when they ought to be
  expect_error(DisplayR::disp_example %>%
                 dr_plot_vot_group(group_var = topic, date_var = date, time_unit = "second"))
  expect_error(DisplayR::disp_example %>%
                 dr_plot_vot_group(group_var = topic2, date_var = date, time_unit = "day"))
  expect_error(DisplayR::disp_example %>%
                 dr_plot_vot_group(group_var = topic, date_var = date2, time_unit = "month"))

})
