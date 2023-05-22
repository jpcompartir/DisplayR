test_that("dr_vot_plots input variables raise errors when they should and don't when they shouldn't", {

  #Get some data for plotting
  df <- DisplayR::disp_example

  #No date2 column so raise error
  expect_error(df %>% dr_plot_vot(date_var = date2))

  #Check data entered correctly renders a plot
  plot <- df %>% dr_plot_vot(date_var = "date")

  # Check for other errors in inputs aside from date_var = dae
  expect_error(df %>% dr_plot_vot(date_var = date,plot_type = "beans"))
  expect_error(df %>% dr_plot_vot(date_var = date,bar_colour = "beans"))
  expect_error(df %>% dr_plot_vot(date_var = date, time_unit = "second"))
  expect_error(df %>% dr_plot_vot(date_var = date, smooth = "True"))
})

test_that("dr_plot_vot bars are working", {

  plot <- DisplayR::disp_example %>% dr_plot_vot(date = date, time_unit = "day", smooth = FALSE, plot_type = "line")
  build <- ggplot2::ggplot_build(plot)

})

test_that("dr_plot_vot bars are working", {

  plot <- DisplayR::disp_example %>% dr_plot_vot(date = date, time_unit = "day", smooth = FALSE, plot_type = "line")
})
