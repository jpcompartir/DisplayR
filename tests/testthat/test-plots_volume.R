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



