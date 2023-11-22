### dr_plot_raincloud

test_that("dr_plot_raincloud is rendering a ggplot when it should and failing when it should", {

  expect_error(dr_plot_raincloud(df, grouping_variable = sentiment, continuous_variable = date))
  df <- datasets::iris

  plot <- dr_plot_raincloud(df,
                            grouping_variable = Species,
                            continuous_variable = Sepal.Length,
                            bandwidth = 0.3)

  # Test plot renders a ggplot object and first value of Sepal.Length is 5.1
  expect_s3_class(plot, "ggplot")
  first_val <- plot$data$Sepal.Length[[1]]
  expect_equal(first_val, 5.1)

  plot_class <-  plot$layers[[1]]$geom
  #Our Geom is boxplot not line

  expect_true("GeomBoxplot" %in% class(plot_class))
  expect_true(!"GeomLine" %in% class(plot_class))

  #We throw errors when columns are not input correctly
  expect_error(dr_plot_raincloud(df, grouping_variable = "asdf"))
  expect_error(dr_plot_raincloud(df, continuous_variable = "asdf"))
  expect_error(dr_plot_raincloud(df, bandwidth = "asdf"))
  expect_error(dr_plot_raincloud(df, bandwidth == 0.5))
})
