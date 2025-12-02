test_that("dr_plot_sent is rendering a ggplot when it should and failing when it should", {
  df <- DisplayR::disp_example

  plot <- dr_plot_sent(df, sentiment, "percent")

  expect_s3_class(plot, "ggplot")
  first_val <- plot$data$percent[[1]]
  expect_gt(first_val, 34)
  expect_lt(first_val, 35)


  plot_class <-  plot$layers[[1]]$geom
  #Our Geom is col not line
  expect_true("GeomCol" %in% class(plot_class))
  expect_true(!"GeomLine" %in% class(plot_class))


  #We throw errors when columns are not input correctly
  expect_error(dr_plot_sent(df, plot_type = "beans"))
  expect_error(dr_plot_sent(df, bar_labels = "beans"))
  expect_error(dr_plot_sent(df, sentiment_colours = c(1, 2, 3)))
})


test_that("grouped sentiment plot is functioning as expeced", {

  df <- DisplayR::disp_example

  #test plot renders a ggplot object and the first value of n == 57
  plot <- df %>%
    dr_plot_sent_group(group_var = topic,
                       sentiment_var = sentiment,
                       plot_type = "percent",
                       bar_labels = "volume")

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$n[[1]], 57L)

  #test plot accepts strings and symbols to group_var and sentimnet_var args
  plot_string <- df %>%
    dr_plot_sent_group(group_var = "topic",
                       sentiment_var = "sentiment",
                       plot_type = "percent",
                       bar_labels = "volume")

  expect_s3_class(plot_string, "ggplot")

  plot_class <-  plot$layers[[1]]$geom
  #Our Geom is col not line
  expect_true("GeomCol" %in% class(plot_class))
  expect_true(!"GeomLine" %in% class(plot_class))

  #Check that error is raised properly when arguments are inputted badly
  expect_error(dr_plot_sent_group(df, plot_type = "beans"))
  expect_error(dr_plot_sent_group(df, bar_labels = "beans"))
  expect_error(dr_plot_sent_group(df, sentiment_colours = c(1, 2, 3)))
})

test_that("Sentiment over time is working as expected", {
  expect_error(data %>% dr_plot_sent_vot(), "is\\.data\\.frame\\(data\\) is not TRUE")

  df <- DisplayR::disp_example
  plot <- df %>% dr_plot_sent_vot()
  plot_line <- df %>% dr_plot_sent_vot(plot_type = "line")

  #PLot worked
  expect_s3_class(plot, "ggplot")

  #Geometries
  plot_class <-  class(plot$layers[[1]][["geom"]])
  plot_line_class <- class(plot_line$layers[[1]][["geom"]])

  expect_true("GeomCol" %in% plot_class) # Bar chart is a bar chart
  expect_false("GeomCol" %in% plot_line_class) # Line chart isn't a bar chart
  expect_true("GeomLine" %in% plot_line_class) # Line chart is a line chart
})
