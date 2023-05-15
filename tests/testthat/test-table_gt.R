#Summary table is created effectively
test_that("make_gt_summary_table creates summaries appropriately", {
  df <- DisplayR::disp_example

  summary <- df %>%
    make_gt_summary_table(topic, sentiment)

  expect_equal(nrow(summary), 9)
  expect_s3_class(summary, "tbl_df")
  expect_vector(colnames(summary), c("topic", "volume", "negative", "neutral", "positive"))

  #Check that strings and symbols are ok to input
summary <- df %>%
  make_gt_summary_table("topic", "sentiment")
expect_s3_class(summary, "tbl_df")

#Check types of volume + a sentiment column
expect_type(summary$volume, "integer")
expect_type(summary$positive, "double")
})


test_that("disp_gt_vot is functioning as expected",{
  df <- DisplayR::disp_example %>%
    dplyr::filter(!is.na(sentiment))

  plot <- disp_gt_vot(df, date)

  #plot is a ggplot object
  expect_s3_class(plot, "ggplot")

  #Values are as expected according to data on 12th May 2023
  weekly_n_1 <- 298
  expect_equal(plot$data[["n"]][1], weekly_n_1)

  plot_build <- ggplot2::ggplot_build(plot)

  #Check that default settings render correct colour
  expect_equal(unique(plot_build$data[[1]][["fill"]]), "#628EFD")

  #time_unit is functioning
  plot_daily <- disp_gt_vot(df, date, time_unit = "day")
  #Check the value of daily is less than weekly value
  expect_lt(plot_daily$data$n[[1]], weekly_n_1)


  #Check that non-default settings render colours appropriately
  plot <- disp_gt_vot(df, date, bar_colour = "#0f50d2")
  plot_build <- ggplot2::ggplot_build(plot)
  expect_equal(unique(plot_build$data[[1]][["fill"]]), "#0f50d2")

  #Function allows strings and sybols
  plot <- disp_gt_vot(df, "date")
  expect_s3_class(plot, "ggplot")

  })

test_that("disp_gt_sent_time is functioning as expected", {
  df <- DisplayR::disp_example %>%
    dplyr::filter(!is.na(sentiment))

  #Function allows strings and sybols
  plot <- disp_gt_sent_time(df, "sentiment")
  expect_s3_class(plot, "ggplot")

  plot <- disp_gt_sent_time(df, sentiment_var = sentiment)
  expect_s3_class(plot, "ggplot")

  plot_build <- ggplot2::ggplot_build(plot)

  #Colours are as expected
  expect_equal(plot_build$data[[1]][[1]][[1]], "#c00000")
  expect_equal(plot_build$data[[1]][[1]][[9]], "black")
  expect_equal(plot_build$data[[1]][[1]][[17]], "#1b7837")

  #Data is as expected
  expect_equal(plot_build$data[[1]][[4]][[1]], 119)

  #time_unit is functioning
  plot_weekly <- disp_gt_sent_time(df, sentiment, time_unit = "week")
  plot_daily <- disp_gt_sent_time(df, sentiment, time_unit = "day")
  #Check the value of daily is less than weekly value
  expect_lt(plot_daily$data$n[[1]], plot_weekly$data$n[[1]])

})

test_that("disp_gt is functioning as expected", {
  df <- DisplayR::disp_example %>%
    dplyr::filter(!is.na(sentiment))

  #Function works with proper inputs
  gt_table <- disp_gt(df, sentiment_var = sentiment, group_var = topic, date_var = date, time_unit = "week")

  #Check the table returns an error when an improper column is input
  expect_error(disp_gt(df, sentiment_var = beans, group_var = topic, date_var = date, time_unit = "week"))

  #Check our function is returning the appropriate class
  expect_s3_class(gt_table, "gt_tbl")

  #Check the first topic is as expected
  expect_equal(gt_table$`_data`[["topic"]][[1]], "Conversational AI")

  #Check strings and symbols can be input:
  gt_string <- disp_gt(df, sentiment_var = "sentiment", group_var = topic, date_var = date, time_unit = "week")
  expect_s3_class(gt_string, "gt_tbl")
})

