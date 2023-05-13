# Test that the function returns an mschart object
test_that("disp_ms_vot returns an mschart object", {
  # Create example data
  data <- data.frame(date = as.Date("2021-01-01") + 0:29, value = rnorm(30))

  chart <- disp_ms_vot(data, date = date)
  expect_s3_class(chart, "ms_linechart")

  #Function accepts string and symbol
  string_inputs <- disp_ms_vot(data, "date")
  expect_s3_class(chart, "ms_linechart")
})

test_that("disp_ms_vot_grouped is returning an mschart object",{

  df <- DisplayR::disp_example
  chart <- df %>%
    disp_ms_vot_grouped(date, topic)
  expect_s3_class(chart, "ms_linechart")

  #Function accepts string and symbol
  string_inputs <- disp_ms_vot_grouped(df, "date", "topic")
  expect_s3_class(chart, "ms_linechart")
})


test_that("disp_ms_sent_grouped is returning an mschart object",{

  df <- DisplayR::disp_example
  chart <- df %>%
    disp_ms_sent_grouped(sentiment, topic, plot_type = "percent")

  expect_s3_class(chart, "ms_barchart")


  #Function accepts string and symbol
  string_inputs <- disp_ms_sent_grouped(df, sentiment_var = "sentiment", group_var = "topic", plot_type = "percent")
  expect_s3_class(chart, "ms_barchart")

})



