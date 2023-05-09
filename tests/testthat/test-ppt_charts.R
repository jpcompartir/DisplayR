# Test that the function returns a mschart object
test_that("disp_ms_vot returns a mschart object", {
  # Create example data
  data <- data.frame(date = as.Date("2021-01-01") + 0:29, value = rnorm(30))

  chart <- disp_ms_vot(data, date = date)
  expect_s3_class(chart, "ms_linechart")
})

