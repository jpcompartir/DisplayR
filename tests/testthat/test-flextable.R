test_that("Basic tests for disp_flextable function", {
  # disp_flextable returns a flextable object
  df <- DisplayR::disp_example

  ft <- disp_flextable(df, group_var = topic, sentiment_var = sentiment)
  expect_s3_class(ft, "flextable")

  # accepts a string and symbols
  ft_string <- disp_flextable(df, group_var = "topic", sentiment_var = "sentiment")
  expect_s3_class(ft_string, "flextable")
  expect_equal(ft_string$col_keys, ft$col_keys)

  # Check column names are still identical
  expect_equal(names(ft$body$dataset), names(ft_string$body$dataset))

  # Check values are still being calculated appropiately
  expect_true(ft$body$dataset[2, 2] == 798)
  expect_true(ft$body$dataset[2, 3] == 42.7)
})
