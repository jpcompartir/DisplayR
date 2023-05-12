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

})
