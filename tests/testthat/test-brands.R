test_that("dr_list_brands works", {
  expect_in("shareds", dr_list_brands())
})


test_that("dr_add_brand adds the _brand.yml to the directory", {
  temp_dir <- file.path(tempdir(), basename(tempfile()))
  dir.create(temp_dir)

  expect_length(list.files(temp_dir), 0)
  dr_add_brand("shareds", temp_dir)
  expect_length(list.files(temp_dir), 2)

  expect_setequal(list.files(temp_dir), c("_brand.yml", "logo.png"))

  unlink(temp_dir, recursive = TRUE)
})
