test_that("themes correctly edit legend title", {

  theme_options <- c(
    # "share",
    "capture", "samy", "microsoft")
  scale_options <- c("_discrete", "_continuous")
  theme_combinations <- expand.grid(theme_options, scale_options)
  themes <- paste0("theme_", theme_combinations$Var1, theme_combinations$Var2)

  for (theme in themes){
    edit_legend_titles_check(theme = theme)
  }

})
