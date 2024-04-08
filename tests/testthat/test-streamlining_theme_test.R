test_that("themes correctly edit legend title", {

  theme_options <- c(
    # "share",
    "capture", "samy", "microsoft")
  scale_options <- c("_discrete", "_continuous")
  theme_combinations <- expand.grid(theme_options, scale_options)
  themes <- paste0("theme_", theme_combinations$Var1, theme_combinations$Var2)

  for (theme in themes){
    theme_edits_legend(theme = theme)
  }

})

test_that("themes return list with default arguments", {

  theme_options <- c(
    # "share",
    "capture", "samy", "microsoft")
  scale_options <- c("_discrete", "_continuous")
  theme_combinations <- expand.grid(theme_options, scale_options)
  themes <- paste0("theme_", theme_combinations$Var1, theme_combinations$Var2)

  for (theme in themes){
    theme_func <- get(theme)
    expect_equal(class(theme_func()), "list", info = paste0("Theme generating failure: ", theme))
  }

})
#
# test_that("direction arguments accept valid inputs and don't accept invalid", {
#
#   theme_options <- c(
#     # "share",
#     "capture",
#     "samy"
#     # "microsoft"
#     )
#   scale_options <- c("_discrete", "_continuous")
#   theme_combinations <- expand.grid(theme_options, scale_options)
#   themes <- paste0("theme_", theme_combinations$Var1, theme_combinations$Var2)
#
#   for (theme in themes){
#     theme_accepts_direction_args(theme = theme)
#   }
#
# })
#
