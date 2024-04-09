test_that("themes correctly edit legend title and aesthetics", {

  theme_options <- c(
    "share",
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
    "share",
    "capture", "samy", "microsoft")
  scale_options <- c("_discrete", "_continuous")
  theme_combinations <- expand.grid(theme_options, scale_options)
  themes <- c(paste0("theme_", theme_combinations$Var1, theme_combinations$Var2),
              paste0("dr_theme_", theme_options))

  for (theme in themes){
    theme_func <- get(theme)
    expect_equal(class(theme_func()), "list", info = paste0("Theme generating failure: ", theme))
  }

})

test_that("direction arguments accept valid inputs and don't accept invalid", {

  theme_options <- c(
    "theme_share_continuous",
    "theme_capture_continuous", "theme_capture_discrete",
    "theme_samy_continuous",
    "theme_microsoft_continuous"
    )

  for (theme in theme_options){
    theme_accepts_direction_args(theme = theme)
  }

})

test_that("themes correctly change palette direction", {

  theme_options <- c(
    "theme_share_continuous",
    "theme_capture_continuous", "theme_capture_discrete",
    "theme_samy_continuous",
    "theme_microsoft_continuous"
  )

  colour_forward <- c("#BC2E96", "#36AD7F",  "#36AD7F", "#D87C6D", "#327B4A")
  colour_backward <- c("#E54E71", "#30728D", "#30728D", "#F67C4C", "#618D0E")

  for (i in length(theme_options)){
    theme_edits_palette_direction(theme = theme_options[i], colour_forward[i], colour_backward[i])
  }
})

test_that("guide arguments accept valid inputs and don't accept invalid", {

  theme_options <- c("share", "capture", "samy", "microsoft")
  scale_options <- "_continuous"
  theme_combinations <- expand.grid(theme_options, scale_options)
  themes <- c(paste0("theme_", theme_combinations$Var1, theme_combinations$Var2),
              paste0("dr_theme_", theme_options))

  for (i in 1:length(themes)){

    theme <- themes[i]
    theme_func <- get(theme)

    if (i < 5){ # explicitly continuous themes
      expect_error(theme_func(guide = "a"),
                   regexp = "^\'arg\' should be one of",
                   info = paste0("Theme generating failure: ", theme))
    } else { # need to specify continuous scales for dr_* functions
      expect_error(theme_func(scale_type = "continuous",
                              guide = "a"),
                   regexp = "^\'arg\' should be one of",
                   info = paste0("Theme generating failure: ", theme))
    }

    # Valid guide type returns list
    expect_equal(class(theme_func(guide = "colourbar")), "list", info = paste0("Theme generating failure: ", theme))
    expect_equal(class(theme_func(guide = "legend")), "list", info = paste0("Theme generating failure: ", theme))

  }

})

test_that("scale_type arguments accept valid inputs and don't accept invalid inputs", {

  theme_options <- c("share", "capture", "samy", "microsoft")
  themes <- paste0("dr_theme_", theme_options)

  for (theme in themes){

    theme_func <- get(theme)

    # Invalid scale_type throws an error
    expect_error(
      theme_func(scale_type = "abcd"),
      "should be one of \"discrete\", \"continuous\"",
      info = paste0("Theme generating failure: ", theme)
    )

    # Valid scale_type argument return a list
    discrete_scale_type <- theme_func(scale_type = "discrete")
    expect_equal(class(discrete_scale_type), "list", info = paste0("Theme generating failure: ", theme))

    continuous_scale_type <- theme_func(scale_type = "continuous")
    expect_equal(class(continuous_scale_type), "list", info = paste0("Theme generating failure: ", theme))
  }


})
