.setup_fonts <- function() {
  # Open source fonts that ship with package
  font_dir <- system.file("fonts", package = "DisplayR")

  sysfonts::font_add("montserrat",
                     regular = file.path(font_dir, "montserrat","static", "Montserrat-Regular.ttf"),
                     bold = file.path(font_dir, "montserrat","static", "Montserrat-Bold.ttf"),
                     italic = file.path(font_dir, "montserrat","static", "Montserrat-Italic.ttf"),
                     bolditalic = file.path(font_dir, "montserrat", "static", "Montserrat-BoldItalic.ttf")
  )

  sysfonts::font_add("source-sans-pro",
                     regular = file.path(font_dir, "source-sans-pro", "SourceSansPro-Regular.ttf"),
                     bold = file.path(font_dir, "source-sans-pro", "SourceSansPro-Bold.ttf"),
                     italic = file.path(font_dir, "source-sans-pro", "SourceSansPro-Italic.ttf"),
                     bolditalic = file.path(font_dir, "source-sans-pro", "SourceSansPro-BoldItalic.ttf")
  )


  # Check for proprietary fonts on system
  proprietary_fonts <- list(
    "Segoe UI" = "segoe-ui.ttf",
    "NeueHaasGroteskText Pro Md" = "NeueHaasGroteskTextProMd.ttf",
    "GT Walsheim Pro" = "GT-Walsheim-Pro-Regular.ttf"
  )

  for (font_name in names(proprietary_fonts)) {
    if (!font_name %in% sysfonts::font_families()) {
      warning(sprintf("Font '%s' not found on system, download and install them individually to access full package functionality", font_name))
    }
  }
}
