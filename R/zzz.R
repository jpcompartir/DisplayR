globals <- utils::globalVariables(c("plot_date", "n", "percent", "fp_text", ":=", "volume", "Sentiment x Time", "Neutral", "Volume x Time", "sentiment", "df", "group", "positive", "neutral", "negative", "Positive", "Negative", "Volume", "group_n", "percent_character", "topic", ".total", "palette", "pdfFonts", "Sepal.Length", "Sepal.Width"))


test_plot <- function() {plot <-
  plot <- ggplot2::ggplot(data = datasets::iris,
                  ggplot2::aes(x = Sepal.Length, y = Sepal.Width, colour = Sepal.Width)) +
  ggplot2::geom_point()

  return(plot)
}
