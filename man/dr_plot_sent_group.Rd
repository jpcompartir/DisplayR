% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots_sentiment.R
\name{dr_plot_sent_group}
\alias{dr_plot_sent_group}
\title{Plot sentiment grouped by a certain variable.}
\usage{
dr_plot_sent_group(
  data,
  group_var = topic,
  sentiment_var = sentiment,
  plot_type = c("percent", "volume"),
  bar_labels = c("volume", "none", "percent"),
  sentiment_colours = c(positive = "#107C10", negative = "#D83B01", neutral = "#FFB900")
)
}
\arguments{
\item{data}{A data frame that includes the grouping and sentiment variables.}

\item{group_var}{The variable to group by. Default is "topic".}

\item{sentiment_var}{The sentiment variable. Default is "sentiment".}

\item{plot_type}{The type of plot. Default is "percent".}

\item{bar_labels}{The type of labels to display on bars. Default is "volume".}

\item{sentiment_colours}{Colour mapping for the sentiment categories}
}
\value{
A ggplot2 object.
}
\description{
Plot sentiment grouped by a certain variable.
}
\examples{
{
DisplayR::disp_example \%>\% dr_plot_sent_group(group_var = topic,
sentiment_var = sentiment,
plot_type = "percent",
bar_labels = "volume")

DisplayR::disp_example \%>\% dr_plot_sent_group(group_var = topic,
 sentiment_var = sentiment,
 plot_type = "volume",
 bar_labels = "none")
}
}
