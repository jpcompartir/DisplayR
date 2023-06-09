% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots_sentiment.R
\name{dr_plot_sent_vot}
\alias{dr_plot_sent_vot}
\title{Plot a bar or line chart of sentiment over time}
\usage{
dr_plot_sent_vot(
  data,
  sentiment_var = sentiment,
  date_var = date,
  sentiment_colours = c(positive = "#107C10", negative = "#D83B01", neutral = "#FFB900"),
  plot_type = c("bar", "line"),
  time_unit = c("week", "day", "month", "quarter", "year")
)
}
\arguments{
\item{data}{A data frame that includes the grouping and sentiment variables.}

\item{sentiment_var}{The sentiment variable. Default is "sentiment".}

\item{date_var}{The variable to date by. Default is "date".}

\item{sentiment_colours}{Colour mapping for the sentiment categories}

\item{plot_type}{The type of plot. Default is "percent".}

\item{time_unit}{A single unit of time fed into lubridate::floor_date  "week", "day", "month","quarter", "year"}
}
\value{
ggplot object
}
\description{
Plot a bar or line chart of sentiment over time
}
\examples{
{
DisplayR::disp_example \%>\% dr_plot_sent_vot(plot_type = "bar",
time_unit = "day")
DisplayR::disp_example \%>\% dr_plot_sent_vot(plot_type = "line",
time_unit = "day")
}
}
