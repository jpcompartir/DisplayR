% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themes.R
\name{theme_share_discrete}
\alias{theme_share_discrete}
\title{theme_share_discrete}
\usage{
theme_share_discrete(
  index = NULL,
  guide = c("legend", "none"),
  fallback_font = "sans"
)
}
\arguments{
\item{index}{Choose palettes colours by index by setting index equal to a character vector e.g. c(1,2,3) or c(1:3)}

\item{guide}{An optional character string specifying the type of guide to use for discrete scales. Either is "legend", which is default, or "none" to remove the legend.}

\item{fallback_font}{Adds a fallback font of 'sans' in case user does not have required font (NeueHaasGroteskText Pro Md).}
}
\description{
Adds SHARE colours and font to discrete plot.
}
\keyword{internal}
