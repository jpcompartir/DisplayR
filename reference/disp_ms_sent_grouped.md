# Display a Stacked Bar Chart of Sentiment Grouped by a Variable

This function takes a data frame, a sentiment variable, and a group
variable, and creates a stacked bar chart representing the distribution
of sentiment. within each group, with the bars displayed horizontally
and the percentage of each sentiment shown.

## Usage

``` r
disp_ms_sent_grouped(
  data,
  sentiment_var,
  group_var,
  plot_type = c("percent", "volume")
)
```

## Arguments

- data:

  A data frame containing the sentiment and group variables.

- sentiment_var:

  The name of the sentiment variable in the data frame (as a string or
  unquoted symbol).

- group_var:

  The name of the group variable in the data frame (as a string or
  unquoted symbol).

- plot_type:

  Where raw volumes or percentages should be plotted.

## Value

A
[`mschart::ms_barchart`](https://ardata-fr.github.io/mschart/reference/ms_barchart.html)
object representing the stacked bar chart.
