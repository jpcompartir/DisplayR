# Display Sentiment Over Time

This function generates a chart showing the sentiment distribution over
time based on a data frame, styled for gt.

## Usage

``` r
disp_gt_sent_time(
  data,
  sentiment_var = sentiment,
  date_var = date,
  chart_type = c("lines", "bars"),
  time_unit = c("week", "day", "month", "quarter", "year"),
  date_breaks = "4 months",
  date_labels = "%b"
)
```

## Arguments

- data:

  The input data frame.

- sentiment_var:

  The name of the variable in the data frame containing the sentiment
  values.

- date_var:

  The name of the variable in the data frame containing the dates.

- chart_type:

  The type of chart to be plotted. Default is "lines".

- time_unit:

  The time unit to group the data by. Default is "week".

- date_breaks:

  The breaks to be used on the x-axis for the date scale. Default is "4
  months".

- date_labels:

  The format of the labels on the x-axis for the date scale. Default is
  "%b".

## Value

A ggplot object representing the plotted chart.
