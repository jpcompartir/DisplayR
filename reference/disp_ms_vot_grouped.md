# Display Time Series Chart of Grouped Data Volume

This function generates a time series chart of grouped data volume over
the specified time period. The chart is created using the mschart
package and is based on the provided data, date column, time period, and
date format. data should be unsummarised, as there is counting under the
hood, i.e. your data should be in long form and the date and group
column should not already be counted.

## Usage

``` r
disp_ms_vot_grouped(
  data,
  date,
  group_var,
  time = c("week", "day", "month", "quarter", "year"),
  date_format = "yyyy/dd/mm"
)
```

## Arguments

- data:

  A data frame containing the data to be plotted.

- date:

  The name of the date column in the data frame.

- group_var:

  The name of the variable you wish to group by, e.g. sentiment or
  topic.

- time:

  A character vector specifying the time unit to be used for aggregation
  (default is "week"). Supported values are "day", "week", "month",
  "quarter", and "year".

- date_format:

  A character string specifying the format for displaying dates on the
  x-axis (default is "d/m/yy").

## Value

A mschart object representing the time series chart of data volume over
the specified time period.
