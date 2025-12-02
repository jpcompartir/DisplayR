# Get unit-specific data for plotting and switch o reduce complexity

This function returns a list of values used for configuring the ggplot2
plot depending on the time unit. This includes date breaks, date labels,
plot title, and y-axis label. The 'variable' and 'unit' parameters allow
for flexibility in describing the data being plotted.

## Usage

``` r
vot_unit_data(
  time_unit = c("day", "week", "month", "quarter", "year"),
  vot_variable = "Volume of Mentions",
  unit = "Count"
)
```

## Arguments

- time_unit:

  A single unit of time fed into lubridate::floor_date.

- vot_variable:

  Will form part of plot's title, should describe type of plot

- unit:

  A character string used to describe the unit of the data in the y-axis
  label. Default is "Count".

## Value

A list with elements 'date_breaks', 'date_labels', 'title', 'yaxis'
