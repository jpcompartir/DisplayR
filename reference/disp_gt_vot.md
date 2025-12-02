# Create volume over time charts for use in gt tables

The plot this function output appears in the cells of a gt table. There
is custom styling applied to remove much of the detail, the output is
not

## Usage

``` r
disp_gt_vot(
  data,
  date_var,
  time_unit = c("week", "day", "month", "quarter", "year"),
  bar_colour = "#628EFD",
  date_breaks = "4 months",
  date_labels = "%b"
)
```

## Arguments

- data:

  The input data frame.

- date_var:

  The name of the variable in the data frame containing the dates.

- time_unit:

  The time unit to group the data by. Default is "week".

- bar_colour:

  The color of the bars in the plot. Default is "#628EFD".

- date_breaks:

  The breaks to be used on the x-axis for the date scale. Default is "4
  months".

- date_labels:

  The format of the labels on the x-axis for the date scale. Default is
  "%b".

## Value

A ggplot object representing the bar plot.
