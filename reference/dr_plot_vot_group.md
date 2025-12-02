# Quickly make a line plot of volume over time for a grouping variable

Quickly make a line plot of volume over time for a grouping variable

## Usage

``` r
dr_plot_vot_group(
  data,
  group_var,
  date_var = date,
  time_unit = c("day", "week", "month", "quarter", "year")
)
```

## Arguments

- data:

  Data frame or tibble

- group_var:

  grouping variable e.g. country, cluster, topic etc.

- date_var:

  Variable which contains date information (can be datetime too I think)

- time_unit:

  A single unit of time fed into lubridate::floor_date "week", "day",
  "month","quarter", "year"

## Value

ggplot object
