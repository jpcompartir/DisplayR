# Create a Summary Table for gt Summary Process

This function creates a summary table as one step of the gt summary
process. The table summarizes the data based on a group variable and a
sentiment variable.

## Usage

``` r
make_gt_summary_table(data, group_var, sentiment_var)
```

## Arguments

- data:

  The input data frame.

- group_var:

  The name of the variable used for grouping the data.

- sentiment_var:

  The name of the variable representing the sentiment values.

## Value

A data frame containing the summary table.
