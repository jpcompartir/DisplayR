# Create a gt_summary table fit for use in client briefs

Function calls various helper functions, @seealso
`make_gt_summary_table`. This function prepares a summary of a given
dataset grouped by a variable of interest and summarised based on
sentiment and time categories. The summary is presented in a 'gt' table
format. Additional summary columns may be created with user-specified
icons. The table may be customised with a specified title and source
note.

## Usage

``` r
disp_gt(
  data,
  sentiment_var,
  group_var,
  date_var,
  time_unit = c("month", "day", "week", "year", "quarter"),
  sentiment_max_colours = list(negative = "#C00000", positive = "#107C10"),
  icons = NULL,
  table_title = "Test",
  source_note = "source_note ="
)
```

## Arguments

- data:

  A data frame.

- sentiment_var:

  The variable in the data frame representing sentiment.

- group_var:

  The variable in the data frame to be used for grouping.

- date_var:

  The variable in the data frame representing the date.

- time_unit:

  Time unit for grouping. Default choices are "month", "day", "week",
  "year", "quarter". Default is "month".

- sentiment_max_colours:

  List of colours for positive and negative sentiments.

- icons:

  Optional vector of icons to be added to the summary table.

- table_title:

  Title of the table. Default is "Test".

- source_note:

  Note to be added at the bottom of the table indicating the source.

## Value

A gt object representing the summary table.

## Examples

``` r
if (FALSE) { # \dontrun{
disp_gt_summary(
  my_data, "sentiment", "group", "date", "month",
  list("negative" = "#C00000", "positive" = "#107C10"),
  NULL, "My Summary", "source_note"
)
} # }
```
