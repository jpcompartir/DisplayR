# Create a flextable for volume + sentiment with a gradient fill.

This function takes a data frame, the grouping variable, and the
sentiment variable to create a summary table and display it as a
flextable. The table contains the count and percentage of positive,
negative, and neutral sentiments per group. The positive and negative
columns are filled with a gradient color.

## Usage

``` r
disp_flextable(
  data,
  group_var,
  sentiment_var,
  positive_colour = "#107C10",
  negative_colour = "#D83B01"
)
```

## Arguments

- data:

  A data frame containing the group variable and sentiment variable.

- group_var:

  The name of the column in the data frame used for grouping.

- sentiment_var:

  The name of the column in the data frame containing sentiment values.

- positive_colour:

  (Optional) The colour for the positive gradient. Default is "#107C10".

- negative_colour:

  (Optional) The colour for the negative gradient. Default is "#D83B01".

## Value

A flextable object displaying the summary table with gradient colouring
for positive and negative sentiments.

## Details

The output of this function can be exported directly to PowerPoint.

The unique values within the sentiment variable should be c("positive",
"negative", "neutral"), the function will try to remove NAs, and lower
case the sentiment variable, but if you have values such as 'pos',
'neg', 'neu', you'll need to replace them with the values the function
expects.
