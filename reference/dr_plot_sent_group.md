# Plot sentiment grouped by a certain variable.

Plot sentiment grouped by a certain variable.

## Usage

``` r
dr_plot_sent_group(
  data,
  group_var = topic,
  sentiment_var = sentiment,
  plot_type = c("percent", "volume"),
  bar_labels = c("volume", "none", "percent"),
  sentiment_colours = c(positive = "#107C10", negative = "#D83B01", neutral = "#FFB900")
)
```

## Arguments

- data:

  A data frame that includes the grouping and sentiment variables.

- group_var:

  The variable to group by. Default is "topic".

- sentiment_var:

  The sentiment variable. Default is "sentiment".

- plot_type:

  The type of plot. Default is "percent".

- bar_labels:

  The type of labels to display on bars. Default is "volume".

- sentiment_colours:

  Colour mapping for the sentiment categories

## Value

A ggplot2 object.

## Examples

``` r
{
DisplayR::disp_example %>% dr_plot_sent_group(group_var = topic,
sentiment_var = sentiment,
plot_type = "percent",
bar_labels = "volume")

DisplayR::disp_example %>% dr_plot_sent_group(group_var = topic,
 sentiment_var = sentiment,
 plot_type = "volume",
 bar_labels = "none")
}
```
