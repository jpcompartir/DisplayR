# Create a sentiment distribution bar chart

We prefer bars over pie charts or donuts, end.

## Usage

``` r
dr_plot_sent(
  data,
  sentiment_var = sentiment,
  bar_labels = c("percent", "volume", "none"),
  sentiment_colours = c(positive = "#107C10", negative = "#D83B01", neutral = "#FFB900")
)
```

## Arguments

- data:

  Data Frame or Tibble object

- sentiment_var:

  Variable containing 'positive', 'negative', 'neutral' sentiment
  classification

- bar_labels:

  Data labels for the bars, percent or volume

- sentiment_colours:

  Colour mapping for the sentiment categories

## Value

ggplot object of sentiment distribution

## Examples

``` r
{
DisplayR::disp_example %>% dr_plot_sent(sentiment_var =
sentiment,
bar_labels = "percent")
}
```
