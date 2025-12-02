# theme_capture_discrete

Adds Capture colours and font to discrete plot.

## Usage

``` r
theme_capture_discrete(
  direction = c("forwards", "backwards"),
  guide = c("legend", "none"),
  fallback_font = "sans"
)
```

## Arguments

- direction:

  The direction of the colours in the scale. Set to -1 to reverse them.

- guide:

  An optional character string specifying the type of guide to use for
  discrete scales. Either is "legend", which is default, or "none" to
  remove the legend.

- fallback_font:

  Adds a fallback font of 'sans' in case user does not have required
  font.
