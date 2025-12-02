# theme_capture_continuous

Adds Capture colours and font to continuous plot.

## Usage

``` r
theme_capture_continuous(
  direction = c("forwards", "backwards"),
  guide = c("colourbar", "colorbar", "legend", "none"),
  fallback_font = "sans"
)
```

## Arguments

- direction:

  The direction of the colours in the scale. Set to -1 to reverse them.

- guide:

  The type of legend. Use "colourbar", "legend" or FALSE.

- fallback_font:

  Adds a fallback font of 'sans' in case user does not have required
  font.
