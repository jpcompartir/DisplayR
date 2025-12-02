# theme_samy_continuous

Adds Samy colours and font to continuous plot.

## Usage

``` r
theme_samy_continuous(
  index = NULL,
  direction = c("forwards", "backwards"),
  guide = c("legend", "colourbar", "colorbar", "none"),
  fallback_font = "sans"
)
```

## Arguments

- index:

  Choose palettes colours by index by setting index equal to a character
  vector e.g. c(1,2,3) or c(1:3)

- direction:

  The direction of the colours in the scale. Set to -1 to reverse them.

- guide:

  The type of legend. Use "colourbar", "legend" or FALSE.

- fallback_font:

  Adds a fallback font of 'sans' in case user does not have required
  font (Montserrat Regular).
