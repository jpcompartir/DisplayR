# theme_microsoft_discrete

Adds Microsoft colours and font to discrete plot.

## Usage

``` r
theme_microsoft_discrete(
  index = NULL,
  guide = c("legend", "none"),
  fallback_font = "sans"
)
```

## Arguments

- index:

  Choose palettes colours by index by setting index equal to a character
  vector e.g. c(1,2,3) or c(1:3)

- guide:

  An optional character string specifying the type of guide to use for
  discrete scales. Either is "legend", which is default, or "none" to
  remove the legend.

- fallback_font:

  Adds a fallback font of 'sans' in case user does not have required
  font (Segoe UI)
