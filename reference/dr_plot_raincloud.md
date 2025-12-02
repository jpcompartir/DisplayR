# Quickly create a raincloud plot

Create a raincloud plot to provide maximal statistical information while
preserving the desired ‘inference at a glance’ nature of barplots and
other similar visualisation devices.

## Usage

``` r
dr_plot_raincloud(
  data,
  grouping_variable,
  continuous_variable,
  smoothness = 0.3
)
```

## Arguments

- data:

  A data frame that includes the grouping and continuous variable.

- grouping_variable:

  The variable we want to map colours to.

- continuous_variable:

  The continious variable we want to be displaying the distribution of.

- smoothness:

  A measure, which must be \> 0, representing the smoothness of the
  density plot (i.e. how closely you want the density to match the
  distribution). Default is 0.3, but too high values can oversmooth the
  data, and too low values can cause undersmoothing. Generally it is
  recommended to stick with values between 0.3-1.

## Value

A ggplot2 object

## Examples

``` r
## Smoothness default value of 0.3
iris %>%
  dr_plot_raincloud(grouping_variable = Species,
                    continuous_variable = Sepal.Length,
                    smoothness = 0.3)


## Smoothness changed to 0.8
iris %>%
  dr_plot_raincloud(grouping_variable = Species,
                    continuous_variable = Sepal.Length,
                    smoothness = 0.8)
```
